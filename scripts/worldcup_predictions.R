# ============================================================
# worldcup_predictions.R
# Predict the Kicktipp-optimal scoreline for today's & tomorrow's
# World Cup matches from real correct-score betting odds (odds-api.io).
#
# Primary method ("market"): pull each fixture's Correct Score market
# across bookmakers, devig it, average -> probability for every quoted
# scoreline -> expected Kicktipp points for each candidate score.
# Fallback ("model"): if a match has no usable correct-score quotes, fit
# an independent Poisson model to its match-winner (ML) odds.
#
# Kicktipp scoring (max applicable tier):
#   4 exact score | 3 right non-draw goal difference |
#   2 right tendency (win/draw/loss) | 0 otherwise.
#
# Data: odds-api.io (https://api.odds-api.io/v3), free tier 5000 req/hour.
# Auth: apiKey query parameter from env ODDSAPIIO_KEY.
#
# Run:  Rscript scripts/worldcup_predictions.R          (live; needs ODDSAPIIO_KEY)
#       MOCK=1 Rscript scripts/worldcup_predictions.R   (offline; uses sample_odds.json)
# Runtime: MOCK ~1 s; live ~10-20 s (one odds call per fixture).
# ============================================================

# Block A: configuration & paths ----
suppressMessages(library(jsonlite))

.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", grep("^--file=", .args, value = TRUE))
script_dir <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
repo_root  <- normalizePath(file.path(script_dir, ".."))

data_out    <- file.path(repo_root, "_data", "worldcup.json")
preview_out <- file.path(dirname(repo_root), "worldcup_preview.html")

API_BASE         <- "https://api.odds-api.io/v3"
API_KEY          <- Sys.getenv("ODDSAPIIO_KEY")
SPORT            <- "football"
WC_SLUG_FALLBACK <- "international-fifa-world-cup"
PREF_BOOKMAKERS  <- c("Bet365")  # free plan allows up to 2 selected bookmakers; Bet365 has full correct-score
TZ               <- "Europe/Berlin"   # "today"/"tomorrow" are defined in this zone

MAXG         <- 15     # goals grid for the score distribution & Poisson fitting
PRED_MAX     <- 6      # candidate predicted scores range 0..PRED_MAX
DEFAULT_LINE <- 2.5    # totals line for the model fallback (unused without O/U)
MIN_SCORES   <- 4      # min quoted scorelines for a bookmaker's correct-score to count
USE_MOCK     <- nzchar(Sys.getenv("MOCK"))
MOCK <- if (USE_MOCK) fromJSON(file.path(script_dir, "sample_odds.json"),
                               simplifyVector = FALSE) else NULL

# score index matrices (home goals down rows, away goals across cols)
gi <- matrix(0:MAXG, nrow = MAXG + 1, ncol = MAXG + 1)
gj <- matrix(0:MAXG, nrow = MAXG + 1, ncol = MAXG + 1, byrow = TRUE)

# Block B: model & scoring helpers ----

score_matrix <- function(lh, la) {
  M <- outer(dpois(0:MAXG, lh), dpois(0:MAXG, la)); M / sum(M)
}

model_probs <- function(lh, la, line = DEFAULT_LINE) {
  M <- score_matrix(lh, la); tot <- gi + gj
  list(H = sum(M[gi > gj]), D = sum(M[gi == gj]), A = sum(M[gi < gj]),
       over = sum(M[tot > line]), under = sum(M[tot < line]))
}

# fit (lh, la) to devigged 1X2 (+ O/U if present) by least squares
fit_lambdas <- function(mk) {
  has_tot <- !is.null(mk$over)
  line <- if (has_tot) mk$line else DEFAULT_LINE
  obj <- function(th) {
    m <- model_probs(exp(th[1]), exp(th[2]), line)
    e <- (m$H - mk$H)^2 + (m$D - mk$D)^2 + (m$A - mk$A)^2
    if (has_tot) e <- e + (m$over - mk$over)^2 + (m$under - mk$under)^2
    e
  }
  op <- optim(c(log(1.3), log(1.1)), obj, method = "BFGS")
  c(lh = exp(op$par[1]), la = exp(op$par[2]))
}

kicktipp_matrix <- function(ph, pa) {
  pd <- ph - pa; ad <- gi - gj
  pts <- matrix(0L, MAXG + 1, MAXG + 1)
  pts[sign(ad) == sign(pd)] <- 2L
  if (pd != 0) pts[ad == pd] <- 3L
  pts[gi == ph & gj == pa] <- 4L
  pts
}

CANDS    <- expand.grid(ph = 0:PRED_MAX, pa = 0:PRED_MAX)
PTS_LIST <- Map(kicktipp_matrix, CANDS$ph, CANDS$pa)

best_predictions <- function(M, n = 3) {
  ep  <- vapply(PTS_LIST, function(P) sum(P * M), numeric(1))
  ord <- order(-ep)[seq_len(n)]
  data.frame(ph = CANDS$ph[ord], pa = CANDS$pa[ord], ep = ep[ord])
}

# Block C: odds-api.io transport & parsing ----

api_get <- function(path) {
  if (USE_MOCK) stop("api_get called in MOCK mode")
  full <- paste0(API_BASE, path)
  txt <- tryCatch({
    con <- url(full, method = "libcurl", open = "rb")
    on.exit(close(con))
    rawToChar(readBin(con, "raw", n = 1e7))
  }, error = function(e) stop(sprintf("odds-api.io %s failed: %s",
                                      sub("&apiKey=.*", "", path), conditionMessage(e))))
  Encoding(txt) <- "UTF-8"
  obj <- fromJSON(txt, simplifyVector = FALSE)
  if (!is.null(obj$error))
    stop(sprintf("odds-api.io %s: %s", sub("&apiKey=.*", "", path), obj$error))
  obj
}

# discover the FIFA World Cup league slug (not qualifiers / women / youth)
get_wc_slug <- function() {
  ls <- api_get(sprintf("/leagues?sport=%s&apiKey=%s", SPORT, API_KEY))
  for (L in ls) {
    nm <- L$name
    if (!is.null(nm) && grepl("FIFA World Cup", nm, ignore.case = TRUE) &&
        !grepl("Qualif|Wom|Beach|Futsal|U-?1[0-9]|U-?2[0-9]", nm, ignore.case = TRUE))
      return(L$slug)
  }
  WC_SLUG_FALLBACK
}

# upcoming WC fixtures -> list(id, home, away, ts)
get_events <- function() {
  if (USE_MOCK) {
    evs <- MOCK$events; n <- length(evs); out <- vector("list", n)
    for (i in seq_len(n)) {
      off  <- if (i <= ceiling(n / 2)) 0 else 1
      base <- as.POSIXct(format(Sys.time() + off * 86400, tz = TZ, "%Y-%m-%d"), tz = TZ)
      out[[i]] <- list(id = evs[[i]]$id, home = evs[[i]]$home, away = evs[[i]]$away,
                       ts = base + (17 + i) * 3600)
    }
    return(out)
  }
  if (!nzchar(API_KEY)) stop("ODDSAPIIO_KEY is not set (and MOCK is off).")
  ev <- api_get(sprintf("/events?sport=%s&league=%s&apiKey=%s", SPORT, get_wc_slug(), API_KEY))
  out <- list()
  for (e in ev) {
    if (identical(e$status, "settled")) next
    ts <- as.POSIXct(e$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (is.na(ts)) next
    out[[length(out) + 1]] <- list(id = e$id, home = e$home, away = e$away, ts = ts)
  }
  out
}

# raw odds object for one fixture (has $bookmakers keyed by bookmaker name)
get_event_odds <- function(id) {
  if (USE_MOCK) return(MOCK$odds[[as.character(id)]])
  api_get(sprintf("/odds?eventId=%s&bookmakers=%s&apiKey=%s", id, BK_PARAM, API_KEY))
}

.find_market <- function(markets, nm) {
  for (m in markets) if (m$name %in% nm) return(m)
  NULL
}

# devig the Correct Score market across bookmakers -> scores + probs (or NULL)
parse_correct_score <- function(od) {
  bms <- od$bookmakers; if (is.null(bms)) return(NULL)
  acc <- list(); nbk <- 0
  for (bk in names(bms)) {
    cs <- .find_market(bms[[bk]], "Correct Score"); if (is.null(cs)) next
    sc <- character(0); pr <- numeric(0)
    for (o in cs$odds) {
      m  <- regmatches(o$label, regexec("^\\s*(\\d+)\\s*[-:]\\s*(\\d+)\\s*$", o$label))[[1]]
      pd <- suppressWarnings(as.numeric(o$odds))
      if (length(m) == 3 && is.finite(pd) && pd > 1) {
        sc <- c(sc, paste0(m[2], ":", m[3])); pr <- c(pr, 1 / pd)
      }
    }
    if (length(pr) < MIN_SCORES) next
    pr <- pr / sum(pr)
    for (k in seq_along(sc)) acc[[sc[k]]] <- c(acc[[sc[k]]], pr[k])
    nbk <- nbk + 1
  }
  if (nbk == 0) return(NULL)
  probs <- vapply(acc, mean, numeric(1))
  list(scores = names(acc), probs = probs / sum(probs), nbk = nbk)
}

build_market_M <- function(cs) {
  M <- matrix(0, MAXG + 1, MAXG + 1)
  for (k in seq_along(cs$scores)) {
    ij <- as.integer(strsplit(cs$scores[k], ":")[[1]])
    if (ij[1] <= MAXG && ij[2] <= MAXG) M[ij[1] + 1, ij[2] + 1] <- cs$probs[k]
  }
  if (sum(M) == 0) return(NULL)
  M / sum(M)
}

# devig match-winner (ML) across bookmakers for the model fallback
parse_ml <- function(od) {
  bms <- od$bookmakers; if (is.null(bms)) return(NULL)
  Hs <- Ds <- As <- numeric(0)
  for (bk in names(bms)) {
    ml <- .find_market(bms[[bk]], "ML"); if (is.null(ml) || !length(ml$odds)) next
    o <- ml$odds[[1]]
    h <- suppressWarnings(as.numeric(o$home)); d <- suppressWarnings(as.numeric(o$draw))
    a <- suppressWarnings(as.numeric(o$away))
    if (all(is.finite(c(h, d, a)))) {
      p <- 1 / c(h, d, a); p <- p / sum(p)
      Hs <- c(Hs, p[1]); Ds <- c(Ds, p[2]); As <- c(As, p[3])
    }
  }
  if (!length(Hs)) return(NULL)
  list(H = mean(Hs), D = mean(Ds), A = mean(As))
}

# Block D: assemble predictions for today & tomorrow ----

now_local <- Sys.time()
today     <- as.Date(format(now_local, tz = TZ, "%Y-%m-%d"))
BK_PARAM  <- paste(gsub(" ", "%20", PREF_BOOKMAKERS), collapse = ",")
events    <- get_events()

matches <- list()
for (e in events) {
  ko_date <- as.Date(format(e$ts, tz = TZ, "%Y-%m-%d"))
  if (is.na(ko_date) || !(ko_date %in% c(today, today + 1, today + 2))) next

  od <- get_event_odds(e$id)
  M <- NULL; method <- NA_character_; nbk <- 0L
  cs <- parse_correct_score(od)
  if (!is.null(cs)) { M <- build_market_M(cs); if (!is.null(M)) { method <- "market"; nbk <- cs$nbk } }
  if (is.null(M)) {
    mk <- parse_ml(od)
    if (!is.null(mk)) { lam <- fit_lambdas(mk); M <- score_matrix(lam["lh"], lam["la"]); method <- "model" }
  }
  if (is.null(M)) next

  ph <- sum(M[gi > gj]); pd <- sum(M[gi == gj]); pa <- sum(M[gi < gj])
  bp <- best_predictions(M, 3)
  matches[[length(matches) + 1]] <- list(
    home = e$home, away = e$away,
    kickoff  = format(e$ts, tz = TZ, "%a %d %b, %H:%M"),
    day      = if (ko_date == today) "Today" else if (ko_date == today + 1) "Tomorrow" else "Day after",
    sort_key = format(e$ts, tz = TZ, "%Y%m%d%H%M"),
    method = method, n_books = nbk,
    exp_goals_home = round(sum(M * gi), 2), exp_goals_away = round(sum(M * gj), 2),
    p_home = round(ph, 3), p_draw = round(pd, 3), p_away = round(pa, 3),
    predictions = lapply(seq_len(nrow(bp)), function(i)
      list(score = sprintf("%d:%d", bp$ph[i], bp$pa[i]),
           points = sprintf("%.2f", bp$ep[i]))))
}
if (length(matches))
  matches <- matches[order(vapply(matches, function(m) m$sort_key, character(1)))]

# Block E: write JSON data & standalone preview ----

updated <- format(now_local, tz = TZ, "%a %d %b %Y, %H:%M %Z")
dir.create(dirname(data_out), showWarnings = FALSE, recursive = TRUE)
writeLines(toJSON(list(updated = updated, timezone = TZ, matches = matches),
                  auto_unbox = TRUE, pretty = TRUE), data_out)
cat("Wrote", data_out, "(", length(matches), "matches )\n")

CARD_CSS <- '
.updated { font-size:0.8rem; color:#777; margin-bottom:1rem; }
.match-card { border:1px solid #e0e0e0; border-radius:6px; background:#fafafa;
  padding:0.85rem 1rem; margin-bottom:1rem; }
.match-head { display:flex; justify-content:space-between; align-items:baseline;
  flex-wrap:wrap; gap:0.4rem; }
.match-head .teams { font-weight:600; font-size:1.05rem; }
.match-head .vs { color:#999; font-weight:400; font-size:0.85rem; margin:0 0.2rem; }
.match-head .when { font-size:0.82rem; color:#555; }
.match-head .day { display:inline-block; background:#018F59; color:#fff; border-radius:3px;
  padding:0 0.4rem; font-size:0.72rem; font-weight:600; margin-right:0.3rem; }
.model { font-size:0.8rem; color:#666; margin:0.4rem 0 0.6rem; }
.src { font-style:italic; }
table.preds { width:100%; border-collapse:collapse; font-size:0.9rem; }
table.preds th, table.preds td { text-align:left; padding:0.3rem 0.5rem; border:none; }
table.preds thead th { font-size:0.72rem; text-transform:uppercase; letter-spacing:0.03em;
  color:#999; border-bottom:1px solid #e0e0e0; }
table.preds td.score { font-weight:600; font-variant-numeric:tabular-nums; }
table.preds td.pts { text-align:right; font-variant-numeric:tabular-nums; }
table.preds tr.top td { background:#eaf7f0; }
table.preds tr.top td.score { color:#018F59; }
.nomatch { color:#777; font-style:italic; }'

src_label <- function(m) {
  if (identical(m$method, "market"))
    sprintf("per-score market odds (%d bookmaker%s)", m$n_books, if (m$n_books == 1) "" else "s")
  else "modelled from match-winner odds (no per-score market)"
}

render_card <- function(m) {
  rows <- paste(vapply(seq_along(m$predictions), function(i) {
    p <- m$predictions[[i]]
    sprintf('<tr%s><td>%d</td><td class="score">%s</td><td class="pts">%s</td></tr>',
            if (i == 1) ' class="top"' else "", i, p$score, p$points)
  }, character(1)), collapse = "\n")
  sprintf(paste0('<div class="match-card">\n',
    '  <div class="match-head"><span class="teams">%s <span class="vs">vs</span> %s</span>',
    '<span class="when"><span class="day">%s</span> %s</span></div>\n',
    '  <div class="model"><span class="src">%s</span> &nbsp;|&nbsp; 1 %.0f%% &middot; X %.0f%%',
    ' &middot; 2 %.0f%% &nbsp;|&nbsp; expected goals %.2f : %.2f</div>\n',
    '  <table class="preds"><thead><tr><th>#</th><th>Score</th><th>Exp. pts</th></tr></thead>',
    '<tbody>\n%s\n</tbody></table>\n</div>'),
    m$home, m$away, m$day, m$kickoff, src_label(m),
    100 * m$p_home, 100 * m$p_draw, 100 * m$p_away,
    m$exp_goals_home, m$exp_goals_away, rows)
}

body <- if (length(matches)) {
  paste(vapply(matches, render_card, character(1)), collapse = "\n")
} else {
  '<p class="nomatch">No World Cup matches scheduled in the next three days.</p>'
}

preview_html <- sprintf(paste0(
  '<!DOCTYPE html>\n<html lang="en"><head><meta charset="utf-8">\n',
  '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
  '<title>World Cup Kicktipp Predictor &mdash; preview</title>\n<style>\n',
  'body{font-family:"Open Sans",Arial,sans-serif;max-width:760px;margin:2rem auto;',
  'padding:0 1rem;color:#333;}\nh1{font-size:1.6rem;color:#018F59;margin-bottom:0.2rem;}\n',
  '.sub{color:#666;margin-top:0;font-size:0.95rem;}\n%s\n</style></head><body>\n',
  '<h1>World Cup Kicktipp Predictor</h1>\n',
  '<p class="sub">Expected-points-optimal scores for today, tomorrow &amp; the day after',
  ' &middot; local preview</p>\n<p class="updated">Last updated: %s</p>\n%s\n',
  '<p style="font-size:0.75rem;color:#999;margin-top:2rem;">Preview generated by ',
  'scripts/worldcup_predictions.R &mdash; the live page renders the same data via Jekyll.</p>\n',
  '</body></html>\n'), CARD_CSS, updated, body)

writeLines(preview_html, preview_out)
cat("Wrote", preview_out, "\n")
