# ============================================================
# worldcup_predictions.R
# Predict the Kicktipp-optimal scoreline for today's & tomorrow's
# World Cup matches from real correct-score betting odds (API-Football).
#
# Primary method ("market"): pull the bookmakers' Correct Score market for
# each fixture, devig it, average across books -> probability for every
# quoted scoreline -> expected Kicktipp points for each candidate score.
# Fallback ("model"): if a match has no usable correct-score quotes, fit an
# independent Poisson model to that fixture's Match Winner + Over/Under odds.
#
# Kicktipp scoring (max applicable tier):
#   4 exact score | 3 right non-draw goal difference |
#   2 right tendency (win/draw/loss) | 0 otherwise.
#
# Data: API-Football (https://v3.football.api-sports.io), free tier 100 req/day.
# Auth: header x-apisports-key from env APIFOOTBALL_KEY.
#
# Run:  Rscript scripts/worldcup_predictions.R          (live; needs APIFOOTBALL_KEY + curl)
#       MOCK=1 Rscript scripts/worldcup_predictions.R   (offline; uses sample_odds.json)
# Runtime: MOCK ~1 s; live ~1-2 min (one odds call per fixture, ~7 s apart).
# ============================================================

# Block A: configuration & paths ----
suppressMessages(library(jsonlite))

.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", grep("^--file=", .args, value = TRUE))
script_dir <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
repo_root  <- normalizePath(file.path(script_dir, ".."))

data_out    <- file.path(repo_root, "_data", "worldcup.json")
preview_out <- file.path(dirname(repo_root), "worldcup_preview.html")

API_BASE     <- "https://v3.football.api-sports.io"
API_KEY      <- Sys.getenv("APIFOOTBALL_KEY")
TZ           <- "Europe/Berlin"   # "today"/"tomorrow" are defined in this zone
REQ_DELAY    <- 7                  # seconds between live odds calls (free per-minute cap)

MAXG         <- 15     # goals grid for the score distribution & Poisson fitting
PRED_MAX     <- 6      # candidate predicted scores range 0..PRED_MAX
DEFAULT_LINE <- 2.5    # totals line for the model fallback
MIN_SCORES   <- 4      # min quoted scorelines for a bookmaker's correct-score to count
USE_MOCK     <- nzchar(Sys.getenv("MOCK"))
MOCK <- if (USE_MOCK) fromJSON(file.path(script_dir, "sample_odds.json"),
                               simplifyVector = FALSE) else NULL

# score index matrices (home goals down rows, away goals across cols)
gi <- matrix(0:MAXG, nrow = MAXG + 1, ncol = MAXG + 1)
gj <- matrix(0:MAXG, nrow = MAXG + 1, ncol = MAXG + 1, byrow = TRUE)

# Block B: model & scoring helpers ----

# scoreline probability matrix for independent Poisson rates (lh, la)
score_matrix <- function(lh, la) {
  M <- outer(dpois(0:MAXG, lh), dpois(0:MAXG, la))
  M / sum(M)
}

# aggregate market probabilities implied by (lh, la) at totals line `line`
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

# Kicktipp points matrix for predicting (ph, pa) against every actual score
kicktipp_matrix <- function(ph, pa) {
  pd <- ph - pa; ad <- gi - gj
  pts <- matrix(0L, MAXG + 1, MAXG + 1)
  pts[sign(ad) == sign(pd)] <- 2L         # correct tendency (win / draw / loss)
  if (pd != 0) pts[ad == pd] <- 3L        # right non-draw goal difference
  pts[gi == ph & gj == pa] <- 4L          # exact score
  pts
}

CANDS    <- expand.grid(ph = 0:PRED_MAX, pa = 0:PRED_MAX)
PTS_LIST <- Map(kicktipp_matrix, CANDS$ph, CANDS$pa)

# top-n predicted scores by expected points, given score distribution M
best_predictions <- function(M, n = 3) {
  ep  <- vapply(PTS_LIST, function(P) sum(P * M), numeric(1))
  ord <- order(-ep)[seq_len(n)]
  data.frame(ph = CANDS$ph[ord], pa = CANDS$pa[ord], ep = ep[ord])
}

# Block C: API-Football transport & parsing ----

api_get <- function(path) {
  if (!requireNamespace("curl", quietly = TRUE))
    stop("Package 'curl' is required for live mode (install.packages('curl')).")
  h <- curl::new_handle()
  curl::handle_setheaders(h, "x-apisports-key" = API_KEY)
  r <- curl::curl_fetch_memory(paste0(API_BASE, path), handle = h)
  if (r$status_code != 200)
    stop(sprintf("API-Football %s -> HTTP %d", path, r$status_code))
  txt <- rawToChar(r$content); Encoding(txt) <- "UTF-8"
  o <- fromJSON(txt, simplifyVector = FALSE)
  if (length(o$errors)) stop("API-Football error: ", paste(unlist(o$errors), collapse = "; "))
  o
}

# find the FIFA World Cup league id + current season (not the qualifiers)
find_league <- function() {
  lg <- api_get("/leagues?search=World%20Cup")
  for (it in lg$response) {
    if (identical(it$league$name, "World Cup") && identical(it$country$name, "World")) {
      cur <- NULL
      for (s in it$seasons) if (isTRUE(s$current)) cur <- as.integer(s$year)
      if (is.null(cur)) cur <- max(vapply(it$seasons, function(s) as.integer(s$year), integer(1)))
      return(list(id = it$league$id, season = cur))
    }
  }
  list(id = 1, season = as.integer(format(today, "%Y")))   # fallback
}

# fixtures within a UTC window around today/tomorrow -> list(id, ts, home, away)
get_fixtures <- function() {
  if (USE_MOCK) {
    fxs <- MOCK$fixtures; n <- length(fxs); out <- vector("list", n)
    for (i in seq_len(n)) {
      off  <- if (i <= ceiling(n / 2)) 0 else 1
      base <- as.POSIXct(format(Sys.time() + off * 86400, tz = TZ, "%Y-%m-%d"), tz = TZ)
      out[[i]] <- list(id = fxs[[i]]$id, ts = as.numeric(base + (17 + i) * 3600),
                       home = fxs[[i]]$home, away = fxs[[i]]$away)
    }
    return(out)
  }
  if (!nzchar(API_KEY)) stop("APIFOOTBALL_KEY is not set (and MOCK is off).")
  lige <- find_league()
  fx <- api_get(sprintf("/fixtures?league=%d&season=%d&from=%s&to=%s",
                        lige$id, lige$season,
                        format(today - 1, "%Y-%m-%d"), format(today + 1, "%Y-%m-%d")))
  lapply(fx$response, function(f) list(id = f$fixture$id, ts = f$fixture$timestamp,
                                       home = f$teams$home$name, away = f$teams$away$name))
}

# raw odds object for one fixture (a list with $bookmakers)
get_fixture_markets <- function(id) {
  if (USE_MOCK) return(MOCK$odds[[as.character(id)]])
  if (REQ_DELAY > 0) Sys.sleep(REQ_DELAY)
  resp <- api_get(sprintf("/odds?fixture=%d", id))
  if (!length(resp$response)) return(list(bookmakers = list()))
  resp$response[[1]]
}

# find a bet whose name matches any of `nm` within a bookmaker's bets
.find_bet <- function(bk, nm) {
  for (b in bk$bets) if (b$name %in% nm) return(b)
  NULL
}

# devig correct-score odds across bookmakers -> scores + probs (or NULL)
parse_correct_score <- function(mkts) {
  acc <- list(); nbk <- 0
  for (bk in mkts$bookmakers) {
    cs <- .find_bet(bk, c("Correct Score", "Exact Score")); if (is.null(cs)) next
    sc <- character(0); pr <- numeric(0)
    for (v in cs$values) {
      m  <- regmatches(v$value, regexec("^\\s*(\\d+)\\s*[:\\-]\\s*(\\d+)\\s*$", v$value))[[1]]
      od <- suppressWarnings(as.numeric(v$odd))
      if (length(m) == 3 && is.finite(od) && od > 1) {
        sc <- c(sc, paste0(m[2], ":", m[3])); pr <- c(pr, 1 / od)
      }
    }
    if (length(pr) < MIN_SCORES) next
    pr <- pr / sum(pr)                       # devig within this bookmaker
    for (k in seq_along(sc)) acc[[sc[k]]] <- c(acc[[sc[k]]], pr[k])
    nbk <- nbk + 1
  }
  if (nbk == 0) return(NULL)
  probs <- vapply(acc, mean, numeric(1))     # average across bookmakers
  list(scores = names(acc), probs = probs / sum(probs), nbk = nbk)
}

# build score matrix M from devigged correct-score probabilities
build_market_M <- function(cs) {
  M <- matrix(0, MAXG + 1, MAXG + 1)
  for (k in seq_along(cs$scores)) {
    ij <- as.integer(strsplit(cs$scores[k], ":")[[1]])
    if (ij[1] <= MAXG && ij[2] <= MAXG) M[ij[1] + 1, ij[2] + 1] <- cs$probs[k]
  }
  if (sum(M) == 0) return(NULL)
  M / sum(M)
}

# devig Match Winner (+ Over/Under 2.5) across bookmakers for the model fallback
parse_1x2_ou <- function(mkts) {
  Hs <- Ds <- As <- ov <- un <- numeric(0)
  for (bk in mkts$bookmakers) {
    mw <- .find_bet(bk, "Match Winner")
    if (!is.null(mw)) {
      h <- d <- a <- NA_real_
      for (v in mw$values) {
        od <- suppressWarnings(as.numeric(v$odd))
        if (identical(v$value, "Home")) h <- od
        else if (identical(v$value, "Draw")) d <- od
        else if (identical(v$value, "Away")) a <- od
      }
      if (all(is.finite(c(h, d, a)))) {
        p <- 1 / c(h, d, a); p <- p / sum(p)
        Hs <- c(Hs, p[1]); Ds <- c(Ds, p[2]); As <- c(As, p[3])
      }
    }
    ou <- .find_bet(bk, "Goals Over/Under")
    if (!is.null(ou)) {
      o <- u <- NA_real_
      for (v in ou$values) {
        od <- suppressWarnings(as.numeric(v$odd))
        if (identical(v$value, "Over 2.5")) o <- od
        else if (identical(v$value, "Under 2.5")) u <- od
      }
      if (all(is.finite(c(o, u)))) {
        p <- 1 / c(o, u); p <- p / sum(p); ov <- c(ov, p[1]); un <- c(un, p[2])
      }
    }
  }
  if (!length(Hs)) return(NULL)
  out <- list(H = mean(Hs), D = mean(Ds), A = mean(As))
  if (length(ov)) { out$line <- 2.5; out$over <- mean(ov); out$under <- mean(un) }
  out
}

# Block D: assemble predictions for today & tomorrow ----

now_local <- Sys.time()
today     <- as.Date(format(now_local, tz = TZ, "%Y-%m-%d"))
fixtures  <- get_fixtures()

matches <- list()
for (f in fixtures) {
  ko      <- as.POSIXct(f$ts, origin = "1970-01-01", tz = "UTC")
  ko_date <- as.Date(format(ko, tz = TZ, "%Y-%m-%d"))
  if (is.na(ko_date) || !(ko_date %in% c(today, today + 1))) next

  mkts <- get_fixture_markets(f$id)
  M <- NULL; method <- NA_character_; nbk <- 0L
  cs <- parse_correct_score(mkts)
  if (!is.null(cs)) { M <- build_market_M(cs); if (!is.null(M)) { method <- "market"; nbk <- cs$nbk } }
  if (is.null(M)) {
    mk <- parse_1x2_ou(mkts)
    if (!is.null(mk)) { lam <- fit_lambdas(mk); M <- score_matrix(lam["lh"], lam["la"]); method <- "model" }
  }
  if (is.null(M)) next

  ph <- sum(M[gi > gj]); pd <- sum(M[gi == gj]); pa <- sum(M[gi < gj])
  bp <- best_predictions(M, 3)
  matches[[length(matches) + 1]] <- list(
    home = f$home, away = f$away,
    kickoff  = format(ko, tz = TZ, "%a %d %b, %H:%M"),
    day      = if (ko_date == today) "Today" else "Tomorrow",
    sort_key = format(ko, tz = TZ, "%Y%m%d%H%M"),
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

# shared card CSS -- keep visually in sync with the <style> block in pages/worldcup.md
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
    sprintf("per-score market odds (%d bookmakers)", m$n_books)
  else "modelled from 1X2 + O/U (no per-score market)"
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
  '<p class="nomatch">No World Cup matches scheduled for today or tomorrow.</p>'
}

preview_html <- sprintf(paste0(
  '<!DOCTYPE html>\n<html lang="en"><head><meta charset="utf-8">\n',
  '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
  '<title>World Cup Kicktipp Predictor &mdash; preview</title>\n<style>\n',
  'body{font-family:"Open Sans",Arial,sans-serif;max-width:760px;margin:2rem auto;',
  'padding:0 1rem;color:#333;}\nh1{font-size:1.6rem;color:#018F59;margin-bottom:0.2rem;}\n',
  '.sub{color:#666;margin-top:0;font-size:0.95rem;}\n%s\n</style></head><body>\n',
  '<h1>World Cup Kicktipp Predictor</h1>\n',
  '<p class="sub">Expected-points-optimal scores for today\'s &amp; tomorrow\'s matches',
  ' &middot; local preview</p>\n<p class="updated">Last updated: %s</p>\n%s\n',
  '<p style="font-size:0.75rem;color:#999;margin-top:2rem;">Preview generated by ',
  'scripts/worldcup_predictions.R &mdash; the live page renders the same data via Jekyll.</p>\n',
  '</body></html>\n'), CARD_CSS, updated, body)

writeLines(preview_html, preview_out)
cat("Wrote", preview_out, "\n")
