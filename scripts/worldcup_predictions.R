# ============================================================
# worldcup_predictions.R
# Predict the Kicktipp-optimal scoreline for today's & tomorrow's
# World Cup matches from free 1X2 + over/under betting odds.
#
# Pipeline: fetch odds (The Odds API) -> devig -> fit independent
# Poisson (lambda_home, lambda_away) -> full scoreline distribution
# -> expected Kicktipp points for every candidate score -> top 3.
# Writes _data/worldcup.json (consumed by /worldcup/) and a
# standalone worldcup_preview.html for local preview via serve.R.
#
# Kicktipp scoring (max applicable tier):
#   4 exact score | 3 right non-draw goal difference |
#   2 right tendency (win/draw/loss) | 0 otherwise.
#
# Run:  Rscript scripts/worldcup_predictions.R          (live; needs ODDS_API_KEY)
#       MOCK=1 Rscript scripts/worldcup_predictions.R   (offline; uses sample_odds.json)
# Runtime: ~3-5 s (R start-up + <1 s compute; + a few s network when live).
# ============================================================

# Block A: configuration & paths ----
suppressMessages(library(jsonlite))

# locate repo root from this script's own location (robust in CI & locally)
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", grep("^--file=", .args, value = TRUE))
script_dir <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
repo_root  <- normalizePath(file.path(script_dir, ".."))

data_out    <- file.path(repo_root, "_data", "worldcup.json")
preview_out <- file.path(dirname(repo_root), "worldcup_preview.html")

API_BASE        <- "https://api.the-odds-api.com/v4"
REGIONS         <- "eu"
MARKETS         <- "h2h,totals"
WC_FALLBACK_KEY <- "soccer_fifa_world_cup"
TZ              <- "Europe/Berlin"   # "today"/"tomorrow" are defined in this zone

MAXG         <- 15     # goals grid for the actual-score distribution & fitting
PRED_MAX     <- 6      # candidate predicted scores range 0..PRED_MAX
DEFAULT_LINE <- 2.5    # totals line used if a match has no over/under market
USE_MOCK     <- nzchar(Sys.getenv("MOCK"))

# actual-score index matrices (home goals down rows, away goals across cols)
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

# fit (lh, la) to devigged market probs by least squares (1X2 always; O/U if present)
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

# precompute points matrices for every candidate prediction (match-independent)
CANDS    <- expand.grid(ph = 0:PRED_MAX, pa = 0:PRED_MAX)
PTS_LIST <- Map(kicktipp_matrix, CANDS$ph, CANDS$pa)

# top-n predicted scores by expected points, given score distribution M
best_predictions <- function(M, n = 3) {
  ep  <- vapply(PTS_LIST, function(P) sum(P * M), numeric(1))
  ord <- order(-ep)[seq_len(n)]
  data.frame(ph = CANDS$ph[ord], pa = CANDS$pa[ord], ep = ep[ord])
}

# Block C: fetch odds & devig ----

fetch_events <- function() {
  if (USE_MOCK) {
    evs <- fromJSON(file.path(script_dir, "sample_odds.json"), simplifyVector = FALSE)
    # stamp kickoffs at today/tomorrow (local) so the preview always shows matches
    n <- length(evs)
    for (i in seq_len(n)) {
      off  <- if (i <= ceiling(n / 2)) 0 else 1
      base <- as.POSIXct(format(Sys.time() + off * 86400, tz = TZ, "%Y-%m-%d"), tz = TZ)
      evs[[i]]$commence_time <- format(base + (17 + i) * 3600, tz = "UTC",
                                       "%Y-%m-%dT%H:%M:%SZ")
    }
    return(evs)
  }
  key <- Sys.getenv("ODDS_API_KEY")
  if (!nzchar(key)) stop("ODDS_API_KEY is not set (and MOCK is off).")
  # discover the active World Cup sport key (skip qualifiers); fall back to default
  sports <- tryCatch(fromJSON(sprintf("%s/sports/?apiKey=%s", API_BASE, key),
                              simplifyVector = FALSE),
                     error = function(e) stop("Could not reach The Odds API /sports: ",
                                              conditionMessage(e)))
  sport_key <- WC_FALLBACK_KEY
  for (s in sports) {
    if (isTRUE(s$active) && identical(s$group, "Soccer") &&
        grepl("World Cup", s$title, ignore.case = TRUE) &&
        !grepl("Qualif", s$title, ignore.case = TRUE)) { sport_key <- s$key; break }
  }
  url <- sprintf(paste0("%s/sports/%s/odds/?apiKey=%s&regions=%s&markets=%s",
                        "&oddsFormat=decimal&dateFormat=iso"),
                 API_BASE, sport_key, key, REGIONS, MARKETS)
  tryCatch(fromJSON(url, simplifyVector = FALSE),
           error = function(e) stop("Could not fetch odds: ", conditionMessage(e)))
}

# average devigged market probabilities across bookmakers for one event
devig_event <- function(ev) {
  Hs <- Ds <- As <- numeric(0)
  ov <- un <- list()                      # over/under devigged probs keyed by line
  for (bk in ev$bookmakers) for (mk in bk$markets) {
    if (identical(mk$key, "h2h")) {
      ph <- pd <- pa <- NA_real_
      for (oc in mk$outcomes) {
        if (identical(oc$name, ev$home_team))      ph <- oc$price
        else if (identical(oc$name, ev$away_team)) pa <- oc$price
        else if (tolower(oc$name) == "draw")       pd <- oc$price
      }
      if (all(is.finite(c(ph, pd, pa)))) {
        p <- 1 / c(ph, pd, pa); p <- p / sum(p)
        Hs <- c(Hs, p[1]); Ds <- c(Ds, p[2]); As <- c(As, p[3])
      }
    } else if (identical(mk$key, "totals")) {
      po <- pu <- ln <- NA_real_
      for (oc in mk$outcomes) {
        ln <- oc$point
        if (tolower(oc$name) == "over")       po <- oc$price
        else if (tolower(oc$name) == "under") pu <- oc$price
      }
      if (all(is.finite(c(po, pu, ln)))) {
        k <- as.character(ln); p <- 1 / c(po, pu); p <- p / sum(p)
        ov[[k]] <- c(ov[[k]], p[1]); un[[k]] <- c(un[[k]], p[2])
      }
    }
  }
  if (!length(Hs)) return(NULL)           # no usable 1X2 -> skip match
  out <- list(H = mean(Hs), D = mean(Ds), A = mean(As))
  if (length(ov)) {
    line <- as.numeric(names(which.max(vapply(ov, length, integer(1)))))  # modal line
    out$line <- line; out$over <- mean(ov[[as.character(line)]])
    out$under <- mean(un[[as.character(line)]])
  }
  out
}

# Block D: assemble predictions for today & tomorrow ----

now_local <- Sys.time()
today     <- as.Date(format(now_local, tz = TZ, "%Y-%m-%d"))
events    <- fetch_events()

matches <- list()
for (ev in events) {
  ko      <- as.POSIXct(ev$commence_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ko_date <- as.Date(format(ko, tz = TZ, "%Y-%m-%d"))
  if (is.na(ko_date) || !(ko_date %in% c(today, today + 1))) next
  mk <- devig_event(ev); if (is.null(mk)) next
  lam <- fit_lambdas(mk)
  bp  <- best_predictions(score_matrix(lam["lh"], lam["la"]), 3)
  matches[[length(matches) + 1]] <- list(
    home = ev$home_team, away = ev$away_team,
    kickoff  = format(ko, tz = TZ, "%a %d %b, %H:%M"),
    day      = if (ko_date == today) "Today" else "Tomorrow",
    sort_key = format(ko, tz = TZ, "%Y%m%d%H%M"),
    exp_goals_home = round(unname(lam["lh"]), 2),
    exp_goals_away = round(unname(lam["la"]), 2),
    p_home = round(mk$H, 3), p_draw = round(mk$D, 3), p_away = round(mk$A, 3),
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
table.preds { width:100%; border-collapse:collapse; font-size:0.9rem; }
table.preds th, table.preds td { text-align:left; padding:0.3rem 0.5rem; border:none; }
table.preds thead th { font-size:0.72rem; text-transform:uppercase; letter-spacing:0.03em;
  color:#999; border-bottom:1px solid #e0e0e0; }
table.preds td.score { font-weight:600; font-variant-numeric:tabular-nums; }
table.preds td.pts { text-align:right; font-variant-numeric:tabular-nums; }
table.preds tr.top td { background:#eaf7f0; }
table.preds tr.top td.score { color:#018F59; }
.nomatch { color:#777; font-style:italic; }'

render_card <- function(m) {
  rows <- paste(vapply(seq_along(m$predictions), function(i) {
    p <- m$predictions[[i]]
    sprintf('<tr%s><td>%d</td><td class="score">%s</td><td class="pts">%s</td></tr>',
            if (i == 1) ' class="top"' else "", i, p$score, p$points)
  }, character(1)), collapse = "\n")
  sprintf(paste0('<div class="match-card">\n',
    '  <div class="match-head"><span class="teams">%s <span class="vs">vs</span> %s</span>',
    '<span class="when"><span class="day">%s</span> %s</span></div>\n',
    '  <div class="model">Market: 1 %.0f%% &middot; X %.0f%% &middot; 2 %.0f%%',
    ' &nbsp;|&nbsp; expected goals %.2f : %.2f</div>\n',
    '  <table class="preds"><thead><tr><th>#</th><th>Score</th><th>Exp. pts</th></tr></thead>',
    '<tbody>\n%s\n</tbody></table>\n</div>'),
    m$home, m$away, m$day, m$kickoff,
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
