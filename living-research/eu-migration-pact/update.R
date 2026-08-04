# Production data pipeline for the "Twenty Percent Border" living research note.
# Runs monthly under GitHub Actions: pulls the current Eurostat series live (no
# cached/committed raw data), rebuilds the panel, and re-estimates every exhibit.
# Expected runtime: ~1-2 minutes (27 live Eurostat API calls plus estimation).
#
# All horizons are indexed by how many months/quarters of data are actually
# available (latest_month, plot_end_date, quarter_axis_breaks), not by literal
# calendar dates, so the figures keep working as new months arrive without
# code changes. The only hardcoded calendar date is policy_date, which is an
# institutional fact (the Pact's Q2 2026 application date), not a moving target.

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(fixest)
  library(rdrobust)
})

DECISION_DATASET <- "migr_asydec1pc"
APPLICATION_DATASET <- "migr_asyappctzm"
ANALYSIS_START <- "2023-01"
EVENT_START <- "2025-01"
PRETREATMENT_YEAR <- "2025"
MIN_PRETREATMENT_APPLICATIONS <- 200
MIN_REPORTING_COVERAGE <- 0.90
API_ROOT <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data"

EU_EX_DK <- c(
  "BE", "BG", "CZ", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT",
  "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO",
  "SI", "SK", "FI", "SE"
)
EU_MEMBERS <- c(EU_EX_DK, "DK")

# The EU Pact on Migration and Asylum enters into application in Q2 2026.
# Fixed at the quarter-start so the reference line lands exactly on the
# quarterly axis tick rather than partway between two quarters.
policy_date <- as.Date("2026-04-01")

dir.create("figures", showWarnings = FALSE)

# --- JSON-stat parsing -------------------------------------------------

category_codes <- function(dimension) {
  idx <- unlist(dimension$category$index, use.names = TRUE)
  names(sort(idx))
}

jsonstat_long <- function(url) {
  x <- fromJSON(url, simplifyVector = FALSE)
  ids <- unlist(x$id)
  sizes <- as.integer(unlist(x$size))
  codes <- lapply(ids, function(id) category_codes(x$dimension[[id]]))
  names(codes) <- ids

  vals <- unlist(x$value, use.names = TRUE)
  if (!length(vals)) return(list(data = tibble(), raw = x))
  linear <- as.integer(names(vals))
  strides <- vapply(seq_along(sizes), function(j) {
    if (j == length(sizes)) 1L else prod(sizes[(j + 1):length(sizes)])
  }, numeric(1))

  out <- lapply(seq_along(ids), function(j) {
    pos <- (linear %/% strides[j]) %% sizes[j] + 1L
    codes[[j]][pos]
  })
  names(out) <- ids
  out$value <- as.numeric(vals)
  list(data = as_tibble(out), raw = x)
}

# --- Recognition rates (2025 assignment variable) -----------------------

decision_url <- paste0(
  API_ROOT, "/", DECISION_DATASET,
  "?lang=en&freq=A&decision=IPROT&unit=PC",
  "&geo=EU27_2020_X_DK&time=", PRETREATMENT_YEAR
)
decision_parsed <- jsonstat_long(decision_url)
citizen_labels <- unlist(decision_parsed$raw$dimension$citizen$category$label)

rates <- decision_parsed$data %>%
  transmute(
    citizen,
    country = unname(citizen_labels[citizen]),
    recognition_rate = value,
    eligible = recognition_rate <= 20
  ) %>%
  filter(citizen != "EXT_EU27_2020") %>%
  arrange(recognition_rate, citizen)

# --- Monthly first-time applications ------------------------------------

application_url <- function(geo) paste0(
  API_ROOT, "/", APPLICATION_DATASET,
  "?lang=en&freq=M&applicant=FRST&age=TOTAL&sex=T&unit=PER",
  "&geo=", geo, "&sinceTimePeriod=", ANALYSIS_START
)

applications_country <- bind_rows(lapply(EU_MEMBERS, function(g) {
  jsonstat_long(application_url(g))$data %>%
    transmute(geo = g, citizen, month = time, applications = value)
}))

# Latest month for which at least MIN_REPORTING_COVERAGE of EU Member States
# have reported any observations. This is what makes the pipeline extend
# itself automatically each month without touching this script.
month_coverage <- applications_country %>%
  distinct(geo, month) %>% count(month, name = "reporting_member_states") %>%
  mutate(coverage = reporting_member_states / length(EU_MEMBERS))
latest_month <- month_coverage %>%
  filter(coverage >= MIN_REPORTING_COVERAGE) %>% summarise(x = max(month)) %>% pull(x)

plot_start_date <- as.Date(paste0(EVENT_START, "-01"))
plot_end_date <- seq(as.Date(paste0(latest_month, "-01")), by = "month", length.out = 4)[4]
quarter_axis_breaks <- seq(plot_start_date, plot_end_date, by = "3 months")
analysis_months <- format(
  seq(as.Date(paste0(ANALYSIS_START, "-01")), as.Date(paste0(latest_month, "-01")), by = "month"),
  "%Y-%m"
)

# Eurostat omits structural zeroes in JSON-stat; complete the panel before summing.
applications <- applications_country %>%
  filter(citizen %in% rates$citizen, month %in% analysis_months) %>%
  complete(geo = EU_MEMBERS, citizen = rates$citizen, month = analysis_months,
           fill = list(applications = 0)) %>%
  group_by(citizen, month) %>%
  summarise(applications = sum(applications, na.rm = TRUE), .groups = "drop") %>%
  left_join(rates, by = "citizen") %>%
  mutate(date = as.Date(paste0(month, "-01")))

pretreatment_volume <- applications %>%
  filter(substr(month, 1, 4) == PRETREATMENT_YEAR) %>%
  group_by(citizen) %>%
  summarise(applications_2025 = sum(applications), .groups = "drop")
eligible_origins <- pretreatment_volume %>%
  filter(applications_2025 >= MIN_PRETREATMENT_APPLICATIONS) %>% pull(citizen)

event_applications <- applications %>% filter(month >= EVENT_START)

baseline <- event_applications %>%
  filter(citizen %in% eligible_origins, recognition_rate > 12.5, recognition_rate <= 27.5) %>%
  mutate(
    treated = recognition_rate <= 20,
    treated_num = as.integer(treated),
    group = if_else(treated, "12.5% < rate <= 20%", "20% < rate <= 27.5%")
  )

group_counts <- baseline %>% distinct(citizen, treated) %>% count(treated, name = "origins")
n_treated <- group_counts$origins[group_counts$treated]
n_control <- group_counts$origins[!group_counts$treated]

# === Figure 2: monthly trends by group ===================================

trend_plot_data <- baseline %>% group_by(group, date) %>%
  summarise(applications = sum(applications), .groups = "drop")

p_trends <- ggplot(trend_plot_data, aes(date, applications, colour = group, group = group)) +
  geom_line(linewidth = 0.85) +
  geom_vline(xintercept = policy_date, linetype = "dashed", colour = "grey30", linewidth = 0.55) +
  annotate("text", x = policy_date, y = Inf, label = "EU Migration Pact",
           angle = 90, hjust = 1.05, vjust = 1.25, colour = "grey20", size = 3) +
  scale_colour_manual(values = c("12.5% < rate <= 20%" = "#003399", "20% < rate <= 27.5%" = "#D95F02")) +
  scale_x_date(limits = c(plot_start_date, plot_end_date), date_breaks = "3 months",
               date_labels = "%b\n%Y", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Monthly first-time applications", colour = NULL,
       title = "Pre-policy application trends around the 20% recognition-rate threshold",
       subtitle = paste0("EU-27; 2025 applications >= ", MIN_PRETREATMENT_APPLICATIONS,
                         "; latest month: ", latest_month)) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))
ggsave("figures/fig2_group_trends.png", p_trends, width = 7.1, height = 4.2, dpi = 200, bg = "white")

# === Figure 3: quarterly PPML pre-trend event study =======================

quarterly_baseline <- baseline %>%
  mutate(
    quarter_start = as.Date(paste0(format(date, "%Y"), "-",
      3 * ((as.integer(format(date, "%m")) - 1) %/% 3) + 1, "-01")),
    quarter = paste0(format(quarter_start, "%Y"), " Q",
      ((as.integer(format(quarter_start, "%m")) - 1) %/% 3) + 1)
  ) %>%
  group_by(citizen, country, treated_num, quarter_start, quarter) %>%
  summarise(applications = sum(applications), months_observed = n(), .groups = "drop")

latest_quarter_start <- max(quarterly_baseline$quarter_start)
latest_quarter <- quarterly_baseline %>%
  filter(quarter_start == latest_quarter_start) %>% distinct(quarter) %>% pull(quarter)
quarterly_baseline <- quarterly_baseline %>%
  mutate(quarter = factor(quarter, levels = unique(quarter[order(quarter_start)])))

# A partial final quarter contributes fewer exposure-months; an offset lets
# the Poisson mean scale with observed exposure instead of biasing the level
# of the (possibly incomplete) most recent quarter downward.
quarter_ppml_model <- fepois(
  applications ~ i(quarter, treated_num, ref = latest_quarter) | citizen + quarter,
  offset = ~ log(months_observed / 3), cluster = ~ citizen, data = quarterly_baseline
)

extract_quarter_event <- function(model) {
  ct <- as.data.frame(coeftable(model)); ct$term <- rownames(ct)
  names(ct)[1:4] <- c("estimate", "std.error", "statistic", "p.value")
  as_tibble(ct) %>%
    filter(grepl("quarter::", term, fixed = TRUE)) %>%
    transmute(
      quarter = sub(":treated_num", "", sub("quarter::", "", term, fixed = TRUE), fixed = TRUE),
      estimate, std.error, conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    ) %>%
    bind_rows(tibble(quarter = latest_quarter, estimate = 0, std.error = 0,
      conf.low = 0, conf.high = 0)) %>%
    mutate(quarter_start = as.Date(paste0(substr(quarter, 1, 4), "-",
      3 * (as.integer(sub(".*Q", "", quarter)) - 1) + 1, "-01"))) %>%
    arrange(quarter_start)
}
quarter_event_ppml <- extract_quarter_event(quarter_ppml_model)

p_quarter_ppml <- ggplot(quarter_event_ppml, aes(quarter_start, estimate)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.5) +
  geom_vline(xintercept = policy_date, linetype = "dashed", colour = "grey30", linewidth = 0.55) +
  annotate("text", x = policy_date, y = Inf, label = "EU Migration Pact",
           angle = 90, hjust = 1.05, vjust = 1.25, colour = "grey20", size = 2.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#9ECAE1", alpha = 0.55) +
  geom_line(colour = "#003399", linewidth = 0.75) +
  geom_point(colour = "#003399", size = 1.5) +
  scale_x_date(limits = c(plot_start_date, plot_end_date), breaks = quarter_axis_breaks,
    labels = function(x) paste0(format(x, "%Y"), " Q", ((as.integer(format(x, "%m")) - 1) %/% 3) + 1),
    expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = NULL, y = "Treated-control difference", title = "Quarterly PPML pre-trend event study",
    subtitle = paste0(latest_quarter, " omitted; origin & quarter FE, partial-quarter exposure offset; 95% origin-clustered CIs")) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 35, hjust = 1))
ggsave("figures/fig3_quarterly_ppml_event_study.png", p_quarter_ppml, width = 7.1, height = 4.2, dpi = 200, bg = "white")

# === Figures 4 and 5: dynamic difference-in-discontinuities ===============
# Origin-specific rate of change is always CURRENT quarter minus REFERENCE
# quarter, scaled to a full-quarter-equivalent using months_observed so a
# partial reference or horizon quarter isn't mechanically under-counted.
# (Uniform y_l - y_ref convention -- no sign flip between pre- and post-
# reference horizons, which would otherwise manufacture a spurious break at
# the reference point.)

rdd_quarter_base <- event_applications %>%
  left_join(pretreatment_volume, by = "citizen") %>%
  mutate(
    quarter_start = as.Date(paste0(format(date, "%Y"), "-",
      3 * ((as.integer(format(date, "%m")) - 1) %/% 3) + 1, "-01")),
    quarter = paste0(format(quarter_start, "%Y"), " Q",
      ((as.integer(format(quarter_start, "%m")) - 1) %/% 3) + 1)
  ) %>%
  group_by(citizen, country, recognition_rate, applications_2025, quarter_start, quarter) %>%
  summarise(quarter_applications = sum(applications), months_observed = n(), .groups = "drop") %>%
  mutate(quarter_applications_full = quarter_applications * 3 / months_observed)

reference_quarter_start <- max(rdd_quarter_base$quarter_start)
reference_quarter <- rdd_quarter_base %>%
  filter(quarter_start == reference_quarter_start) %>% distinct(quarter) %>% pull(quarter)

rdd_quarter_base <- rdd_quarter_base %>%
  left_join(
    rdd_quarter_base %>% filter(quarter_start == reference_quarter_start) %>%
      select(citizen, reference_quarter_applications = quarter_applications_full),
    by = "citizen"
  ) %>%
  mutate(
    relative_change_pct = if_else(
      applications_2025 == 0, 0,
      (quarter_applications_full - reference_quarter_applications) / (applications_2025 / 4)
    ),
    running_variable = recognition_rate - 20
  )

quarter_horizons <- sort(unique(rdd_quarter_base$quarter))

estimate_rdd_quarter_fixed20 <- function(horizon, order) {
  dat <- rdd_quarter_base %>% filter(quarter == horizon, abs(running_variable) <= 20)
  label <- if (order == 1) "Linear" else "Quadratic"
  if (unique(dat$quarter_start) == reference_quarter_start) {
    return(tibble(quarter = horizon, quarter_start = reference_quarter_start, polynomial = label,
      estimate = 0, std_error = 0, conf_low = 0, conf_high = 0, p_value = NA_real_))
  }
  fit <- tryCatch(rdrobust(
    y = dat$relative_change_pct, x = dat$running_variable,
    weights = dat$applications_2025, cluster = dat$citizen,
    c = 0, p = order, q = order + 1, h = c(20, 20), b = c(20, 20),
    kernel = "triangular", vce = "nn", masspoints = "adjust"
  ), error = function(e) NULL)
  if (is.null(fit)) return(tibble(quarter = horizon, quarter_start = unique(dat$quarter_start),
    polynomial = label, estimate = NA_real_, std_error = NA_real_, conf_low = NA_real_,
    conf_high = NA_real_, p_value = NA_real_))
  # rdrobust reports right-minus-left; the treated side is running_variable <= 0
  # (left), so negate to report treated-minus-control.
  tibble(quarter = horizon, quarter_start = unique(dat$quarter_start), polynomial = label,
    estimate = -unname(fit$coef["Robust", 1]), std_error = unname(fit$se["Robust", 1]),
    conf_low = -unname(fit$ci["Robust", "CI Upper"]), conf_high = -unname(fit$ci["Robust", "CI Lower"]),
    p_value = unname(fit$pv["Robust", 1]))
}

rdd_quarter_fixed20 <- bind_rows(
  lapply(quarter_horizons, estimate_rdd_quarter_fixed20, order = 1),
  lapply(quarter_horizons, estimate_rdd_quarter_fixed20, order = 2)
)

p_fixed20_event <- ggplot(rdd_quarter_fixed20, aes(quarter_start, estimate, colour = polynomial, fill = polynomial)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.5) +
  geom_vline(xintercept = policy_date, linetype = "dashed", colour = "grey30", linewidth = 0.55) +
  annotate("text", x = policy_date, y = Inf, label = "EU Migration Pact",
           angle = 90, hjust = 1.05, vjust = 1.25, colour = "grey20", size = 2.8) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high, group = polynomial), alpha = 0.13, colour = NA) +
  geom_line(aes(linetype = polynomial), linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c("Linear" = "#003399", "Quadratic" = "#7A0177"), name = NULL) +
  scale_fill_manual(values = c("Linear" = "#9ECAE1", "Quadratic" = "#D4B9DA"), name = NULL) +
  scale_linetype_manual(values = c("Linear" = "solid", "Quadratic" = "longdash"), name = NULL) +
  scale_x_date(limits = c(plot_start_date, plot_end_date), breaks = quarter_axis_breaks,
    labels = function(x) paste0(format(x, "%Y"), " Q", ((as.integer(format(x, "%m")) - 1) %/% 3) + 1),
    expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = NULL, y = "RDD estimate (normalized quarterly change)",
    title = "Fixed-bandwidth dynamic difference-in-discontinuities estimates",
    subtitle = "20 recognition-rate points per side; robust 95% intervals; full-quarter scaling") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 35, hjust = 1))
ggsave("figures/fig4_dynamic_disc_event_study.png", p_fixed20_event, width = 7.1, height = 4.4, dpi = 200, bg = "white")

fixed20_raw <- rdd_quarter_base %>%
  filter(quarter != reference_quarter, abs(running_variable) <= 20, applications_2025 > 0) %>%
  mutate(side = if_else(running_variable <= 0, "Treated side", "Control side"),
         kernel_weight = applications_2025 * pmax(0, 1 - abs(running_variable) / 20),
         quarter = factor(quarter, levels = quarter_horizons))
fixed20_bins <- fixed20_raw %>%
  group_by(quarter, side) %>% mutate(bin = ntile(running_variable, pmin(6L, n()))) %>%
  group_by(quarter, side, bin) %>%
  summarise(running_variable = weighted.mean(running_variable, applications_2025),
    relative_change_pct = weighted.mean(relative_change_pct, applications_2025),
    bin_weight = sum(applications_2025), .groups = "drop")

p_fixed20_panels <- ggplot(fixed20_bins, aes(running_variable, relative_change_pct, colour = side)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey35", linewidth = 0.4) +
  geom_point(aes(size = bin_weight), alpha = 0.68) +
  geom_smooth(data = fixed20_raw, aes(weight = kernel_weight, group = side, linetype = "Linear"),
    method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.75) +
  geom_smooth(data = fixed20_raw, aes(weight = kernel_weight, group = side, linetype = "Quadratic"),
    method = "lm", formula = y ~ x + I(x^2), se = FALSE, linewidth = 0.75) +
  facet_wrap(~quarter, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = c("Treated side" = "#003399", "Control side" = "#D95F02"), guide = "none") +
  scale_linetype_manual(values = c("Linear" = "solid", "Quadratic" = "longdash"), name = "Fit") +
  scale_size_area(max_size = 4.5, guide = "none") +
  scale_x_continuous(breaks = c(-20, -10, 0, 10, 20), labels = c("0", "10", "20", "30", "40"),
    name = "2025 recognition rate (%)") +
  labs(y = "Change / average quarterly 2025 applications", title = "Fixed-bandwidth quarterly RDD plots",
    subtitle = "20 points per side; application-weighted bins and triangular-kernel fits") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold"))
ggsave("figures/fig5_quarterly_rdd_panels.png", p_fixed20_panels, width = 8.0, height = 6.2, dpi = 200, bg = "white")

# === Summary numbers consumed by index.qmd ================================

latest_pretrend <- quarter_event_ppml %>% filter(quarter_start != latest_quarter_start) %>%
  slice_max(quarter_start, n = 1)

update_timestamp <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%d %H:%M UTC")

results_summary <- list(
  data_vintage = latest_month,
  last_successful_update = update_timestamp,
  n_treated = n_treated,
  n_control = n_control,
  min_pretreatment_applications = MIN_PRETREATMENT_APPLICATIONS,
  min_reporting_coverage = MIN_REPORTING_COVERAGE,
  reference_quarter = reference_quarter,
  n_quarters = length(quarter_horizons),
  latest_pretrend_quarter = latest_pretrend$quarter,
  latest_pretrend_estimate = round(latest_pretrend$estimate, 3),
  latest_pretrend_se = round(latest_pretrend$std.error, 3)
)
write_json(results_summary, "results_summary.json", auto_unbox = TRUE, pretty = TRUE)

metadata <- list(
  slug = "eu-migration-pact",
  title = "The Twenty Percent Border: a living research note",
  summary = "Tracking whether EU-wide fast-track asylum eligibility at the 20% recognition-rate cutoff shifts application volume, timing, or destination.",
  status = "pre-policy monitoring",
  data_vintage = latest_month,
  last_successful_update = update_timestamp,
  source_urls = list(
    "https://ec.europa.eu/eurostat/databrowser/product/view/migr_asydec1pc",
    "https://ec.europa.eu/eurostat/databrowser/product/view/migr_asyappctzm"
  )
)
write_json(metadata, "metadata.json", auto_unbox = TRUE, pretty = TRUE)

message("Living research update complete. Data vintage: ", latest_month,
        "; treated origins: ", n_treated, "; control origins: ", n_control)
