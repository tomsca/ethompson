# =============================================================================
# Module 5 – Project 2: Insurance Claims Analysis
# Advanced dplyr & ggplot2 Curriculum
# Emmanuel Thompson | Southeast Missouri State University
#
# dplyr focus  : non-equi joins, rowwise workflows, tidy evaluation,
#                list-columns with nested models
# ggplot2 focus: distribution geoms (ggdist), gghighlight, facets,
#                annotation layers
#
# Scenario: Analyse simulated auto-insurance claims data to identify
#           high-risk segments, pricing bands, and loss trends.
# =============================================================================

library(tidyverse)
library(lubridate)
library(ggdist)
library(gghighlight)
library(patchwork)
library(ggrepel)
library(scales)


# =============================================================================
# 1. SIMULATE INSURANCE CLAIMS DATA
# =============================================================================
set.seed(314)

n_policies <- 5000

policies <- tibble(
  policy_id    = sprintf("POL-%05d", 1:n_policies),
  issue_date   = sample(seq(ymd("2018-01-01"), ymd("2022-12-31"), by = "day"),
                        n_policies, replace = TRUE),
  age_group    = sample(c("18-25", "26-35", "36-45", "46-55", "56-65", "66+"),
                        n_policies, replace = TRUE,
                        prob = c(0.12, 0.22, 0.25, 0.20, 0.13, 0.08)),
  vehicle_type = sample(c("Sedan", "SUV", "Truck", "Sports", "Minivan"),
                        n_policies, replace = TRUE,
                        prob = c(0.35, 0.28, 0.15, 0.12, 0.10)),
  territory    = sample(c("Urban", "Suburban", "Rural"),
                        n_policies, replace = TRUE,
                        prob = c(0.45, 0.35, 0.20)),
  premium      = round(runif(n_policies, 400, 3000), 2),
  exposure_yrs = round(runif(n_policies, 0.25, 1), 4)
)

# Claims: not every policy has a claim
n_claims <- 1800
claims <- tibble(
  claim_id    = sprintf("CLM-%05d", 1:n_claims),
  policy_id   = sample(policies$policy_id, n_claims, replace = TRUE),
  claim_date  = sample(seq(ymd("2018-06-01"), ymd("2023-06-30"), by = "day"),
                       n_claims, replace = TRUE),
  claim_type  = sample(c("Collision", "Comprehensive", "Liability", "Medical"),
                       n_claims, replace = TRUE,
                       prob = c(0.40, 0.25, 0.22, 0.13)),
  severity    = round(rgamma(n_claims, shape = 2, rate = 1/3000), 2)  # dollars
)


# =============================================================================
# 2. DATA WRANGLING (advanced dplyr)
# =============================================================================

# --- 2a. Join claims to policies --------------------------------------------
claims_full <- claims |>
  left_join(policies, by = "policy_id") |>
  mutate(
    claim_year  = year(claim_date),
    claim_month = month(claim_date, label = TRUE),
    lag_days    = as.numeric(claim_date - issue_date)  # days since policy issued
  )

# --- 2b. Non-equi join: assign claims to severity bands ---------------------
severity_bands <- tibble(
  band      = c("Minor", "Moderate", "Major", "Catastrophic"),
  sev_min   = c(0,     2500,   10000,  50000),
  sev_max   = c(2499,  9999,   49999,  Inf)
)

claims_banded <- claims_full |>
  left_join(
    severity_bands,
    join_by(severity >= sev_min, severity <= sev_max)
  )

# --- 2c. Rowwise: compute multi-metric risk score per claim -----------------
claims_scored <- claims_banded |>
  rowwise() |>
  mutate(
    risk_score = sum(
      c(age_group    == "18-25")  * 3,
      c(age_group    == "66+")    * 2,
      c(vehicle_type == "Sports") * 2,
      c(territory    == "Urban")  * 1,
      c(band         %in% c("Major", "Catastrophic")) * 4
    )
  ) |>
  ungroup()

# --- 2d. Policy-level aggregation with tidy eval function -------------------
summarise_by <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(
      n_claims       = n(),
      total_loss     = sum(severity, na.rm = TRUE),
      avg_severity   = mean(severity, na.rm = TRUE),
      median_severity = median(severity, na.rm = TRUE),
      loss_ratio     = sum(severity, na.rm = TRUE) / sum(premium, na.rm = TRUE),
      .groups        = "drop"
    )
}

by_type      <- summarise_by(claims_banded, claim_type)
by_age_type  <- summarise_by(claims_banded, age_group, claim_type)
by_territory <- summarise_by(claims_banded, territory, vehicle_type)
by_year      <- summarise_by(claims_banded, claim_year)

# --- 2e. Window function: frequency trend per segment -----------------------
freq_trend <- claims_full |>
  count(claim_year, territory, claim_type) |>
  group_by(territory, claim_type) |>
  mutate(
    pct_change  = (n - lag(n)) / lag(n),
    cumulative  = cumsum(n),
    ma3         = zoo::rollmean(n, k = 3, fill = NA, align = "right")
  ) |>
  ungroup()

# --- 2f. List-column: fit severity model per claim type ---------------------
severity_models <- claims_banded |>
  filter(!is.na(severity), !is.na(age_group)) |>
  nest(data = -claim_type) |>
  mutate(
    model  = map(data, \(d) lm(log1p(severity) ~ age_group + territory + vehicle_type, data = d)),
    tidy   = map(model, broom::tidy),
    glance = map(model, broom::glance)
  )

# Model fit quality per claim type
severity_models |>
  select(claim_type, glance) |>
  unnest(glance) |>
  select(claim_type, r.squared, AIC, BIC)


# =============================================================================
# 3. VISUALISATION (advanced ggplot2)
# =============================================================================

# Custom theme
theme_insurance <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", colour = "#174ea6", size = 12),
      plot.subtitle    = element_text(colour = "#555555", size = 9),
      plot.caption     = element_text(colour = "#888888", size = 7, hjust = 1),
      strip.background = element_rect(fill = "#e3ecf7", colour = NA),
      strip.text       = element_text(face = "bold", colour = "#174ea6"),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
}

# --- Plot 1: ggdist – severity distribution by claim type -------------------
p1 <- claims_banded |>
  ggplot(aes(x = severity, y = claim_type, fill = claim_type)) +
  stat_halfeye(
    adjust    = 0.7,
    width     = 0.6,
    .width    = c(0.5, 0.95),
    justification = -0.2,
    point_colour  = NA
  ) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.5) +
  scale_x_log10(labels = dollar_format()) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title    = "Claim Severity Distribution by Type",
    subtitle = "Half-eye: 50% and 95% intervals; log x-axis",
    x        = "Claim Severity (USD, log scale)",
    y        = NULL
  ) +
  theme_insurance()

# --- Plot 2: Loss ratio heatmap (age × vehicle type) -----------------------
p2 <- claims_banded |>
  group_by(age_group, vehicle_type) |>
  summarise(loss_ratio = sum(severity) / sum(premium), .groups = "drop") |>
  ggplot(aes(x = vehicle_type, y = age_group, fill = loss_ratio)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = number(loss_ratio, accuracy = 0.01)), size = 3) +
  scale_fill_gradient2(
    low      = "#25b37e",
    mid      = "#ffd700",
    high     = "#d95a11",
    midpoint = 1,
    labels   = number_format(accuracy = 0.01)
  ) +
  labs(
    title    = "Loss Ratio Heatmap",
    subtitle = "Values > 1 indicate unprofitable segment",
    x        = "Vehicle Type",
    y        = "Age Group",
    fill     = "Loss Ratio"
  ) +
  theme_insurance() +
  theme(legend.position = "right")

# --- Plot 3: gghighlight – claims trend with spotlight on Urban territory ---
p3 <- freq_trend |>
  filter(claim_type == "Collision") |>
  ggplot(aes(x = claim_year, y = n, colour = territory, group = territory)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  gghighlight(
    territory == "Urban",
    label_key = territory,
    unhighlighted_params = list(colour = "grey80", linewidth = 0.4)
  ) +
  scale_colour_manual(values = c("Urban" = "#d95a11")) +
  labs(
    title    = "Collision Claim Frequency Trend: Urban Spotlight",
    subtitle = "Non-urban territories dimmed",
    x        = "Year",
    y        = "Number of Claims"
  ) +
  theme_insurance() +
  theme(legend.position = "none")

# --- Plot 4: Severity band breakdown by territory ---------------------------
p4 <- claims_banded |>
  count(territory, band) |>
  mutate(band = factor(band, levels = c("Minor", "Moderate", "Major", "Catastrophic"))) |>
  group_by(territory) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(x = territory, y = pct, fill = band)) +
  geom_col(position = "stack", colour = "white") +
  geom_text(aes(label = percent(pct, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 2.8, colour = "white", fontface = "bold") +
  scale_fill_manual(
    values = c("Minor"         = "#25b37e",
               "Moderate"      = "#2567b3",
               "Major"         = "#ffd700",
               "Catastrophic"  = "#d95a11")
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Claim Severity Band by Territory",
    x     = "Territory",
    y     = "Share of Claims",
    fill  = "Severity Band"
  ) +
  theme_insurance()

# --- Compose dashboard -------------------------------------------------------
(p1 + p2) / (p3 + p4) +
  plot_annotation(
    title   = "Auto Insurance Claims Analytics Dashboard",
    caption = "Data: simulated for educational purposes",
    theme   = theme(
      plot.title   = element_text(size = 15, face = "bold", colour = "#174ea6"),
      plot.caption = element_text(size = 7, colour = "#888888", hjust = 1)
    )
  )


# =============================================================================
# 4. PRICING INSIGHT: expected severity per segment
# =============================================================================
pricing_table <- claims_scored |>
  group_by(age_group, vehicle_type, territory) |>
  summarise(
    n_claims       = n(),
    avg_severity   = mean(severity),
    avg_premium    = mean(premium),
    avg_risk_score = mean(risk_score),
    implied_premium = avg_severity * 1.15,  # 15% load for expenses/profit
    .groups = "drop"
  ) |>
  mutate(
    price_adequacy = avg_premium / implied_premium,
    flag = case_when(
      price_adequacy < 0.85 ~ "Under-priced",
      price_adequacy > 1.15 ~ "Over-priced",
      TRUE                  ~ "Adequate"
    )
  )

# Visualise pricing adequacy
pricing_table |>
  ggplot(aes(x = avg_risk_score, y = price_adequacy,
             colour = flag, shape = territory)) +
  geom_hline(yintercept = c(0.85, 1.15),
             linetype = "dashed", colour = "grey60", linewidth = 0.6) +
  geom_point(size = 3, alpha = 0.8) +
  geom_label_repel(
    data = pricing_table |> filter(flag != "Adequate"),
    aes(label = paste0(age_group, " / ", vehicle_type)),
    size = 2.5, max.overlaps = 8, box.padding = 0.4
  ) +
  scale_colour_manual(values = c("Under-priced" = "#d95a11",
                                 "Over-priced"  = "#2567b3",
                                 "Adequate"     = "#25b37e")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Pricing Adequacy by Risk Segment",
    subtitle = "Dashed lines: ±15% adequacy corridor",
    x        = "Average Risk Score",
    y        = "Price Adequacy (premium / implied premium)",
    colour   = "Pricing Status",
    shape    = "Territory"
  ) +
  theme_insurance()
