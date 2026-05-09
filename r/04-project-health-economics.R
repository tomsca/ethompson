# =============================================================================
# Module 4 – Project 1: Health Economics Analysis
# Advanced dplyr & ggplot2 Curriculum
# Emmanuel Thompson | Southeast Missouri State University
#
# dplyr focus  : grouped operations, window functions, tidy evaluation
# ggplot2 focus: multi-panel dashboard (patchwork), annotation, custom theme
#
# Scenario: Analyse simulated public-health expenditure data across 30 countries
#           over 15 years to identify spending patterns and health outcomes.
# =============================================================================

library(tidyverse)
library(lubridate)
library(patchwork)
library(ggrepel)
library(scales)


# =============================================================================
# 1. SIMULATE DATA
# =============================================================================
set.seed(2024)

countries <- tibble(
  country = paste("Country", LETTERS[1:30]),
  region  = rep(c("Africa", "Americas", "Asia", "Europe", "Oceania"), 6),
  income_group = sample(c("Low", "Lower-Middle", "Upper-Middle", "High"),
                        30, replace = TRUE,
                        prob = c(0.2, 0.25, 0.3, 0.25))
)

# Panel data: 30 countries × 15 years
panel <- expand_grid(
  country = countries$country,
  year    = 2008:2022
) |>
  left_join(countries, by = "country") |>
  mutate(
    # GDP per capita increases each year with country-level random effect
    gdp_base    = runif(n_distinct(country), 800, 45000)[match(country, unique(country))],
    gdp_pc      = gdp_base * (1 + rnorm(n(), 0.025, 0.015)) ^ (year - 2008),
    # Health expenditure as % of GDP (between 3% and 14%)
    health_pct  = pmax(3, pmin(14,
                    5 + 0.08 * log(gdp_pc) + rnorm(n(), 0, 0.8))),
    health_exp  = gdp_pc * health_pct / 100,
    # Life expectancy tied to health spending and GDP (with noise)
    life_exp    = 45 + 12 * plogis(log(health_exp / 300)) +
                  5  * plogis(log(gdp_pc / 3000)) + rnorm(n(), 0, 1.2),
    # Infant mortality (per 1000 live births) – inversely related
    infant_mort = pmax(2, 120 * exp(-0.0003 * gdp_pc) + rnorm(n(), 0, 3))
  )


# =============================================================================
# 2. DATA WRANGLING (advanced dplyr)
# =============================================================================

# --- 2a. Compute year-over-year change with lag() ---------------------------
panel <- panel |>
  arrange(country, year) |>
  group_by(country) |>
  mutate(
    health_exp_yoy  = (health_exp  - lag(health_exp))  / lag(health_exp),
    gdp_pc_yoy      = (gdp_pc      - lag(gdp_pc))      / lag(gdp_pc),
    life_exp_change = life_exp - lag(life_exp)
  ) |>
  ungroup()

# --- 2b. Rolling 3-year average with cumulative functions --------------------
panel <- panel |>
  group_by(country) |>
  mutate(
    health_exp_3yr = zoo::rollmean(health_exp, k = 3, fill = NA, align = "right")
  ) |>
  ungroup()

# --- 2c. Within-group ranking -----------------------------------------------
panel <- panel |>
  group_by(year) |>
  mutate(
    health_rank = rank(desc(health_exp)),    # 1 = highest spender
    life_rank   = rank(desc(life_exp))
  ) |>
  ungroup()

# --- 2d. Tidy-eval reusable summary function --------------------------------
regional_summary <- function(df, value_var, ...) {
  df |>
    group_by(region, year, ...) |>
    summarise(
      mean_val  = mean({{ value_var }}, na.rm = TRUE),
      median_val = median({{ value_var }}, na.rm = TRUE),
      n          = n(),
      .groups    = "drop"
    )
}

reg_health  <- regional_summary(panel, health_exp)
reg_life    <- regional_summary(panel, life_exp)
reg_infant  <- regional_summary(panel, infant_mort)

# --- 2e. across() for multi-column summary ----------------------------------
income_summary <- panel |>
  group_by(income_group, year) |>
  summarise(
    across(c(gdp_pc, health_exp, life_exp, infant_mort),
           list(mean = ~ mean(.x, na.rm = TRUE),
                sd   = ~ sd(.x,   na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

# Latest year snapshot
latest <- panel |>
  filter(year == max(year)) |>
  left_join(
    panel |>
      filter(year == min(year)) |>
      select(country, life_exp_base = life_exp, health_exp_base = health_exp),
    by = "country"
  ) |>
  mutate(
    life_exp_gain    = life_exp  - life_exp_base,
    health_exp_ratio = health_exp / health_exp_base
  )


# =============================================================================
# 3. VISUALISATION (advanced ggplot2)
# =============================================================================

# Custom publication theme
theme_health <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", colour = "#174ea6", size = 12),
      plot.subtitle    = element_text(colour = "#555555", size = 9),
      plot.caption     = element_text(colour = "#888888", size = 7, hjust = 1),
      strip.background = element_rect(fill = "#e3ecf7", colour = NA),
      strip.text       = element_text(face = "bold", colour = "#174ea6"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank()
    )
}

# --- Plot 1: Health expenditure trend by region ----------------------------
p1 <- ggplot(reg_health, aes(x = year, y = mean_val, colour = region)) +
  geom_ribbon(aes(ymin = mean_val - median_val * 0.1,
                  ymax = mean_val + median_val * 0.1,
                  fill = region), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = dollar_format()) +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title    = "Health Expenditure per Capita by Region",
    subtitle = "Mean with ±10% shaded band",
    x        = NULL,
    y        = "Health Exp. (USD)",
    colour   = "Region"
  ) +
  theme_health()

# --- Plot 2: Life expectancy vs. health spending (latest year) ---------------
p2 <- ggplot(latest, aes(x = health_exp, y = life_exp, colour = region)) +
  geom_point(aes(size = gdp_pc), alpha = 0.72) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, linetype = "dashed",
              colour = "grey50") +
  geom_label_repel(
    data    = latest |> slice_max(life_exp_gain, n = 5),
    aes(label = country),
    size    = 2.8,
    max.overlaps = 10,
    box.padding  = 0.4
  ) +
  scale_x_log10(labels = dollar_format()) +
  scale_colour_brewer(palette = "Set2") +
  scale_size_continuous(range = c(2, 10), guide = "none") +
  labs(
    title    = "Health Spending vs. Life Expectancy (2022)",
    subtitle = "Bubble size = GDP pc; labelled = top 5 life-expectancy gainers",
    x        = "Health Exp. per Capita (log, USD)",
    y        = "Life Expectancy (years)",
    colour   = "Region"
  ) +
  theme_health()

# --- Plot 3: Infant mortality decline by income group -------------------------
p3 <- income_summary |>
  ggplot(aes(x = year, y = infant_mort_mean, colour = income_group)) +
  geom_line(linewidth = 0.9) +
  geom_ribbon(aes(ymin = infant_mort_mean - infant_mort_sd,
                  ymax = infant_mort_mean + infant_mort_sd,
                  fill = income_group), alpha = 0.10, colour = NA) +
  scale_colour_viridis_d(option = "D", end = 0.85) +
  scale_fill_viridis_d(option = "D", end = 0.85, guide = "none") +
  labs(
    title    = "Infant Mortality by Income Group",
    subtitle = "Mean ± 1 SD shaded",
    x        = NULL,
    y        = "Infant Mortality (per 1,000)",
    colour   = "Income Group"
  ) +
  theme_health()

# --- Plot 4: YoY health expenditure growth distribution ----------------------
p4 <- panel |>
  filter(!is.na(health_exp_yoy), year >= 2009) |>
  ggplot(aes(x = health_exp_yoy, fill = income_group)) +
  geom_density(alpha = 0.45) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "D", end = 0.85) +
  labs(
    title    = "Distribution of YoY Health Spending Growth",
    subtitle = "2009–2022 pooled",
    x        = "Year-over-Year Change",
    y        = "Density",
    fill     = "Income Group"
  ) +
  theme_health()

# --- Compose dashboard ------------------------------------------------------
dashboard <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title   = "Global Health Economics Dashboard",
    caption = "Data: simulated for educational purposes",
    theme   = theme(
      plot.title   = element_text(size = 15, face = "bold", colour = "#174ea6"),
      plot.caption = element_text(size = 7,  colour = "#888888", hjust = 1)
    )
  )

print(dashboard)

# Save if needed
# ggsave("r/plots/health_economics_dashboard.png", dashboard,
#        width = 14, height = 10, dpi = 200)


# =============================================================================
# 4. MODELLING INTERLUDE (list-columns)
# =============================================================================
# Fit a country-level OLS: life_exp ~ health_exp + gdp_pc

country_models <- panel |>
  filter(!is.na(health_exp), !is.na(gdp_pc)) |>
  nest(data = -country) |>
  mutate(
    model  = map(data,  \(d) lm(life_exp ~ log(health_exp) + log(gdp_pc), data = d)),
    glance = map(model, broom::glance),
    tidy   = map(model, broom::tidy)
  )

# Which countries have the best model fit?
country_models |>
  select(country, glance) |>
  unnest(glance) |>
  arrange(desc(r.squared)) |>
  select(country, r.squared, adj.r.squared, p.value) |>
  head(10)

# Coefficient stability across countries
country_models |>
  select(country, tidy) |>
  unnest(tidy) |>
  filter(term == "log(health_exp)") |>
  left_join(countries, by = "country") |>
  ggplot(aes(x = estimate, y = reorder(country, estimate), colour = region)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = estimate - std.error,
                     xmax = estimate + std.error), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_brewer(palette = "Set2") +
  labs(
    title    = "Country-Level Coefficient: log(Health Expenditure) → Life Expectancy",
    subtitle = "OLS estimate ± 1 SE per country",
    x        = "Coefficient",
    y        = NULL,
    colour   = "Region"
  ) +
  theme_health() +
  theme(axis.text.y = element_text(size = 7))
