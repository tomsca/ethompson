# =============================================================================
# Module 6 – Capstone: Before/After Transformations & Portfolio
# Advanced dplyr & ggplot2 Curriculum
# Emmanuel Thompson | Southeast Missouri State University
#
# Goals:
#   • Demonstrate "before vs. after" data transformations
#   • Showcase publication-ready chart redesigns (poor → polished)
#   • Explain design choices, performance tradeoffs, and wrangling logic
# =============================================================================

library(tidyverse)
library(lubridate)
library(patchwork)
library(ggrepel)
library(ggdist)
library(scales)


# =============================================================================
# PART A: BEFORE/AFTER DATA TRANSFORMATIONS
# =============================================================================

# --------------------------------------------------------------------------
# A1. BEFORE: messy, wide, type-incorrect raw data
# --------------------------------------------------------------------------
raw_data <- tribble(
  ~PatientID, ~`Visit Date`,  ~`Age (yrs)`, ~`Blood Pressure`, ~Cholesterol_mg_dL, ~Outcome,
  "P001",     "01/15/2023",   "45",         "120/80",           "195",              "Alive",
  "P002",     "02-20-2023",   "67",         "145/92",           "230",              "alive",
  "P003",     "March 5 2023", "39",         "118/76",           "210",              "ALIVE",
  "P004",     "04/10/2023",   "52",         "135/88",           "180",              "Deceased",
  "P005",     "2023-05-22",   "71",         "150/98",           "260",              "deceased",
  "P006",     "06/01/2023",   "28",         "115/72",           "175",              "alive"
)

cat("=== BEFORE: raw messy data ===\n")
glimpse(raw_data)

# --------------------------------------------------------------------------
# A1. AFTER: clean, tidy, well-typed
# --------------------------------------------------------------------------

# Helper to parse multiple date formats
parse_flexible_date <- function(x) {
  formats <- c("%m/%d/%Y", "%m-%d-%Y", "%B %d %Y", "%Y-%m-%d")
  parsed <- rep(as.Date(NA), length(x))
  for (fmt in formats) {
    nas <- is.na(parsed)
    if (!any(nas)) break
    parsed[nas] <- suppressWarnings(as.Date(x[nas], format = fmt))
  }
  parsed
}

clean_data <- raw_data |>
  rename_with(~ str_to_lower(str_replace_all(., "[ /()]", "_"))) |>
  rename(
    patient_id       = patientid,
    visit_date       = `visit_date_`,
    age              = `age__yrs_`,
    bp               = blood_pressure,
    cholesterol      = cholesterol_mg_dl,
    outcome          = outcome
  ) |>
  mutate(
    visit_date   = parse_flexible_date(visit_date),
    age          = as.integer(age),
    cholesterol  = as.numeric(cholesterol),
    outcome      = str_to_title(str_trim(outcome)),
    outcome      = factor(outcome, levels = c("Alive", "Deceased"))
  ) |>
  separate(bp, into = c("systolic", "diastolic"), sep = "/", convert = TRUE) |>
  mutate(
    age_group     = cut(age, breaks = c(0, 30, 45, 60, Inf),
                        labels = c("<30", "30–44", "45–59", "60+"),
                        right  = FALSE),
    visit_quarter = paste0("Q", quarter(visit_date))
  )

cat("\n=== AFTER: clean, tidy, typed ===\n")
glimpse(clean_data)
print(clean_data)


# --------------------------------------------------------------------------
# A2. BEFORE: un-grouped, repetitive calculation (no tidy eval)
# --------------------------------------------------------------------------
# Imagine doing this for 10 groups – copy-paste hell:
cat("\n=== BEFORE: repeated, fragile aggregation ===\n")
# (Example only – do not scale this approach)
alive_summary <- raw_data |>
  filter(Outcome %in% c("Alive", "alive", "ALIVE")) |>
  summarise(n = n(), avg_age = mean(as.numeric(`Age (yrs)`)))

deceased_summary <- raw_data |>
  filter(Outcome %in% c("Deceased", "deceased")) |>
  summarise(n = n(), avg_age = mean(as.numeric(`Age (yrs)`)))

cat("Alive:", alive_summary$n, " | Deceased:", deceased_summary$n, "\n")

# --------------------------------------------------------------------------
# A2. AFTER: grouped, reusable, tidy-eval powered
# --------------------------------------------------------------------------
cat("\n=== AFTER: grouped tidy-eval summary ===\n")

group_report <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(
      n             = n(),
      across(c(age, systolic, diastolic, cholesterol),
             list(mean = ~ round(mean(.x, na.rm = TRUE), 1),
                  sd   = ~ round(sd(.x,   na.rm = TRUE), 1)),
             .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
}

group_report(clean_data, outcome)
group_report(clean_data, age_group, outcome)


# =============================================================================
# PART B: CHART REDESIGNS (POOR → POLISHED)
# =============================================================================

# Larger synthetic dataset for meaningful charts
set.seed(2025)
n <- 300
clinical <- tibble(
  patient_id   = sprintf("P%03d", 1:n),
  age          = round(rnorm(n, 55, 12)),
  age_group    = cut(age, c(0, 40, 55, 70, Inf),
                     labels = c("<40", "40–54", "55–69", "70+"), right = FALSE),
  systolic     = round(rnorm(n, 130, 18)),
  diastolic    = round(rnorm(n, 82,  11)),
  cholesterol  = round(rnorm(n, 210, 40)),
  treatment    = sample(c("Drug A", "Drug B", "Placebo"), n,
                        replace = TRUE, prob = c(0.35, 0.35, 0.30)),
  outcome      = factor(
    rbinom(n, 1, prob = plogis(-2 + 0.015 * systolic + 0.01 * age)),
    labels = c("Alive", "Deceased")
  )
)

# ---- B1: BEFORE – raw default pie chart ------------------------------------
pie_data <- clinical |> count(treatment)

# Poor practice: hard to compare slices, no labels on chart
b1_before <- ggplot(pie_data, aes(x = "", y = n, fill = treatment)) +
  geom_col() +
  coord_polar("y") +
  labs(title = "BEFORE: Default Pie Chart") +
  theme_void()

# ---- B1: AFTER – labelled bar chart (easy comparison) ----------------------
b1_after <- clinical |>
  count(treatment) |>
  mutate(
    pct   = n / sum(n),
    label = paste0(treatment, "\n", n, " (", percent(pct, accuracy = 1), ")")
  ) |>
  ggplot(aes(x = reorder(treatment, n), y = n, fill = treatment)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = label), hjust = -0.08, size = 3.2) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("Drug A"  = "#2567b3",
                               "Drug B"  = "#25b37e",
                               "Placebo" = "#d95a11")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "AFTER: Horizontal Bar Chart",
    subtitle = "Counts and percentages clearly labelled",
    x        = NULL,
    y        = "Number of Patients"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", colour = "#174ea6"),
    plot.subtitle = element_text(colour = "#555555"),
    panel.grid.major.y = element_blank()
  )

# ---- B2: BEFORE – clutter, rainbow, no story -------------------------------
b2_before <- ggplot(clinical, aes(x = age, y = systolic, colour = patient_id)) +
  geom_point() +
  labs(title = "BEFORE: Too Many Colours, No Story") +
  theme_gray()

# ---- B2: AFTER – focused scatter with regression and annotation ------------
b2_after <- ggplot(clinical, aes(x = age, y = systolic, colour = outcome)) +
  geom_point(alpha = 0.55, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_colour_manual(values = c("Alive"    = "#2567b3",
                                 "Deceased" = "#d95a11")) +
  annotate("text", x = 80, y = 100,
           label = "Each point = one patient",
           colour = "grey50", size = 3, hjust = 1) +
  labs(
    title    = "AFTER: Age vs. Systolic Blood Pressure",
    subtitle = "Colour encodes outcome; regression line per group",
    x        = "Age (years)",
    y        = "Systolic BP (mmHg)",
    colour   = "Outcome"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", colour = "#174ea6"),
    plot.subtitle = element_text(colour = "#555555"),
    legend.position = "bottom"
  )

# ---- B3: BEFORE – default boxplot, no distribution view -------------------
b3_before <- ggplot(clinical, aes(x = age_group, y = cholesterol)) +
  geom_boxplot() +
  labs(title = "BEFORE: Plain Boxplot")

# ---- B3: AFTER – raincloud plot (ggdist + jitter) -------------------------
b3_after <- ggplot(clinical, aes(x = cholesterol, y = age_group, fill = age_group)) +
  stat_halfeye(
    adjust = 0.7, width = 0.5,
    .width = c(0.50, 0.95),
    justification = -0.25,
    point_colour = NA,
    show.legend  = FALSE
  ) +
  geom_boxplot(
    width = 0.12, outlier.shape = NA,
    alpha = 0.6, show.legend = FALSE
  ) +
  geom_jitter(
    aes(colour = age_group),
    width = 0, height = 0.08,
    size = 1.2, alpha = 0.35,
    show.legend = FALSE
  ) +
  scale_fill_brewer(palette = "Blues")  +
  scale_colour_brewer(palette = "Blues") +
  labs(
    title    = "AFTER: Raincloud Plot",
    subtitle = "Full distribution + boxplot + individual points",
    x        = "Total Cholesterol (mg/dL)",
    y        = "Age Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", colour = "#174ea6"),
    plot.subtitle = element_text(colour = "#555555")
  )

# ---- Compose the before/after portfolio ------------------------------------
portfolio <- (b1_before | b1_after) /
             (b2_before | b2_after) /
             (b3_before | b3_after) +
  plot_annotation(
    title   = "Capstone Portfolio: Chart Redesigns",
    subtitle = "Left column = before; Right column = after",
    caption = "Data: simulated for educational purposes",
    theme   = theme(
      plot.title    = element_text(size = 15, face = "bold", colour = "#174ea6"),
      plot.subtitle = element_text(size = 10,  colour = "#555555"),
      plot.caption  = element_text(size = 7,   colour = "#888888", hjust = 1)
    )
  )

print(portfolio)


# =============================================================================
# PART C: DESIGN CHOICES EXPLAINED (comments as documentation)
# =============================================================================

# C1. Colour strategy
#   - Use a semantic palette: blue = safe/alive, orange-red = risk/deceased.
#   - Limit to ≤ 5 distinct hues; use alpha for overplotting.
#   - Always test for colour-blind accessibility (scale_colour_viridis_*).

# C2. Chart type selection
#   - Pie charts → bar charts when >3 categories or when exact proportions matter.
#   - Plain boxplots → rainclouds when n < 500 (individual points visible).
#   - Line charts for trends; scatter for relationships; heatmap for 2-D tables.

# C3. Annotation over legends
#   - Direct labels on lines/bars reduce eye travel and improve readability.
#   - Use geom_label_repel for scatter labels; annotate() for explanatory callouts.

# C4. Performance tradeoffs
#   - For > 100,000 rows: switch to dtplyr / data.table or arrow backends.
#   - geom_point on 1M rows → use geom_hex or geom_density_2d instead.
#   - Avoid rowwise() on large data; use vectorised across() or data.table.

# C5. Reproducibility checklist
#   □ Set seed (set.seed()) before any random operation.
#   □ Pin package versions with renv or pak.
#   □ Keep raw data immutable; write clean copies to separate files.
#   □ Use relative paths; render via quarto render or rmarkdown::render().

cat("\n=== Capstone complete. Review the portfolio plot and design notes above. ===\n")
