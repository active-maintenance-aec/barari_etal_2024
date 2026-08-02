# barari_etal_2024/maintained/table_a2_magnitude_by_primary.R
# Output: output/table_a2_magnitude_by_primary.csv
# Depends on: clean_data.R output, helpers.R
# Description: Magnitude of self-reported effect sizes by intended primary and question
#   format (Table A.2), the tabular version of Figure 1.

source(here::here("maintained", "helpers.R"))

dat_long <- read_rds(here::here("maintained", "clean_data", "dat_long_clean.rds"))
long_topic <- read_rds(here::here("maintained", "clean_data", "long_topic_clean.rds"))

magnitude_levels <- c("1-5%", "6-10%", "11-20%", "20% or more")

# Counterfactual format: weighted share in each signed magnitude band ----
tab_bands <-
  dat_long |>
  filter(variable == "tau", !is.na(value)) |>
  mutate(
    band = case_when(
      abs_value == 0 ~ "No effect",
      abs_value <= 0.05 ~ "1-5%",
      abs_value <= 0.10 ~ "6-10%",
      abs_value <= 0.20 ~ "11-20%",
      abs_value > 0.20 ~ "20% or more"
    )
  ) |>
  summarize(weight_sum = sum(weight), .by = c(topic2, primary_party, Sign, band)) |>
  mutate(pct = 100 * weight_sum / sum(weight_sum), .by = c(topic2, primary_party))

# The "Counterfactual" summary row is the sum of the bands within a sign ----
tab_counterfactual_any <-
  tab_bands |>
  filter(Sign != "No effect") |>
  summarize(pct = sum(pct), .by = c(topic2, primary_party, Sign)) |>
  mutate(row_label = "Counterfactual")

tab_counterfactual_none <-
  tab_bands |>
  filter(Sign == "No effect") |>
  transmute(topic2, primary_party, Sign, pct, row_label = "Counterfactual")

# Change format: weighted share saying more likely, less likely, no change ----
tab_change <-
  long_topic |>
  filter(!is.na(change)) |>
  mutate(
    Sign = recode(change, `-1` = "Negative", `0` = "No effect", `1` = "Positive"),
    Sign = factor(Sign, levels(dat_long$Sign))
  ) |>
  summarize(weight_sum = sum(weight), .by = c(topic2, primary_party, Sign)) |>
  mutate(pct = 100 * weight_sum / sum(weight_sum), .by = c(topic2, primary_party)) |>
  transmute(topic2, primary_party, Sign, pct, row_label = "Change")

tab_a2 <-
  bind_rows(
    tab_change,
    tab_counterfactual_any,
    tab_counterfactual_none,
    tab_bands |> filter(Sign != "No effect") |> transmute(topic2, primary_party, Sign, pct, row_label = band)
  ) |>
  mutate(
    row_label = factor(row_label, c("Change", "Counterfactual", magnitude_levels)),
    panel = recode(topic2, docs = "Beliefs", vote = "Vote choice")
  ) |>
  arrange(topic2, Sign, row_label) |>
  pivot_wider(
    id_cols = c(panel, Sign, row_label),
    names_from = primary_party,
    values_from = pct
  ) |>
  rename(effect = Sign, format = row_label) |>
  select(panel, effect, format, Republican, Democratic, Neither) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))

write_csv(tab_a2, here::here("maintained", "output", "table_a2_magnitude_by_primary.csv"))
print(tab_a2, n = nrow(tab_a2))
