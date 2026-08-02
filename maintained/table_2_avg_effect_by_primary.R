# barari_etal_2024/maintained/table_2_avg_effect_by_primary.R
# Output: output/table_2_avg_effect_by_primary.csv
# Depends on: clean_data.R output, helpers.R
# Description: Average self-reported effect by intended primary and question format (Table 2).

source(here::here("maintained", "helpers.R"))

dat_long <- read_rds(here::here("maintained", "clean_data", "dat_long_clean.rds"))
long_topic <- read_rds(here::here("maintained", "clean_data", "long_topic_clean.rds"))

# Weighted levels: the two counterfactual questions and the two change-format shares ----
tab_means <-
  long_topic |>
  summarize(
    more_pct = 100 * weighted.mean(change == 1, w = weight, na.rm = TRUE),
    less_pct = 100 * weighted.mean(change == -1, w = weight, na.rm = TRUE),
    actual = 100 * weighted.mean(Y1[!is.na(tau)], w = weight[!is.na(tau)], na.rm = TRUE),
    predicted = 100 * weighted.mean(Y0[!is.na(tau)], w = weight[!is.na(tau)], na.rm = TRUE),
    .by = c(topic, primary_party)
  ) |>
  filter(!is.na(more_pct))

# Weighted differences with HC2 standard errors, one intercept-only fit per cell ----
tab_diffs <-
  dat_long |>
  filter(variable %in% c("change", "tau"), !is.na(value)) |>
  group_by(topic, primary_party, variable) |>
  reframe(tidy(lm_robust(value ~ 1, weights = weight))) |>
  filter(!is.na(estimate)) |>
  transmute(
    topic,
    primary_party,
    variable = recode(variable, tau = "cf", change = "change"),
    diff = 100 * estimate,
    se = 100 * std.error,
    p = p.value
  ) |>
  pivot_wider(names_from = variable, values_from = c(diff, se, p), names_glue = "{variable}_{.value}")

tab_2 <-
  tab_means |>
  left_join(tab_diffs, by = c("topic", "primary_party")) |>
  mutate(
    Topic = recode(
      topic,
      docs = "Believe Trump mishandled docs",
      general = "Vote for Trump (general election)",
      primary = "Vote for Trump (primary election)"
    )
  ) |>
  arrange(topic, primary_party) |>
  select(
    Topic, Party = primary_party,
    actual, predicted, cf_diff, cf_se, cf_p,
    more_pct, less_pct, change_diff, change_se, change_p
  )

write_csv(tab_2, here::here("maintained", "output", "table_2_avg_effect_by_primary.csv"))
print(tab_2, width = 200)
