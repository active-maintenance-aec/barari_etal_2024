# barari_etal_2024/maintained/text_sample_and_weighting.R
# Output: output/text_sample_and_weighting.csv
# Depends on: clean_data.R output, helpers.R
# Description: In-text quantities that belong to no table: how many respondents the
#   deposit carries, how they divide by intended primary and by partisan identity, and
#   the weighted Biden approval figure the article uses to validate the weights.

source(here::here("maintained", "helpers.R"))

dat <- read_rds(here::here("maintained", "clean_data", "dat_clean.rds"))

group_counts <- function(variable, prefix) {
  dat |>
    count(category = .data[[variable]]) |>
    transmute(
      quantity = paste0(prefix, tolower(replace_na(as.character(category), "missing"))),
      value = n
    )
}

biden <-
  dat |>
  summarize(
    approve_unweighted_pct = 100 * mean(biden_approval %in% c("Strongly approve", "Somewhat approve")),
    approve_weighted_pct = 100 * weighted.mean(
      biden_approval %in% c("Strongly approve", "Somewhat approve"), w = weight
    ),
    disapprove_weighted_pct = 100 * weighted.mean(
      biden_approval %in% c("Strongly disapprove", "Somewhat disapprove"), w = weight
    )
  ) |>
  pivot_longer(everything(), names_to = "quantity", values_to = "value")

text_values <-
  bind_rows(
    tibble(quantity = "n_respondents_deposited", value = nrow(dat)),
    group_counts("primary_party", "n_primary_"),
    group_counts("party", "n_party_id_"),
    biden
  )

write_csv(text_values, here::here("maintained", "output", "text_sample_and_weighting.csv"))
print(text_values, n = nrow(text_values))
