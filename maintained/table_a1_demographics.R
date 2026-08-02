# barari_etal_2024/maintained/table_a1_demographics.R
# Output: output/table_a1_demographics.csv
# Depends on: clean_data.R output, helpers.R
# Description: Unweighted and weighted demographic distributions and Biden approval
#   (Table A.1). The deposit ships no script for this table; it is rebuilt here from the
#   deposited respondent-level data.

source(here::here("maintained", "helpers.R"))

dat <- read_rds(here::here("maintained", "clean_data", "dat_clean.rds"))

# Collapse the deposited response options into the categories the table prints ----
demographics <-
  dat |>
  transmute(
    weight,
    Gender = if_else(gender %in% c("Male", "Female"), gender, NA_character_),
    Age = recode(age6, `65 and up` = "65+"),
    `Race and ethnicity` = recode(
      race5,
      White = "White, non-Hispanic",
      Black = "Black, non-Hispanic",
      Asian = "Asian, non-Hispanic",
      Other = "Other, non-Hispanic"
    ),
    `Educational attainment` = case_when(
      education %in% c("Did not complete high school", "High school or G.E.D.") ~ "High school or less",
      education %in% c("Some college", "Associate's degree") ~ "Some college/associate's",
      education == "College graduate" ~ "Bachelor's",
      education == "Post graduate degree" ~ "Graduate degree"
    ),
    `Biden approval` = case_when(
      biden_approval %in% c("Strongly approve", "Somewhat approve") ~ "Approve",
      biden_approval %in% c("Strongly disapprove", "Somewhat disapprove") ~ "Disapprove"
    )
  )

# Percentages share the full-sample denominator, so categories the table omits
# (a third gender option, "no answer" on Biden approval) are counted in it but not shown.
n_respondents <- nrow(demographics)
total_weight <- sum(demographics$weight)

tab_a1 <-
  demographics |>
  pivot_longer(
    -weight,
    names_to = "demographic",
    values_to = "category",
    values_transform = list(category = as.character)
  ) |>
  filter(!is.na(category)) |>
  summarize(
    unweighted_pct = 100 * n() / n_respondents,
    weighted_pct = 100 * sum(weight) / total_weight,
    .by = c(demographic, category)
  ) |>
  mutate(
    demographic = factor(
      demographic,
      c("Gender", "Age", "Race and ethnicity", "Educational attainment", "Biden approval")
    ),
    category = factor(
      category,
      c(
        "Male", "Female",
        "18-24", "25-34", "35-44", "45-54", "55-64", "65+",
        "White, non-Hispanic", "Black, non-Hispanic", "Hispanic",
        "Asian, non-Hispanic", "Other, non-Hispanic",
        "High school or less", "Some college/associate's", "Bachelor's", "Graduate degree",
        "Approve", "Disapprove"
      )
    )
  ) |>
  arrange(demographic, category) |>
  mutate(across(c(unweighted_pct, weighted_pct), \(x) round(x, 1)))

write_csv(tab_a1, here::here("maintained", "output", "table_a1_demographics.csv"))
print(tab_a1, n = nrow(tab_a1))
