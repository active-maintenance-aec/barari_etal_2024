# barari_etal_2024/maintained/clean_data.R
# Output: clean_data/dat_clean.rds, clean_data/dat_long_clean.rds,
#   clean_data/long_topic_clean.rds, output/clean_data_vs_deposit.csv
# Depends on: helpers.R, original/data.csv
# Description: Reshape the deposited respondent-level data into the three analysis-ready
#   objects every table and figure script uses, and check them against the deposit's own
#   copies of the same objects. All three carry the names of files the deposit itself
#   ships, and together they weigh 51 MB, so they land in gitignored clean_data/ rather
#   than in committed output/: this repository does not redistribute the deposit.

source(here::here("maintained", "helpers.R"))

# Respondent level ----
dat <-
  read_csv(here::here("original", "data.csv"), show_col_types = FALSE) |>
  mutate(
    primary_party = factor(primary_party, c("Republican", "Democratic", "Neither")),
    party = factor(party, c("Republican", "Democrat", "Independent"))
  )

# Respondent by topic by variable ----
dat_long <-
  dat |>
  pivot_longer(docs_change:general_sign) |>
  separate("name", c("topic", "variable"), sep = "_") |>
  mutate(
    topic2 = gsub("general|primary", "vote", topic),
    Topic = recode(
      topic,
      docs = "Belief Trump Mishandled Documents",
      primary = "Vote Choice, Republican Primary",
      general = "Vote Choice, General Election"
    )
  )

# Respondent by topic, variables in columns ----
long_topic <-
  dat_long |>
  pivot_wider(names_from = variable, values_from = value)

dat_long <-
  dat_long |>
  mutate(
    abs_value = abs(value),
    sign = sign(value),
    Sign = recode_factor(sign, `1` = "Positive", `-1` = "Negative", `0` = "No effect")
  )

write_rds(dat, here::here("maintained", "clean_data", "dat_clean.rds"))
write_rds(dat_long, here::here("maintained", "clean_data", "dat_long_clean.rds"))
write_rds(long_topic, here::here("maintained", "clean_data", "long_topic_clean.rds"))

# Agreement with the deposit's own copies ----
# The deposit ships these three objects as .rds alongside the raw csv they are built from.
# Rebuilding them from the csv rather than loading them is only defensible if the two agree,
# so the comparison runs in the pipeline rather than sitting in prose. identical() rather
# than all.equal(): the column claims identity and all.equal() would pass a numeric column
# that had drifted in its eighth digit. The serialized bytes do differ, because a .rds
# records the R version that wrote it.
dat_deposit <- read_rds(here::here("original", "dat_clean.rds"))
dat_long_deposit <- read_rds(here::here("original", "dat_long_clean.rds"))
long_topic_deposit <- read_rds(here::here("original", "long_topic_clean.rds"))

deposit_check <-
  tibble(
    object = c("dat_clean.rds", "dat_long_clean.rds", "long_topic_clean.rds"),
    rows_rebuilt = c(nrow(dat), nrow(dat_long), nrow(long_topic)),
    rows_deposited = c(nrow(dat_deposit), nrow(dat_long_deposit), nrow(long_topic_deposit)),
    identical_to_deposit = c(
      identical(dat, dat_deposit),
      identical(dat_long, dat_long_deposit),
      identical(long_topic, long_topic_deposit)
    )
  )

write_csv(deposit_check, here::here("maintained", "output", "clean_data_vs_deposit.csv"))
print(deposit_check)
