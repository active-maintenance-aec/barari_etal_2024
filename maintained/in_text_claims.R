# barari_etal_2024/maintained/in_text_claims.R
# Output: none (prints CLAIM lines and a CLAIM SUMMARY line to the console)
# Depends on: helpers.R, output/table_2_avg_effect_by_primary.csv,
#             output/table_a1_demographics.csv, output/table_a2_magnitude_by_primary.csv,
#             output/table_a3_avg_effect_by_party.csv, output/text_sample_and_weighting.csv,
#             output/figure_1_thresholds_by_primary.csv,
#             output/figure_a1_thresholds_by_party.csv, clean_data/,
#             ground_truth/published_claims.csv, errata_entries.csv, excheckr
# Description: One entry per numeric claim the article makes, in the order a reader meets
#   them. Each prose entry carries the article's sentence verbatim, then reads the pipeline
#   output that backs it and hands the number to excheckr::claim(), which prints it at the
#   article's precision beside a verdict. Float cells carry no sentence, because they are
#   read off a page rather than out of a sentence, and their entries walk the whole
#   reproduced table one cell at a time.
#
#   Nothing here is re-estimated: every value comes out of maintained/output/ or
#   maintained/clean_data/, which the analysis scripts have already written. The
#   ground-truth csv is never read. This file and ground_truth/build_ground_truth.R reach
#   the same claimed numbers from the same outputs by separate paths, each doing its own
#   selection, unit conversion and rounding, so a disagreement between them means one of
#   the two is wrong.
#
#   Every claim prints a line of the form
#
#       CLAIM <claim_id> = <value> || [verdict] <label>
#
#   The expectation is derived, never typed: published_claims.csv says what the article
#   prints, and errata_entries.csv says which of those a published correction contradicts
#   and what it says instead. [match] and [corrected] assert; [hedged] (a descriptive claim
#   that does not agree) and [shape] (no published value) are derived from the extraction
#   and assert nothing. Two exemptions are declared at the call site, where a reader of the
#   block sees them: expect = "derived" where the block prints something other than the
#   article's token (a p-value the article states only as a bound, or a quantity the
#   deposit cannot rebuild because it holds 4,838 of the 5,011 respondents the article
#   describes), and expect = "absent" where the deposit has no counterpart at all. A
#   MISMATCH, STALE, DRIFT or MISSING verdict fails the run. DRIFT is the weakest of the
#   three comparisons, because errata.qmd computes its corrected values from the same
#   output files this script reads.
#
#   The "# covers:" comment on each entry names the claim in ground_truth/published_claims.csv
#   that the entry checks. A * in a marker is a wildcard matching any run of characters, which
#   is how one entry covers a whole table. build_ground_truth.R runs this file, counts the
#   CLAIM lines it prints, and halts unless they account for every claim the extraction
#   marks needs_block, and unless the markers and the printed lines name the same set.

source(here::here("maintained", "helpers.R"))
library(excheckr)

options(width = 200)

read_output <- function(file) {
  read_csv(here::here("maintained", "output", file), show_col_types = FALSE)
}

t2 <- read_output("table_2_avg_effect_by_primary.csv")
a1 <- read_output("table_a1_demographics.csv")
a2 <- read_output("table_a2_magnitude_by_primary.csv")
a3 <- read_output("table_a3_avg_effect_by_party.csv")
txt <- read_output("text_sample_and_weighting.csv")
fig1 <- read_output("figure_1_thresholds_by_primary.csv")
figa1 <- read_output("figure_a1_thresholds_by_party.csv")

dat <- read_rds(here::here("maintained", "clean_data", "dat_clean.rds"))
long_topic <- read_rds(here::here("maintained", "clean_data", "long_topic_clean.rds"))

# The extraction and the errata spine ----
# Both are read with every column as character: value_paper carries "4.0" and the spine
# carries "0.124", and a reader left to guess would destroy the precision the comparison
# depends on.
published <- read_csv(here::here("ground_truth", "published_claims.csv"),
                      col_types = cols(.default = col_character()))
errata <- read_csv(here::here("errata_entries.csv"), col_types = cols(.default = col_character()))

# The spine names the claims it corrects by their ground-truth ids, which this repository
# derives from the float and the claim text, while the extraction carries ids of its own.
# This map puts each corrected ground-truth id under the extraction id of the same claim.
# Only entries that publish a corrected value are passed on: entry 8 is a note that reprints
# its sentence as published and corrects nothing, so it sets no expectation here.
errata_id_map <- tribble(
  ~ground_truth_id, ~claim_id,
  "text_p_1222_neither_primary_effect_on_belief_pp", "results_neither_cf_belief_diff",
  "text_p_1222_neither_primary_effect_on_belief_p_value", "results_neither_cf_belief_p",
  "text_p_1222_neither_primary_effect_on_general_election_vote_pp", "results_neither_cf_general_diff",
  "text_p_1222_neither_primary_effect_on_general_election_vote_p_value", "results_neither_cf_general_p",
  "text_p_1222_republican_primary_voters_effect_on_primary_vote_p_value", "results_rep_cf_primary_p",
  "figure_1_caption_n_respondents_intending_neither_primary", "figure_1_caption_n_neither",
  "figure_1_caption_n_republican_primary_voters", "figure_1_caption_n_republican",
  "figure_1_caption_n_democratic_primary_voters", "figure_1_caption_n_democratic"
)

stopifnot(all(errata_id_map$claim_id %in% published$claim_id))

rekey <- function(ids) {
  ids <- str_split_1(ids, ";")
  stopifnot(all(ids %in% errata_id_map$ground_truth_id))
  str_c(errata_id_map$claim_id[match(ids, errata_id_map$ground_truth_id)], collapse = ";")
}

corrections <- errata |>
  filter(!is.na(corrected_values)) |>
  mutate(claim_ids = map_chr(claim_ids, rekey))

claim_start(published = published, errata = corrections, format = "id")

# The article's own transcription of a claim, read from the extraction where a block has to
# reshape it before excheckr can compare: a p-value printed as a bound loses its "<", a range
# splits into its two ends, and a band label keeps its lower bound.
stated <- function(id) {
  value <- published$value_paper[published$claim_id == id]
  stopifnot(length(value) == 1, !is.na(value))
  value
}

stated_bound <- function(id) str_remove(stated(id), "^<")
stated_range <- function(id) str_split_1(stated(id), " to ")
band_floor <- function(x) str_extract(x, "^[0-9]+")

# Selection ----
# Every value below comes through one of these, so an entry cannot quietly read a row that
# is not there: each stops unless its filter selects exactly one row.
effect_cell <- function(table, topic_key, party_name, quantity) {
  rows <- str_detect(table$Topic, fixed(topic_key)) & table$Party == party_name
  stopifnot(sum(rows) == 1)
  table[[quantity]][which(rows)]
}

a2_cell <- function(panel_name, effect_name, format_name, column) {
  rows <- a2$panel == panel_name & a2$effect == effect_name & a2$format == format_name
  stopifnot(sum(rows) == 1)
  a2[[column]][which(rows)]
}

txt_value <- function(quantity_name) {
  value <- txt$value[txt$quantity == quantity_name]
  stopifnot(length(value) == 1, !is.na(value))
  value
}

# One plotted proportion, as a percentage. The change format has no threshold and is drawn
# on the negative part of the axis, so it is selected at its own threshold rather than at
# the zero the counterfactual format shares with it.
fig_share <- function(figure, group_column, group, topic_name, sign_value, format_name) {
  the_threshold <- if (format_name == "Change") -0.01 else 0
  rows <- figure[[group_column]] == group & figure$topic == topic_name &
    figure$sign == sign_value & str_trim(figure$measure) == format_name &
    figure$threshold == the_threshold
  stopifnot(sum(rows) == 1)
  100 * figure$P[which(rows)]
}

slug <- function(x) {
  x |> str_to_lower() |> str_replace_all("[^a-z0-9]+", "_") |> str_remove_all("^_|_$")
}

pp <- function(x) sprintf("%.1f", x)

# Abstract ----

# "Using this method, Republican primary voters report that the indictment increased their
# belief that Trump mishandled documents (+2.5 pp) and decreased their intention to vote
# for him in the primaries (-1.6 pp)."
# covers: abstract_belief_effect
# covers: abstract_primary_effect
claim("abstract_belief_effect", effect_cell(t2, "Believe", "Republican", "cf_diff"),
      "Republican primary voters, counterfactual effect on belief (pp)")
claim("abstract_primary_effect", effect_cell(t2, "primary election", "Republican", "cf_diff"),
      "Republican primary voters, counterfactual effect on primary vote (pp)")

# Introduction ----

# "For example, after former US president Donald Trump was indicted in June 2023 for
# allegedly mishandling classified documents, a CBS poll asked likely Republican primary
# voters "how might the indictment charges change their view of Trump"; 14 percent said
# "for the better," and 7 percent "for the worse," implying a net increase in electoral
# support (CBS News 2023)."
#
# The indictment date and the two CBS shares are facts about the world and about another
# organisation's poll. Nothing in this survey records them, and they are verified against
# the cited sources rather than here, so they have no entry.

# "In collaboration with SurveyMonkey, we conducted an opinion poll weighted to national
# demographic targets that randomly assigned half the respondents to the change format and
# half to the counterfactual format."
#
# Format assignment is not a column of the deposited data. It is recoverable because the
# two formats ask disjoint questions: a respondent assigned to the change format answered
# the three change items and none of the counterfactual ones.
# covers: intro_random_half
# covers: design_n_formats
assigned_format <- case_when(
  !is.na(dat$docs_change) ~ "Change",
  !is.na(dat$docs_Y1) ~ "Counterfactual"
)
claim(
  "intro_random_half",
  100 * mean(replace_na(assigned_format == "Change", FALSE)),
  str_c("share assigned to the change format (%); counterfactual ",
        pp(100 * mean(replace_na(assigned_format == "Counterfactual", FALSE))),
        "%, neither ", pp(100 * mean(is.na(assigned_format))), "%"),
  digits = 1
)

# "We randomly assigned respondents to one of two methods for retrospectively assessing
# causal effects, the change format or the counterfactual format."
claim("design_n_formats", n_distinct(assigned_format, na.rm = TRUE),
      "question formats respondents were assigned to")

# "Among Republican primary voters, 43 percent said the indictment made them "more likely"
# to support Trump in the primary and 16 percent said "less likely.""
# covers: intro_change_rep_primary_more
# covers: intro_change_rep_primary_less
claim("intro_change_rep_primary_more", effect_cell(t2, "primary election", "Republican", "more_pct"),
      "Republican primary voters, change format, more likely to support Trump (%)")
claim("intro_change_rep_primary_less", effect_cell(t2, "primary election", "Republican", "less_pct"),
      "Republican primary voters, change format, less likely to support Trump (%)")

# "By contrast, in the counterfactual format, the average Republican primary voter gives
# themselves a 64.1 percent chance of supporting Trump. When asked how they would have
# responded if they didn't know about the indictment, the average response was 65.7
# percent, for an estimated effect of -1.6 percentage points."
# covers: intro_cf_rep_primary_actual
# covers: intro_cf_rep_primary_predicted
# covers: intro_cf_rep_primary_diff
claim("intro_cf_rep_primary_actual", effect_cell(t2, "primary election", "Republican", "actual"),
      "Republican primary voters, chance of supporting Trump (%)")
claim("intro_cf_rep_primary_predicted", effect_cell(t2, "primary election", "Republican", "predicted"),
      "Republican primary voters, chance if not for the indictment (%)")
claim("intro_cf_rep_primary_diff", effect_cell(t2, "primary election", "Republican", "cf_diff"),
      "Republican primary voters, counterfactual effect on primary vote (pp)")

# Motivation ----
#
# The two exposure estimates (93 and 92 percent), the two FiveThirtyEight primary support
# figures (53.8 and 53.5 percent) and the year of the indictments are quoted from other
# polls and from a poll aggregator. This survey measures none of them.

# Research Design ----

# "We surveyed 5,011 Americans between June 22 and 27, 2023, using SurveyMonkey's "river
# sample," wherein a random sample of the platform's over two million daily respondents to
# customer-generated surveys are invited to take an additional, voluntary survey."
#
# The deposited data carry fewer respondents than the article describes. The entry prints
# what the deposit holds, which is not the article's token, so it is declared derived; the
# shortfall is a property of the archive and is recorded in the ground truth, the report and
# errata entry 8.
# covers: design_n_surveyed
# covers: design_field_dates
claim("design_n_surveyed", txt_value("n_respondents_deposited"),
      "respondents in the deposited data", expect = "derived")

field_dates <- mdy(str_sub(c(dat$StartDate, dat$EndDate), 1, 10))
claim("design_field_dates", as.numeric(max(field_dates) - min(field_dates)) + 1,
      str_c("calendar days in the field, ", format(min(field_dates)), " to ",
            format(max(field_dates))),
      digits = 0)

# "Of the 6,877 respondents that began the survey, 5,011 completed it (73 percent)."
#
# Respondents who did not complete are not in the deposit, so neither the denominator nor
# the rate can be rebuilt from it, and both are declared absent. The completed count prints
# the count the deposit does carry, which is the only half of the arithmetic the data can
# supply, and is declared derived for the same reason as the entry above.
# covers: design_n_began
# covers: design_n_completed
# covers: design_completion_rate
claim("design_n_began", NA_real_,
      "no counterpart in the deposit: respondents who did not complete are not in it",
      expect = "absent")
claim("design_n_completed", txt_value("n_respondents_deposited"),
      "respondents in the deposited data", expect = "derived")
claim("design_completion_rate", NA_real_,
      "no counterpart in the deposit: the denominator is not in it", expect = "absent")

# "As an additional demonstration that our weighting procedure works as expected, we
# calculated a weighted estimate of approval of President Joe Biden (40.4 percent) from our
# survey and compared it with the June 26 average approval ratings calculated by
# FiveThirtyEight (40.3 percent; FiveThirtyEight 2023a)."
#
# Computed on the 4,838 deposited respondents rather than the 5,011 the sentence describes,
# so declared derived.
# covers: design_biden_approval_weighted
claim("design_biden_approval_weighted", txt_value("approve_weighted_pct"),
      "weighted Biden approval on the deposited respondents (%)", expect = "derived")

# "By contrast, the counterfactual format uses a sequence of two questions for each of these
# opinions. The first measures the level of opinion given the event occurred and the second
# measures the level supposing (counterfactually) the respondent did not know about
# the event."
# covers: design_counterfactual_n_questions
claim("design_counterfactual_n_questions", length(intersect(names(long_topic), c("Y1", "Y0"))),
      "counterfactual format questions per opinion")

# Table 1. Randomized question format conditions in survey ----

# "In your opinion, how likely is it that Trump mishandled nuclear secrets?" [0-100 scale]"
#
# The deposited responses are stored as proportions, so the scale endpoints and the number
# of positions on it are read back from the data rather than asserted.
# covers: table_1_scale_min
# covers: table_1_scale_max
# covers: why_scale_points
# covers: discussion_scale_points
# covers: table_2_note_scale_points
counterfactual_responses <- c(long_topic$Y1, long_topic$Y0)
claim("table_1_scale_min", 100 * min(counterfactual_responses, na.rm = TRUE),
      "lowest counterfactual response")
claim("table_1_scale_max", 100 * max(counterfactual_responses, na.rm = TRUE),
      "highest counterfactual response")

scale_points <- n_distinct(counterfactual_responses, na.rm = TRUE)
claim("why_scale_points", scale_points, "distinct counterfactual responses")
claim("discussion_scale_points", scale_points, "distinct counterfactual responses")
claim("table_2_note_scale_points", scale_points, "distinct counterfactual responses")

# "Does the indictment make you think it is more likely or less likely that Trump
# mishandled nuclear secrets?" [More likely/No change/Less Likely]"
#
# "Note: ... The change format was measured on a three-point scale: more likely, no
# difference, less likely."
# covers: table_1_change_response_options
# covers: table_2_note_change_scale_points
# covers: why_n_categories
change_options <- n_distinct(long_topic$change, na.rm = TRUE)
claim("table_1_change_response_options", change_options, "distinct change format responses")
claim("table_2_note_change_scale_points", change_options, "distinct change format responses")
claim("why_n_categories", change_options, "distinct change format responses")

# Results ----

# "When asked directly, just 16 percent of Republican primary voters said that the
# indictments increased their belief that Trump had mishandled documents."
# covers: results_rep_change_belief_more
claim("results_rep_change_belief_more", effect_cell(t2, "Believe", "Republican", "more_pct"),
      "Republican primary voters, change format, belief more likely (%)")

# Table 2. Average self-reported effect by primary and question format ----
#
# A table cell is read off the page rather than out of a sentence, so these entries carry
# no quotation. Every cell of the published table is claimed, in the article's order.
# Fourteen p-values across Tables 2 and A.3 are printed only as p < 0.001; for those the
# extraction carries the bound, the block prints the p-value at three decimals, and the
# claim is declared derived, since a bound is not a token excheckr can compare against.
# build_ground_truth.R counts the cells under each bound.
# covers: table_2_belief_*
# covers: table_2_vote_general_*
# covers: table_2_vote_primary_*

topic_slugs <- c(
  "Believe Trump mishandled docs" = "belief",
  "Vote for Trump (general election)" = "vote_general",
  "Vote for Trump (primary election)" = "vote_primary"
)

effect_quantities <- c("actual", "predicted", "cf_diff", "cf_se", "cf_p",
                       "more_pct", "less_pct", "change_diff", "change_se", "change_p")

effect_table_claims <- function(table, prefix) {
  table |>
    pivot_longer(all_of(effect_quantities), names_to = "quantity", values_to = "value") |>
    transmute(
      claim_id = str_c(prefix, "_", topic_slugs[Topic], "_", slug(Party), "_", quantity),
      value
    )
}

claim_table_cell <- function(claim_id, value) {
  if (str_starts(stated(claim_id), "<")) {
    claim(claim_id, value, "p-value; the table prints a bound",
          published = stated_bound(claim_id), digits = 3, expect = "derived")
  } else {
    claim(claim_id, value)
  }
}

table_2_claims <- effect_table_claims(t2, "table_2")
walk2(table_2_claims$claim_id, table_2_claims$value, claim_table_cell)

# Results, continued ----

# "Many more, 40 percent, said that the indictments made them less likely to believe he had
# mishandled documents. Similarly, 43 percent said the indictment made them more likely to
# support Trump, against just 16 percent saying "less likely.""
# covers: results_rep_change_belief_less
# covers: results_rep_change_primary_more
# covers: results_rep_change_primary_less
claim("results_rep_change_belief_less", effect_cell(t2, "Believe", "Republican", "less_pct"),
      "Republican primary voters, change format, belief less likely (%)")
claim("results_rep_change_primary_more", effect_cell(t2, "primary election", "Republican", "more_pct"),
      "Republican primary voters, change format, more likely to support Trump (%)")
claim("results_rep_change_primary_less", effect_cell(t2, "primary election", "Republican", "less_pct"),
      "Republican primary voters, change format, less likely to support Trump (%)")

# "After the indictment, the average Republican primary voter said that there is a 27.1
# percent chance that Trump mishandled classified documents. They estimated that if they
# had not known about the indictment, they would have said 24.6 percent on average, a
# difference of 2.5 percentage points (s.e. = 0.6, p < 0.01)."
# covers: results_rep_cf_belief_actual
# covers: results_rep_cf_belief_predicted
# covers: results_rep_cf_belief_diff
# covers: results_rep_cf_belief_se
# covers: results_rep_cf_belief_p
claim("results_rep_cf_belief_actual", effect_cell(t2, "Believe", "Republican", "actual"),
      "Republican primary voters, chance Trump mishandled documents (%)")
claim("results_rep_cf_belief_predicted", effect_cell(t2, "Believe", "Republican", "predicted"),
      "Republican primary voters, chance if not for the indictment (%)")
claim("results_rep_cf_belief_diff", effect_cell(t2, "Believe", "Republican", "cf_diff"),
      "Republican primary voters, counterfactual effect on belief (pp)")
claim("results_rep_cf_belief_se", effect_cell(t2, "Believe", "Republican", "cf_se"),
      "standard error (pp)")
claim("results_rep_cf_belief_p", effect_cell(t2, "Believe", "Republican", "cf_p"),
      "p-value; the sentence states a bound", published = stated_bound("results_rep_cf_belief_p"),
      digits = 3, expect = "derived")

# "Republican primary voters also thought that the indictment made them less likely to vote
# for Trump in the primary: on average, they reported a 64.1 percent chance of doing so,
# compared with 65.7 percent if the indictment had not been issued (difference = 1.6 pp,
# s.e. = 0.6, p = 0.02)."
#
# The sentence prints the magnitude of the difference and gives its direction in words, so
# the magnitude is what this entry claims.
# covers: results_rep_cf_primary_actual
# covers: results_rep_cf_primary_predicted
# covers: results_rep_cf_primary_diff
# covers: results_rep_cf_primary_se
# covers: results_rep_cf_primary_p
claim("results_rep_cf_primary_actual", effect_cell(t2, "primary election", "Republican", "actual"),
      "Republican primary voters, chance of voting for Trump (%)")
claim("results_rep_cf_primary_predicted", effect_cell(t2, "primary election", "Republican", "predicted"),
      "Republican primary voters, chance if not for the indictment (%)")
claim("results_rep_cf_primary_diff", abs(effect_cell(t2, "primary election", "Republican", "cf_diff")),
      "Republican primary voters, magnitude of the effect on primary vote (pp)")
claim("results_rep_cf_primary_se", effect_cell(t2, "primary election", "Republican", "cf_se"),
      "standard error (pp)")
claim("results_rep_cf_primary_p", effect_cell(t2, "primary election", "Republican", "cf_p"),
      "p-value")

# "Using the change format, 76 percent said the indictment made them more likely to believe
# that Trump mishandled documents, with 60 percent saying it made them less likely to vote
# for Trump. The opposite sentiments stood in the single digits."
# covers: results_dem_change_belief_more
# covers: results_dem_change_general_less
# covers: results_dem_opposite_single_digits
claim("results_dem_change_belief_more", effect_cell(t2, "Believe", "Democratic", "more_pct"),
      "Democratic primary voters, change format, belief more likely (%)")
claim("results_dem_change_general_less", effect_cell(t2, "general election", "Democratic", "less_pct"),
      "Democratic primary voters, change format, less likely to vote for Trump (%)")
dem_opposite <- c(belief_less = effect_cell(t2, "Believe", "Democratic", "less_pct"),
                  vote_more = effect_cell(t2, "general election", "Democratic", "more_pct"))
claim(
  "results_dem_opposite_single_digits",
  max(dem_opposite),
  str_c("larger of the two opposite sentiments (%): belief less likely ",
        pp(dem_opposite[["belief_less"]]), ", vote more likely ", pp(dem_opposite[["vote_more"]])),
  digits = 1
)

# "The average Democratic primary voter said that there was an 85.3 percent chance that
# Trump mishandled documents and guessed that if the indictment had not been issued, they
# would have said 79.5 percent (difference = 5.8 pp, s.e. = 0.6, p < 0.01)."
# covers: results_dem_cf_belief_actual
# covers: results_dem_cf_belief_predicted
# covers: results_dem_cf_belief_diff
# covers: results_dem_cf_belief_se
# covers: results_dem_cf_belief_p
claim("results_dem_cf_belief_actual", effect_cell(t2, "Believe", "Democratic", "actual"),
      "Democratic primary voters, chance Trump mishandled documents (%)")
claim("results_dem_cf_belief_predicted", effect_cell(t2, "Believe", "Democratic", "predicted"),
      "Democratic primary voters, chance if not for the indictment (%)")
claim("results_dem_cf_belief_diff", effect_cell(t2, "Believe", "Democratic", "cf_diff"),
      "Democratic primary voters, counterfactual effect on belief (pp)")
claim("results_dem_cf_belief_se", effect_cell(t2, "Believe", "Democratic", "cf_se"),
      "standard error (pp)")
claim("results_dem_cf_belief_p", effect_cell(t2, "Believe", "Democratic", "cf_p"),
      "p-value; the sentence states a bound", published = stated_bound("results_dem_cf_belief_p"),
      digits = 3, expect = "derived")

# "Either way, they said there was only a 10 to 11 percent chance they would vote for Trump
# (difference = 0.9 pp, s.e. = 0.8, p = 0.22)."
#
# The range is passed as its two ends. A single value cannot equal both, so the claim, which
# the extraction types descriptive, is scored hedged and the label carries both values.
# covers: results_dem_cf_general_range
# covers: results_dem_cf_general_diff
# covers: results_dem_cf_general_se
# covers: results_dem_cf_general_p
dem_general_actual <- effect_cell(t2, "general election", "Democratic", "actual")
dem_general_predicted <- effect_cell(t2, "general election", "Democratic", "predicted")
claim(
  "results_dem_cf_general_range",
  dem_general_actual,
  str_c("Democratic primary voters, chance of voting for Trump (%): actual ",
        pp(dem_general_actual), ", if not for the indictment ", pp(dem_general_predicted)),
  published = stated_range("results_dem_cf_general_range"),
  digits = 1
)
claim("results_dem_cf_general_diff", effect_cell(t2, "general election", "Democratic", "cf_diff"),
      "Democratic primary voters, counterfactual effect on general election vote (pp)")
claim("results_dem_cf_general_se", effect_cell(t2, "general election", "Democratic", "cf_se"),
      "standard error (pp)")
claim("results_dem_cf_general_p", effect_cell(t2, "general election", "Democratic", "cf_p"),
      "p-value")

# "Pluralities of about 45 to 50 percent said that the indictment had no effect on their
# views or vote intentions."
# covers: results_neither_change_no_effect_range
neither_no_effect <- c(beliefs = a2_cell("Beliefs", "No effect", "Change", "Neither"),
                       vote = a2_cell("Vote choice", "No effect", "Change", "Neither"))
claim(
  "results_neither_change_no_effect_range",
  neither_no_effect[["beliefs"]],
  str_c("respondents intending neither primary, change format, no effect (%): beliefs ",
        pp(neither_no_effect[["beliefs"]]), ", vote choice ", pp(neither_no_effect[["vote"]])),
  published = stated_range("results_neither_change_no_effect_range"),
  digits = 1
)

# "Those who reported an effect were more likely to say the indictment increased their
# belief that Trump mishandled documents (difference = 21.6 pp, s.e. = 3.9, p < 0.01) and
# made them less likely to vote for Trump in the general election (difference = -10.5 pp,
# s.e. = 4.0, p < 0.01)."
# covers: results_neither_change_belief_diff
# covers: results_neither_change_belief_se
# covers: results_neither_change_belief_p
# covers: results_neither_change_general_diff
# covers: results_neither_change_general_se
# covers: results_neither_change_general_p
claim("results_neither_change_belief_diff", effect_cell(t2, "Believe", "Neither", "change_diff"),
      "neither primary, change format effect on belief (pp)")
claim("results_neither_change_belief_se", effect_cell(t2, "Believe", "Neither", "change_se"),
      "standard error (pp)")
claim("results_neither_change_belief_p", effect_cell(t2, "Believe", "Neither", "change_p"),
      "p-value; the sentence states a bound",
      published = stated_bound("results_neither_change_belief_p"), digits = 3, expect = "derived")
claim("results_neither_change_general_diff", effect_cell(t2, "general election", "Neither", "change_diff"),
      "neither primary, change format effect on general election vote (pp)")
claim("results_neither_change_general_se", effect_cell(t2, "general election", "Neither", "change_se"),
      "standard error (pp)")
claim("results_neither_change_general_p", effect_cell(t2, "general election", "Neither", "change_p"),
      "p-value; the sentence states a bound",
      published = stated_bound("results_neither_change_general_p"), digits = 3, expect = "derived")

# "By contrast, the counterfactual format suggests indifference: these respondents report
# that the indictment slightly revised their beliefs in favor of the idea that Trump
# mishandled documents (+1.8 pp, s.e. = 0.9, p = 0.05), with no substantial effect on vote
# choice (+0.1 pp, s.e. = 0.9, p = 0.89)."
# covers: results_neither_cf_belief_diff
# covers: results_neither_cf_belief_se
# covers: results_neither_cf_belief_p
# covers: results_neither_cf_general_diff
# covers: results_neither_cf_general_se
# covers: results_neither_cf_general_p
claim("results_neither_cf_belief_diff", effect_cell(t2, "Believe", "Neither", "cf_diff"),
      "neither primary, counterfactual effect on belief (pp)")
claim("results_neither_cf_belief_se", effect_cell(t2, "Believe", "Neither", "cf_se"),
      "standard error (pp)")
claim("results_neither_cf_belief_p", effect_cell(t2, "Believe", "Neither", "cf_p"),
      "p-value")
claim("results_neither_cf_general_diff", effect_cell(t2, "general election", "Neither", "cf_diff"),
      "neither primary, counterfactual effect on general election vote (pp)")
claim("results_neither_cf_general_se", effect_cell(t2, "general election", "Neither", "cf_se"),
      "standard error (pp)")
claim("results_neither_cf_general_p", effect_cell(t2, "general election", "Neither", "cf_p"),
      "p-value")

# Why Do the Answers Differ? ----

# "For example, at the 10 position on the horizontal axis, we count differences smaller
# than 10 points as "no change." At the 0 position, any difference is counted as change."
#
# The axis positions are the labelled breaks of the threshold grid the figure plots, so
# they are read back from the plotted values.
# covers: why_threshold_zero
# covers: why_threshold_example
# covers: figure_1_axis_threshold_max
axis_breaks <- sort(unique(100 * fig1$threshold[fig1$threshold >= 0]))
axis_breaks <- axis_breaks[axis_breaks %% 10 == 0]
claim("why_threshold_zero", axis_breaks[1], "first labelled threshold on the axis")
claim("why_threshold_example", axis_breaks[2], "second labelled threshold on the axis")
claim("figure_1_axis_threshold_max", max(axis_breaks), "largest labelled threshold on the axis")

# Which group each printed panel of figure 1 shows ----
#
# The panel letters come from the names of the files the figure script writes, and the
# groups from the plotted data, so neither is a typed assertion about a panel. The
# maintained figure reproduces the published one panel for panel, in the order the
# deposited script exports them. Each entry claims 1 when the panel shows the group the
# sentence names and 0 when it does not; the article states no number, so the verdict is
# shape and the label names the group the panel actually shows.
panel_groups <- tibble(
  file = list.files(here::here("maintained", "output"), pattern = "^figure_1[abc]_thresholds_.+\\.pdf$")
) |>
  transmute(
    letter = str_sub(file, 9, 9),
    group = str_remove(str_remove(file, "^figure_1[abc]_thresholds_"), "\\.pdf$")
  ) |>
  arrange(letter)

stopifnot(nrow(panel_groups) == 3, all(panel_groups$group %in% slug(levels(dat$primary_party))))

panel_group <- function(letter) {
  value <- panel_groups$group[panel_groups$letter == letter]
  stopifnot(length(value) == 1)
  str_to_title(value)
}

claim_panel <- function(id, letter, named_group) {
  claim(id, as.numeric(panel_group(letter) == named_group),
        str_c("panel (", letter, ") shows ", panel_group(letter),
              " primary voters (1 if ", named_group, ", as the sentence says)"),
        digits = 0)
}

# "In fact, this pattern is approximately what we observe in figure 1a among those who do
# not intend to vote in either party primary. At the point where the change and
# counterfactual formats meet, the proportions in each category are similar."
# covers: why_fig1a_group
# covers: why_fig1a_formats_similar
claim_panel("why_fig1a_group", "a", "Neither")

format_gap <- function(group, topic_name, sign_value) {
  abs(fig_share(fig1, "primary_party", group, topic_name, sign_value, "Change") -
        fig_share(fig1, "primary_party", group, topic_name, sign_value, "Counterfactual"))
}

claim(
  "why_fig1a_formats_similar",
  max(map_dbl(c(-1, 0, 1), \(s) max(format_gap("Neither", "docs", s),
                                    format_gap("Neither", "general", s)))),
  str_c("largest gap between the formats (pp); belief more ",
        pp(fig_share(fig1, "primary_party", "Neither", "docs", 1, "Change")),
        " vs ", pp(fig_share(fig1, "primary_party", "Neither", "docs", 1, "Counterfactual")),
        ", no change ", pp(fig_share(fig1, "primary_party", "Neither", "docs", 0, "Change")),
        " vs ", pp(fig_share(fig1, "primary_party", "Neither", "docs", 0, "Counterfactual")),
        ", less ", pp(fig_share(fig1, "primary_party", "Neither", "docs", -1, "Change")),
        " vs ", pp(fig_share(fig1, "primary_party", "Neither", "docs", -1, "Counterfactual"))),
  digits = 1
)

# "In figure 1b, huge fractions of Republican primary voters report that the indictment
# made them think it was "less likely" Trump mishandled documents and make them "more
# likely" to support him in the primary."
# covers: why_fig1b_group
claim_panel("why_fig1b_group", "b", "Republican")

# "Even at the smallest possible threshold for change, the percentages reporting changes in
# the congenial direction are substantially reduced by the counterfactual format."
#
# The congenial direction is doubting the indictment's premise and rallying to Trump for
# Republican primary voters, and the reverse for Democratic primary voters. The entry
# claims how many of the four congenial shares the counterfactual format reduces.
# covers: why_congenial_reduced
congenial <- tribble(
  ~group, ~topic_name, ~sign_value,
  "Republican", "docs", -1,
  "Republican", "primary", 1,
  "Democratic", "docs", 1,
  "Democratic", "general", -1
) |>
  mutate(
    change = pmap_dbl(list(group, topic_name, sign_value),
                      \(g, t, s) fig_share(fig1, "primary_party", g, t, s, "Change")),
    counterfactual = pmap_dbl(list(group, topic_name, sign_value),
                              \(g, t, s) fig_share(fig1, "primary_party", g, t, s, "Counterfactual"))
  )
claim(
  "why_congenial_reduced",
  sum(congenial$counterfactual < congenial$change),
  str_c("congenial shares of 4 the counterfactual format reduces; ",
        str_c(congenial$group, " ", congenial$topic_name, " ", pp(congenial$change), " to ",
              pp(congenial$counterfactual), collapse = "; ")),
  digits = 0
)

# "We see this same basic pattern (reversed) among Democratic primary voters in figure 1c."
# covers: why_fig1c_group
claim_panel("why_fig1c_group", "c", "Democratic")

# Figure 1. Distribution of self-reported effects by primary and question format ----

# "Figure 1a displays results for respondents who did not plan to vote in either primary
# (N = 754). Figure 1b and 1c displays results for those who plan to vote in the Republican
# primary (N = 2,026) and Democratic primary (N = 1,953)."
# covers: figure_1_panel_a_group
# covers: figure_1_caption_n_neither
# covers: figure_1_panel_b_group
# covers: figure_1_caption_n_republican
# covers: figure_1_panel_c_group
# covers: figure_1_caption_n_democratic
claim_panel("figure_1_panel_a_group", "a", "Neither")
claim("figure_1_caption_n_neither", txt_value("n_primary_neither"),
      "respondents intending neither primary")
claim_panel("figure_1_panel_b_group", "b", "Republican")
claim("figure_1_caption_n_republican", txt_value("n_primary_republican"),
      "respondents intending the Republican primary")
claim_panel("figure_1_panel_c_group", "c", "Democratic")
claim("figure_1_caption_n_democratic", txt_value("n_primary_democratic"),
      "respondents intending the Democratic primary")

# Appendix A. Survey Information ----

# Table A.1. Unweighted and weighted demographic distributions and Biden approval ----
#
# The two American Community Survey columns and the FiveThirtyEight column are transcribed
# from those sources and are checked against them, not here. The two survey columns are
# claimed cell by cell. Every one of them is computed on the 4,838 deposited respondents
# rather than the 5,011 the table profiles, so each is declared derived: what the deposit
# gives, not the article's token. The ground truth records the comparison cell by cell.
# covers: table_a1_*_unweighted
# covers: table_a1_*_weighted
table_a1_claims <-
  a1 |>
  pivot_longer(c(unweighted_pct, weighted_pct), names_to = "column", values_to = "value") |>
  transmute(
    claim_id = str_c("table_a1_", slug(demographic), "_", slug(category), "_",
                     str_remove(column, "_pct")),
    value
  )
walk2(table_a1_claims$claim_id, table_a1_claims$value,
      \(id, value) claim(id, value, "share of the deposited respondents (%)", expect = "derived"))

# "18-24 ... 25-34 ... 35-44 ... 45-54 ... 55-64 ... 65+"
#
# The age bands are the labels of the deposited age variable, read back from the table. Each
# entry claims the band's lower bound against the lower bound of the label the article
# prints, and the label carries the band in full.
# covers: table_a1_age_bin_*
age_bins <- a1$category[a1$demographic == "Age"]
age_ids <- str_c("table_a1_age_bin_", slug(str_replace(age_bins, fixed("+"), "_plus")))
walk2(age_ids, age_bins, \(id, band) {
  claim(id, as.numeric(band_floor(band)), str_c("age band ", band, ", lower bound"),
        published = band_floor(stated(id)))
})

# Appendix B. Supplementary Results ----

# "Table A2 is a numerical version of figure 1."
#
# The two are built from the same responses but not in the same way: figure 1 plots
# unweighted shares of respondents and table A.2 reports weighted percentages, so the
# corresponding cells do not agree. The entry claims the largest disagreement between the
# change-format shares the figure plots and the change rows of the table.
# covers: appendix_b_table_a2_matches_figure_1
figure_change_shares <-
  fig1 |>
  filter(str_trim(measure) == "Change", threshold == -0.01) |>
  transmute(
    panel = if_else(topic == "docs", "Beliefs", "Vote choice"),
    effect = recode(as.character(sign), `1` = "Positive", `-1` = "Negative", `0` = "No effect"),
    column = as.character(primary_party),
    figure_pct = 100 * P
  )

table_change_pct <-
  a2 |>
  filter(format == "Change") |>
  pivot_longer(c(Republican, Democratic, Neither), names_to = "column", values_to = "table_pct") |>
  select(panel, effect, column, table_pct)

figure_vs_table <-
  figure_change_shares |>
  inner_join(table_change_pct, by = c("panel", "effect", "column")) |>
  mutate(gap = abs(figure_pct - table_pct)) |>
  arrange(desc(gap))

stopifnot(nrow(figure_vs_table) == nrow(table_change_pct))

claim(
  "appendix_b_table_a2_matches_figure_1",
  figure_vs_table$gap[1],
  str_c("largest gap (pp): ", figure_vs_table$panel[1], ", ", figure_vs_table$effect[1], ", ",
        figure_vs_table$column[1], ", figure ", pp(figure_vs_table$figure_pct[1]),
        " vs table ", pp(figure_vs_table$table_pct[1])),
  digits = 1
)

# Table A.2. Magnitude of self-reported effect sizes by primary and question format ----
# covers: table_a2_beliefs_*
# covers: table_a2_vote_choice_*
table_a2_claims <-
  a2 |>
  pivot_longer(c(Republican, Democratic, Neither), names_to = "column", values_to = "value") |>
  transmute(
    claim_id = str_c("table_a2_", slug(panel), "_", slug(effect), "_", slug(format), "_", slug(column)),
    value
  )
walk2(table_a2_claims$claim_id, table_a2_claims$value, \(id, value) claim(id, value))

# "1-5% ... 6-10% ... 11-20% ... 20% or more"
#
# The bands are the cut points the table is built on, read back from the table itself, and
# claimed by their lower bounds as the age bands are.
# covers: table_a2_bin_*
a2_bins <- setdiff(unique(a2$format), c("Change", "Counterfactual"))
a2_bin_ids <- str_c("table_a2_bin_", slug(str_replace(a2_bins, "or more", "plus")))
walk2(a2_bin_ids, a2_bins, \(id, band) {
  claim(id, as.numeric(band_floor(band)), str_c("effect size band ", band, ", lower bound"),
        published = band_floor(stated(id)))
})

# Table A.3. Average self-reported effect by partisan identity ----
# covers: table_a3_belief_*
# covers: table_a3_vote_general_*
# covers: table_a3_vote_primary_*
table_a3_claims <- effect_table_claims(a3, "table_a3")
walk2(table_a3_claims$claim_id, table_a3_claims$value, claim_table_cell)

# "For the belief that Trump mishandled documents, all of the estimates in the top three
# rows of table A3 are within a few percentage points of the equivalent estimates in main
# text table 2, and the first column of figure A1 bears a striking resemblance to the first
# column in main text figure 1."
# covers: appendix_b_a3_within_few_pp
# covers: appendix_b_fig_a1_resembles_fig_1
equivalent_groups <- c(Republican = "Republican", Democrat = "Democratic", Independent = "Neither")

belief_gaps <-
  a3 |>
  filter(str_detect(Topic, "Believe")) |>
  pivot_longer(all_of(setdiff(effect_quantities, c("cf_p", "change_p"))),
               names_to = "quantity", values_to = "a3_value") |>
  mutate(primary_group = equivalent_groups[as.character(Party)]) |>
  inner_join(
    t2 |>
      filter(str_detect(Topic, "Believe")) |>
      pivot_longer(all_of(setdiff(effect_quantities, c("cf_p", "change_p"))),
                   names_to = "quantity", values_to = "t2_value") |>
      transmute(primary_group = as.character(Party), quantity, t2_value),
    by = c("primary_group", "quantity")
  ) |>
  mutate(gap = abs(a3_value - t2_value)) |>
  arrange(desc(gap))

stopifnot(nrow(belief_gaps) == 24)

claim(
  "appendix_b_a3_within_few_pp",
  belief_gaps$gap[1],
  str_c("largest gap (pp): ", belief_gaps$Party[1], ", ", belief_gaps$quantity[1], ", ",
        pp(belief_gaps$a3_value[1]), " vs ", pp(belief_gaps$t2_value[1])),
  digits = 1
)

belief_column_gaps <-
  figa1 |>
  filter(topic == "docs") |>
  transmute(primary_group = equivalent_groups[as.character(party)],
            sign, threshold = round(threshold, 6), measure = str_trim(measure), pct_a1 = 100 * P) |>
  inner_join(
    fig1 |>
      filter(topic == "docs") |>
      transmute(primary_group = as.character(primary_party), sign,
                threshold = round(threshold, 6), measure = str_trim(measure), pct_1 = 100 * P),
    by = c("primary_group", "sign", "threshold", "measure")
  ) |>
  mutate(gap = abs(pct_a1 - pct_1)) |>
  arrange(desc(gap))

claim(
  "appendix_b_fig_a1_resembles_fig_1",
  belief_column_gaps$gap[1],
  str_c("largest gap (pp) over ", nrow(belief_column_gaps), " matched proportions"),
  digits = 1
)

# "For example, Republicans who plan to vote in the Republican primary are a bit more
# supportive of Trump than Republican primary voters overall (67.4 percent versus 64.1
# percent) and predict their support would also have been higher if not for the indictment
# (69.1 percent versus 65.7 percent), leading to identical estimates of the indictment's
# effect on vote choice (-1.6, s.e. = 0.6)."
# covers: appendix_b_rep_pid_primary_actual
# covers: appendix_b_rep_primary_actual
# covers: appendix_b_rep_pid_primary_predicted
# covers: appendix_b_rep_primary_predicted
# covers: appendix_b_estimates_identical
# covers: appendix_b_identical_estimate
# covers: appendix_b_identical_se
claim("appendix_b_rep_pid_primary_actual", effect_cell(a3, "primary election", "Republican", "actual"),
      "Republican identifiers in the Republican primary, chance of voting for Trump (%)")
claim("appendix_b_rep_primary_actual", effect_cell(t2, "primary election", "Republican", "actual"),
      "Republican primary voters, chance of voting for Trump (%)")
claim("appendix_b_rep_pid_primary_predicted", effect_cell(a3, "primary election", "Republican", "predicted"),
      "Republican identifiers in the Republican primary, chance if not for the indictment (%)")
claim("appendix_b_rep_primary_predicted", effect_cell(t2, "primary election", "Republican", "predicted"),
      "Republican primary voters, chance if not for the indictment (%)")

primary_effects <- c(
  t2_diff = effect_cell(t2, "primary election", "Republican", "cf_diff"),
  t2_se = effect_cell(t2, "primary election", "Republican", "cf_se"),
  a3_diff = effect_cell(a3, "primary election", "Republican", "cf_diff"),
  a3_se = effect_cell(a3, "primary election", "Republican", "cf_se")
)
claim(
  "appendix_b_estimates_identical",
  abs(primary_effects[["t2_diff"]] - primary_effects[["a3_diff"]]),
  str_c("gap between the two estimates (pp): table 2 ", pp(primary_effects[["t2_diff"]]),
        " (", pp(primary_effects[["t2_se"]]), "), table A.3 ", pp(primary_effects[["a3_diff"]]),
        " (", pp(primary_effects[["a3_se"]]), ")"),
  digits = 1
)
claim("appendix_b_identical_estimate", primary_effects[["a3_diff"]],
      "Republican identifiers in the Republican primary, counterfactual effect on primary vote (pp)")
claim("appendix_b_identical_se", primary_effects[["a3_se"]], "standard error (pp)")

# "There are more rows than in main text table 2, and more columns than in main text figure
# 1, because partisan identity does not exactly line up with intention to vote in the
# Republican primary."
# covers: appendix_b_more_rows
topics_per_group <- function(figure, group_column) {
  figure |>
    distinct(group = .data[[group_column]], topic) |>
    count(group) |>
    pull(n) |>
    max()
}
claim(
  "appendix_b_more_rows",
  nrow(a3) - nrow(t2),
  str_c("rows table A.3 has beyond table 2: ", nrow(a3), " vs ", nrow(t2), "; figure A.1 ",
        topics_per_group(figa1, "party"), " columns per panel vs figure 1 ",
        topics_per_group(fig1, "primary_party")),
  digits = 0
)

# "Among such respondents, the standard errors in table A3 are usually too large to make
# any inferences about average effects on vote choice."
# covers: appendix_b_se_too_large
vote_choice_ses <-
  a3 |>
  filter(!str_detect(Topic, "Believe")) |>
  transmute(label = str_c(Party, ", ", if_else(str_detect(Topic, "general"), "general", "primary")),
            cf_se, change_se) |>
  arrange(desc(change_se))
claim(
  "appendix_b_se_too_large",
  vote_choice_ses$change_se[1],
  str_c("largest change-format standard error (pp); the three largest ",
        str_c(pp(head(vote_choice_ses$change_se, 3)), " (", head(vote_choice_ses$label, 3), ")",
              collapse = ", "),
        "; largest counterfactual ", pp(max(vote_choice_ses$cf_se))),
  digits = 1
)

# "In figure A1, the similarity of the center and right columns suggests that party
# identifiers had similar distributions of responses regardless of their primary
# vote intention."
# covers: appendix_b_center_right_similar
center_right_gaps <-
  figa1 |>
  filter(topic %in% c("general", "primary")) |>
  select(party, sign, threshold, measure, topic, P) |>
  mutate(threshold = round(threshold, 6), measure = str_trim(measure)) |>
  pivot_wider(names_from = topic, values_from = P) |>
  drop_na(general, primary) |>
  mutate(gap = 100 * abs(general - primary)) |>
  arrange(desc(gap))

claim(
  "appendix_b_center_right_similar",
  center_right_gaps$gap[1],
  str_c("largest gap (pp) over ", nrow(center_right_gaps), " matched proportions"),
  digits = 1
)

# Gates ----
# assert_claims() is what makes the verdicts above assertions: it stops on any MISMATCH,
# STALE, DRIFT or MISSING, on a quantity erratum naming a claim this file does not print,
# and on any difference between the claims printed and the claims the extraction marks
# needs_block. claim_summary() then prints the counted verdicts, so the exemptions cannot
# grow in silence.
assert_claims()
invisible(claim_summary())
