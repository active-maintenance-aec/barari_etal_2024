# barari_etal_2024/maintained/in_text_claims.R
# Output: none (prints an audit trail to the console)
# Depends on: helpers.R, output/table_2_avg_effect_by_primary.csv,
#             output/table_a1_demographics.csv, output/table_a2_magnitude_by_primary.csv,
#             output/table_a3_avg_effect_by_party.csv, output/text_sample_and_weighting.csv,
#             output/figure_1_thresholds_by_primary.csv,
#             output/figure_a1_thresholds_by_party.csv, clean_data/
# Description: One entry per numeric claim the article makes, in the order a reader meets
#   them. Each prose entry carries the article's sentence verbatim, then reads the pipeline
#   output that backs it and prints the number in the article's own units and rounding.
#   Float cells carry no sentence, because they are read off a page rather than out of a
#   sentence, and their entries print the whole reproduced table one cell at a time.
#
#   Nothing here is re-estimated: every value comes out of maintained/output/ or
#   maintained/clean_data/, which the analysis scripts have already written. The
#   ground-truth csv is never read. This file and ground_truth/build_ground_truth.R reach
#   the same claimed numbers from the same outputs by separate paths, each doing its own
#   selection, unit conversion and rounding, so a disagreement between them means one of
#   the two is wrong.
#
#   The "# covers:" comment on each entry names the claim in ground_truth/published_claims.csv
#   that the entry checks. A * in a marker is a wildcard matching any run of characters, which
#   is how one entry covers a whole table. build_ground_truth.R runs this file, counts the
#   CLAIM lines it prints, and halts unless they account for every claim the extraction says
#   needs one, and unless the markers and the printed lines name the same set.
#
#   cat() is used throughout, per the in-text-claims spec: a labelled line per claim is what
#   makes the output scannable beside the article.

source(here::here("maintained", "helpers.R"))

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

# Printing ----
# Every line of output carries its claim identifier, so the audit trail can be read
# straight down beside the article and build_ground_truth.R can parse it line by line.
claim <- function(id, value) cat("CLAIM ", id, " = ", value, "\n", sep = "")

# The article prints every estimate, percentage and standard error to one decimal place,
# every p-value to three, and every count in whole units.
pp <- function(x) sprintf("%.1f", x)
pct0 <- function(x) sprintf("%.0f", x)
count_str <- function(x) formatC(x, format = "d", big.mark = ",")
p_printed <- function(x) if_else(x < 0.001, "<0.001", sprintf("%.3f", x))

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

# Abstract ----

# "Using this method, Republican primary voters report that the indictment increased their
# belief that Trump mishandled documents (+2.5 pp) and decreased their intention to vote
# for him in the primaries (-1.6 pp)."
# covers: abstract_belief_effect
# covers: abstract_primary_effect
claim("abstract_belief_effect", pp(effect_cell(t2, "Believe", "Republican", "cf_diff")))
claim("abstract_primary_effect", pp(effect_cell(t2, "primary election", "Republican", "cf_diff")))

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
  str_c("change ", pp(100 * mean(replace_na(assigned_format == "Change", FALSE))),
        "%, counterfactual ", pp(100 * mean(replace_na(assigned_format == "Counterfactual", FALSE))),
        "%, neither ", pp(100 * mean(is.na(assigned_format))), "%")
)

# "We randomly assigned respondents to one of two methods for retrospectively assessing
# causal effects, the change format or the counterfactual format."
claim("design_n_formats", pct0(n_distinct(assigned_format, na.rm = TRUE)))

# "Among Republican primary voters, 43 percent said the indictment made them "more likely"
# to support Trump in the primary and 16 percent said "less likely.""
# covers: intro_change_rep_primary_more
# covers: intro_change_rep_primary_less
claim("intro_change_rep_primary_more", pct0(effect_cell(t2, "primary election", "Republican", "more_pct")))
claim("intro_change_rep_primary_less", pct0(effect_cell(t2, "primary election", "Republican", "less_pct")))

# "By contrast, in the counterfactual format, the average Republican primary voter gives
# themselves a 64.1 percent chance of supporting Trump. When asked how they would have
# responded if they didn't know about the indictment, the average response was 65.7
# percent, for an estimated effect of -1.6 percentage points."
# covers: intro_cf_rep_primary_actual
# covers: intro_cf_rep_primary_predicted
# covers: intro_cf_rep_primary_diff
claim("intro_cf_rep_primary_actual", pp(effect_cell(t2, "primary election", "Republican", "actual")))
claim("intro_cf_rep_primary_predicted", pp(effect_cell(t2, "primary election", "Republican", "predicted")))
claim("intro_cf_rep_primary_diff", pp(effect_cell(t2, "primary election", "Republican", "cf_diff")))

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
# what the deposit holds; the shortfall is a property of the archive and is recorded in the
# ground truth and the report.
# covers: design_n_surveyed
# covers: design_field_dates
claim("design_n_surveyed", count_str(txt_value("n_respondents_deposited")))

field_dates <- mdy(str_sub(c(dat$StartDate, dat$EndDate), 1, 10))
claim("design_field_dates", str_c(format(min(field_dates)), " to ", format(max(field_dates))))

# "Of the 6,877 respondents that began the survey, 5,011 completed it (73 percent)."
#
# Respondents who did not complete are not in the deposit, so neither the denominator nor
# the rate can be rebuilt from it. Both entries print the completed count the deposit does
# carry, which is the only half of the arithmetic the data can supply.
# covers: design_n_began
# covers: design_n_completed
# covers: design_completion_rate
claim("design_n_began", "NA")
claim("design_n_completed", count_str(txt_value("n_respondents_deposited")))
claim("design_completion_rate", "NA")

# "As an additional demonstration that our weighting procedure works as expected, we
# calculated a weighted estimate of approval of President Joe Biden (40.4 percent) from our
# survey and compared it with the June 26 average approval ratings calculated by
# FiveThirtyEight (40.3 percent; FiveThirtyEight 2023a)."
# covers: design_biden_approval_weighted
claim("design_biden_approval_weighted", pp(txt_value("approve_weighted_pct")))

# "By contrast, the counterfactual format uses a sequence of two questions for each of these
# opinions. The first measures the level of opinion given the event occurred and the second
# measures the level supposing (counterfactually) the respondent did not know about
# the event."
# covers: design_counterfactual_n_questions
claim("design_counterfactual_n_questions",
      pct0(length(intersect(names(long_topic), c("Y1", "Y0")))))

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
claim("table_1_scale_min", pct0(100 * min(counterfactual_responses, na.rm = TRUE)))
claim("table_1_scale_max", pct0(100 * max(counterfactual_responses, na.rm = TRUE)))

scale_points <- n_distinct(counterfactual_responses, na.rm = TRUE)
claim("why_scale_points", pct0(scale_points))
claim("discussion_scale_points", pct0(scale_points))
claim("table_2_note_scale_points", pct0(scale_points))

# "Does the indictment make you think it is more likely or less likely that Trump
# mishandled nuclear secrets?" [More likely/No change/Less Likely]"
#
# "Note: ... The change format was measured on a three-point scale: more likely, no
# difference, less likely."
# covers: table_1_change_response_options
# covers: table_2_note_change_scale_points
# covers: why_n_categories
change_options <- n_distinct(long_topic$change, na.rm = TRUE)
claim("table_1_change_response_options", pct0(change_options))
claim("table_2_note_change_scale_points", pct0(change_options))
claim("why_n_categories", pct0(change_options))

# Results ----

# "When asked directly, just 16 percent of Republican primary voters said that the
# indictments increased their belief that Trump had mishandled documents."
# covers: results_rep_change_belief_more
claim("results_rep_change_belief_more", pct0(effect_cell(t2, "Believe", "Republican", "more_pct")))

# Table 2. Average self-reported effect by primary and question format ----
#
# A table cell is read off the page rather than out of a sentence, so these entries carry
# no quotation. Every cell of the published table is printed, in the article's order and at
# the article's precision.
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
      printed = if_else(quantity %in% c("cf_p", "change_p"), p_printed(value), pp(value))
    )
}

table_2_claims <- effect_table_claims(t2, "table_2")
walk2(table_2_claims$claim_id, table_2_claims$printed, claim)

# Results, continued ----

# "Many more, 40 percent, said that the indictments made them less likely to believe he had
# mishandled documents. Similarly, 43 percent said the indictment made them more likely to
# support Trump, against just 16 percent saying "less likely.""
# covers: results_rep_change_belief_less
# covers: results_rep_change_primary_more
# covers: results_rep_change_primary_less
claim("results_rep_change_belief_less", pct0(effect_cell(t2, "Believe", "Republican", "less_pct")))
claim("results_rep_change_primary_more", pct0(effect_cell(t2, "primary election", "Republican", "more_pct")))
claim("results_rep_change_primary_less", pct0(effect_cell(t2, "primary election", "Republican", "less_pct")))

# "After the indictment, the average Republican primary voter said that there is a 27.1
# percent chance that Trump mishandled classified documents. They estimated that if they
# had not known about the indictment, they would have said 24.6 percent on average, a
# difference of 2.5 percentage points (s.e. = 0.6, p < 0.01)."
# covers: results_rep_cf_belief_actual
# covers: results_rep_cf_belief_predicted
# covers: results_rep_cf_belief_diff
# covers: results_rep_cf_belief_se
# covers: results_rep_cf_belief_p
claim("results_rep_cf_belief_actual", pp(effect_cell(t2, "Believe", "Republican", "actual")))
claim("results_rep_cf_belief_predicted", pp(effect_cell(t2, "Believe", "Republican", "predicted")))
claim("results_rep_cf_belief_diff", pp(effect_cell(t2, "Believe", "Republican", "cf_diff")))
claim("results_rep_cf_belief_se", pp(effect_cell(t2, "Believe", "Republican", "cf_se")))
claim("results_rep_cf_belief_p", p_printed(effect_cell(t2, "Believe", "Republican", "cf_p")))

# "Republican primary voters also thought that the indictment made them less likely to vote
# for Trump in the primary: on average, they reported a 64.1 percent chance of doing so,
# compared with 65.7 percent if the indictment had not been issued (difference = 1.6 pp,
# s.e. = 0.6, p = 0.02)."
#
# The sentence prints the magnitude of the difference and gives its direction in words, so
# the magnitude is what this entry prints.
# covers: results_rep_cf_primary_actual
# covers: results_rep_cf_primary_predicted
# covers: results_rep_cf_primary_diff
# covers: results_rep_cf_primary_se
# covers: results_rep_cf_primary_p
claim("results_rep_cf_primary_actual", pp(effect_cell(t2, "primary election", "Republican", "actual")))
claim("results_rep_cf_primary_predicted", pp(effect_cell(t2, "primary election", "Republican", "predicted")))
claim("results_rep_cf_primary_diff", pp(abs(effect_cell(t2, "primary election", "Republican", "cf_diff"))))
claim("results_rep_cf_primary_se", pp(effect_cell(t2, "primary election", "Republican", "cf_se")))
claim("results_rep_cf_primary_p", p_printed(effect_cell(t2, "primary election", "Republican", "cf_p")))

# "Using the change format, 76 percent said the indictment made them more likely to believe
# that Trump mishandled documents, with 60 percent saying it made them less likely to vote
# for Trump. The opposite sentiments stood in the single digits."
# covers: results_dem_change_belief_more
# covers: results_dem_change_general_less
# covers: results_dem_opposite_single_digits
claim("results_dem_change_belief_more", pct0(effect_cell(t2, "Believe", "Democratic", "more_pct")))
claim("results_dem_change_general_less", pct0(effect_cell(t2, "general election", "Democratic", "less_pct")))
claim(
  "results_dem_opposite_single_digits",
  str_c("belief less likely ", pp(effect_cell(t2, "Believe", "Democratic", "less_pct")),
        "%, vote more likely ", pp(effect_cell(t2, "general election", "Democratic", "more_pct")), "%")
)

# "The average Democratic primary voter said that there was an 85.3 percent chance that
# Trump mishandled documents and guessed that if the indictment had not been issued, they
# would have said 79.5 percent (difference = 5.8 pp, s.e. = 0.6, p < 0.01)."
# covers: results_dem_cf_belief_actual
# covers: results_dem_cf_belief_predicted
# covers: results_dem_cf_belief_diff
# covers: results_dem_cf_belief_se
# covers: results_dem_cf_belief_p
claim("results_dem_cf_belief_actual", pp(effect_cell(t2, "Believe", "Democratic", "actual")))
claim("results_dem_cf_belief_predicted", pp(effect_cell(t2, "Believe", "Democratic", "predicted")))
claim("results_dem_cf_belief_diff", pp(effect_cell(t2, "Believe", "Democratic", "cf_diff")))
claim("results_dem_cf_belief_se", pp(effect_cell(t2, "Believe", "Democratic", "cf_se")))
claim("results_dem_cf_belief_p", p_printed(effect_cell(t2, "Believe", "Democratic", "cf_p")))

# "Either way, they said there was only a 10 to 11 percent chance they would vote for Trump
# (difference = 0.9 pp, s.e. = 0.8, p = 0.22)."
# covers: results_dem_cf_general_range
# covers: results_dem_cf_general_diff
# covers: results_dem_cf_general_se
# covers: results_dem_cf_general_p
claim(
  "results_dem_cf_general_range",
  str_c("actual ", pp(effect_cell(t2, "general election", "Democratic", "actual")),
        "%, predicted ", pp(effect_cell(t2, "general election", "Democratic", "predicted")), "%")
)
claim("results_dem_cf_general_diff", pp(effect_cell(t2, "general election", "Democratic", "cf_diff")))
claim("results_dem_cf_general_se", pp(effect_cell(t2, "general election", "Democratic", "cf_se")))
claim("results_dem_cf_general_p", p_printed(effect_cell(t2, "general election", "Democratic", "cf_p")))

# "Pluralities of about 45 to 50 percent said that the indictment had no effect on their
# views or vote intentions."
# covers: results_neither_change_no_effect_range
claim(
  "results_neither_change_no_effect_range",
  str_c("beliefs ", pp(a2_cell("Beliefs", "No effect", "Change", "Neither")),
        "%, vote choice ", pp(a2_cell("Vote choice", "No effect", "Change", "Neither")), "%")
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
claim("results_neither_change_belief_diff", pp(effect_cell(t2, "Believe", "Neither", "change_diff")))
claim("results_neither_change_belief_se", pp(effect_cell(t2, "Believe", "Neither", "change_se")))
claim("results_neither_change_belief_p", p_printed(effect_cell(t2, "Believe", "Neither", "change_p")))
claim("results_neither_change_general_diff", pp(effect_cell(t2, "general election", "Neither", "change_diff")))
claim("results_neither_change_general_se", pp(effect_cell(t2, "general election", "Neither", "change_se")))
claim("results_neither_change_general_p", p_printed(effect_cell(t2, "general election", "Neither", "change_p")))

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
claim("results_neither_cf_belief_diff", pp(effect_cell(t2, "Believe", "Neither", "cf_diff")))
claim("results_neither_cf_belief_se", pp(effect_cell(t2, "Believe", "Neither", "cf_se")))
claim("results_neither_cf_belief_p", p_printed(effect_cell(t2, "Believe", "Neither", "cf_p")))
claim("results_neither_cf_general_diff", pp(effect_cell(t2, "general election", "Neither", "cf_diff")))
claim("results_neither_cf_general_se", pp(effect_cell(t2, "general election", "Neither", "cf_se")))
claim("results_neither_cf_general_p", p_printed(effect_cell(t2, "general election", "Neither", "cf_p")))

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
claim("why_threshold_zero", pct0(axis_breaks[1]))
claim("why_threshold_example", pct0(axis_breaks[2]))
claim("figure_1_axis_threshold_max", pct0(max(axis_breaks)))

# Which group each printed panel of figure 1 shows ----
#
# The panel letters come from the names of the files the figure script writes, and the
# groups from the plotted data, so neither is a typed assertion about a panel. The
# maintained figure reproduces the published one panel for panel, in the order the
# deposited script exports them.
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

# "In fact, this pattern is approximately what we observe in figure 1a among those who do
# not intend to vote in either party primary. At the point where the change and
# counterfactual formats meet, the proportions in each category are similar."
# covers: why_fig1a_group
# covers: why_fig1a_formats_similar
claim("why_fig1a_group", panel_group("a"))

format_gap <- function(group, topic_name, sign_value) {
  abs(fig_share(fig1, "primary_party", group, topic_name, sign_value, "Change") -
        fig_share(fig1, "primary_party", group, topic_name, sign_value, "Counterfactual"))
}

claim(
  "why_fig1a_formats_similar",
  str_c("belief more ", pp(fig_share(fig1, "primary_party", "Neither", "docs", 1, "Change")),
        " vs ", pp(fig_share(fig1, "primary_party", "Neither", "docs", 1, "Counterfactual")),
        ", no change ", pp(fig_share(fig1, "primary_party", "Neither", "docs", 0, "Change")),
        " vs ", pp(fig_share(fig1, "primary_party", "Neither", "docs", 0, "Counterfactual")),
        ", less ", pp(fig_share(fig1, "primary_party", "Neither", "docs", -1, "Change")),
        " vs ", pp(fig_share(fig1, "primary_party", "Neither", "docs", -1, "Counterfactual")),
        "; largest gap ",
        pp(max(map_dbl(c(-1, 0, 1), \(s) max(format_gap("Neither", "docs", s),
                                             format_gap("Neither", "general", s))))), " pp")
)

# "In figure 1b, huge fractions of Republican primary voters report that the indictment
# made them think it was "less likely" Trump mishandled documents and make them "more
# likely" to support him in the primary."
# covers: why_fig1b_group
claim("why_fig1b_group", panel_group("b"))

# "Even at the smallest possible threshold for change, the percentages reporting changes in
# the congenial direction are substantially reduced by the counterfactual format."
#
# The congenial direction is doubting the indictment's premise and rallying to Trump for
# Republican primary voters, and the reverse for Democratic primary voters.
# covers: why_congenial_reduced
congenial <- tribble(
  ~group, ~topic_name, ~sign_value,
  "Republican", "docs", -1,
  "Republican", "primary", 1,
  "Democratic", "docs", 1,
  "Democratic", "general", -1
)
claim(
  "why_congenial_reduced",
  str_c(
    pmap_chr(congenial, function(group, topic_name, sign_value) {
      str_c(group, " ", topic_name, " ",
            pp(fig_share(fig1, "primary_party", group, topic_name, sign_value, "Change")),
            " to ",
            pp(fig_share(fig1, "primary_party", group, topic_name, sign_value, "Counterfactual")))
    }),
    collapse = "; "
  )
)

# "We see this same basic pattern (reversed) among Democratic primary voters in figure 1c."
# covers: why_fig1c_group
claim("why_fig1c_group", panel_group("c"))

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
claim("figure_1_panel_a_group", panel_group("a"))
claim("figure_1_caption_n_neither", count_str(txt_value("n_primary_neither")))
claim("figure_1_panel_b_group", panel_group("b"))
claim("figure_1_caption_n_republican", count_str(txt_value("n_primary_republican")))
claim("figure_1_panel_c_group", panel_group("c"))
claim("figure_1_caption_n_democratic", count_str(txt_value("n_primary_democratic")))

# Appendix A. Survey Information ----

# Table A.1. Unweighted and weighted demographic distributions and Biden approval ----
#
# The two American Community Survey columns and the FiveThirtyEight column are transcribed
# from those sources and are checked against them, not here. The two survey columns are
# printed cell by cell.
# covers: table_a1_*_unweighted
# covers: table_a1_*_weighted
table_a1_claims <-
  a1 |>
  pivot_longer(c(unweighted_pct, weighted_pct), names_to = "column", values_to = "value") |>
  transmute(
    claim_id = str_c("table_a1_", slug(demographic), "_", slug(category), "_",
                     str_remove(column, "_pct")),
    printed = pp(value)
  )
walk2(table_a1_claims$claim_id, table_a1_claims$printed, claim)

# "18-24 ... 25-34 ... 35-44 ... 45-54 ... 55-64 ... 65+"
#
# The age bands are the labels of the deposited age variable, read back from the table.
# covers: table_a1_age_bin_*
age_bins <- a1$category[a1$demographic == "Age"]
walk2(str_c("table_a1_age_bin_", slug(str_replace(age_bins, fixed("+"), "_plus"))), age_bins, claim)

# Appendix B. Supplementary Results ----

# "Table A2 is a numerical version of figure 1."
#
# The two are built from the same responses but not in the same way: figure 1 plots
# unweighted shares of respondents and table A.2 reports weighted percentages, so the
# corresponding cells do not agree. The entry prints the largest disagreement between the
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
  str_c("largest gap ", pp(figure_vs_table$gap[1]), " pp (", figure_vs_table$panel[1], ", ",
        figure_vs_table$effect[1], ", ", figure_vs_table$column[1], ": figure ",
        pp(figure_vs_table$figure_pct[1]), " vs table ", pp(figure_vs_table$table_pct[1]), ")")
)

# Table A.2. Magnitude of self-reported effect sizes by primary and question format ----
# covers: table_a2_beliefs_*
# covers: table_a2_vote_choice_*
table_a2_claims <-
  a2 |>
  pivot_longer(c(Republican, Democratic, Neither), names_to = "column", values_to = "value") |>
  transmute(
    claim_id = str_c("table_a2_", slug(panel), "_", slug(effect), "_", slug(format), "_", slug(column)),
    printed = pp(value)
  )
walk2(table_a2_claims$claim_id, table_a2_claims$printed, claim)

# "1-5% ... 6-10% ... 11-20% ... 20% or more"
#
# The bands are the cut points the table is built on, read back from the table itself.
# covers: table_a2_bin_*
a2_bins <- setdiff(unique(a2$format), c("Change", "Counterfactual"))
walk2(str_c("table_a2_bin_", slug(str_replace(a2_bins, "or more", "plus"))), a2_bins, claim)

# Table A.3. Average self-reported effect by partisan identity ----
# covers: table_a3_belief_*
# covers: table_a3_vote_general_*
# covers: table_a3_vote_primary_*
table_a3_claims <- effect_table_claims(a3, "table_a3")
walk2(table_a3_claims$claim_id, table_a3_claims$printed, claim)

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
  str_c("largest gap ", pp(belief_gaps$gap[1]), " pp (", belief_gaps$Party[1], ", ",
        belief_gaps$quantity[1], ": ", pp(belief_gaps$a3_value[1]), " vs ",
        pp(belief_gaps$t2_value[1]), ")")
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
  str_c("largest gap ", pp(belief_column_gaps$gap[1]), " pp over ",
        count_str(nrow(belief_column_gaps)), " matched proportions")
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
claim("appendix_b_rep_pid_primary_actual", pp(effect_cell(a3, "primary election", "Republican", "actual")))
claim("appendix_b_rep_primary_actual", pp(effect_cell(t2, "primary election", "Republican", "actual")))
claim("appendix_b_rep_pid_primary_predicted", pp(effect_cell(a3, "primary election", "Republican", "predicted")))
claim("appendix_b_rep_primary_predicted", pp(effect_cell(t2, "primary election", "Republican", "predicted")))
claim(
  "appendix_b_estimates_identical",
  str_c("table 2 ", pp(effect_cell(t2, "primary election", "Republican", "cf_diff")),
        " (", pp(effect_cell(t2, "primary election", "Republican", "cf_se")),
        "), table A.3 ", pp(effect_cell(a3, "primary election", "Republican", "cf_diff")),
        " (", pp(effect_cell(a3, "primary election", "Republican", "cf_se")), ")")
)
claim("appendix_b_identical_estimate", pp(effect_cell(a3, "primary election", "Republican", "cf_diff")))
claim("appendix_b_identical_se", pp(effect_cell(a3, "primary election", "Republican", "cf_se")))

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
  str_c("table A.3 ", nrow(a3), " rows vs table 2 ", nrow(t2), "; figure A.1 ",
        topics_per_group(figa1, "party"), " columns per panel vs figure 1 ",
        topics_per_group(fig1, "primary_party"))
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
  str_c("largest change-format standard errors ",
        str_c(str_c(pp(head(vote_choice_ses$change_se, 3)), " (",
                    head(vote_choice_ses$label, 3), ")"), collapse = ", "),
        "; largest counterfactual ", pp(max(vote_choice_ses$cf_se)))
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
  str_c("largest gap ", pp(center_right_gaps$gap[1]), " pp over ",
        count_str(nrow(center_right_gaps)), " matched proportions")
)
