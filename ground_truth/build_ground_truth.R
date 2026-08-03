# barari_etal_2024/ground_truth/build_ground_truth.R
# Output: ground_truth/barari_etal_2024_ground_truth.csv
# Depends on: maintained/output/ and ground_truth/deposited_output/ (run run_all.R first)
# Description: Assemble the ground truth table. Every value_paper entry was typed from the
#   article, read off a rendered page rather than a text dump, and is used only as a
#   comparison target: no published number is an input to any computation here or anywhere
#   in maintained/. value_script is read out of what the deposited scripts themselves
#   produce, and value_rewrite out of maintained/output/, so neither column can drift from
#   the code that fills it.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)
dep <- function(f) read_csv(here::here("ground_truth", "deposited_output", f), show_col_types = FALSE)

t2 <- out("table_2_avg_effect_by_primary.csv")
a1 <- out("table_a1_demographics.csv")
a2 <- out("table_a2_magnitude_by_primary.csv")
a3 <- out("table_a3_avg_effect_by_party.csv")
txt <- out("text_sample_and_weighting.csv")
fig1 <- out("figure_1_thresholds_by_primary.csv")
figa1 <- out("figure_a1_thresholds_by_party.csv")

cells <- dep("deposited_table_cells.csv")
status <- dep("script_status.csv")
panel_order <- dep("deposited_panel_order.csv")
dep_fig1 <- dep("deposited_figure_1_plot_data.csv")
dep_figa1 <- dep("deposited_figure_a1_plot_data.csv")

txt_value <- function(q) txt$value[txt$quantity == q]

# The deposited scripts print rounded, starred cells on their way to LaTeX, which is the
# form the article prints. Stars and parentheses are typography, not value.
cell_value <- function(tbl, r, c) {
  x <- cells$cell[cells$table == tbl & cells$row_index == r & cells$col_index == c]
  if (length(x) != 1) return(NA_real_)
  suppressWarnings(as.numeric(str_trim(str_remove_all(x, "[*()]"))))
}

# Which deposited column holds each published quantity, and whether it sits on the
# estimate row or the standard error row beneath it.
quantity_map <- tribble(
  ~quantity, ~col_index, ~on_se_row, ~label,
  "actual", 4, FALSE, "actual (%)",
  "predicted", 5, FALSE, "predicted if not for indictment (%)",
  "cf_diff", 6, FALSE, "counterfactual difference (pp)",
  "cf_se", 6, TRUE, "counterfactual standard error (pp)",
  "cf_p", NA, FALSE, "counterfactual p-value",
  "more_pct", 7, FALSE, "change format, more likely (%)",
  "less_pct", 8, FALSE, "change format, less likely (%)",
  "change_diff", 9, FALSE, "change format difference (pp)",
  "change_se", 9, TRUE, "change format standard error (pp)",
  "change_p", NA, FALSE, "change format p-value"
)

# Builds the rows for Table 2 and Table A.3, which share a layout: one estimate row and
# one standard error row per cell, in the same order in the article, in the deposited
# LaTeX and in the rewrite's csv.
effect_table_rows <- function(paper, rewrite, dep_table, table_figure, party_column) {
  paper |>
    mutate(cell = row_number()) |>
    pivot_longer(
      all_of(quantity_map$quantity),
      names_to = "quantity",
      values_to = "value_paper"
    ) |>
    left_join(quantity_map, by = "quantity") |>
    left_join(
      rewrite |>
        mutate(cell = row_number()) |>
        pivot_longer(all_of(quantity_map$quantity), names_to = "quantity", values_to = "value_rewrite") |>
        select(cell, quantity, value_rewrite),
      by = c("cell", "quantity")
    ) |>
    mutate(
      value_script = pmap_dbl(
        list(cell, col_index, on_se_row),
        function(cell, col_index, on_se_row) {
          if (is.na(col_index)) return(NA_real_)
          cell_value(dep_table, 2 * cell - if (on_se_row) 0 else 1, col_index)
        }
      ),
      table_figure = table_figure,
      claim = paste0(topic, ", ", party, ", ", label),
      decimals = if_else(quantity %in% c("cf_p", "change_p"), 3, 1),
      notes = if_else(
        is.na(value_paper) & quantity %in% c("cf_p", "change_p"),
        "The article prints this p-value as p < 0.001 rather than as a number; see the row counting those cells",
        if_else(
          quantity %in% c("cf_p", "change_p"),
          "The deposited script computes the p-value but prints only a significance star, so it has no value_script",
          ""
        )
      )
    ) |>
    select(table_figure, claim, value_script, value_paper, value_rewrite, decimals, notes)
}

# Table 2 ----
paper_t2 <- tribble(
  ~topic, ~party, ~actual, ~predicted, ~cf_diff, ~cf_se, ~cf_p, ~more_pct, ~less_pct, ~change_diff, ~change_se, ~change_p,
  "Believe Trump mishandled docs", "Republican", 27.1, 24.6, 2.5, 0.6, NA, 16.2, 39.6, -23.4, 2.8, NA,
  "Believe Trump mishandled docs", "Democratic", 85.3, 79.5, 5.8, 0.6, NA, 76.0, 5.0, 71.0, 2.3, NA,
  "Believe Trump mishandled docs", "Neither", 55.7, 54.2, 1.5, 0.9, 0.124, 37.4, 15.7, 21.6, 3.9, NA,
  "Vote for Trump (general election)", "Democratic", 11.1, 10.1, 0.9, 0.8, 0.217, 8.0, 60.2, -52.1, 2.7, NA,
  "Vote for Trump (general election)", "Neither", 42.2, 41.9, 0.3, 0.9, 0.752, 20.3, 30.8, -10.5, 4.0, 0.009,
  "Vote for Trump (primary election)", "Republican", 64.1, 65.7, -1.6, 0.6, 0.007, 43.0, 16.3, 26.7, 2.9, NA
)

t2_rows <- effect_table_rows(paper_t2, t2, "table_2", "Table 2")

# Table A.3 ----
paper_a3 <- tribble(
  ~topic, ~party, ~actual, ~predicted, ~cf_diff, ~cf_se, ~cf_p, ~more_pct, ~less_pct, ~change_diff, ~change_se, ~change_p,
  "Believe Trump mishandled docs", "Republican", 25.0, 22.5, 2.5, 0.6, NA, 16.7, 38.5, -21.8, 2.8, NA,
  "Believe Trump mishandled docs", "Democrat", 85.4, 80.3, 5.2, 0.6, NA, 73.9, 6.1, 67.8, 2.5, NA,
  "Believe Trump mishandled docs", "Independent", 59.2, 56.7, 2.5, 1.0, 0.017, 39.6, 15.3, 24.2, 4.4, NA,
  "Vote for Trump (general election)", "Republican", 64.8, 61.4, 3.4, 3.1, 0.277, 33.8, 15.3, 18.5, 7.4, 0.013,
  "Vote for Trump (general election)", "Democrat", 8.5, 7.9, 0.5, 0.7, 0.451, 7.1, 61.8, -54.7, 2.6, NA,
  "Vote for Trump (general election)", "Independent", 37.5, 37.0, 0.6, 1.0, 0.585, 19.0, 32.6, -13.6, 4.8, 0.005,
  "Vote for Trump (primary election)", "Republican", 67.4, 69.1, -1.6, 0.6, 0.010, 45.8, 14.1, 31.7, 3.0, NA,
  "Vote for Trump (primary election)", "Democrat", 35.3, 33.7, 1.6, 1.1, 0.182, 21.0, 39.9, -18.9, 15.7, 0.235,
  "Vote for Trump (primary election)", "Independent", 42.5, 44.9, -2.3, 2.6, 0.373, 23.4, 27.4, -4.0, 12.2, 0.742
)

a3_rows <- effect_table_rows(paper_a3, a3, "table_a3", "Table A.3")

# Seven cells in each table carry a p-value the article prints only as an inequality.
bounded_p <- function(rewrite, table_figure) {
  tibble(
    table_figure = table_figure,
    claim = "Cells whose p-value the article prints as p < 0.001, reproducing below 0.001",
    value_script = NA_real_,
    value_paper = 7,
    value_rewrite = sum(c(rewrite$cf_p, rewrite$change_p) < 0.001),
    decimals = 0,
    notes = "Counts the cells the article marks p < 0.001 that the rewrite also puts below 0.001"
  )
}

# Table A.2 ----
paper_a2 <- tribble(
  ~panel, ~effect, ~format, ~Republican, ~Democratic, ~Neither,
  "Beliefs", "Positive", "Change", 16.2, 76.0, 37.4,
  "Beliefs", "Positive", "Counterfactual", 33.3, 36.5, 29.5,
  "Beliefs", "Positive", "1-5%", 13.1, 9.7, 11.3,
  "Beliefs", "Positive", "6-10%", 6.1, 4.9, 4.8,
  "Beliefs", "Positive", "11-20%", 6.6, 8.4, 6.2,
  "Beliefs", "Positive", "20% or more", 7.5, 13.4, 7.3,
  "Beliefs", "Negative", "Change", 39.6, 5.0, 15.7,
  "Beliefs", "Negative", "Counterfactual", 15.0, 11.9, 20.5,
  "Beliefs", "Negative", "1-5%", 7.9, 6.5, 9.2,
  "Beliefs", "Negative", "6-10%", 2.6, 3.1, 4.8,
  "Beliefs", "Negative", "11-20%", 1.5, 1.3, 2.1,
  "Beliefs", "Negative", "20% or more", 3.0, 1.0, 4.3,
  "Beliefs", "No effect", "Change", 44.3, 19.0, 46.9,
  "Beliefs", "No effect", "Counterfactual", 51.8, 51.6, 50.0,
  "Vote choice", "Positive", "Change", 43.0, 8.0, 20.3,
  "Vote choice", "Positive", "Counterfactual", 13.0, 13.3, 22.6,
  "Vote choice", "Positive", "1-5%", 7.8, 7.4, 11.1,
  "Vote choice", "Positive", "6-10%", 1.4, 1.5, 2.7,
  "Vote choice", "Positive", "11-20%", 2.2, 1.0, 4.1,
  "Vote choice", "Positive", "20% or more", 1.5, 3.4, 4.6,
  "Vote choice", "Negative", "Change", 16.3, 60.2, 30.8,
  "Vote choice", "Negative", "Counterfactual", 17.7, 14.1, 19.9,
  "Vote choice", "Negative", "1-5%", 9.4, 8.8, 8.3,
  "Vote choice", "Negative", "6-10%", 2.2, 1.2, 4.1,
  "Vote choice", "Negative", "11-20%", 2.0, 3.3, 3.5,
  "Vote choice", "Negative", "20% or more", 4.1, 0.9, 3.9,
  "Vote choice", "No effect", "Change", 40.7, 31.8, 48.8,
  "Vote choice", "No effect", "Counterfactual", 69.3, 72.5, 57.5
)

a2_rows <-
  paper_a2 |>
  mutate(dep_table = if_else(panel == "Beliefs", "table_a2_beliefs", "table_a2_vote"),
         dep_row = rep(2:15, times = 2),
         cell = row_number()) |>
  pivot_longer(c(Republican, Democratic, Neither), names_to = "party", values_to = "value_paper") |>
  left_join(
    a2 |>
      mutate(cell = row_number()) |>
      pivot_longer(c(Republican, Democratic, Neither), names_to = "party", values_to = "value_rewrite") |>
      select(cell, party, value_rewrite),
    by = c("cell", "party")
  ) |>
  mutate(
    dep_col = recode(party, Republican = 3, Democratic = 4, Neither = 5),
    value_script = pmap_dbl(list(dep_table, dep_row, dep_col), cell_value),
    table_figure = "Table A.2",
    claim = paste0(panel, ", ", effect, ", ", format, ", ", party, " (%)"),
    decimals = 1,
    notes = ""
  ) |>
  select(table_figure, claim, value_script, value_paper, value_rewrite, decimals, notes)

# Table A.1 ----
# The deposit ships no script for this table, so there is nothing to put in value_script.
paper_a1 <- tribble(
  ~demographic, ~category, ~unweighted_pct, ~weighted_pct,
  "Gender", "Male", 48.1, 47.5,
  "Gender", "Female", 49.9, 50.2,
  "Age", "18-24", 5.5, 12.1,
  "Age", "25-34", 10.3, 17.5,
  "Age", "35-44", 13.1, 16.7,
  "Age", "45-54", 17.0, 16.0,
  "Age", "55-64", 21.0, 16.6,
  "Age", "65+", 33.0, 21.0,
  "Race and ethnicity", "White, non-Hispanic", 68.2, 63.9,
  "Race and ethnicity", "Black, non-Hispanic", 11.5, 12.7,
  "Race and ethnicity", "Hispanic", 12.5, 16.4,
  "Race and ethnicity", "Asian, non-Hispanic", 2.7, 5.4,
  "Race and ethnicity", "Other, non-Hispanic", 5.0, 1.6,
  "Educational attainment", "High school or less", 17.9, 38.5,
  "Educational attainment", "Some college/associate's", 31.1, 30.5,
  "Educational attainment", "Bachelor's", 28.5, 19.5,
  "Educational attainment", "Graduate degree", 22.5, 11.5,
  "Biden approval", "Approve", 43.3, 40.4,
  "Biden approval", "Disapprove", 54.0, 56.3
)

a1_rows <-
  paper_a1 |>
  pivot_longer(c(unweighted_pct, weighted_pct), names_to = "quantity", values_to = "value_paper") |>
  left_join(
    a1 |> pivot_longer(c(unweighted_pct, weighted_pct), names_to = "quantity", values_to = "value_rewrite"),
    by = c("demographic", "category", "quantity")
  ) |>
  transmute(
    table_figure = "Table A.1",
    claim = paste0(demographic, ", ", category, ", ",
                   recode(quantity, unweighted_pct = "unweighted (%)", weighted_pct = "weighted (%)")),
    value_script = NA_real_,
    value_paper,
    value_rewrite,
    decimals = 1,
    notes = "No deposited script produces this table; rebuilt here from the deposited respondent data"
  )

# Figures ----
# The published figures print no numbers, so what can be checked is whether the rewrite's
# plotted values match the ones the deposited scripts compute before they fail.
# The threshold column is the join key and is a seq() step, so a handful of its values
# carry a last-bit representation error. The two files were written by different csv
# writers, one of which prints it and one of which does not, so the key is rounded before
# joining. The proportions themselves are compared at full precision.
figure_agreement <- function(rewrite, deposited, keys) {
  prepare <- function(d, value_name) {
    d |>
      mutate(threshold = round(threshold, 6)) |>
      select(all_of(keys), !!value_name := P)
  }
  joined <- inner_join(prepare(rewrite, "P_rewrite"), prepare(deposited, "P_deposited"), by = keys)
  stopifnot(nrow(joined) == nrow(rewrite), nrow(joined) == nrow(deposited))
  list(n = nrow(joined), max_diff = max(abs(joined$P_rewrite - joined$P_deposited)))
}

fig1_agree <- figure_agreement(fig1, dep_fig1, c("topic", "primary_party", "sign", "threshold", "measure"))
figa1_agree <- figure_agreement(figa1, dep_figa1, c("topic", "party", "sign", "threshold", "measure"))

# The report quotes these, so they are written out rather than recomputed there.
write_csv(
  tibble(
    figure = c("Figure 1", "Figure A.1"),
    proportions = c(fig1_agree$n, figa1_agree$n),
    max_abs_difference = c(fig1_agree$max_diff, figa1_agree$max_diff)
  ),
  here::here("ground_truth", "figure_agreement.csv")
)

# Which group each panel letter belongs to, on the deposit's side and on the rewrite's ----
# The deposited order comes from the export calls in figure_1.R; the rewrite's comes from
# the names of the files it wrote, so neither is a typed assertion about a panel.
dep_panel <- function(letter) {
  panel_order$group[panel_order$script == "figure_1.R" & panel_order$panel_letter == letter]
}

rewrite_panel <- function(letter) {
  f <- list.files(here::here("maintained", "output"),
                  pattern = paste0("^figure_1", letter, "_thresholds_.+\\.pdf$"))
  str_remove(str_remove(f, paste0("^figure_1", letter, "_thresholds_")), "\\.pdf$")
}

# What the printed panel shows, in the proportions the panel itself plots. The article's
# Table A.2 gives the weighted versions of these; Figure 1 plots unweighted counts.
change_share <- function(party, s) {
  100 * fig1$P[fig1$topic == "docs" & str_detect(fig1$measure, "Change") &
                 fig1$primary_party == party & fig1$sign == s & fig1$threshold == -0.01]
}

figure_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~notes,
  "Figure 1", "All plotted proportions", NA_real_, NA_real_, NA_real_,
  paste0("The figure prints no numbers, so there is nothing to compare against the page. All ",
         fig1_agree$n, " plotted proportions agree with the values the deposited figure_1.R computes ",
         "before it fails, to within ", format(fig1_agree$max_diff, digits = 2), ". Table A.2 is the article's own numerical version of this figure and is checked cell by cell above"),
  "Figure A.1", "All plotted proportions", NA_real_, NA_real_, NA_real_,
  paste0("As for Figure 1: all ", figa1_agree$n, " plotted proportions agree with the deposited ",
         "figure_A1.R values to within ", format(figa1_agree$max_diff, digits = 2)),
  "Figure 1", "Panel (b) shows Republican primary voters (1 = yes)",
  as.numeric(dep_panel("b") == "Republican"), 1, as.numeric(rewrite_panel("b") == "republican"),
  paste0("Does not hold. The caption and the body text both assign panel (b) to Republican primary ",
         "voters, but the panel the article prints is the Democratic one: its belief column shows ",
         sprintf("%.1f", change_share("Democratic", 1)), " per cent saying more likely against ",
         sprintf("%.1f", change_share("Democratic", -1)), " per cent less likely, and its vote column ",
         "is the general election. The deposited figure_1.R writes the panels in the order ",
         paste(panel_order$group[panel_order$script == "figure_1.R"], collapse = ", "),
         ", which is the order the article prints them in"),
  "Figure 1", "Panel (c) shows Democratic primary voters (1 = yes)",
  as.numeric(dep_panel("c") == "Democratic"), 1, as.numeric(rewrite_panel("c") == "democratic"),
  paste0("The other half of the same transposition: panel (c) carries the Republican primary column ",
         "and a belief split of ", sprintf("%.1f", change_share("Republican", 1)), " per cent more ",
         "likely against ", sprintf("%.1f", change_share("Republican", -1)), " per cent less likely"),
  "Table 1", "Randomized question format conditions", NA_real_, NA_real_, NA_real_,
  "The table prints the two question wordings and no numbers, so it carries nothing to verify"
)

# In-text quantities ----
text_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~notes,
  "Text, p. 1219", "Respondents who completed the survey", NA_real_, 5011, txt_value("n_respondents_deposited"),
  "The deposited data carry 4,838 respondents, 173 fewer than the article describes, with ids running 1 to 4838 and no gaps",
  "Text, p. 1219", "Respondents who began the survey", NA_real_, 6877, NA_real_,
  "Respondents who did not complete are not in the deposit, so the denominator cannot be rebuilt",
  "Text, p. 1219", "Completion rate (%)", NA_real_, 73, NA_real_,
  "Follows from the two counts above and is unverifiable for the same reason",
  "Text, p. 1219", "Weighted Biden approval (%)", NA_real_, 40.4, txt_value("approve_weighted_pct"),
  "Weighted share approving, strongly or somewhat. The gap is the same 173 missing respondents",
  "Text, p. 1219", "FiveThirtyEight Biden approval, June 26 2023 (%)", NA_real_, 40.3, NA_real_,
  "An external benchmark, not a quantity the survey produces",
  "Figure 1 caption", "N, respondents intending neither primary", NA_real_, 754, txt_value("n_primary_neither"),
  "The caption's three Ns are the partisan identity counts, which belong to Figure A.1: 754 is the number of Independents, and the number intending neither primary is 889",
  "Figure 1 caption", "N, Republican primary voters", NA_real_, 2026, txt_value("n_primary_republican"),
  "2,026 is the number of Republican identifiers; 2,028 intend to vote in the Republican primary",
  "Figure 1 caption", "N, Democratic primary voters", NA_real_, 1953, txt_value("n_primary_democratic"),
  "1,953 is the number of Democratic identifiers; 1,921 intend to vote in the Democratic primary",
  "Text, p. 1217", "Republican primary voters, chance of voting for Trump (%)", NA_real_, 64.1,
  t2$actual[t2$Party == "Republican" & str_detect(t2$Topic, "primary")], "",
  "Text, p. 1217", "Republican primary voters, chance if not for the indictment (%)", NA_real_, 65.7,
  t2$predicted[t2$Party == "Republican" & str_detect(t2$Topic, "primary")], "",
  "Text, p. 1222", "Republican primary voters, effect on primary vote (pp)", NA_real_, -1.6,
  t2$cf_diff[t2$Party == "Republican" & str_detect(t2$Topic, "primary")],
  "The sentence prints the magnitude as 1.6 with no minus sign and gives the direction in words; Table 2, the abstract and page 1231 all print -1.6",
  "Text, p. 1222", "Republican primary voters, effect on primary vote, p-value", NA_real_, 0.02,
  t2$cf_p[t2$Party == "Republican" & str_detect(t2$Topic, "primary")],
  "The text gives p = 0.02 for an estimate its own Table 2 reports at p = 0.007, which is what the data give",
  "Text, p. 1222", "Republican primary voters, effect on belief (pp)", NA_real_, 2.5,
  t2$cf_diff[t2$Party == "Republican" & str_detect(t2$Topic, "Believe")], "",
  "Text, p. 1222", "Democratic primary voters, effect on belief (pp)", NA_real_, 5.8,
  t2$cf_diff[t2$Party == "Democratic" & str_detect(t2$Topic, "Believe")], "",
  "Text, p. 1222", "Democratic primary voters, effect on general election vote (pp)", NA_real_, 0.9,
  t2$cf_diff[t2$Party == "Democratic" & str_detect(t2$Topic, "general")], "",
  "Text, p. 1222", "Democratic primary voters, effect on general election vote, p-value", NA_real_, 0.22,
  t2$cf_p[t2$Party == "Democratic" & str_detect(t2$Topic, "general")], "",
  "Text, p. 1222", "Neither primary, effect on belief (pp)", NA_real_, 1.8,
  t2$cf_diff[t2$Party == "Neither" & str_detect(t2$Topic, "Believe")],
  "The text reports +1.8 pp with p = 0.05 where its own Table 2 reports 1.5 pp with p = 0.124. The data give the table's value",
  "Text, p. 1222", "Neither primary, effect on belief, standard error (pp)", NA_real_, 0.9,
  t2$cf_se[t2$Party == "Neither" & str_detect(t2$Topic, "Believe")], "",
  "Text, p. 1222", "Neither primary, effect on belief, p-value", NA_real_, 0.05,
  t2$cf_p[t2$Party == "Neither" & str_detect(t2$Topic, "Believe")],
  "See the estimate above; Table 2 prints 0.124",
  "Text, p. 1222", "Neither primary, effect on general election vote (pp)", NA_real_, 0.1,
  t2$cf_diff[t2$Party == "Neither" & str_detect(t2$Topic, "general")],
  "The text reports +0.1 pp with p = 0.89 where Table 2 reports 0.3 pp with p = 0.752. The data give the table's value",
  "Text, p. 1222", "Neither primary, effect on general election vote, standard error (pp)", NA_real_, 0.9,
  t2$cf_se[t2$Party == "Neither" & str_detect(t2$Topic, "general")], "",
  "Text, p. 1222", "Neither primary, effect on general election vote, p-value", NA_real_, 0.89,
  t2$cf_p[t2$Party == "Neither" & str_detect(t2$Topic, "general")],
  "See the estimate above; Table 2 prints 0.752",
  "Text, p. 1231", "Republican identifiers in the Republican primary, chance of voting for Trump (%)",
  NA_real_, 67.4, a3$actual[a3$Party == "Republican" & str_detect(a3$Topic, "primary")], "",
  "Text, p. 1231", "Republican identifiers in the Republican primary, chance if not for the indictment (%)",
  NA_real_, 69.1, a3$predicted[a3$Party == "Republican" & str_detect(a3$Topic, "primary")], ""
)

# Assemble ----
# Every cell in the four tables is printed to one decimal place and every p-value to
# three, which the typed values cannot record on their own: R does not distinguish 21
# from 21.0, and reading the precision off the number would let a cell agree to within
# half a point where the article prints tenths.
gt <-
  bind_rows(
    t2_rows,
    bounded_p(t2, "Table 2"),
    a2_rows,
    a3_rows,
    bounded_p(a3, "Table A.3"),
    a1_rows,
    figure_rows,
    text_rows
  )

# Agreement at the precision the article prints ----
printed_decimals <- function(x) {
  map_dbl(x, function(v) {
    if (is.na(v)) return(NA_real_)
    txt <- str_remove(formatC(v, format = "fg", digits = 15, flag = "#"), "0+$")
    if (str_detect(txt, "\\.")) nchar(str_remove(txt, "^.*\\.")) else 0
  })
}

agrees <- function(value, target, decimals) {
  case_when(
    is.na(value) | is.na(target) ~ NA_real_,
    abs(value - target) <= 0.5 * 10^(-decimals) ~ 1,
    .default = 0
  )
}

gt <-
  gt |>
  mutate(
    paper_id = "barari_etal_2024",
    decimals = coalesce(decimals, printed_decimals(value_paper)),
    match = agrees(value_script, value_paper, decimals),
    match_rewrite = agrees(value_rewrite, value_paper, decimals),
    defect_locus = case_when(
      is.na(match_rewrite) | match_rewrite == 1 ~ NA_character_,
      table_figure == "Table A.1" ~ "archive",
      str_detect(claim, "completed the survey|Biden approval") ~ "archive",
      .default = "paper_internal"
    ),
    notes = if_else(
      !is.na(match_rewrite) & match_rewrite == 0 & defect_locus == "archive" & notes == "",
      "Does not reproduce from the deposit, which carries 4,838 of the 5,011 respondents the article describes",
      notes
    )
  ) |>
  select(paper_id, table_figure, claim, value_script, value_paper, match,
         value_rewrite, match_rewrite, defect_locus, notes)

# Gate: the article's claims are all covered ----
# ground_truth/published_claims.csv is the exhaustive extraction: every numeric token in
# the article and its appendix, with its location and a hand-assigned type. Pipeline and
# descriptive claims must be checked by maintained/in_text_claims.R, as must the
# definitional and structural claims the pipeline can actually reach. The rest are question
# wordings, field dates, and numbers copied out of other people's polls, which are verified
# against those sources rather than by an entry here.
#
# The check runs the claims file rather than reading it. A marker comment proves an entry
# was written, not that it runs: an entry that errors, or that prints nothing, satisfies a
# textual check completely. So the file is sourced in its own environment, the CLAIM lines
# it prints are counted, and both the count and the identifiers are compared with the
# extraction.
published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = "c")
)

stopifnot(!anyDuplicated(published_claims$claim_id))

recoverable_claims <- c(
  "design_field_dates", "design_n_formats", "design_counterfactual_n_questions",
  "table_1_scale_min", "table_1_scale_max", "table_1_change_response_options",
  "table_2_note_scale_points", "table_2_note_change_scale_points",
  "why_scale_points", "why_n_categories", "why_threshold_zero", "why_threshold_example",
  "figure_1_axis_threshold_max", "discussion_scale_points",
  str_subset(published_claims$claim_id, "^table_a1_age_bin_"),
  str_subset(published_claims$claim_id, "^table_a2_bin_")
)

stopifnot(all(recoverable_claims %in% published_claims$claim_id))

needs_entry <-
  published_claims |>
  filter(claim_type %in% c("pipeline", "descriptive") | claim_id %in% recoverable_claims)

claims_output <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env())
)

printed_claims <-
  tibble(line = str_subset(claims_output, "^CLAIM ")) |>
  transmute(claim_id = str_match(line, "^CLAIM (\\S+) = ")[, 2])

stopifnot(!anyDuplicated(printed_claims$claim_id))

missing_entry <- setdiff(needs_entry$claim_id, printed_claims$claim_id)
unclaimed <- setdiff(printed_claims$claim_id, published_claims$claim_id)
surplus <- setdiff(printed_claims$claim_id, needs_entry$claim_id)

if (length(missing_entry) + length(unclaimed) + length(surplus) > 0) {
  stop(str_glue(
    "maintained/in_text_claims.R does not cover ground_truth/published_claims.csv. ",
    "Claims with no entry ({length(missing_entry)}): {str_c(head(missing_entry, 20), collapse = ', ')}. ",
    "Entries naming a claim the article does not make ({length(unclaimed)}): {str_c(head(unclaimed, 20), collapse = ', ')}. ",
    "Entries for claims that need none ({length(surplus)}): {str_c(head(surplus, 20), collapse = ', ')}."
  ))
}

# The count is what the identifier comparison cannot do on its own: a file that dies part
# way through prints a prefix of its entries, and counting the lines against the extraction
# is what says so.
stopifnot(nrow(printed_claims) == nrow(needs_entry))

# Every entry declares the claim it covers, and the declarations must name the set the run
# printed. A * in a marker is a wildcard, which is how one entry declares a whole table.
markers <- str_trim(str_remove(
  str_subset(read_lines(here::here("maintained", "in_text_claims.R")), "^#\\s*covers:"),
  "^#\\s*covers:"
))

covered <- unique(unlist(map(markers, function(marker) {
  str_subset(
    published_claims$claim_id,
    str_c("^", str_replace_all(str_escape(marker), fixed("\\*"), ".*"), "$")
  )
})))

undeclared <- setdiff(printed_claims$claim_id, covered)
overdeclared <- setdiff(covered, printed_claims$claim_id)

if (length(undeclared) + length(overdeclared) > 0) {
  stop(str_glue(
    "The covers markers in maintained/in_text_claims.R do not match what it prints. ",
    "Printed but not declared ({length(undeclared)}): {str_c(head(undeclared, 20), collapse = ', ')}. ",
    "Declared but not printed ({length(overdeclared)}): {str_c(head(overdeclared, 20), collapse = ', ')}."
  ))
}

write_csv(gt, here::here("ground_truth", "barari_etal_2024_ground_truth.csv"))

print(gt |> select(table_figure, claim, value_script, value_paper, match, value_rewrite, match_rewrite),
      n = nrow(gt), width = 250)
print(count(gt, table_figure, match_rewrite), n = 100)
print(str_glue(
  "rows: {nrow(gt)}  ",
  "script match=1: {sum(gt$match == 1, na.rm = TRUE)} of {sum(!is.na(gt$match))}  ",
  "rewrite match=1: {sum(gt$match_rewrite == 1, na.rm = TRUE)} of {sum(!is.na(gt$match_rewrite))}"
))
print(count(filter(gt, !is.na(defect_locus)), defect_locus))
print(count(published_claims, claim_type))
print(str_glue(
  "published claims: {nrow(published_claims)}  ",
  "needing an entry in maintained/in_text_claims.R: {nrow(needs_entry)}  ",
  "entries printed: {nrow(printed_claims)}"
))
print(status, width = 200)
