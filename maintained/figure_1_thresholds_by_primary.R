# barari_etal_2024/maintained/figure_1_thresholds_by_primary.R
# Output: output/figure_1a_thresholds_neither.{pdf,png},
#   output/figure_1b_thresholds_democratic.{pdf,png},
#   output/figure_1c_thresholds_republican.{pdf,png},
#   output/figure_1_thresholds_by_primary.csv
# Depends on: clean_data.R output, helpers.R
# Description: Distribution of self-reported effects by intended primary and question
#   format (Figure 1), one file per published panel, plus the plotted values.

source(here::here("maintained", "helpers.R"))

long_topic <- read_rds(here::here("maintained", "clean_data", "long_topic_clean.rds"))

# Counterfactual format, counted at every threshold on the horizontal axis ----
# A respondent counts as changed only if the gap between their two answers reaches the
# threshold, so the shares at threshold 0 count any difference at all.
summary_by_threshold <- function(x) {
  long_topic |>
    filter(!is.na(sign)) |>
    mutate(sign = if_else(abs(tau) < x, 0, sign)) |>
    group_by(topic, topic2, Topic, primary_party, sign) |>
    summarize(threshold = x, N = n(), .groups = "drop")
}

tab_thresholds <-
  map(seq(0, 0.50, 0.01), summary_by_threshold) |>
  list_rbind() |>
  group_by(topic, topic2, Topic, primary_party, threshold) |>
  mutate(P = N / sum(N), measure = "Counterfactual") |>
  ungroup()

# Change format, which has no threshold ----
tab_change <-
  long_topic |>
  filter(!is.na(change)) |>
  group_by(topic, topic2, Topic, primary_party, sign = change) |>
  summarize(N = n(), .groups = "drop_last") |>
  mutate(
    P = N / sum(N),
    measure = "Change",
    measure = if_else(topic == "docs", measure, paste0("     ", measure))
  ) |>
  ungroup()

# The change format occupies the negative part of the axis so both formats can share it
tab_change <- map((-1:-16) / 100, \(x) tab_change |> mutate(threshold = x)) |> list_rbind()

tab_plot <-
  bind_rows(tab_change, tab_thresholds) |>
  group_by(topic, topic2, primary_party, measure, threshold) |>
  mutate(
    label_Y_position = cumsum(P) - (P * 0.5),
    Label = recode(sign, `-1` = "Less\nlikely", `1` = "More\nlikely", `0` = "No\nchange"),
    # The Democratic "less likely" band is too thin to carry a legible label.
    Label = if_else(primary_party == "Democratic" & sign == -1, NA_character_, Label)
  ) |>
  ungroup()

write_csv(tab_plot, here::here("maintained", "output", "figure_1_thresholds_by_primary.csv"))

make_plot_thresholds <- function(the_party) {
  ggplot(
    tab_plot |> filter(primary_party == the_party),
    aes(x = as.numeric(threshold), y = P, color = sign, fill = sign)
  ) +
    geom_col() +
    geom_text(
      data = tab_plot |> filter(threshold == -0.09, primary_party == the_party, topic == "docs"),
      aes(label = Label, y = label_Y_position),
      position = position_nudge(0.005),
      color = "white",
      vjust = 0.5,
      lineheight = 0.9,
      size = 2.8,
      fontface = "bold"
    ) +
    geom_blank(
      data = tab_change |> filter(topic != "docs", primary_party == the_party),
      aes(x = -0.22, y = 1)
    ) +
    scale_x_continuous(breaks = (0:5) / 10, expand = c(0, 0), labels = (0:5) * 10) +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      panel.spacing.x = unit(0.05, "cm"),
      strip.text.x = element_text(margin = margin(t = 2, b = 3)),
      axis.ticks.x = element_line(),
      axis.title = element_text(size = 9),
      axis.title.x = element_text(margin = margin(t = 8))
    ) +
    labs(x = "Threshold for counting as attitude change", y = "Percent of respondents") +
    facet_nested(~ Topic + measure, space = "free_x", scales = "free_x")
}

the_width <- 5
the_height <- 2.75

# Panels are lettered as the article prints them: (a) neither primary, (b) Democratic
# primary, (c) Republican primary.
panels <- tribble(
  ~party, ~stem,
  "Neither", "figure_1a_thresholds_neither",
  "Democratic", "figure_1b_thresholds_democratic",
  "Republican", "figure_1c_thresholds_republican"
)

walk2(panels$party, panels$stem, function(party, stem) {
  g <- make_plot_thresholds(party)
  ggsave(here::here("maintained", "output", paste0(stem, ".pdf")),
         plot = g, width = the_width, height = the_height)
  ggsave(here::here("maintained", "output", paste0(stem, ".png")),
         plot = g, width = the_width, height = the_height, dpi = 300)
})
