# barari_etal_2024/maintained/figure_a1_thresholds_by_party.R
# Output: output/figure_a1a_thresholds_independent.{pdf,png},
#   output/figure_a1b_thresholds_democrat.{pdf,png},
#   output/figure_a1c_thresholds_republican.{pdf,png},
#   output/figure_a1_thresholds_by_party.csv
# Depends on: clean_data.R output, helpers.R
# Description: Distribution of self-reported effects by partisan identity and question
#   format (Figure A.1), the party-identity counterpart of Figure 1.

source(here::here("maintained", "helpers.R"))

long_topic <- read_rds(here::here("maintained", "clean_data", "long_topic_clean.rds"))

# Counterfactual format at every threshold ----
summary_by_threshold_party <- function(x) {
  long_topic |>
    filter(!is.na(sign), !is.na(party)) |>
    mutate(sign = if_else(abs(tau) < x, 0, sign)) |>
    group_by(topic, topic2, Topic, party, sign) |>
    summarize(threshold = x, N = n(), .groups = "drop")
}

tab_thresholds_party <-
  map(seq(0, 0.50, 0.01), summary_by_threshold_party) |>
  list_rbind() |>
  group_by(topic, topic2, Topic, party, threshold) |>
  mutate(P = N / sum(N), measure = "Counterfactual") |>
  ungroup()

# Change format ----
tab_change_party <-
  long_topic |>
  filter(!is.na(change), !is.na(party)) |>
  group_by(topic, topic2, Topic, party, sign = change) |>
  summarize(N = n(), .groups = "drop_last") |>
  mutate(
    P = N / sum(N),
    measure = "Change",
    measure = if_else(topic == "docs", measure, paste0("     ", measure))
  ) |>
  ungroup()

tab_change_party <-
  map((-1:-16) / 100, \(x) tab_change_party |> mutate(threshold = x)) |> list_rbind()

tab_plot_party <-
  bind_rows(tab_change_party, tab_thresholds_party) |>
  group_by(topic, topic2, party, measure, threshold) |>
  mutate(
    label_Y_position = cumsum(P) - (P * 0.5),
    Label = recode(sign, `-1` = "Less\nlikely", `1` = "More\nlikely", `0` = "No\nchange"),
    # The Democratic "less likely" band is too thin to carry a legible label.
    Label = if_else(party == "Democrat" & sign == -1, NA_character_, Label),
    Topic = gsub("ument", "", Topic)
  ) |>
  ungroup()

write_csv(tab_plot_party, here::here("maintained", "output", "figure_a1_thresholds_by_party.csv"))

make_plot_thresholds_party <- function(the_party) {
  ggplot(
    tab_plot_party |> filter(party == the_party),
    aes(x = as.numeric(threshold), y = P, color = sign, fill = sign)
  ) +
    geom_col() +
    geom_text(
      data = tab_plot_party |> filter(threshold == -0.09, party == the_party, topic == "docs"),
      aes(label = Label, y = label_Y_position),
      position = position_nudge(0.005),
      color = "white",
      vjust = 0.5,
      lineheight = 0.9,
      size = 2.8,
      fontface = "bold"
    ) +
    geom_blank(
      data = tab_change_party |> filter(topic != "docs", party == the_party),
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

the_width <- 6.5
the_height <- 2.75

panels <- tribble(
  ~party, ~stem,
  "Independent", "figure_a1a_thresholds_independent",
  "Democrat", "figure_a1b_thresholds_democrat",
  "Republican", "figure_a1c_thresholds_republican"
)

walk2(panels$party, panels$stem, function(party, stem) {
  g <- make_plot_thresholds_party(party)
  ggsave(here::here("maintained", "output", paste0(stem, ".pdf")),
         plot = g, width = the_width, height = the_height)
  ggsave(here::here("maintained", "output", paste0(stem, ".png")),
         plot = g, width = the_width, height = the_height, dpi = 300)
})
