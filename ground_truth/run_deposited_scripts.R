# barari_etal_2024/ground_truth/run_deposited_scripts.R
# Output: ground_truth/deposited_output/script_status.csv,
#   ground_truth/deposited_output/deposited_panel_order.csv,
#   ground_truth/deposited_output/deposited_table_cells.csv,
#   ground_truth/deposited_output/deposited_figure_1_plot_data.csv,
#   ground_truth/deposited_output/deposited_figure_a1_plot_data.csv
# Depends on: original/ (run download_original.R first)
# Description: Run each deposited script in a throwaway copy of the archive and record
#   what it did. Three things come back: whether the script completes in a clean session,
#   whether it still completes once the copy is stripped to data plus code, and the numbers
#   it produces, read out of the objects the script itself builds rather than typed from a
#   console transcript. The copy matters because a script run inside original/ can overwrite
#   deposited files. The stripped run matters because this deposit ships the intermediate
#   objects its own first script is supposed to write, so a script can appear to pass on
#   files that nothing in the archive actually produced.

library(here)
library(tidyverse)

here::i_am("ground_truth/run_deposited_scripts.R")

out_dir <- here::here("ground_truth", "deposited_output")
dir.create(out_dir, showWarnings = FALSE)

# Throwaway copy of the deposit ----
work_dir <- file.path(tempdir(), "deposited_archive")
unlink(work_dir, recursive = TRUE)
dir.create(work_dir, recursive = TRUE)
invisible(file.copy(list.files(here::here("original"), full.names = TRUE), work_dir))

# One driver per deposited script ----
# Each driver sources the deposited file unmodified and then writes out the objects that
# file leaves behind. The figure drivers wrap the source() call in try() because both
# figure scripts die on their last statement, after every quantity has been computed.
drivers <- list(
  prep_data = 'source("prep_data.R")',
  figure_1 = 'res <- try(source("figure_1.R"), silent = TRUE)
write.csv(tab_plot, "figure_1_plot_data.csv", row.names = FALSE)
if (inherits(res, "try-error")) stop(attr(res, "condition"))',
  figure_A1 = 'res <- try(source("figure_A1.R"), silent = TRUE)
write.csv(tab_plot_party, "figure_a1_plot_data.csv", row.names = FALSE)
if (inherits(res, "try-error")) stop(attr(res, "condition"))',
  table_2 = 'source("table_2.R")
writeLines(out, "table_2_rows.txt")',
  table_A2 = 'source("table_A2.R")
writeLines(tab_effect_sizes_beliefs, "table_a2_beliefs_rows.txt")
writeLines(tab_effect_sizes_vote, "table_a2_vote_rows.txt")',
  table_A3 = 'source("table_A3.R")
writeLines(out, "table_a3_rows.txt")'
)

rscript <- file.path(R.home("bin"), "Rscript")

run_one <- function(name, driver, dir, tag) {
  driver_path <- file.path(dir, paste0("driver_", tag, "_", name, ".R"))
  # The deposited scripts load their data by bare filename, so the driver moves into the
  # throwaway copy first. Doing it inside the driver keeps run_all.R's session where it is.
  write_lines(c(str_glue('setwd("{dir}")'), driver), driver_path)
  log_path <- file.path(dir, paste0("driver_", tag, "_", name, ".log"))
  status <- system2(
    rscript,
    c("--vanilla", shQuote(driver_path)),
    stdout = log_path, stderr = log_path, env = paste0("R_LIBS_USER=", shQuote(.libPaths()[1]))
  )
  log_lines <- read_lines(log_path)
  first_error <- log_lines[str_detect(log_lines, "^Error")]
  tibble(
    script = paste0(name, ".R"),
    exit_status = status,
    completes = status == 0,
    message = if (length(first_error) > 0) str_squish(first_error[1]) else ""
  )
}

script_status <- imap(drivers, \(driver, name) run_one(name, driver, work_dir, "full")) |> list_rbind()

# The same six scripts against a copy holding only the data and the code ----
# prep_data.R is the only script that reads data.csv; the other five read the three .rds
# objects prep_data.R would write if its write_rds() calls were not commented out. Stripping
# the copy separates a script that works from a script that works on shipped intermediates.
strip_dir <- file.path(tempdir(), "deposited_archive_stripped")
unlink(strip_dir, recursive = TRUE)
dir.create(strip_dir, recursive = TRUE)
keep <- c("data.csv", str_subset(list.files(here::here("original")), "\\.R$"))
invisible(file.copy(file.path(here::here("original"), keep), strip_dir))

stripped_status <-
  imap(drivers, \(driver, name) run_one(name, str_glue('source("{name}.R")'), strip_dir, "strip")) |>
  list_rbind() |>
  transmute(script, completes_stripped = completes, message_stripped = message)

script_status <-
  script_status |>
  rename(exit_status_as_deposited = exit_status,
         completes_as_deposited = completes,
         message_as_deposited = message) |>
  left_join(stripped_status, by = "script") |>
  select(script, exit_status_as_deposited, completes_as_deposited, completes_stripped,
         message_as_deposited, message_stripped)

write_csv(script_status, file.path(out_dir, "script_status.csv"))
print(script_status, width = 200)

# Which group each deposited figure script plots into which panel ----
# The article letters Figure 1's panels and assigns groups to those letters in its caption.
# The order the deposited script exports them in is the check on that assignment, and it is
# read out of the script rather than asserted.
panel_order <-
  tibble(script = c("figure_1.R", "figure_A1.R")) |>
  mutate(
    group = map(script, function(s) {
      lines <- read_lines(file.path(here::here("original"), s))
      str_match(str_subset(lines, "theParty = \""), 'theParty = "([^"]+)"')[, 2]
    })
  ) |>
  unnest_longer(group, indices_to = "panel_index") |>
  mutate(panel_letter = letters[panel_index])

write_csv(panel_order, file.path(out_dir, "deposited_panel_order.csv"))
print(panel_order, n = nrow(panel_order))

# Figure data the deposited scripts computed before failing ----
invisible(file.copy(
  file.path(work_dir, c("figure_1_plot_data.csv", "figure_a1_plot_data.csv")),
  file.path(out_dir, c("deposited_figure_1_plot_data.csv", "deposited_figure_a1_plot_data.csv")),
  overwrite = TRUE
))

# Table cells, split out of the LaTeX the deposited scripts emit ----
# Cells are kept as printed text: the deposit rounds and adds significance stars on its
# way to the page, and that rounded form is exactly what the article prints.
parse_latex_rows <- function(path, table_name) {
  rows <- read_lines(path)
  rows <- rows[str_detect(rows, "&")]
  tibble(row_index = seq_along(rows), raw = rows) |>
    mutate(
      raw = str_remove(raw, fixed("\\\\[-1.5ex]")),
      raw = str_remove(raw, "\\\\\\\\\\s*$"),
      cell = str_split(raw, fixed("&"))
    ) |>
    select(-raw) |>
    unnest_longer(cell, indices_to = "col_index") |>
    transmute(table = table_name, row_index, col_index, cell = str_squish(cell))
}

deposited_cells <-
  bind_rows(
    parse_latex_rows(file.path(work_dir, "table_2_rows.txt"), "table_2"),
    parse_latex_rows(file.path(work_dir, "table_a2_beliefs_rows.txt"), "table_a2_beliefs"),
    parse_latex_rows(file.path(work_dir, "table_a2_vote_rows.txt"), "table_a2_vote"),
    parse_latex_rows(file.path(work_dir, "table_a3_rows.txt"), "table_a3")
  )

write_csv(deposited_cells, file.path(out_dir, "deposited_table_cells.csv"))
print(count(deposited_cells, table), width = 200)
