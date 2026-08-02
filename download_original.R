# barari_etal_2024/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse and verify every file.
#   Run this once before running anything in maintained/. Re-running is free: files
#   already present with the right checksum are not downloaded again. The deposit is
#   about 52 MB, most of it one reshaped data object, so a first run takes a minute.
#
#   The manifest carries two checksums per file. md5_served is the MD5 of the bytes
#   Dataverse returns for `?format=original`, which is what this code was written
#   against. md5_published is the checksum Dataverse displays. Here all twelve agree,
#   but they do not always, so verification runs against md5_served and any disagreement
#   is reported rather than silently failing the check.
#
#   One file, data.csv, was ingested by Dataverse into a tabular .tab representation;
#   the served_as column records the name Dataverse gives that derived file. The unf
#   column carries its Universal Numeric Fingerprint. `?format=original` returns the
#   deposited csv, not the .tab.
#
#   The check at the end is deliberately two-sided. Every manifest file must be present
#   with the right bytes, and original/ must contain nothing else, because a stray file
#   left behind by a previous run is exactly the difference a reproduction check exists
#   to catch.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/QDOBJF"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

dir.create(here::here("original"), showWarnings = FALSE)
walk(unique(dirname(manifest$file)), function(d) {
  if (d != ".") dir.create(here::here("original", d), recursive = TRUE, showWarnings = FALSE)
})

# Download what is missing or wrong ----
# format=original asks for the deposited bytes rather than the tabular representation
# Dataverse derives for ingested files.
planned <-
  manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify ----
verified <-
  planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    match = md5_downloaded == md5_served,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, md5_served, md5_downloaded, match, published_agrees)

print(verified, n = nrow(verified))

if (!all(verified$match)) {
  stop("Checksum mismatch: the downloaded archive does not match what Dataverse served when this code was written.")
}

extra <- setdiff(list.files(here::here("original"), recursive = TRUE), manifest$file)
if (length(extra) > 0) {
  print(str_glue("original/ holds {length(extra)} file(s) that are not in the deposit: ",
                 "{paste(extra, collapse = ', ')}"))
  stop("original/ must contain the deposit and nothing else.")
}

print(str_glue("All {nrow(verified)} files match, and original/ contains nothing else. ",
               "{sum(!verified$published_agrees)} carry a published checksum that disagrees."))
print(str_glue("Archive: {dataset_doi}"))
