# barari_etal_2024/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive, reshape it,
# then every published table, then the figures, then the in-text quantities, then the
# check on the deposited scripts and the ground truth built from all of it. Every script
# is self-contained and can also be run on its own once clean_data.R has run.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Data preparation ----
source(here::here("maintained", "clean_data.R"))

# Tables ----
source(here::here("maintained", "table_a1_demographics.R"))
source(here::here("maintained", "table_2_avg_effect_by_primary.R"))
source(here::here("maintained", "table_a2_magnitude_by_primary.R"))
source(here::here("maintained", "table_a3_avg_effect_by_party.R"))

# Figures ----
source(here::here("maintained", "figure_1_thresholds_by_primary.R"))
source(here::here("maintained", "figure_a1_thresholds_by_party.R"))

# In-text quantities ----
source(here::here("maintained", "text_sample_and_weighting.R"))

# The deposited scripts, run in a throwaway copy of the archive ----
source(here::here("ground_truth", "run_deposited_scripts.R"))

# Ground truth ----
# Rebuilt from the outputs above, so it cannot go stale.
source(here::here("ground_truth", "build_ground_truth.R"))

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ was intact
# before anything ran. Nothing above writes to original/, and this second pass is what
# demonstrates it rather than assuming it. Nothing is downloaded; the files are already
# present and are re-checked against the manifest on checksum, byte size and membership.
source(here::here("download_original.R"))
