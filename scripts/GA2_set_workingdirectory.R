################################################################################

## 0-1_set_workingdirectory.R
### Authors: Shannon Still & Emily Beckman ### Date: 05/21/2020

### DESCRIPTION:
# This script sets the working environment for the computer on which you are
#   working.

################################################################################
# Set working environment depending on your computer
################################################################################

# Use this to check your "nodename"
# Sys.info()[4]

## For Emily Beckman:
} else if (Sys.info()[4] == "Africa.local") {
  # set main working directory
  main_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Gap Analysis/occurrence_points"
  # set location of scripts
  script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"
  # OPTIONAL: set local working directory, for trialing locally before saving
  #   to main working directory
  local_dir <- "./Desktop/work"
  # set location for login information (e.g., for GBIF)
  log_loc <- file.path(local_dir, "IMLS_passwords.txt")
  # prints computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))

## can add as many additional "else if" sections as needed to cover other
#   workstations

} else {
  # default, which sets the working driectory as the folder from which you
  #   opened the scripts/project
  setwd(getwd())
}
