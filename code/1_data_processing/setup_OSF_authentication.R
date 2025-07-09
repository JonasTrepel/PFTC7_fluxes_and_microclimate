# Set up OSF authentication
# Modify this code for your own computer but DO NOT PUSH IT to GitHub

# First make sure you're vaccinated against leaks
library(usethis)
usethis::git_vaccinate()

# Read the help article
?osfr::osf_auth

# Go to https://osf.io/settings/tokens/
# Follow the instructions
# (More details in the help article)

# Read the warnings
# Save your token somewhere that it won't be accidentally pushed to GitHub

# Tell R your PAT token
# Run:
usethis::edit_r_environ()

# A new file called .Renviron will pop up
# Paste into this file OSF_PAT='[your unique token from OSF]'
# Save the file
# Restart R
# Check that the token worked
Sys.getenv("OSF_PAT")
