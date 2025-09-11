# Package Test Build

library(devtools)
library(usethis)

usethis::use_git_config(user.name = "Christopher Runyon", 
                        user.email = "CRunyon@nbme.org")

# Set up GitHub token for authentication
usethis::create_github_token()
# Follow the prompts, then store the token:
gitcreds::gitcreds_set()

# Create a new package (replace 'workshopname' with your actual package name)
usethis::create_package("~/SimpPackage")

# This opens a new RStudio session in your package directory