library(roxygen2)
library(magrittr)
setwd("C:/Users/mahamilton/Desktop/R_Projects")
devtools::create("ready.space")
## At this point a number of additional set-up tasks were undertaken:
## RStudio: Save this script in the newly created ready.space folder.
## RStudio: Open the newly created ready.space Project file in the ready.space folder.
## RStudio: Manually make ready.space a local git repository.
##                [Project drop down menu > project options > Git/SVN > Version Control System: Git]
## Command line: Create README.md.
##               echo "# ready.space" >> README.md
## RStudio: Make first commit.
##                Go to Git tab, select all folders and files, click on Commit button.
## GitHub: Create ready.space remote repository without README.md .
## Command line: Add remote repo to local repo.
##               git remote add origin https://github.com/matthewphamilton/ready.space.git
## Command line: Push first commit from local to remote repo.
##               git push -u origin master
## From this point on, all commits and pull/pushes are from within RStudio.
## Also implemented LFS as outlined: https://git-lfs.github.com/
##               git lfs install
##               git lfs track "*.rda"
##               git add .gitattributes
##
#usethis::use_package("sf")
#usethis:use_package("ready.data")
usethis::use_dev_package("ready.agents")
usethis::use_dev_package("ready.s4")
usethis::use_dev_package("ready.utils")
usethis::use_package("dplyr")
usethis::use_package("googleway")
usethis::use_package("httr")
usethis::use_package("invgamma")
usethis::use_package("jsonlite")
usethis::use_package("lubridate")
usethis::use_package("magrittr")
usethis::use_package("osrm")
usethis::use_package("purrr")
usethis::use_package("readxl")
usethis::use_package("rlang")
#usethis::use_package("seplyr")
usethis::use_package("sf")
usethis::use_package("stats")
usethis::use_package("stringr")
usethis::use_package("tibble")
usethis::use_package("units")
devtools::document()
##
# devtools::use_vignette()
# # Next bit implements: http://www.davekleinschmidt.com/r-packages/
# devtools::use_data_raw()
# # Now create a data.R file in data-raw directory to import and process data,
# devtools::use_testthat()
## RStudio: Created test scripts and saved them in testthat folder in tests folder.
#testthat::test()
#devtools::test()
##
#setwd("tests/testthat")
devtools::load_all(".")
##
setwd("../")
devtools::install_local("ready.space",force=TRUE)
setwd("./ready.space")
## extra line to test merging of orygen forked repo with personal account original repo.
##
