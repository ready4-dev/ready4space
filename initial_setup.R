library(roxygen2)
library(magrittr)
setwd("C:/Users/mahamilton/Desktop/R_Projects")
devtools::create("ready.space")
setwd("C:/Users/mahamilton/Desktop/R_Projects/ready.space")
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
setwd("C:/Users/mahamilton/Desktop/R_Projects/ready.space")
#setwd("/Users/Alejandra/Desktop/R_Projects/ready.space")
usethis::use_package("sf")
usethis:use_package("ready.data")
devtools::document()
##
devtools::use_vignette()
# Next bit implements: http://www.davekleinschmidt.com/r-packages/
devtools::use_data_raw()
# Now create a data.R file in data-raw directory to import and process data,
devtools::use_testthat()
## RStudio: Created test scripts and saved them in testthat folder in tests folder.
#testthat::test()
#devtools::test()
##
#setwd("tests/testthat")
devtools::load_all(".")
##
setwd("../")
devtools::install_local("ready.space")
setwd("./ready.space")
## extra line to test merging of orygen forked repo with personal account original repo.
##
