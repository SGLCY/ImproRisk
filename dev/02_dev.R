# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("thinkr" )
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("forcats")
usethis::use_package("purrr")
usethis::use_package("readxl")  
usethis::use_package("writexl")
usethis::use_package("ggplot2")  
usethis::use_package("stringr")
usethis::use_package("lubridate")
usethis::use_package("shinipsum")
usethis::use_package("shinydashboard")
usethis::use_package("shinyWidgets")
usethis::use_package("janitor")
usethis::use_package("Hmisc")
usethis::use_package("scales")
usethis::use_package("glue")
usethis::use_package("reactable")
usethis::use_package("ggiraph")
usethis::use_package("ggridges")
usethis::use_package("rintrojs")
usethis::use_package("sortable")
usethis::use_package("ggthemes")
usethis::use_package("knitr")
usethis::use_package("kableExtra")
usethis::use_package("DT")
usethis::use_package("htmltools")
usethis::use_package("htmlwidgets")
usethis::use_package("shinyFeedback")
usethis::use_package("shinycssloaders")

# usethis::use_package("ggthemes")
usethis::use_pipe()



## Add modules ----
## Create a module infrastructure in R/
# golem::add_module( name = "showSubstanceInfo" ) # Name of the module
# golem::add_module( name = "slct_scenario" ) # Name of the module
# 
# golem::add_module(name = "downloadPlot")
# golem::add_module(name = "downloadTable")
#golem::add_module( name = "name_of_module2" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
#golem::add_fct( "helpers" ) 
#golem::add_utils( "helpers" )

#golem::add_utils( "tbl_helpers" )

#golem::add_fct("merge_data")


## External resources
## Creates .js and .css files at inst/app/www
#golem::add_js_file( "script" )
#golem::add_js_handler( "handlers" )
#golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
#usethis::use_data_raw( name = "internal_data", open = FALSE ) 

#usethis::use_data(sample_consumption, foodex.1, internal = TRUE, overwrite = TRUE )


## Tests ----
## Add one line by test you want to create
#usethis::use_test( "app" )

# Documentation

## Vignette ----
# usethis::use_vignette("get_started")
# devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
# usethis::use_github()
# usethis::use_travis()
# usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
#rstudioapi::navigateToFile("dev/03_deploy.R")

