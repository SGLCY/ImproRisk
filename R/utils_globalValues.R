
# Steup the global valuessuch as lists foodex names etc

fdx1_group_level1 <- c("foodex_l1_desc")
fdx1_group_level2 <- c("foodex_l1_desc", "foodex_l2_desc")
fdx1_group_level3 <- c("foodex_l1_desc" ,"foodex_l2_desc", "foodex_l3_desc")



# TODO change the names of these vaiables
# need to be more descriptive

fdx1_levels <- list(
  "Level 1" = fdx1_group_level1,
  "Level 2" = fdx1_group_level2,
  "Level 3" = fdx1_group_level3
)


fdx1_levels_cons <- c(
  
  "Level 1" = "foodex_l1_desc",
  "Level 2" = "foodex_l2_desc",
  "Level 3" = "foodex_l3_desc",
  "Level 4" = "foodex_l4_desc"
  
)



# fdx1_l1_desc <- unique(foodex.1$foodex_l1_desc)
# fdx1_l2_desc <- unique(foodex.1$foodex_l2_desc)
# fdx1_l3_desc <- unique(foodex.1$foodex_l3_desc)
# fdx1_l4_desc <- unique(foodex.1$foodex_l4_desc)
# 
# fdx1_l1_code <- unique(foodex.1$foodex_l1_code)
# fdx1_l2_code <- unique(foodex.1$foodex_l2_code)
# fdx1_l3_code <- unique(foodex.1$foodex_l3_code)
# fdx1_l4_code <- unique(foodex.1$foodex_l4_code)


var_names <- list(
  # new = #old
  Gender      = "gender",
  Age         = "age",
  'Age group' = "pop_class",
  Area        = "area",
  Weight      = "weight",
  'Consumption days' = "cons_days"
  
)


vars_exposure <- c(
  
  "nday_lb",
  "nday_mb",
  "nday_ub",
  
  "subExp_LB",
  "subExp_MB",
  "subExp_UB"
)


# for naming the  columns of the occurence files

occur_l2_names <- c(
  
  "foodex_l1_desc",
  "foodex_l2_desc",
  "No of Samples",
  "LB_min",
  "MB_min",
  "UB_min",
  "LB_mean",
  "MB_mean",
  "UB_mean",
  "LB_median",
  "MB_median",
  "UB_median",
  "LB_p95",
  "MB_p95",
  "UB_p95"
)



occur_l3_names <- c(
  
  "foodex_l1_desc",
  "foodex_l2_desc",
  "foodex_l3_desc",
  "No of Samples",
  "LB_min",
  "MB_min",
  "UB_min",
  "LB_mean",
  "MB_mean",
  "UB_mean",
  "LB_median",
  "MB_median",
  "UB_median",
  "LB_p95",
  "MB_p95",
  "UB_p95"
  
)

# tab menu items

# see icons at http://fontawesome.io/icons/

tab_items <- tibble::tribble(
  ~tabTitle,            ~tabName,         ~icon,
  "Exposure",           "exposure",       "th",
  "Exposure by Demo",   "exposureDemo",   "th",
  "Contribution",       "contribution",   "th",
  "Explore Consumption","consumption",    "th",
  "Drill down",         "drillDown",      "chart-bar",
  "Occurrence",         "occurrence",     "th",
  "Level 2",            "occurrenceL2",   "th",
  "Level 3",            "occurrenceL3",   "th",
  "Foodex1",            "foodex1",        "th",
  "Tables",             "tables",         "th",
  "Merged data",        "merged",         "",
  "Individual exposure","individual",     "",
  "Update data",        "updateData",     "th",
  "Consumption",        "consumptionUpdate",    "",
  "Occurrence",         "occurrenceUpdate",     "",
  "Log",                "log",            "columns",
  "ABOUT",               "info",           "th"
)


# summary statistics for exposure
exposure_summary <- list(
  # N      = ~n(),
  min    = ~min(., na.rm = TRUE),
  mean   = ~mean(., na.rm = TRUE),
  #sd     = ~sd(., na.rm = TRUE),
  median = ~median(., na.rm = TRUE),
  #max    = ~max(., na.rm = TRUE),
  p95    = ~quantile(., 0.95),
  p951   = ~Hmisc::wtd.quantile(., weights = !!enquo(wcoeff), probs = 0.95)
  
  # Statistic on the LOD
)



# Global values -----------------------------------------------------------


#> Reading consumption ####
vars_needed_consumption <- c(
  
  "serial", "subjectid", "day", 
  "foodname","amountfood", "foodex_l4_code", 
  "gender", "area", "pop_class", 
  "age","weight", "wcoeff"
  
  )

vars_numeric_consumption <- c("day", "amountfood", "age",  "weight", "wcoeff")

vars_character_consumption <- setdiff(vars_needed_consumption,
                                      vars_numeric_consumption)



#> Occurrence ####

# Range of cells in Occurrence template that holds 
# the occurrence values
range_level2 <- "B9:P171"

range_subInfo <- "B1:C5"

sheets_needed <- c("Level2", "Level3")



# These come first in tbl_exposure
vars_order_1 <- c(
  "subjectid","gender", "area", 
  "pop_class", "age","weight", 
  "wcoeff", "cons_days"
)

vars_demo <- c("Gender"="gender",  
               "Area" = "area",
               "Population class" = "pop_class"
               )

scenarios <- c("LB", "MB", "UB")

water_level1 <- "Drinking water (water without any additives except carbon dioxide; includes water ice for consumption)"

globals <- list(
  min.n.breaks = 5,
  max.n.breaks = 30,
  min.digits = 3,
  max.digits = 10
)


impro_colours <- c(
  "#a6bddb",
  "#756bb1",
  "#2ca25f"
  
)


info_improrisk <- tagList(
  tags$img(src = "www/ImproRisk_Logo_Transparent_Left.png", width= "25%"),
  br(),
  h3("ImproRisk shiny app for Dietary Risk Assessment"),
  br(),
  p("The ImproRisk Shiny app was built for the",
    a(href = "https://www.moh.gov.cy/moh/sgl/sgl.nsf/home_en/home_en?opendocument",
      "State General Laboratory (SGL) of the Republic of Cyprus"
    ), 
    "by the private company", 
    a(href = "www.improvast.com", "Improvast"), 
    "and it is owned by SGL."
  ),
  p(" The current version is 0.0.1"
    , br()
    , "Last update November 2020"
  ),
  p("This version supports",
    tags$li("Weighting Coefficients for a non-representative food survey sample"),
    tags$li("Exposure assessment at FoodEx1 Level3 food categorisation"),
  ),
  p("For further information please contact:",
    tags$li("1: gstavroulakis@sgl.moh.gov.cy"),
    tags$li("2: lefkios@improvast.com"),
    br(),
    "Feel free to forward any bugs and/or recommendations."
  ),
  p("The code for the app lives at", 
    a(href="https://github.com/SGLCY/ImproRisk", "SGL's github page"),
  ),
  br(),
  tags$img(src = "www/sgl_logo.png", width= "15%"),
  tags$img(src = "www/improvast_logo.png", width = "10%")
  
  
  
)

# Sortable ####

# Stolen from SO.
max_2_item_opts <- sortable::sortable_options(
  # inspiration from https://jsbin.com/nacoyah/edit?js,output
  # Sortable.create(qux, {
  #   group: {
  #     name: 'qux',
  #     put: function (to) {
  #       return to.el.children.length < 4;
  #     }
  #   },
  #   animation: 100
  # });
  
  group = list(
    # use a group name to allow sharing between lists
    name = "drill_down_group",
    # add a `put` function that can determine if an element may be placed
    put = htmlwidgets::JS("
      function(to) {
        // only allow a 'put' if there is less than 1 child already
        return to.el.children.length < 2;
      }
    ")
  )
)


max_1_item_opts <- sortable::sortable_options(
  group = list(
    name = "drill_down_group",
    put = htmlwidgets::JS("
      function(to) {
        // only allow a 'put' if there is less than 1 child already
        return to.el.children.length < 1;
      }
    ")
  )
)


