
# some tables

tbl_foodex_desc <- 
  foodex.1 %>% 
  distinct(FOODEX_L1_DESC, FOODEX_L2_DESC, FOODEX_L3_DESC, FOODEX_L4_DESC) %>% 
  relocate(FOODEX_L1_DESC, FOODEX_L2_DESC, FOODEX_L3_DESC, FOODEX_L4_DESC)


fdx1_group_level1 <- c("FOODEX_L1_DESC")
fdx1_group_level2 <- c("FOODEX_L1_DESC", "FOODEX_L2_DESC")
fdx1_group_level3 <- c("FOODEX_L1_DESC" ,"FOODEX_L2_DESC", "FOODEX_L3_DESC")

fdx1_levels <- list(
  "Level 1" = fdx1_group_level1,
  "Level 2" = fdx1_group_level2,
  "Level 3" = fdx1_group_level3
)


tbl_unique_level3 <- 
  foodex.1 %>% 
  distinct(FOODEX_L3_DESC, FOODEX_L2_DESC, FOODEX_L1_DESC)

temp <- 
  foodex.1 %>% 
  distinct(FOODEX_L4_DESC, FOODEX_L4_CODE ) %>% 
  tibble::deframe()

foodex.1 %>% 
  distinct(FOODEX_L4_CODE, FOODEX_L4_DESC) %>% 
  relocate(FOODEX_L4_DESC) %>% 
  tibble::deframe()



fdx1_l1 <- unique(foodex.1$FOODEX_L1_DESC)
fdx1_l2 <- unique(foodex.1$FOODEX_L2_DESC)
fdx1_l3 <- unique(foodex.1$FOODEX_L3_DESC)
fdx1_l4 <- unique(foodex.1$FOODEX_L4_DESC)



match("Cheese",  fdx1_l4)


sub_info <- 
# data.frame(
#              ~Chemical.Substance, ~`Mercury.(Hg),.MERCURY`,
#             "Substance Category",            "Contaminant",
#   "Reference value (?g/Kg b.w.)",                   "4.00",
#        "Type of Reference value",       "Tolerable Intake",
#                           "Type",                 "WEEKLY"
#   )
data.frame(
         stringsAsFactors = FALSE,
              check.names = FALSE,
         
                       row.names = c("Chemical Substance", "Substance Category","Reference value (μg/Kg b.w.)",
                                              "Type of Reference value","Frequency"),
                  values = c("Mercury (Hg)","Contaminant",
                                              "4.00","Tolerable Intake","WEEKLY")
                )


# tab menu items

# see icons at http://fontawesome.io/icons/

tab_items <- tibble::tribble(
           ~tabTitle,          ~tabName, ~icon,
          "Exposure",        "exposure",  "th",
  "Exposure by Demo",    "exposureDemo",  "th",
      "Contribution",    "contribution",  "th",
       "Update data",      "updateData",  "th",
        "Occurrence",      "occurrence",  "th",
           "Level 2",    "occurrenceL2",  "th",
           "Level 3",    "occurrenceL3",  "th",
           "Foodex1",         "foodex1",  "th",
  "Mean Consumption", "meanConsumption",  "th",
            "Tables",          "tables",  "th",
              "Info",            "info",  "th"
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


summarise_weighted <- function(data){
  
  # Note the ref_value  
  
  data %>% 
    dplyr::summarise(
      #N           = dplyr::n(),
      Min         = min(exposure, na.rm = TRUE),
      Max         = max(exposure, na.rm = TRUE),
      Mean        = Hmisc::wtd.mean(exposure, wcoeff),
      SD          = sqrt(Hmisc::wtd.var(exposure, wcoeff)),
      #SE          = wtd.std*sqrt(sum(wcoeff))/sum(wcoeff),
      #wtd.Q1      = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.25),
      Median      = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.50),
      P75         = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.75),
      P95         = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.95),
      # % above reference value
      "%over"  = sum(wcoeff[exposure>ref_value])/sum(wcoeff)
    )
  
}


# Global values ####

vars_demo <- c("Gender"="gender",  "Area" = "area","Population class" = "pop_class")

scenarios <- c("LB", "MB", "UB")


globals <- list(
  min.n.breaks = 5,
  max.n.breaks = 30,
  min.digits = 3,
  max.digits = 10
)

# Demo data ####


#  TODO Need to use a reactive value in the server for these..
ref_value <- 4
exposure_factor = 7

consumption_days <- 
  sample_consumption %>% 
  dplyr::group_by(subjectid) %>% 
  dplyr::summarise(cons_days  = dplyr::n_distinct(day)) %>% 
  dplyr::ungroup()


tbl_subjects <- 
  sample_consumption %>% 
  dplyr::distinct(
    subjectid, 
    gender, 
    age,
    weight,
    area,
    pop_class,
    wcoeff
  ) %>% 
  dplyr::left_join(consumption_days)


tbl_exposure <- 
  sample_consumption %>% 
  dplyr::group_by(
    subjectid, 
  ) %>% 
  dplyr::summarise(
    nday_lb = sum(wcoeff_adjusted_refined_exposure_lb, na.rm = TRUE),
    nday_mb = sum(wcoeff_adjusted_refined_exposure_mb, na.rm = TRUE),
    nday_ub = sum(wcoeff_adjusted_refined_exposure_ub, na.rm = TRUE)
  ) %>% 
  dplyr::left_join(tbl_subjects) %>% 
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with(("nday_")), ~ ./wcoeff
    )
  ) %>% 
  dplyr::mutate(
    subExp_LB = (nday_lb/cons_days) * exposure_factor,
    subExp_MB = (nday_mb/cons_days) * exposure_factor,
    subExp_UB = (nday_ub/cons_days) * exposure_factor
  ) %>% 
  dplyr::ungroup()


tbl_exposure_stats <- 
  tbl_exposure %>% 
  tidyr::pivot_longer(
    cols = starts_with("subExp_"),
    names_to = "scenario",
    values_to = "exposure"
  ) %>% 
  dplyr::group_by(scenario,  gender) %>% 
  summarise_weighted() %>% 
  # summarise(
  #   across(exposure, exposure_summary,.names = "{.fn}")
  # ) %>%
  
  {.}

# 
# 
# tbl_exposure_stats %>%
# tidyr::gather(key, value, - scenario) %>%
#   tidyr::pivot_wider(names_from = scenario, values_from = value) %>% 
#   dplyr::rename_with(
#     ~stringr::str_remove(., "subExp_"),
#     dplyr::starts_with("SubExp_")
#   ) %>% 
#   data.frame(row.names = "key")

food_level <- fdx1_levels[[2]]

# Need to define levels in the food groups. in case some food groups are not found in the 
# food survey


temp <- 
sample_consumption %>% 
  rename(
    FOODEX_L3_DESC = consumed_food_at_level_3,
    FOODEX_L2_DESC = consumed_food_at_level_2,
    FOODEX_L1_DESC = consumed_food_at_level_1
  ) %>% 
  rename_with(~scenarios, .cols = contains("refined_exposure")) %>% 
  group_by(
    across(all_of(food_level))
  ) %>% 
  summarise(
    across(.cols= all_of(scenarios), .fns = ~sum(., na.rm = TRUE))
  ) %>% 
  ungroup() %>% 
  full_join(
    tbl_foodex_desc %>% 
      distinct(
        across(all_of(food_level))
      )
  ) %>% 
  tidyr::gather(
    "scenario", "exposure", all_of(scenarios)
  ) %>% 
  tidyr::replace_na(list(exposure = 0)) %>% 
  # here i need the grouping to be onthe scenario levels
  group_by(scenario) %>% 
  mutate(
    contribution = exposure/ sum(exposure, na.rm = TRUE)
  ) 
  
  # Calculate the Within Percentage 
  
tbl_contribution <- 
  temp %>% 
  group_by(scenario, .data[[dplyr::nth(food_level, -2,default = food_level)]]) %>% 
  mutate(
    contr_within = exposure/ sum(exposure, na.rm = TRUE)
  ) %>% 
  #  Set NAN 0/0 as NA. Need to show this in the table 
  mutate(contr_within = if_else(is.nan(contr_within), NA_real_, contr_within)) %>% 
  ungroup()
  
tbl_contribution 

# filter(FOODEX_L2_DESC == "Alcoholic mixed drinks")
  # filter(contr_within>0) %>% 
  #filter(scenario == "MB") 
  #View()
  
food_level


nth(c("a", "b", "c"), -4, default = "f")

#library(reactable)

#  Level 1 Show depends on the requested depth
if(length(food_level) == 3) {
  show_level1 <- FALSE
  
  } else{
    show_level1 <- TRUE
  }

if(length(food_level) == 1) {
  show_contr <- FALSE
  
} else{
  show_contr <- TRUE
}

reactable::reactable(tbl_contribution %>% 
            filter(scenario  == "MB") %>% 
            #filter(FOODEX_L1_DESC == "Fish and other seafood (including amphibians, reptiles, snails and insects)") %>% 
           # select(-FOODEX_L1_DESC) %>% 
            {.}, 
          # in the Level 1 case
          groupBy = nth(food_level, -2, default = food_level),
          columns = list(
            scenario = colDef(show = FALSE),
            exposure = colDef(name = "Total exposure (μg/Kg b.w)", 
                              aggregate = "sum",
                              format = colFormat(digits = 1),
                              filterable = FALSE
                              ),
            contribution = colDef(name = "Contribution to Total exposure",aggregate = "sum",
                                  format = colFormat(percent = TRUE, digits =  2),
                                  filterable = FALSE
                                  
                                  ),
            contr_within = colDef(name = "Contribution within"
                                   ,show = show_contr
                                  ,aggregate = "sum",
                                  filterable = FALSE,
                                  format = colFormat(percent = TRUE, digits =  2)
            ),
            FOODEX_L1_DESC = colDef(show = show_level1)
          ),
          striped = TRUE,
          bordered = TRUE,
          highlight = TRUE,
          #filterable = TRUE,
          searchable = TRUE,
          rowStyle = reactable::JS("function(rowInfo) {
          if (rowInfo.aggregated   == true) {
          return {background: 'rgba(0,0,0,0.07', fontWeight: 'bold' }
          }
          }")
          # Not working
          # theme = reactableTheme(
          #   rowGroupStyle = list(background= "rgba(0, 0, 0, 0.23)"
          #                        ),
          #   rowStyle = list(colour = "#008000")
          )
          

dplyr::nth(food_level, 2)

food_level <- fdx1_levels[[3]]

# in the Level 1 case
nth(food_level, -2, default = food_level)


# Graph

#library(forcats)


max(stringr::str_length(fdx1_l2))

p <- 
tbl_contribution %>% 
  filter(scenario == "MB") %>% 
  filter(contribution  != 0) %>% 
  filter(contribution > 0.001) %>% 
  #filter(stringr::str_detect(FOODEX_L1_DESC, "Fish")) %>% 
  ggplot(
    aes(forcats::fct_reorder(FOODEX_L2_DESC, contribution), contribution, 
        tooltip = scales::percent(contribution,  accuracy = 0.1)
        )
  )+
  ggiraph::geom_col_interactive(width = 0.5)+
  coord_flip()+
  labs(
    x = "Level 2 food groups", y  = "Contribution to Total exposure",
    title = "Contribution to Total Exposure by FOODEX2"
  )+
  scale_y_continuous(labels = scales::percent, expand = c(0,0.001))+
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, 30))+
  theme_bw()+
  theme(
    axis.text = element_text(size = 5),
    axis.title.y = element_text(hjust = 1, angle = 0 )
  )

p
  
ggiraph::girafe(ggobj = p)



facet_wrap(~ FOODEX_L1_DESC, scales = "free_y")
  


