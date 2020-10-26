## code to prepare `internal_data` dataset goes here

#usethis::use_data(internal_data, overwrite = TRUE)


library(dplyr, warn.conflicts = FALSE)
library(janitor)

# # Load and clean names
# #Also, I saved it in R/sysdata.rda and  this lod autmtically in the pacjage for
# #and can use it. It does not show in the globalenvironment


sample_consumption <- 
  readxl::read_xlsx("SampleData/consumption_sample.xlsx") %>% 
  janitor::clean_names()


occurrence_example_l2 <- readxl::read_excel(
  "SampleData/occurrence_example.xlsx", sheet = "level2"
)


occurrence_example_l3 <- readxl::read_excel(
  "SampleData/occurrence_example.xlsx", sheet = "level3"
)


foodex.1 <- readxl::read_xlsx("SampleData/foodex.1.xlsx") %>%
  select(
    -ends_with("_HCODE"),
    -ends_with("_ID")
  ) %>%
  mutate(across(everything(), as.factor))


tbl_foodex_desc <- 
  foodex.1 %>% 
  distinct(FOODEX_L1_DESC, FOODEX_L2_DESC, FOODEX_L3_DESC, FOODEX_L4_DESC) %>% 
  relocate(FOODEX_L1_DESC, FOODEX_L2_DESC, FOODEX_L3_DESC, FOODEX_L4_DESC)

tbl_unique_level3 <- 
  foodex.1 %>% 
  distinct(FOODEX_L3_DESC, FOODEX_L2_DESC, FOODEX_L1_DESC)



fdx1_l1 <- unique(foodex.1$FOODEX_L1_DESC)
fdx1_l2 <- unique(foodex.1$FOODEX_L2_DESC)
fdx1_l3 <- unique(foodex.1$FOODEX_L3_DESC)
fdx1_l4 <- unique(foodex.1$FOODEX_L4_DESC)



sub_info <- 
  data.frame(
    stringsAsFactors = FALSE,
    check.names = FALSE,
    
    row.names = c("Chemical Substance", "Substance Category","Reference value (μg/Kg b.w.)",
                  "Type of Reference value","Frequency"),
    values = c("Mercury (Hg)","Contaminant",
               "4.00","Tolerable Intake","WEEKLY")
  )

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
    subjectid
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


usethis::use_data(sample_consumption, 
                  foodex.1, 
                  occurrence_example_l2,
                  occurrence_example_l3,
                  consumption_days,
                  fdx1_l1, fdx1_l2,  fdx1_l3, fdx1_l4,
                  sub_info,
                  tbl_exposure,
                  tbl_foodex_desc,
                  tbl_subjects,
                  tbl_unique_level3,
                  internal = TRUE, overwrite = TRUE )



# usethis::use_data(sample_consumption, 
#                   foodex.1, 
#                   occurrence_example_l2,
#                   occurrence_example_l3,
#                   internal = FALSE, overwrite = TRUE )

