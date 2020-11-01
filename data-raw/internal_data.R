## code to prepare `internal_data` dataset goes here

#usethis::use_data(internal_data, overwrite = TRUE)


library(dplyr, warn.conflicts = FALSE)
devtools::load_all()

# # Load and clean names
# #Also, I saved it in R/sysdata.rda and  this lod autmtically in the pacjage for
# #and can use it. It does not show in the globalenvironment



# FOODEX ####

foodex.1 <- readxl::read_xlsx("SampleData/Foodex.1.xlsx") %>%
  select(
    -ends_with("_HCODE"),
    -ends_with("_ID")
  ) %>%
  mutate(across(everything(), as.factor)) %>% 
  janitor::clean_names()


tbl_foodex_desc <- 
  foodex.1 %>% 
  distinct(foodex_l1_desc, foodex_l2_desc, foodex_l3_desc, foodex_l4_desc) %>% 
  relocate(foodex_l1_desc, foodex_l2_desc, foodex_l3_desc, foodex_l4_desc)

tbl_unique_level3 <- 
  foodex.1 %>% 
  distinct(foodex_l3_desc, foodex_l2_desc, foodex_l1_desc)


fdx1_l1 <- unique(foodex.1$foodex_l1_desc)
fdx1_l2 <- unique(foodex.1$foodex_l2_desc)
fdx1_l3 <- unique(foodex.1$foodex_l3_desc)
fdx1_l4 <- unique(foodex.1$foodex_l4_desc)


# Consumption  ####

sample_tbl_consumption <- 
  readxl::read_xlsx("SampleData/sample_consumption_CYP300.xlsx") %>% 
  janitor::clean_names()


# Occurrence ####

file_occur <- "SampleData/sample_occurrence_EFSA_Pb.xlsx"

sample_occurrence_l2 <- readxl::read_excel(
  file_occur, sheet = "Level2"
) %>% 
  purrr::set_names(occur_l2_names)

sample_occurrence_l3 <- readxl::read_excel(
  file_occur, sheet = "Level3"
) %>% 
  purrr::set_names(occur_l3_names)


# TODO Here I need to read form the file
sample_substance_info <- 
  data.frame(
    stringsAsFactors = FALSE,
    check.names = FALSE,
    
    row.names = c("Chemical Substance", "Substance Category","Reference value (μg/Kg b.w.)",
                  "Type of Reference value","Frequency"),
    values = c("Lead (Pb)","Contaminant",
               "0.63","Benchmark Dose Level (BMDL)","DAILY")
  )


exposure_factor = if(sample_substance_info$values[5]=="DAILY") {1} else {7}

sample_dataset_info <- 
data.frame(
  stringsAsFactors = FALSE,
  row.names = c("Consumption", 
                "Occurence"
  ),
  dataset = c("Subjects_Consumption_ex.3 (N=300, Days=3) (CYP Weights).xlsx",
              "Occurrence Example-EFSA-Pb.xlsm"
  )
  
)




# CREATE ####

sample_tbl_subjects <- improrisk.shiny:::create_tbl_subjects(sample_tbl_consumption)

sample_tbl_merged <- improrisk.shiny:::create_tbl_merged(sample_tbl_consumption,
                                                        sample_occurrence_l2,
                                                        sample_occurrence_l3
                                                        )

sample_tbl_exposure <- improrisk.shiny:::create_tbl_exposure(sample_tbl_merged, 
                                                     sample_tbl_subjects, 
                                                     exposure_factor)

usethis::use_data(
                  sample_occurrence_l2,
                  sample_occurrence_l3,
                  
                  sample_tbl_consumption,
                  sample_tbl_exposure,
                  sample_tbl_merged,
                  sample_tbl_subjects,
                  
                  sample_dataset_info,
                  
                  foodex.1, 
                  tbl_foodex_desc,
                  tbl_unique_level3,
                  fdx1_l1, fdx1_l2,  fdx1_l3, fdx1_l4,
                  sample_substance_info,
                  
                  internal = TRUE, overwrite = TRUE )


# Foodex1 can be external
foodex1_classification <- foodex.1

usethis::use_data(foodex1_classification,internal = FALSE, overwrite = TRUE )

