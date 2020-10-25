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

foodex.1 <- readxl::read_xlsx("SampleData/foodex.1.xlsx") %>%
  select(
    -ends_with("_HCODE"),
    -ends_with("_ID")
  ) %>%
  mutate(across(everything(), as.factor))



occurrence_example_l2 <- readxl::read_excel(
  "SampleData/occurrence_example.xlsx", sheet = "level2"
)


occurrence_example_l3 <- readxl::read_excel(
  "SampleData/occurrence_example.xlsx", sheet = "level3"
)


usethis::use_data(sample_consumption, 
                  foodex.1, 
                  occurrence_example_l2,
                  occurrence_example_l3,
                  internal = TRUE, overwrite = TRUE )



usethis::use_data(sample_consumption, 
                  foodex.1, 
                  occurrence_example_l2,
                  occurrence_example_l3,
                  internal = TRUE, overwrite = TRUE )

