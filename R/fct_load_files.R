# Functions to read the consumption and occurence data

#'Function to load the consumption data
#'This function loads the consumption file.
#'@param name filename.String
#'@param path String.thepath to the file
#'@noRd
load_consumption <- function(name, path) {
  
  ext <- tools::file_ext(name)
  
  out <-  
    switch(ext,
           #csv  = readr::read_csv(path),
           #tsv  = vroom::vroom(path, delim = "\t"),
           xlsx = readxl::read_xlsx(path),
           NULL #validate("\nInvalid file; Please upload a .csv or .xlsx file")
    )
  
  out %>% 
    janitor::clean_names() 
  
}

#'Function to load the Occurrence data
#'This function loads the Occurrence file.
#'@param path The path to the excel file. The 'datapath'!
#'@param sheet The sheet to read
#'@noRd
#'@details Level 2 sheet. I specifically read the full range (rectangular) since
#'there might be times when the user does not supply the values in the 
#'p95 scenario. This way I capture even the empty values. Then I set the names
#'of the dataframe with the proper names.Notice the col_names=FALSE (don;t need 
#'the names of the columns). I avoid any improper namings there.     
#'
#'Level 3 is trickier. It doesnt have a fixed length (rowwise). I don;t know how many
#'food itemsthe user has, so I have to read the entire sheet.I also then explicitly
#'privide the names of the coumns
#'
#'Also, the subastance info, I read it within the server with another call to 
#'readxl::read_xlsx(). There I capture only the specific rnge of cells
#'TODO Is there another , better way of doing this??? (read three times :( )
load_occurrence <- function(path, sheet) {
  
  switch(sheet,
         
         "Level2" =
           readxl::read_xlsx(path,
                             range = range_level2,
                             col_names = FALSE,
                             sheet = "Level2") %>% 
           purrr::set_names(occur_l2_names)
         
         
         ,"Level3" = 
           readxl::read_xlsx(path,
                             skip = 5,
                             col_names = TRUE,
                             sheet = "Level3") %>% 
           purrr::set_names(occur_l3_names)
         
         ,NULL
  )
  
}
