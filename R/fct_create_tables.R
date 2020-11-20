

#'The table of subjecs
#'@param consumption The uploaded consumption file
#'@param ... Unused yet
#'@noRd
create_tbl_subjects <- function(consumption, ...){
  
  dt_names <- names(consumption)
  
  #  food occasion related columns
  food_rel_names <- c("serial","day", "amountfood", "foodex_l4_code","foodname")
  
  subject_vars <- setdiff(dt_names, food_rel_names)
  
  consumption_days <- 
    consumption %>% 
    dplyr::group_by(subjectid) %>% 
    dplyr::summarise(cons_days  = dplyr::n_distinct(day)) %>% 
    dplyr::ungroup()
  
  
  tbl_subjects <- 
    consumption %>% 
    dplyr::select(
      dplyr::all_of(subject_vars)
    ) %>% 
    distinct(subjectid, .keep_all = TRUE) %>% 
    dplyr::left_join(consumption_days, by = "subjectid")
  
  
  tbl_subjects
}

# Merge Consumption and Occurrence files

#'@param consumption A tibble of the consumption 
#'@param occurrence_l2 A tibble of the occurence at level 2
#'@param occurrence_l3 A tibble of the occurrence at level 3 
#'@param ... Unused yet
#'@noRd
create_tbl_merged <- function(consumption, occurrence_l2, occurrence_l3,...){
  
  # Add FoodeX ####
  
  with_foodex <- 
    consumption %>% 
    left_join(
      select(foodex.1, 
             ends_with("code"),
             ends_with("desc")
      )
      , by= c("foodex_l4_code")         
    ) %>% 
    #janitor::clean_names() %>% 
    {.}
  
  
  # Add occurrence ####
  with_occurrence <- 
    with_foodex %>%
    
    # L2
    left_join(
      occurrence_l2 %>% 
        select(
          foodex_l2_desc,
          ends_with("_mean")
        ) 
      , by= c("foodex_l2_desc") #= "FoodExL2_name")
      
    ) %>% 
    
    # L3
    left_join(
      occurrence_l3 %>% 
        select(
          foodex_l3_desc,
          ends_with("mean")
          
        )
      , by= c("foodex_l3_desc")# = "Level 3")
      # we already have LB_mean , UB_mean etc...
      #TODO perhaps change the names of the L3 sheet. I do it anyways
      #when reading it - see load_occurrence()
      , suffix=c("","_l3")
    ) 
  
  
  # Merged ####
  
  merged <- 
    with_occurrence %>% 
    
    # Exposure L2 at each food consumptio occassion
    mutate(
      meal_exp_mean_LB = amountfood * LB_mean / weight,
      meal_exp_mean_MB = amountfood * MB_mean / weight,
      meal_exp_mean_UB = amountfood * UB_mean / weight
    ) %>% 
    
    #Exposure L3 at each food consumption occassion
    
    mutate(
      l3_meal_exp_mean_LB	= amountfood * LB_mean_l3 / weight,
      l3_meal_exp_mean_MB	= amountfood * MB_mean_l3 / weight,
      l3_meal_exp_mean_UB = amountfood * UB_mean_l3 / weight
    ) %>% 
    
    # Refined exposure. If we have info from level 3 then that else from l2
    mutate(
      refined_exp_lb = if_else(is.na(LB_mean_l3), meal_exp_mean_LB, l3_meal_exp_mean_LB),
      refined_exp_mb = if_else(is.na(MB_mean_l3), meal_exp_mean_MB, l3_meal_exp_mean_MB),
      refined_exp_ub = if_else(is.na(UB_mean_l3), meal_exp_mean_UB, l3_meal_exp_mean_UB)
    ) %>% 
    
    # Adjust by wcoeff
    mutate(
      wcoeff_adjusted_refined_exposure_lb  = refined_exp_lb  * wcoeff,
      wcoeff_adjusted_refined_exposure_mb	 = refined_exp_mb  * wcoeff,
      wcoeff_adjusted_refined_exposure_ub  = refined_exp_ub  * wcoeff,
    ) %>% 
    
    #Other Level 3
    # Need at least 1 value at any scenario in L3
    mutate(
      foodex_l3_desc_aggr = if_else(rowSums(is.na(across(LB_mean_l3:UB_mean_l3)))==3,
                                    paste0("-Other-", foodex_l2_desc), 
                                    foodex_l3_desc)
    ) 
    # mutate(
    #   foodex_l3_desc_aggr = if_else(is.na(l3_meal_exp_mean_MB), 
    #                         paste0("-Other-", foodex_l2_desc), 
    #                         foodex_l3_desc)
    # )
  
  #return
  merged
  
}


#'Create the Individual Exposure table
#'@param merged_data The merged data derived from create_tbl_merged()
#'@param tbl_subjecs The `tbl_subjecs` derived from `create_tbl_subjects()`
#'@param exposure_factor Integer. 1 (daily), 7 (weekly)
#'@details The `exposure_factor` is derived from the the `occurrence_l2` table
#'@param ... Unused yet
#'@noRd
create_tbl_exposure <- function(merged_data,tbl_subjects, exposure_factor,...){
  
  
  merged_data %>% 
    dplyr::group_by(
      subjectid
    ) %>% 
    dplyr::summarise(
      nday_lb = sum(wcoeff_adjusted_refined_exposure_lb, na.rm = TRUE),
      nday_mb = sum(wcoeff_adjusted_refined_exposure_mb, na.rm = TRUE),
      nday_ub = sum(wcoeff_adjusted_refined_exposure_ub, na.rm = TRUE)
    ) %>% 
    dplyr::left_join(tbl_subjects,by = "subjectid") %>% 
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
  
}