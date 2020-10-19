
#' Create a 'select Scenario input
#' @param tab_name The tab where the input will appear
#' @return A Shiny input
#' @noRd
slct_scenario <- function(tab_name, label = "Scenario", choices){
  
  inputId =  paste0("slct_scenario_", tab_name)
  print(inputId)
  shinyWidgets::radioGroupButtons(
    
    inputId  = inputId,
    label = label,
    choices = choices,
    selected = choices[2] #MB
    
  )
  
  
}