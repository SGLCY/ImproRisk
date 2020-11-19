#' slct_scenario UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_slct_scenario_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::radioGroupButtons(ns("slct_scenario"),
                                    label = "Scenario",
                                    choices = scenarios,
                                    selected = scenarios[2]  #MB
    )
  )
}
    
#' slct_scenario Server Function
#'
#' @noRd 
mod_slct_scenario_server <- function(input, output, session){
  ns <- session$ns
 
  # what towrite here??
}
    
## To be copied in the UI
# mod_slct_scenario_ui("slct_scenario_ui_1")
    
## To be copied in the server
# callModule(mod_slct_scenario_server, "slct_scenario_ui_1")