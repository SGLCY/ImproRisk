#' showSubstanceInfo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_showSubstanceInfo_ui <- function(id){
  ns <- NS(id)
  tagList(

    textOutput(ns("substance_info"))
  )
}
    
#' showSubstanceInfo Server Function
#'
#' @noRd 
mod_showSubstanceInfo_server <- function(input, output, session){
  ns <- session$ns
 
  
  output$substance_info <- renderTable({
    
    #shinipsum::random_DT(10,4)
    
    sub_info
    
  })
  
}
    
## To be copied in the UI
# mod_showSubstanceInfo_ui("showSubstanceInfo_ui_1")
    
## To be copied in the server
# callModule(mod_showSubstanceInfo_server, "showSubstanceInfo_ui_1")
