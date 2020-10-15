#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    
    dashboardPage(
      dashboardHeader(title = "ImproRisk", titleWidth = 200),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Exposure", tabName = "exposure", icon = icon("th")),
          menuItem("Exposure by Demo", tabName = "exposureDemo", icon = icon("th")),
          menuItem("Contribution", tabName = "contribution", icon = icon("th")),
          menuItem("Update Data", tabName = "updateData", icon = icon("th")),
          
          menuItem("Occurrence", tabName = "occurrence", icon = icon("th"),
                   startExpanded = FALSE,
                   menuSubItem("Level 2", tabName = "occurrenceL2"),
                   menuSubItem("Level 3", tabName = "occurrenceL3")
                   ),
          menuItem("FoodEx1", tabName = "foodex1", icon = icon("th")),
          menuItem("Mean Consumption", tabName = "meanConsumption", icon = icon("th")),
          menuItem("Tables", tabName = "tables", icon = icon("th")),
          menuItem("INFO", tabName = "info", icon = icon("th"))
          #tableOutput("substance_info")
          #,DT::dataTableOutput("substance_info")
          
          
        )
      ),
      dashboardBody(
        tabItems(
          # EXPOSURE TAB ####
          tabItem(tabName = "exposure",
                  h3("Explore exposure"),
                  fluidRow(
                    box(title= "Substance Info", tableOutput("subInfo_exposure"))
                    #box(title = "fff", mod_showSubstanceInfo_ui("showSubstanceInfo_ui_1"))
                    
                  ),
                  tags$hr(style="border-color: purple;"),
                  #h4("Exposure Statistics"),
                  fluidRow(
                    column(4,
                      
                    box(title="Exposure Statistics (adjusted by Population)", 
                        textOutput("stats_label"),
                        tableOutput("tbl_exposure_stats")
                        ,width = NULL 
                        ),
                    box(width = NULL,
                        title = "Customise tables",
                        fluidRow(
                          column(width = 6,
                            numericInput("cust_digits", "Digits",value = 3, min = 0, max = 10, step = 1)
                            
                          ),
                          column(width =  6,
                            numericInput("cust_digits2", "Digits",value = 3, min = 0,  max = 10, step = 1)
                            
                          )
                          )
                        )
                    ),
                    column(7,
                           box(title = "Distribution of exposure", width = NULL,
                               shinyWidgets::radioGroupButtons("slct_scenario",
                                                               label = "Scenario",
                                                               choices = scenarios,
                                                               selected = scenarios[2]  #MB
                               ),
                               tabBox(id = "graphs",width = NULL, 
                                      title= "",
                                      tabPanel(title = "PDF",
                                               plotOutput("exposure_pdf")
                                      ),
                                      tabPanel(title = "CDF",
                                               "Cummulative Probability distribution",
                                               plotOutput("exposure_cdf")
                                      )
                               ),
                               box(title = "Customise graphs",collapsed = TRUE, collapsible = TRUE,width = NULL, 
                                   fluidRow(
                                     column(width = 6,
                                            numericInput("n.breaks",
                                                         value = 10, 
                                                         min = globals$min.n.breaks,
                                                         max = globals$max.n.breaks,
                                                         step = 1,
                                                         label = "Number of breaks" 
                                            )
                                            )
                                     ,column(
                                       width = 6,
                                       shinyWidgets::prettyCheckbox(
                                         inputId = "nice_breaks",
                                         value = TRUE,
                                         label ="Nice breaks?", 
                                         icon = icon("check"),
                                         status = "success"
                                       )
                                     )
                                     
                                   )
                                   
                               )
                               
                               )
                          
                    )
                    
                    )
                    
          ),
          
          # EXPOSURE by GROUP TAB ####
          tabItem(tabName = "exposureDemo",
                  h3("Explore exposure, grouped by demographic information"),
                  fluidRow(
                    box("Substance Info", tableOutput("subInfo_exposureDemo")),
                    
                  ),
                  tags$hr(style="border-color: black;"),
                  fluidRow(
                    
                    column(3,
                           box(
                             selectInput("slct_demo",  "Select demographic", choices = vars_demo),
                             shinyWidgets::radioGroupButtons("slct_scenario_Demo",
                                                             label = "Scenario",
                                                             choices = scenarios,
                                                             selected = scenarios[2]  #MB
                                                             )
                             , width = NULL
                           )
                        )
                    ),
                  # Stats and Graphs
                  fluidRow(
                    column(7,
                           box(
                             
                             tableOutput("tbl_exposure_statsDemo")
                             #box("Substance Info", mod_showSubstanceInfo_ui("showSubstanceInfo_ui_2"))
                             ,width = NULL
                           )
                    ),
                    column(5,
                           box(title = "Distribution of exposure", width = NULL,
                               tabBox(id = "graphsDemo",width = NULL, 
                                      title= "",
                                      tabPanel(title = "PDF",
                                               plotOutput("exposure_pdfDemo")
                                      ),
                                      tabPanel(title = "CDF",
                                               "Cummulative Probability distribution",
                                               plotOutput("exposure_cdfDemo")
                                      )
                               ),
                               box(title = "Customise graphs",
                                   collapsed = TRUE, collapsible = TRUE,width = NULL, 
                                   numericInput("n.breaksDemo",
                                                value = 10, min = 5,max = 20,step = 1,label = "Number of breaks" 
                                                ),
                                   shinyWidgets::prettyCheckbox(
                                     inputId = "nice_breaksDemo",
                                     value = TRUE,
                                     label ="Nice breaks?", 
                                     icon = icon("check"),
                                     status = "success"
                                   )
                               )
                               
                           )
                    )
                    )
                    ),
                           
          
          # Contribution TAB ####
          tabItem("contribution",
                  h3("Explore the contribution of food items to the total exposure"),
                  fluidRow(
                    column(4,
                           box(width = NULL,
                               title = "Select",
                               fluidRow(
                                 column(width = 6,
                                        selectInput("slct_level", "FoodEx1 Level",
                                                    choices = names(fdx1_levels),
                                                    selected = names(fdx1_levels)[2]
                                        )                                        
                                 ),
                                 column(width =  6,
                                        uiOutput("scenario_UI_contribution"),
                                        
                                 )
                               )
                               
                           )
                    )
                    
                ),
                fluidRow(
                  # column(4,
                  # box(width = NULL,
                  #     title = "Customise tables and Graphs",
                  #     collapsed = TRUE,
                  #     collapsible = TRUE,
                  #     fluidRow(
                  #       column(width = 3,
                  #              numericInput("contr_digitsExp", "Digits exposure",
                  #                           value = 1, min = 0, max = 10, step = 1)
                  #              
                  #       ),
                  #       column(width = 3,
                  #              numericInput("contr_digitsPct", "Digits percent",
                  #                           value = 2, min = 0, max = 10, step = 1)
                  #              ),
                  #       column(width =  6,
                  #              sliderInput(
                  #                inputId = "contr_filter",
                  #                value = 1,
                  #                label ="Show contribution greater than..", 
                  #                min  = 0, max = 50,
                  #                post = "%"
                  #              )                               
                  #       ),
                  #       column(width =  6,
                  #              sliderInput(
                  #                inputId = "contr_height",
                  #                value = 800,
                  #                min  = 500, max = 1200,
                  #                label ="Graph height", 
                  #                step = 50,
                  #                post = "px"
                  #              )                               
                  #       )
                  #     )
                  # )
                  # )
                ),
                fluidRow(
                  column(width = 9,
                         
                         tabBox(id = "contribution_panel",width = NULL, 
                                title= "",
                                tabPanel(title = "Table",
                                         reactable::reactableOutput("contribution")
                                ),
                                tabPanel(title = "Graphs",
                                         uiOutput("contr_UI",inline = TRUE)
                                         #ggiraph::girafeOutput("contr_graph", height = NULL)
                                )
                         ) 
                    
                  ),
                  column(3,
                         box(width = NULL,
                             title = "Customise tables and Graphs",
                             collapsed = TRUE,
                             collapsible = TRUE,
                             fluidRow(
                               column(width = 3,
                                      numericInput("contr_digitsExp", "Digits exposure",
                                                   value = 1, min = 0, max = 10, step = 1)
                                      
                               ),
                               column(width = 3,
                                      numericInput("contr_digitsPct", "Digits percent",
                                                   value = 2, min = 0, max = 10, step = 1)
                               ),
                               column(width =  6,
                                      # sliderInput(
                                      #   inputId = "contr_filter",
                                      #   value = 1,
                                      #   label ="Show contribution greater than..",
                                      #   min  = 0, max = 50,
                                      #   post = "%"
                                      # )
                                      shinyWidgets::radioGroupButtons(
                                        inputId = "contr_filter",
                                        choices = c("1%" =1, "5%"  =  5, "10%" =  10),
                                        label ="Show contribution greater than..",
                                        selected = 1
                                      )
                               ),
                               column(width =  6,
                                      sliderInput(
                                        inputId = "contr_height",
                                        value = 800,
                                        min  = 500, max = 1200,
                                        label ="Graph height",
                                        step = 50,
                                        post = "px"
                                      )
                               )
                             )
                         )
                  )
                  
                  
                  
                  
                )
                


                
                ),
          
          
          
          # Tables TAB ####
          tabItem(tabName = "tables",
                  h3("The data"),
                  box("Substance Info", DT::DTOutput("sample_data"),width = 12),
                  #box("Substance Info", mod_showSubstanceInfo_ui("showSubstanceInfo_ui_2"))
                  
          ),
          tabItem(tabName = "foodex1",
                  h3("The FoodEx1 food classification system"),
                  box(DT::DTOutput("foodex.1"),width = 12)
                  )
        )
      )
    )
    
    # fluidPage(
    #   h1("improrisk.shiny")
    # )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'improrisk.shiny'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

