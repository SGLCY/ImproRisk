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
          menuItem("Consumption", tabName = "consumption", icon = icon("th")),
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
                    box(title= "Substance Info"#, tableOutput("subInfo_exposure")
                    )
                    #box(title = "fff", mod_showSubstanceInfo_ui("showSubstanceInfo_ui_1"))
                    
                  ),
                  tags$hr(style="border-color: purple;"),
                  #h4("Exposure Statistics"),
                  fluidRow(
                    column(3,
                           box(title="Exposure Statistics (adjusted by Population)", 
                               textOutput("stats_label"),
                               br(),
                               tableOutput("tbl_exposure_stats")
                               ,width = NULL 
                           ),
                           box(width = NULL,
                               title = "Customise tables & graphs",
                               collapsed = TRUE,
                               collapsible = TRUE,
                               
                               fluidRow(
                                 column(width = 5,
                                        h5("Graphs"),
                                        numericInput("n.breaks",
                                                     value = 10,
                                                     min = globals$min.n.breaks,
                                                     max = globals$max.n.breaks,
                                                     step = 1,
                                                     label = "Number of breaks"
                                        ),
                                        sliderInput("pct.digits_exposure",
                                                    value = 1,
                                                    min = 0,
                                                    max = 3,
                                                    step = 1,
                                                    ticks = TRUE,
                                                    label = "% decimals"
                                        ),
                                        # shinyWidgets::noUiSliderInput(
                                        #   "pct.digits_exposure",
                                        #   value = 1,
                                        #   min = 0,
                                        #   max = 3,
                                        #   step = 1,
                                        #   label = "% accuracy"
                                        # ),
                                        shinyWidgets::prettyCheckbox(
                                          inputId = "show_stats_exposure",
                                          value = TRUE,
                                          label ="Show reference(s) on graph?",
                                          icon = icon("check"),
                                          status = "success"
                                        )
                                        
                                 ),
                                 column(width = 4,
                                        offset = 2,
                                        h5("Tables  & Graph"),
                                        numericInput("digits_exposure", 
                                                     "Digits",
                                                     value = 3, 
                                                     min = 1, max = 10, 
                                                     step = 1
                                        )
                                 )
                                 
                               )
                           )
                           
                    ),
                    column(7,
                           offset = 1,
                           box(title = "Distribution of exposure", 
                               width = NULL,
                               uiOutput("scenario_UI_exposure"),
                               tabBox(id = "graphs",width = NULL, 
                                      title= "",
                                      tabPanel(title = "PDF",
                                               plotOutput("exposure_pdf")
                                      ),
                                      tabPanel(title = "CDF",
                                               plotOutput("exposure_cdf")
                                      )
                               )
                               
                               
                           )
                    )
                    
                  ) #  fluidRow
                  
          ),
          
          # EXPOSURE by GROUP TAB ####
          tabItem(tabName = "exposureDemo",
                  h3("Explore exposure, grouped by demographic information"),
                  fluidRow(
                    #box("Substance Info", tableOutput("subInfo_exposureDemo")),
                    
                  ),
                  tags$hr(style="border-color: black;"),
                  fluidRow(
                    
                    column(3,
                           box(
                             selectInput("slct_demo",  
                                         "Select demographic", 
                                         choices = vars_demo
                             )
                             , uiOutput("scenario_UI_exposureDemo")
                             , width = NULL
                           )
                    ),
                    column(7,
                           box(
                             
                             tableOutput("tbl_exposure_statsDemo")
                             ,width = NULL
                           )
                    )
                  ),
                  # Stats and Graphs
                  fluidRow(
                    column(3,
                           box(title = "Customise tables & graphs",
                               collapsed = TRUE, collapsible = TRUE,width = NULL, 
                               numericInput("digits_exposureDemo", 
                                            "Digits",
                                            value = 2, 
                                            min = 1, max = 10, 
                                            step = 1
                               ),
                               numericInput("pct.digits_exposureDemo", 
                                            "% Digits",
                                            value = 2, 
                                            min = 0, max = 5, 
                                            step = 1
                               ),
                               sliderInput("bandwidthDemo",
                                           "Bandwidth (PDF)",
                                           value = NULL,
                                           min = 0, max = 0.5,
                                           step = 0.01,
                                           round = -3 #3 decimals
                                           
                               )
                           )
                    ),
                    column(7,
                           box(title = "Distribution of exposure", width = NULL,
                               tabBox(id = "graphsDemo",width = NULL, 
                                      title= "",
                                      tabPanel(title = "PDF",
                                               plotOutput("exposure_pdfDemo")
                                      ),
                                      tabPanel(title = "CDF",
                                               plotOutput("exposure_cdfDemo")
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
                    column(width = 9,
                           
                           tabBox(id = "contribution_panel",width = NULL, 
                                  title= "",
                                  tabPanel(title = "Table",
                                           htmlOutput("contr_tbl_title"),
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
                               title = "Customise tables and graphs",
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
                               ) #fluidRow
                           )
                    )#column 3
                    
                    
                    
                    
                  )
                  
                  
                  
                  
          ),
          
          
          
          # Tables TAB ####
          tabItem(tabName = "tables",
                  h3("The data"),
                  box("Substance Info", DT::DTOutput("sample_data"),width = 12),
                  #box("Substance Info", mod_showSubstanceInfo_ui("showSubstanceInfo_ui_2"))
                  
          ),
          
          # FOODEX1 TAB ####
          tabItem(tabName = "foodex1",
                  h3("The FoodEx1 food classification system"),
                  box(DT::DTOutput("foodex.1"),width = 12)
          ),
          
          
          # Consumption TAB  ####
          tabItem(tabName = "consumption",
                  h3("Consumption statistics"),
                  tags$hr(style="border-color: black;"),
                  fluidRow(
                    box("Select food level", width = 2,
                        selectInput("slct_food_levelConsumption",
                                    label = "FoodEx1 Level",
                                    choices = fdx1_levels_cons,
                                    selected = "Level 1"
                        )
                    )
                    
                  ),
                  fluidRow(
                    column(width = 8,
                           tabBox(id = "consumptionTabBox",width = NULL,
                                  title= "",
                                  tabPanel(title = "Table",
                                           h2("Mean Consumption (grams)"),
                                           tableOutput("tbl_aggr_consumption")
                                  ),
                                  tabPanel(title = "Graph",
                                           uiOutput("plot_aggr_consumption_UI", inline = TRUE)
                                  )
                           )
                           ),
                    column(width = 4,
                           
                           box(title = "Graph options ",
                               selectInput("slct_consumptionType",
                                           "Select consumption type",
                                           choices = c("Consumer based" = "consumer", 
                                                       "Population based" =  "population",
                                                       "Both"  = "Both"),
                                           selected = "Consumer based"
                                           ),
                               collapsible = TRUE,
                               collapsed = FALSE
                               )
                           )
                    
                        
                    )
                    
          )
        )
      )
    )
    
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

