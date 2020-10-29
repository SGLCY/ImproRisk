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
        sidebarMenu(id = "tabs",
          menuItem("Exposure", tabName = "exposure", icon = icon("atom")),
          menuItem("Exposure by Demo", tabName = "exposureDemo", icon = icon("user-friends")),
          menuItem("Contribution", tabName = "contribution", icon = icon("percent")),
          menuItem("Explore Consumption", tabName = "consumption", icon = icon("utensils")),
          menuItem("Drill down", tabName = "drillDown", icon = icon("chart-bar")),
          menuItem("Occurrence", tabName = "occurrence", icon = icon("flask"),
                   startExpanded = FALSE,
                   menuSubItem("Level 2", tabName = "occurrenceL2"),
                   menuSubItem("Level 3", tabName = "occurrenceL3")
          ),
          menuItem("FoodEx1", tabName = "foodex1", icon = icon("bread-slice")),
          menuItem("Tables", tabName = "tables", icon = icon("table"),
                   startExpanded = FALSE,
                   #menuSubItem("Subjects", tabName = "subjects"),
                   menuSubItem("Merged data", tabName = "merged"),
                   menuSubItem("Individual exposure", tabName = "individual")
          ),
          menuItem("Update Data", tabName = "updateData", icon = icon("file-import")),
          menuItem("Log", tabName = "log",icon = icon("columns")),
          menuItem("ABOUT", tabName = "info", icon = icon("info"), selected = TRUE),
          shinyWidgets::actionBttn(
            inputId = "help_exposure",
            label = NULL,
            style = "material-circle", 
            color = "default",
            size = "xs",
            icon = icon("question")
          ) 
          #tableOutput("substance_info")
          #,DT::dataTableOutput("substance_info")
          
          
        )
      ),
      dashboardBody(
        tabItems(
          # EXPOSURE TAB ####
          tabItem(tabName = "exposure",
                  h3("Exposure estimates"),
                  tags$hr(style="border-color: black;"),
                  #h4("Exposure Statistics"),
                  fluidRow(
                    column(3,
                           box(title= "Substance Info", 
                               tableOutput("subInfo_exposure"),
                               width = NULL
                           ),
                           box(title="Exposure Statistics (μg/Kg b.w.)", 
                               uiOutput("stats_label"),
                               br(),
                               tableOutput("tbl_exposure_stats"),
                               mod_downloadTable_ui("tbl_exposure_stats")
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
                                        h5("Tables  & Graphs"),
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
                                               plotOutput("exposure_pdf"),
                                               mod_downloadPlot_ui("exposurePDF")
                                      ),
                                      tabPanel(title = "CDF",
                                               plotOutput("exposure_cdf"),
                                               mod_downloadPlot_ui("exposureCDF")
                                               
                                      )
                               )
                               
                               
                           )
                    )
                    
                  ) #  fluidRow
                  
          ),
          
          # EXPOSURE by GROUP TAB ####
          tabItem(tabName = "exposureDemo",
                  h3("Explore exposure, grouped by demographic information"),
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
                    column(7,offset = 1,
                           box(
                             
                             tableOutput("tbl_exposure_statsDemo"),
                             mod_downloadTable_ui("tbl_exposure_statsDemo")
                             ,width = NULL
                           )
                    )
                  ),
                  # Stats and Graphs
                  fluidRow(
                    column(3,
                           box("Substance Info", 
                               tableOutput("subInfo_exposureDemo"),
                               width = NULL
                           ),
                           box(title = "Customise tables & graphs",
                               collapsed = TRUE, collapsible = TRUE,
                               width = NULL, 
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
                    column(7, offset = 1,
                           box(title = "Distribution of exposure", width = NULL,
                               tabBox(id = "graphsDemo",width = NULL, 
                                      title= "",
                                      tabPanel(title = "PDF",
                                               plotOutput("exposure_pdfDemo"),
                                               mod_downloadPlot_ui("exposure_pdfDemo")
                                               
                                      ),
                                      tabPanel(title = "CDF",
                                               plotOutput("exposure_cdfDemo"),
                                               mod_downloadPlot_ui("exposure_cdfDemo")
                                               
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
                                           reactable::reactableOutput("tbl_aggr_contribution"),
                                           mod_downloadTable_ui("tbl_contribution")
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
                                          value = 700,
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
          # tabItem(tabName = "subjects",
          #         h3("Participants in the food survey"),
          #         box(title = "", 
          #             #DT::DTOutput("sample_data"),
          #             width = 12
          #         ),
          #         
          #         #box("Substance Info", mod_showSubstanceInfo_ui("showSubstanceInfo_ui_2"))
          #         
          # ),
          tabItem(tabName = "merged",
                  h3("Consumption and exposure at food consumption occassion"),
                  box(title = "", 
                      DT::DTOutput("sample_data"),
                      mod_downloadTable_ui("full_data"),
                      width = 12
                  )
          ),
          tabItem(tabName = "individual",
                  h3("Participants and individual exposure"),
                  box(title = "", 
                      reactable::reactableOutput("tbl_exposure"),
                      mod_downloadTable_ui("temp_tbl_exposure"),
                      width = 12
                  )
          ),
          
          # FOODEX1 TAB ####
          tabItem(tabName = "foodex1",
                  h3("The FoodEx1 food classification system"),
                  box(DT::DTOutput("tbl_foodex1"),width = 12),
                  mod_downloadTable_ui("foodex1")
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
                                           DT::DTOutput("tbl_aggr_consumption"),
                                           mod_downloadTable_ui("tbl_aggr_consumption")
                                  ),
                                  tabPanel(title = "Graph",
                                           uiOutput("plot_aggr_consumption_UI", inline = TRUE),
                                           mod_downloadPlot_ui("plot_aggr_consumption")
                                           
                                  )
                           )
                    ),
                    column(width = 4,
                           
                           box(title = "Options ",
                               shinyWidgets::prettyCheckbox(
                                 inputId = "hide_water",
                                 value = TRUE,
                                 label ="Filter out water?",
                                 icon = icon("check"),
                                 status = "success"
                               ),
                               selectInput("slct_consumptionType",
                                           "Select consumption type  (for Graph",
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
                  
          ), #end  of tabItem
          
          # Drill Down ####
          tabItem(tabName = "drillDown",
                  h3("Cross tabulations of demographics"),
                  h4("Get the mean exposure by 2 demographics"),
                  fluidRow(
                    column(width = 6,
                           box(width = NULL,
                               plotOutput("plot_cross_demoExposure"),
                               uiOutput("downBtn_crossDemoPlot")
                               
                           ),
                           box(width = NULL,
                               tableOutput("tbl_cross_demoExposure"),
                               uiOutput("downBtn_crossDemoTable")
                           )
                           
                    ),
                    column(width = 5, offset = 1, 
                           box(title = "", height = 350, width = NULL,
                               column(width = 12/3,
                                      h4("List of demographics"),
                                      sortable::rank_list(
                                        text = "Choose from here and drag to the right",
                                        labels = unname(vars_demo),
                                        input_id = "drill_varsDemo",
                                        options = sortable::sortable_options(group = "drill_down_group")
                                      ),
                               ),
                               column(width = 12/3,
                                      h4("Cross demo"),
                                      sortable::rank_list(
                                        text = "Put exactly 2 demographics here",
                                        labels = c(),
                                        input_id = "drill_varsExplore",
                                        options = max_2_item_opts
                                      )
                               )
                               # ,column(width = 12/3,
                               #        h4("Filter by"),
                               #        sortable::rank_list(
                               #          text = "1 demographic only",
                               #          labels = c(),
                               #          input_id = "drill_varsFilter",
                               #          options = max_1_item_opts
                               #        )
                               # )
                           )  
                           
                           
                    )
                    
                    
                  )
                  
          ),
          
          # Update Data #####
          tabItem(tabName = "updateData",
                  h3("Import Consumption and Occurrene Data"),
                  fluidRow(
                    column(width = 3,
                           box(width = NULL,
                               fileInput("consumption_file",
                                         "Import Consumption data",
                                         width = NULL
                               ),
                               br(),
                               fileInput("occurrence_file",
                                         "Import Occurrence Data"
                               )     
                               
                           )
                           
                           
                    )
                  )
                  
                  
          ),
          
          # Occurence tabs ####
          # Level 2.
          tabItem(tabName = "occurrenceL2",
                  fluidRow(
                    box(title = h3("Occurence at Level 2"),
                        width = 12,
                        tableOutput("subInfo_occurrenceL2"),
                        DT::DTOutput("occurrence_l2"),
                        mod_downloadTable_ui("occurrence_l2")
                    )
                  )
                  
                  
          ),
          
          # Level 3
          tabItem(tabName = "occurrenceL3",
                  fluidRow(
                    box(title = h3("Occurence at Level 3"),
                        width = 12,
                        tableOutput("subInfo_occurrenceL3"),
                        DT::DTOutput("occurrence_l3"),
                        mod_downloadTable_ui("occurrence_l3")
                        
                    )
                  )
          ),
          
          # Log ####
          tabItem(tabName = "log",
                  fluidRow(
                    
                    column(width = 6,
                           
                           box(width = NULL,
                               title = "Dataset Info",
                               tableOutput("dataset_info")
                               
                               
                           )
                           
                           
                    )
                  )
                  
          ),
          
          # INFO ####
          
          tabItem(tabName = "info",
                  box(width = 6, 
                      title = "",
                      info_improrisk
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
    , rintrojs::introjsUI()
  )
}

