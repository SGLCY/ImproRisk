#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @importFrom reactable colDef  colFormat
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_showSubstanceInfo_server, "showSubstanceInfo_ui_1")
  callModule(mod_showSubstanceInfo_server, "showSubstanceInfo_ui_2")
  
  
  
  # Reactive Values ####
  rv <- rv(
    scenario = NULL,
    title = NULL,
    title_statsDemo =  NULL,
    exposure_factor = 1,
    ref_value = 4,
    demo = c("gender" = "Gender", "area" ="Area", "pop_class" = "Population_Class"),
    sample_size = nrow(tbl_exposure),
    pop_size = sum(tbl_exposure$wcoeff),
    
    show_level1 = NULL, #for the contribution tables
    
    x_label = "μg/Kg body weight", # somtimeI will give the option to the user to change it
    y_label = "Population" # this might be `sample`? in case of non weighted
  )
  
  
  
  #  Exposure Statistics ####
  output$tbl_exposure_stats <- renderTable({
    
    tbl_exposure %>% 
      tidyr::pivot_longer(
        cols = starts_with("subExp_"),
        names_to = "scenario",
        values_to = "exposure"
      ) %>% 
      dplyr::group_by(scenario) %>% 
      summarise_weighted() %>% 
      # stats on the left
      tidyr::gather(key, value, - scenario) %>%
      tidyr::pivot_wider(names_from = scenario, values_from = value) %>% 
      {.} %>% 
      dplyr::rename_with(
        ~stringr::str_remove(., "subExp_"),
        dplyr::starts_with("subExp_")
      ) %>% 
      data.frame(row.names = "key")
    
  },rownames = TRUE)
    
  
  
  tbl_exposure_statsDemo <- reactive({
    
    tbl_exposure %>% 
      tidyr::pivot_longer(
        cols = starts_with("subExp_"),
        names_to = "scenario",
        values_to = "exposure"
      ) %>% 
      dplyr::group_by(scenario,.data[[input$slct_demo]]) %>% 
      summarise_weighted() %>% 
      # summarise(
      #   across(exposure, exposure_summary,.names = "{.fn}")
      # ) %>%
      {.} %>% 
      ungroup() %>% 
      dplyr::mutate(
        scenario = stringr::str_remove(scenario, "subExp_")
      ) %>% 
      dplyr::filter(scenario == input$slct_scenario_Demo) %>% 
      dplyr::select(-scenario) 
  })
  
  # Kable  is HTML
  # https://cran.r-project.org/web/packages/kableExtra/vignettes/use_kable_in_shiny.html
  output$tbl_exposure_statsDemo <- function(){

    digits = 3
    caption  = rv$title_statsDemo

    tbl_exposure_statsDemo() %>%
      knitr::kable("html", caption = caption,digits = digits ) %>%
      kableExtra::kable_styling("striped")

  }
  
  
  # Other ####
  
  
  # output$substance_info <- renderTable({
  #   
  #   #shinipsum::random_DT(10,4)
  #   
  #   sub_info
  #   
  # }, rownames = TRUE, colnames = FALSE)
  
  
  
  output$substance_info <- renderTable({sub_info}, rownames = TRUE, colnames = FALSE,  width = "50px")
  
  # output$substance_info <- DT::renderDataTable({
  #   sub_info %>% 
  #     tibble::rownames_to_column() %>% 
  #     DT::datatable(
  #       filter = "none",
  #       options = list(
  #         autoWidth = TRUE,
  #         columnDefs = list(list(width = '10%', targets = c(1,2))),
  #         info = FALSE
  #       )
  #     )
  #   
  #   })
  
  
  # An output binding of SUBSTABCE INFO for each left tab in the app
  # see https://stackoverflow.com/questions/44205137/plotting-the-same-output-in-two-tabpanels-in-shiny/44206812#44206812
  # Note: The module way has some issues in the rendering of the table
  lapply(tab_items$tabName, function(tab_name){
    
    output[[paste0("subInfo_", tab_name)]] <- renderTable({sub_info}, rownames = TRUE, colnames = FALSE)      
  })
  
  # Create an output binding of select_scenario inputs for each Tab
  # The id for these inputs follow the convention 'slct_scenario_{tabName}'- see fct_helpers.R
  # The output binding follows the convention 'ouput$scenario_UI_{tabName}' - see inside function(x)
  # NOTE: Renders a bit slowly (expected)... Might need  to think of better ways
  purrr::map(
    .x = tab_items$tabName,
    .f=  function(x) {
      
      output[[paste0("scenario_UI_", x)]] <- 
        
        renderUI({slct_scenario(x, choices = scenarios)}) 
    }

  )
  
  
  output$exposure_statistics <- renderTable({
    
    shinipsum::random_table(10,3, type = "numeric")
    
    
  }, rownames = TRUE, colnames = TRUE)
  
  
  
  # Exposure distributions ####
  
  
  exposure_pdf <- reactive({
    
    ref_value <- isolate(rv$ref_value)
    scenario <- input$slct_scenario
    
    var_to_use <- paste0("subExp_",scenario)
    title <- glue::glue("Probability distribution of exposure at the {scenario} scenario")
    x_label <- rv$x_label
    y_label <- rv$y_label
    
    n.breaks <- input$n.breaks
    
    # req(n.breaks >=  globals$min.n.breaks,
    #     n.breaks <=  globals$max.n.breaks
    #     )
    
    validate(
      need(n.breaks>globals$min.n.breaks && n.breaks <=  globals$max.n.breaks, 
           glue::glue(
             "# of breaks should be:
             >=  {globals$min.n.breaks} and <= {globals$max.n.breaks}"
             )
           )
      )
    
    tbl_exposure %>% 
      ggplot(aes(.data[[var_to_use]]))+
      geom_bar(aes(y =..prop..))+
      geom_text(aes( label = scales::percent(..prop..,  accuracy = 0.1 ),
                     y= ..prop.. ), stat= "count", vjust = -.5) +
      scale_x_binned(nice.breaks = input$nice_breaks, n.breaks = n.breaks )+
      scale_y_continuous(labels = scales::percent)+
      geom_vline(aes(xintercept=median(.data$subExp_MB),
                     color="median"), linetype="dashed",
                 size=1) +
      geom_vline(aes(xintercept=mean(.data$subExp_MB),
                     color="mean"), linetype="dotted",
                 size=1) +
      geom_vline(aes(xintercept=ref_value,
                     color="Reference_value"), linetype="dotted",
                 size=1) +
      scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"
                                                         , Reference_value = "black"
                                                         )
                         )+
      labs(
        title = title,
        x= x_label,
        y = y_label
      )+
      NULL
    
    
  })
  
  output$exposure_pdf <- renderPlot({
    
    #shinipsum::random_ggplot(type = "histogram")
    
    exposure_pdf()
    
  })
  
  
  exposure_cdf <- reactive({
    
    ref_value <- isolate(rv$ref_value)
    
    scenario <- input$slct_scenario
    var_to_use <- paste0("subExp_",scenario)
    #title <- paste0("Cummulative distribution at the ", scenario, "scenario")
    title <- glue::glue("Cummulative distribution of exposure at the {scenario} scenario")
    x_label <- rv$x_label
    y_label <- rv$y_label
    
    
    tbl_exposure %>% 
      ggplot(aes(.data[[var_to_use]]))+
      stat_ecdf(pad = FALSE)+
      scale_y_continuous(labels = scales::percent)+
      geom_vline(xintercept = ref_value, linetype = "dashed" )+
      annotate("text", x = ref_value+0.05*ref_value, y = 0.51,
               hjust = 0, label= paste0("Reference: ",ref_value, "μg/Kw b.w.")
               )+
      labs(
        title = title,
        x  = x_label,
        y = y_label
      )+
      NULL
    
    
    
  })
  
  output$exposure_cdf <- renderPlot({
    
    #shinipsum::random_ggplot(type = "line")
    exposure_cdf()
  })

  
  output$exposure_pdfDemo <- renderPlot({
    
    shinipsum::random_ggplot(type = "histogram")
  })
  
  output$exposure_cdfDemo <- renderPlot({
    
    shinipsum::random_ggplot(type = "line")

      })
  
  
  # Contribution ####
  
  tbl_contribution <- reactive({
    
      food_level <- fdx1_levels[[input$slct_level]]
    
      temp <- 
        sample_consumption %>% 
        rename(
          FOODEX_L3_DESC = consumed_food_at_level_3,
          FOODEX_L2_DESC = consumed_food_at_level_2,
          FOODEX_L1_DESC = consumed_food_at_level_1
        ) %>% 
        rename_with(~scenarios, .cols = contains("refined_exposure")) %>% 
        group_by(
          across(all_of(food_level))
        ) %>% 
        summarise(
          across(.cols= all_of(scenarios), .fns = ~sum(., na.rm = TRUE))
        ) %>% 
        ungroup() %>% 
        full_join(
          tbl_foodex_desc %>% 
            distinct(
              across(all_of(food_level))
            )
        ) %>% 
        tidyr::gather(
          "scenario", "exposure", all_of(scenarios)
        ) %>% 
        tidyr::replace_na(list(exposure = 0)) %>% 
        # here i need the grouping to be onthe scenario levels
        group_by(scenario) %>% 
        mutate(
          contribution = exposure/ sum(exposure, na.rm = TRUE)
        ) 
      
      # Calculate the Within Percentage 
      
      tbl_contribution <- 
        temp %>% 
        group_by(scenario, .data[[dplyr::nth(food_level, -2,default = food_level)]]) %>% 
        mutate(
          contr_within = exposure/ sum(exposure, na.rm = TRUE)
        ) %>% 
        #  Set NAN 0/0 as NA. Need to show this in the table 
        #mutate(contr_within = if_else(is.nan(contr_within), NA_real_, contr_within)) %>% 
        ungroup()
    
    tbl_contribution
    
  })
  
  
  contribution_filtered <- reactive({
    
    req(input$slct_scenario_contribution)
    
    tbl_contribution() %>% 
      filter(
        scenario == input$slct_scenario_contribution,
        contribution >= as.numeric(input$contr_filter)/100
      ) 
  
  })
  
  
  # output$contribution <- DT::renderDT({
  #   
  #   contribution_filtered() %>% 
  #     DT::datatable(
  #       extensions = 'RowGroup',
  #       options = list(rowGroup = list(dataSrc  = 2))
  #     )
  #   
  #   
  # })
  
  output$contribution <- reactable::renderReactable({
    
    food_level <- fdx1_levels[[input$slct_level]]
    
    #  Level 1 Show depends on the requested depth
    if(length(food_level) == 3) {
      rv$show_level1 <- FALSE
      
    } else{
      rv$show_level1 <- TRUE
    }
    
    # if Level 1 only then no need for the contribution within
    if(length(food_level) == 1) {
      rv$show_contr <- FALSE
      
    } else{
      rv$show_contr <- TRUE
    }
    
    contribution_filtered() %>% 
      reactable::reactable(
              # in the Level 1 case
              groupBy = nth(food_level, -2, default = food_level),
              columns = list(
                scenario = colDef(show = FALSE),
                exposure = colDef(name = "Total exposure", 
                                  aggregate = "sum",
                                  format = colFormat(digits = input$contr_digitsExp),
                                  filterable = FALSE
                ),
                contribution = colDef(name = "Contribution to Total exposure",aggregate = "sum",
                                      format = colFormat(percent = TRUE, digits = input$contr_digitsPct),
                                      filterable = FALSE,
                                      defaultSortOrder = "desc"
                                      
                ),
                contr_within = colDef(name = "Contribution within"
                                      ,show = rv$show_contr
                                      ,aggregate = "sum",
                                      filterable = FALSE,
                                      format = colFormat(percent = TRUE, digits =  input$contr_digitsPct)
                ),
                FOODEX_L1_DESC = colDef(show = rv$show_level1)
              ),
              #fullWidth = FALSE,
              #width = 1000,
              striped = TRUE,
              bordered = TRUE,
              highlight = TRUE,
              #filterable = TRUE,
              searchable = TRUE,
              #minRows = 15,
              defaultSorted = "contribution",
              rowStyle = reactable::JS("function(rowInfo) {
          if (rowInfo.aggregated   == true) {
          return {background: 'rgba(0,0,0,0.07', fontWeight: 'bold' }
          }
          }")
              # Not working
              # theme = reactableTheme(
              #   rowGroupStyle = list(background= "rgba(0, 0, 0, 0.23)"
              #                        ),
              #   rowStyle = list(colour = "#008000")
    )
    
    
    
    
  })
  
  
  contr_graph <- reactive({
    
    top_level <- dplyr::nth(fdx1_levels[[input$slct_level]], -1)
    second_top_level <- dplyr::nth(fdx1_levels[[input$slct_level]], -2, default = "FOODEX_L1_DESC")
    
    
    # Graph peripherals
    title <- glue::glue("Contribution Total Exposure ({isolate(input$slct_scenario_contribution)})")
    
    if(isolate(as.numeric(input$contr_filter))== 0) {
      subtitle = ""
    }else {
      subtitle <- glue::glue("Food items with greater than {isolate(input$contr_filter)}% contribution\n")
    }
    
    max_contr <- max(contribution_filtered()$contribution)
    
    
    # The plot
    
    contribution_filtered() %>%
      ggplot(
        aes(
          x = forcats::fct_reorder(.data[[top_level]], contribution), 
          y = contribution,
          tooltip = scales::percent(contribution,  accuracy = 0.1),
          data_id = .data[[second_top_level]] 
        )
      )+
      ggiraph::geom_col_interactive(width = 0.5)+
      coord_flip(ylim = c(0, max_contr*1.10))+
      labs(
        x = top_level,
        y  = "Contribution",
        title = title,
        subtitle = subtitle
      )+
      scale_y_continuous(labels = scales::percent, expand = c(0,0.001))+
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, 30))+
      theme_bw()+
      theme(
        axis.text = element_text(size = 7),
        #axis.title.y = element_text(hjust = 1, angle = 0 )
        plot.caption = element_text(face = "italic", size = 5)
      )
        
  })
  
  output$contr_graph <- ggiraph::renderGirafe({
    
    ggiraph::girafe(ggobj =contr_graph(),width_svg = 6 )
    
  })
  
  output$contr_UI <- renderUI({
    
    ggiraph::girafeOutput({
      "contr_graph"
    },height = paste0(input$contr_height, "px")
    )
    
  })
  
  # Observers ####
  
  
  observeEvent({input$slct_demo
                input$slct_scenario_Demo}
               , 
               {
                 rv$title_statsDemo <- 
                   paste0("Exposure estimates by ", 
                          rv$demo[input$slct_demo],
                          " at the ",
                          input$slct_scenario_Demo,
                          " scenario"
                          )
               }
  )
  
  output$title_statsDemo <- renderText(
    rv$title_statsDemo
  )
  
  
  #Sample Data ####
  
  output$sample_data <- DT::renderDataTable({
    
    sample_consumption %>% 
      head(200) %>% 
      DT::datatable(
        caption = "The final table"
        , style = "bootstrap"
        , rownames = FALSE
        , options = list(
          
          paging = FALSE,
          scrollX = TRUE, scrollY = "500px"
        )
        , filter = "top"
      )
    
    
  })
  
  output$stats_label <- renderText({
    
    s_size <- rv$sample_size
    p_size <- rv$pop_size
    
    glue::glue('Sample size:{s_size}\nPopulation size: {prettyNum(p_size, big.mark = ",")}')
  })
  
  output$foodex.1 <- DT::renderDataTable({
    
    foodex.1
    
  }, filter = "top")
  
  
  
}
