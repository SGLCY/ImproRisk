#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @importFrom reactable colDef colFormat colGroup
#' @importFrom forcats fct_rev fct_inorder fct_reorder fct_infreq fct_inseq
#' @importFrom glue glue
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_showSubstanceInfo_server, "showSubstanceInfo_ui_1")
  callModule(mod_showSubstanceInfo_server, "showSubstanceInfo_ui_2")
  
  
  # Module Plots ####
  callModule(mod_downloadPlot_server, "exposurePDF", 
             plot_name = "exposurePDF", 
             the_plot = exposure_pdf
  )
  callModule(mod_downloadPlot_server, "exposureCDF", 
             plot_name = "exposureCDF", 
             the_plot = exposure_cdf
  )
  
  callModule(mod_downloadPlot_server, "exposure_pdfDemo", 
             plot_name = "exposure_pdfDemo", 
             the_plot = exposure_pdfDemo
  )
  
  callModule(mod_downloadPlot_server, "exposure_cdfDemo", 
             plot_name = "exposure_cdfDemo", 
             the_plot = exposure_cdfDemo
  )
  
  
  callModule(mod_downloadPlot_server, "plot_aggr_consumption", 
             plot_name = "plot_aggr_consumption", 
             the_plot = plot_aggr_consumption, width = 14, height  = 10
  )
  
  
  callModule(mod_downloadPlot_server, "plot_cross_demoExposure", 
             plot_name = "plot_cross_demoExposure", 
             the_plot = plot_cross_demoExposure
  )
  
  
  
  # Module Download Tables ####
  
  callModule(mod_downloadTable_server, "tbl_aggr_consumption",
             table_name = "tbl_aggr_consumption",
             the_table = tbl_aggr_consumption)
  
  
  callModule(mod_downloadTable_server, "tbl_exposure_stats",
             table_name = "tbl_exposure_stats",
             the_table = tbl_exposure_stats)
  
  
  callModule(mod_downloadTable_server, "tbl_exposure_statsDemo",
             table_name = "tbl_exposure_statsDemo",
             the_table = tbl_exposure_statsDemo)
  
  callModule(mod_downloadTable_server, "tbl_contribution",
             table_name = "tbl_contribution",
             the_table = contribution_filtered)
  
  callModule(mod_downloadTable_server, "temp_tbl_exposure",
             table_name = "temp_tbl_exposure",
             the_table = temp_tbl_exposure)
  
  
  callModule(mod_downloadTable_server, "foodex1",
             table_name = "foodex1",
             the_table = foodex1)
  
  
  callModule(mod_downloadTable_server, "full_data",
             table_name = "full_data",
             the_table = full_data)
  
  
  callModule(mod_downloadTable_server, "tbl_cross_demoExposure",
             table_name = "tbl_cross_demoExposure",
             the_table = tbl_cross_demoExposure)
  
  callModule(mod_downloadTable_server, "occurrence_l3",
             table_name = "occurrence_l3",
             the_table = occurrence_l3)
  
  callModule(mod_downloadTable_server, "occurrence_l2",
             table_name = "occurrence_l2",
             the_table = occurrence_l2)
  
  
  ggplot2::theme_set(ggthemes::theme_clean(base_size = 15)+
                       theme(
                         plot.background = element_blank(),
                         legend.background = element_blank(),
                         axis.text = element_text(size= 14)
                       )
  )
  
  # Reactive Values ####
  rv <- rv(
    scenario = NULL,
    title = NULL,
    title_statsDemo =  NULL,
    exposure_factor = 1,
    exposure_frequency= "DAILY",
    ref_value = 0.63,
    demo = c("gender" = "Gender", "area" ="Area", "pop_class" = "Population_Class"),
    tbl_exposure = NULL,
    sample_size = nrow(tbl_exposure),
    pop_size = sum(tbl_exposure$wcoeff),
    
    show_level1 = NULL, #for the contribution tables
    
    x_label = "μg/Kg body weight", # somtimeI will give the option to the user to change it
    y_label = "Population" # this might be `sample`? in case of non weighted
  )
  
  #temp for download
  foodex1 <- reactive({
    foodex.1
  })
  
  full_data <- reactive({
    sample_consumption
  })
  
  #  Exposure Statistics ####
  
  
  tbl_exposure_stats <- reactive({
    
    req(input$digits_exposure)
    
    digits  <- input$digits_exposure
    
    tbl_exposure %>% 
      tidyr::pivot_longer(
        cols = starts_with("subExp_"),
        names_to = "scenario",
        values_to = "exposure"
      ) %>% 
      dplyr::group_by(scenario) %>% 
      summarise_weighted(ref_value= rv$ref_value) %>% 
      mutate(pctOver  = percent(pctOver)) %>% 
      mutate_at(vars(-scenario, -pctOver), ~ round(.,digits)) %>% 
      tidyr::gather(key, value, - scenario) %>%
      tidyr::pivot_wider(names_from = scenario, values_from = value) %>% 
      {.} %>% 
      dplyr::rename_with(
        ~stringr::str_remove(., "subExp_"),
        dplyr::starts_with("subExp_")
      ) 
  })
  
  
  output$tbl_exposure_stats <- renderTable({
    
    #style the pctOver row
    ind <- match("pctOver",tbl_exposure_stats()$key)
    
    tbl_exposure_stats()%>% 
      data.frame(row.names = "key")
    
  }
  ,rownames = TRUE
  #,digits = function() input$digits_exposure
  ,caption = as.character(p(br(),"pctOver: % of population over the reference value"))
  )
  
  
  
  tbl_exposure_statsDemo <- reactive({
    
    
    req(input$slct_scenario_exposureDemo,
        input$slct_demo
    )
    
    pct.digits <- input$pct.digits_exposureDemo
    
    tbl_exposure %>% 
      tidyr::pivot_longer(
        cols = starts_with("subExp_"),
        names_to = "scenario",
        values_to = "exposure"
      ) %>% 
      dplyr::group_by(scenario,.data[[input$slct_demo]]) %>% 
      summarise_weighted(ref_value = rv$ref_value) %>% 
      # summarise(
      #   across(exposure, exposure_summary,.names = "{.fn}")
      # ) %>%
      {.} %>% 
      ungroup() %>% 
      dplyr::mutate(
        scenario = stringr::str_remove(scenario, "subExp_")
      ) %>% 
      dplyr::filter(scenario == input$slct_scenario_exposureDemo) %>% 
      dplyr::select(-scenario) %>% 
      mutate(
        pctOver = glue::glue("{round(pctOver*100, pct.digits)}%")
      )
    
  })
  
  # Kable  is HTML
  # https://cran.r-project.org/web/packages/kableExtra/vignettes/use_kable_in_shiny.html
  output$tbl_exposure_statsDemo <- function(){
    
    digits = input$digits_exposureDemo
    caption  = as.character(h4(strong(rv$title_statsDemo)))
    
    tbl_exposure_statsDemo() %>%
      knitr::kable("html", caption = caption,digits = digits ) %>%
      kableExtra::kable_styling("striped")
    
  }
  
  
  # Other ####
  
  
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
  
  
  
  # Exposure distributions ####
  
  
  exposure_pdf <- reactive({
    
    req(input$slct_scenario_exposure)
    
    ref_value  <- isolate(rv$ref_value)
    scenario   <- input$slct_scenario_exposure
    
    var_to_use <- paste0("subExp_",scenario)
    title      <- glue::glue("Probability distribution of exposure at the {scenario} scenario")
    x_label    <- rv$x_label
    y_label    <- rv$y_label
    
    n.breaks   <- input$n.breaks
    digits     <- input$digits_exposure
    accuracy   <- 1/(10^input$pct.digits_exposure)
    
    add_stats  <- input$show_stats_exposure
    
    validate(
      need(n.breaks>globals$min.n.breaks && n.breaks <=  globals$max.n.breaks, 
           glue::glue(
             "# of breaks should be:>=  {globals$min.n.breaks} and <= {globals$max.n.breaks}"
           )
      )
    )
    
    exp_plot <- pdf_exposure(tbl_exposure,
                             var_exp = var_to_use,
                             bins  = n.breaks +1,
                             digits = digits,
                             accuracy = accuracy
    )
    
    if(add_stats){
      exp_plot <- 
        exp_plot+
        geom_vline(aes(xintercept=median(.data[[var_to_use]], na.rm = TRUE),
                       color="Median exposure"), linetype="dashed",
                   size=1) +
        geom_vline(aes(xintercept=mean(.data[[var_to_use]], na.rm = TRUE),
                       color="Mean exposure"), linetype="dotted",
                   size=1) +
        geom_vline(aes(xintercept=ref_value,
                       color="Reference value"), linetype="dotted",
                   size=1) +
        scale_color_manual(name = "Statistics", values = c('Median exposure' = "blue", 
                                                           'Mean exposure' = "red"
                                                           , 'Reference value' = "black"
        )
        )
    }
    
    # Add the labs
    
    exp_plot +
      labs(
        title = title,
        x     = x_label,
        y     = y_label
      )+
      NULL
    
  })
  
  output$exposure_pdf <- renderPlot({
    
    exposure_pdf()
    
  })
  
  
  exposure_cdf <- reactive({
    
    ref_value <- isolate(rv$ref_value)
    
    scenario <- input$slct_scenario_exposure
    var_to_use <- paste0("subExp_",scenario)
    
    title <- glue::glue("Cummulative distribution of exposure at the {scenario} scenario")
    x_label <- rv$x_label
    y_label <- rv$y_label
    
    cdf_exposure(tbl_exposure,
                 var_exp = var_to_use,
                 ref_value = ref_value
    )+
      labs(
        title = title,
        x  = x_label,
        y = y_label
      )+
      NULL
    
  })
  
  output$exposure_cdf <- renderPlot({
    
    exposure_cdf()
  })
  
  
  exposure_pdfDemo <-reactive({
    
    # A Ridgline plot
    
    req(input$slct_scenario_exposureDemo,
        input$slct_demo,
        input$bandwidthDemo>0
    )
    
    
    ref_value  <- isolate(rv$ref_value)
    scenario   <- input$slct_scenario_exposureDemo
    
    var_to_use <- paste0("subExp_",scenario)
    var_group  <- input$slct_demo
    
    title      <- glue::glue("Density distribution of exposure at the {scenario} scenario")
    x_label    <- rv$x_label
    y_label    <- rv$y_label
    
    digits     <- input$digits_exposureDemo
    pct.digits <- input$pct.digits_exposureDemo
    bandwidth  <- input$bandwidthDemo
    
    exp_plot <- pdf_exposureDemo(tbl_exposure,
                                 var_exp = var_to_use,
                                 var_group =  var_group,
                                 bandwith  = bandwidth,
                                 scale = 1.1,
                                 ref_value= ref_value
    )
    
    # Add the labs
    
    exp_plot +
      labs(
        title = title,
        x     = "",
        y     = y_label
      )+
      NULL
    
  })
  
  
  output$exposure_pdfDemo <- renderPlot({
    
    exposure_pdfDemo()
  })
  
  exposure_cdfDemo <- reactive({
    
    ref_value <- isolate(rv$ref_value)
    
    scenario <- input$slct_scenario_exposureDemo
    var_to_use <- paste0("subExp_",scenario)
    var_group <- input$slct_demo
    
    title <- glue::glue("Cummulative distribution of exposure at the {scenario} scenario")
    x_label <- rv$x_label
    y_label <- rv$y_label
    
    cdf_exposure(tbl_exposure,
                 var_exp = var_to_use,
                 var_group = var_group,
                 ref_value = ref_value
    )+
      labs(
        title = title,
        x  = x_label,
        y = y_label
      )+
      NULL
    
  })
  
  output$exposure_cdfDemo <- renderPlot({
    
    exposure_cdfDemo()
  })
  
  
  
  # Consumption ####
  
  tbl_aggr_consumption <- reactive({
    
    req(input$slct_food_levelConsumption)
    
    level_var <- input$slct_food_levelConsumption
    
    #level_var <- fdx1_levels_cons[[food_level]]
    
    if(input$hide_water){
      consumption <- sample_consumption %>% 
        filter(consumed_food_at_level_1 != water_level1)
    } else {
      consumption <- sample_consumption
    }
    
    aggr_consumption_by_group(consumption, level_var)
    
    
  })
  
  output$tbl_aggr_consumption <- DT::renderDT({
    
    tbl_aggr_consumption() %>% 
      arrange(desc(consumer)) %>% 
      DT::datatable(
        
        style = "bootstrap"
        , rownames = FALSE
        , options = list(
          
          paging = TRUE,
          scrollX = TRUE, scrollY = "500px"
        )
      )
  })
  
  
  plot_aggr_consumption <- reactive({
    
    
    level_var <- input$slct_food_levelConsumption
    
    consumptionType <- input$slct_consumptionType
    
    # Graph peripherals
    title <- glue::glue("Consumption Per Day per Person (grams) by food group")
    
    if(consumptionType  == "Both"){
      subtitle =  "Top 20 food items"
    } else {
      subtitle <- glue::glue("Top 20 food items - {consumptionType} based\n")
      
    }
    
    # leave as is for consumers. to set the max y limit in plot.
    max_consumption <- max(tbl_aggr_consumption()$consumer)
    
    
    if(consumptionType  == "Both"){
      
      p <- 
        tbl_aggr_consumption() %>% 
        slice_max(n= 20, order_by = .data[["consumer"]]) %>% 
        tidyr::gather(key, value, consumer, population) %>% 
        ggplot(
          aes(
            x = forcats::fct_rev(forcats::fct_inorder(.data[[level_var]])), #, consumer), 
            y = value,
            fill = key
          )
        )+
        geom_col(width = 0.8, position = position_dodge())+
        geom_text(aes(label= round(value, 0))
                  ,hjust = -0.5, colour= "grey10",
                  position = position_dodge(width = 0.8)
        )+
        coord_flip(ylim = c(0, max_consumption*1.10))+
        labs(
          x = "",
          y  = "grams",
          title = title,
          subtitle = subtitle,
          fill = ""
        )+
        scale_y_continuous( expand = c(0,0.1))+
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, 50))+
        scale_fill_brewer(type = "qual", palette = 2, 
                          guide =  guide_legend(reverse = TRUE),
                          labels = c(consumer = "Consumer based", 
                                     population = "Population based"
                          )
        )+
        NULL
      
      
    } else {
      p <- 
        tbl_aggr_consumption() %>%
        slice_max(n = 20, order_by = .data[[consumptionType]]) %>%
        ggplot(
          aes(
            x = forcats::fct_rev(forcats::fct_inorder(.data[[level_var]])),
            y = .data[[consumptionType]]
          )
        )+
        geom_col(width = 0.5, fill = impro_colours[2])+
        geom_text(aes(label= round(.data[[consumptionType]], 0))
                  ,hjust = 1.1
                  , colour= "grey90"
        )+
        coord_flip(ylim = c(0, max_consumption*1.10))+
        labs(
          x = "",
          y  = "grams",
          title = title,
          subtitle = subtitle
        )+
        scale_y_continuous( expand = c(0,0.01))+
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, 50))+
        theme(
          plot.caption = element_text(face = "italic", size = 5)
        )
    }
    
    
    p
    
  })
  
  output$plot_aggr_consumption_UI <- renderUI({
    
    plotOutput({
      "plot_aggr_consumption"
    }
    , height = 800
    )
    
  })
  
  output$plot_aggr_consumption <- renderPlot({
    
    plot_aggr_consumption()
    
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
  
  
  
  tbl_aggr_contribution <- reactive({
    
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
  
  
  output$tbl_aggr_contribution <- reactable::renderReactable({
    
    tbl_aggr_contribution()
    
  })
  output$contr_tbl_title <- renderText({
    
    if(as.numeric(input$contr_filter)== 0) {
      title <- ""
    }else {
      title <- glue::glue("<h2>Food items with greater than {isolate(input$contr_filter)}% contribution\n</h2>")
    }
    title
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
      ggiraph::geom_col_interactive(width = 0.5, fill = impro_colours[2])+
      coord_flip(ylim = c(0, max_contr*1.15))+
      labs(
        x = top_level,
        y  = "Contribution",
        title = title,
        subtitle = subtitle
      )+
      scale_y_continuous(labels = scales::percent, expand = c(0,0.001))+
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, 30))+
      theme(
        axis.text = element_text(size = 7),
        #axis.title.y = element_text(hjust = 1, angle = 0 )
        plot.caption = element_text(face = "italic", size = 5),
        plot.title.position = "plot"
      )
    
  })
  
  
  # I dont output in the UI the below outtput binding.
  # I  use renderUI (see below) 
  # a)  to use the height argument from within the server
  # b) using UI, the height of the tabBox expands as the graphs grows
  
  output$contr_graph <- ggiraph::renderGirafe({
    
    ggiraph::girafe(ggobj =contr_graph(),width_svg = 6 )
    
  })
  
  output$contr_UI <- renderUI({
    
    ggiraph::girafeOutput({
      "contr_graph"
    },height = paste0(input$contr_height, "px")
    )
    
  })
  
  
  # Drill Down ####
  
  # https://rstudio.github.io/sortable/articles/novel_solutions.html
  
  vars_cross <- reactive({
    
    input$drill_varsExplore
    
  })
  
  
  tbl_cross_demoExposure <- reactive({
    
    
    req(length(vars_cross()) == 2)
    
    vars_cross <- vars_cross()
    
    x <- vars_cross[[1]]
    y <- vars_cross[[2]]
    
    
    tbl_exposure %>% 
      group_by(
        .data[[x]], .data[[y]]
      ) %>% 
      summarise(
        exposure    = mean(subExp_MB)
      ) 
    
  })
  
  
  output$tbl_cross_demoExposure <- renderTable({
    
    tbl_cross_demoExposure() %>% 
      tidyr::spread(.data[[vars_cross()[[2]]]], exposure)
    
  }, caption = as.character(h4("Mean exposure (μg/Kg body weight)"))
  , caption.placement = "top"
  )
  
  plot_cross_demoExposure <- reactive({
    
    
    req(length(vars_cross()) == 2)
    
    vars_cross <- vars_cross()
    
    x <- vars_cross[[1]]
    y <- vars_cross[[2]]
    
    tbl_cross_demoExposure() %>%
      ggplot(aes(x=.data[[x]], y=exposure, fill = .data[[y]]))+
      geom_col(width=0.5, position = position_dodge(width = 0.6))+
      geom_text(aes(label = round(exposure, 3))
                , position = position_dodge(width = 0.6)
                , hjust = 1.1
      )+
      coord_flip()+
      scale_fill_brewer(type = "qual", guide = guide_legend(reverse = TRUE))+
      labs(
        y  =  rv$x_label,
        x= "",
        fill = "",
        title = glue::glue("Mean exposure across {x} and {y}")
      )
    
    
  })
  
  output$plot_cross_demoExposure <- renderPlot({
    
    not_good = length(vars_cross()) < 2
    
    if(not_good){
      validate(message = "Drag 2 variables from the 'List of demographics' to the 'Cross Demo' box")
    }
    
    plot_cross_demoExposure()
  })
  
  # Show the download button once the grpah is created.
  output$downBtn_crossDemoPlot <- renderUI({
    
    req(plot_cross_demoExposure())
    
    mod_downloadPlot_ui("plot_cross_demoExposure")
    
  })
  
  output$downBtn_crossDemoTable <- renderUI({
    
    req(tbl_cross_demoExposure())
    
    mod_downloadTable_ui("tbl_cross_demoExposure")
    
  })
  
  
  
  
  # Observers ####
  
  
  observeEvent({input$slct_demo
    input$slct_scenario_exposureDemo}
    , 
    {
      # Change title
      
      rv$title_statsDemo <- 
        paste0("Exposure estimates by ", 
               rv$demo[input$slct_demo],
               " at the ",
               input$slct_scenario_exposureDemo,
               " scenario"
        )
      
      # Estimate bandwidth and range
      
      bw <- calc_bandwidth(tbl_exposure,
                           target  = paste0("subExp_", input$slct_scenario_exposureDemo),
                           group   = input$slct_demo
      )
      
      updateSliderInput(session,"bandwidthDemo", 
                        value  = bw[["mean"]],
                        min = round(bw[["low"]], 3),
                        max = round(bw[["high"]], 3)
      )
      
    }
  )
  
  # output$title_statsDemo <- renderText(
  #   rv$title_statsDemo
  # )
  # 
  
  
  #Sample Data ####
  
  output$sample_data <- DT::renderDataTable({
    
    
    vars_numeric <- 
      sample_consumption %>% select_if(is.numeric) %>% 
      select(contains("exp")) %>% names()
    
    vars_numeric_ind <- match(vars_numeric, names(sample_consumption))
    
    sample_consumption %>% 
      #head(200) %>% 
      DT::datatable(
        caption = "The final table"
        , style = "bootstrap"
        , rownames = FALSE
        , options = list(
          
          paging = TRUE,
          scrollX = TRUE, scrollY = "500px"
        )
        , filter = "top"
      ) %>% 
      DT::formatRound(columns =vars_numeric_ind, digits = 3 )
    
    
  })
  
  occurrence_l2 <- reactive({
    occurrence_example_l2
    
  })
    
    
  output$occurrence_l2 <- DT::renderDataTable({
    
    occurrence_l2()
    
  })
  
  occurrence_l3 <- reactive({
    
    occurrence_example_l3
    
  })
  
  output$occurrence_l3 <- DT::renderDataTable({
    
    occurrence_l3()
    
  })
  
  dataset_info <- reactive({
    
    data.frame(
      
      row.names = c("Consumption", 
                    "Occurence"
      ),
      # dataset = c("Subjects_Consumption_EUMENU Lot2 (N=803).xlsx",
      #             "Occurrence - Mercury (Hg) - DK.xlsx"
      # )
      
      dataset = c("Subjects_Consumption_ex.3 (N=300, Days=3) (CYP Weights).xlsx",
                  "Occurrence Example-EFSA-Pb.xlsm"
      )
      
    )
    
    
  }
  )
  
  output$dataset_info <- renderTable({
    dataset_info()
  }
  , rownames = TRUE
  , caption = ""
  )
  
  
  info_improrisk <- renderUI({
    
    info_improrisk
    
  })
  
  
  output$stats_label <- renderUI({
    
    s_size <- rv$sample_size
    p_size <- rv$pop_size
    
    #glue::glue('Sample size:{s_size}\nPopulation size: {prettyNum(p_size, big.mark = ",")}')
    
      p('Sample size:', s_size, 
             br(), 
             'Population size: ',prettyNum(p_size, big.mark = ",")
    )
    
  })
  
  output$tbl_foodex1 <- DT::renderDataTable({
    
    foodex.1
    
  }, filter = "top")
  
  
  temp_tbl_exposure <- reactive({
    
    
      tbl_exposure %>% 
      relocate(
        subjectid,gender, area, pop_class, age,weight, wcoeff, cons_days, everything() 
      ) %>% 
      #mutate_at(all_of(vars_exposure), ~round(., 3)) %>% 
      {.}
    
  })
  
  output$tbl_exposure <- reactable::renderReactable({
    
    digits <- 3 #input$contr_digitsExp
    
    
    temp_tbl_exposure() %>% 
      rename(!!!var_names) %>% 
      reactable::reactable(
        filterable = TRUE,
        bordered = TRUE,
        resizable = TRUE,
        columns = list(
          
          nday_lb = colDef("LB", format = colFormat(digits = digits)),
          nday_mb = colDef("MB", format = colFormat(digits = digits)),
          nday_ub = colDef("UB", format = colFormat(digits = digits)),
          
          subExp_LB = colDef("LB", format = colFormat(digits = digits)),
          subExp_MB = colDef("MB", format = colFormat(digits = digits)),
          subExp_UB = colDef("UB", format = colFormat(digits = digits))
          
        ),
        columnGroups = list(
          colGroup(name = "N_day", columns = c("nday_lb", "nday_mb", "nday_ub")),
          colGroup(name = rv$exposure_frequency, columns = c("subExp_LB", "subExp_MB", "subExp_UB"))
          
        )
        
      ) 
    
  })
  
  # Inntroductions ####
  
  steps <- reactive(
    
    switch(input$tabs,
           
           "exposure" = intro_exposure,
           "exposureDemo" = intro_exposureDemo
           )
    
    )
  
  observeEvent(input$help_exposure,{
    
    rintrojs::introjs(session,options = list(steps=steps()))
    
  })
  
  
}
