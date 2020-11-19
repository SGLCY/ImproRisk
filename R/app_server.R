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
  #callModule(mod_showSubstanceInfo_server, "showSubstanceInfo_ui_1")
  #callModule(mod_showSubstanceInfo_server, "showSubstanceInfo_ui_2")
  
  
  # Module Download Plots ####
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
  
  callModule(mod_downloadTable_server, "tbl_exposure",
             table_name = "tbl_exposure",
             the_table = tbl_exposure)
  
  
  callModule(mod_downloadTable_server, "foodex1",
             table_name = "foodex1",
             the_table = foodex1)
  
  
  callModule(mod_downloadTable_server, "tbl_merged",
             table_name = "tbl_merged",
             the_table = tbl_merged)
  
  
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
  
  
  # Reactive Values ---------------------------------------------------------
  
  
  rv <- rv(
    
    scenario = NULL,
    title = NULL,
    title_statsDemo =  NULL,
    
    # initialise with the sample_ files
    exposure_factor    = if(sample_substance_info$values[5]=="DAILY") {1} else {7},
    exposure_frequency = sample_substance_info$values[5],
    ref_value          = as.numeric(sample_substance_info$values[3]),
    sample_size        = nrow(sample_tbl_subjects) , 
    pop_size           = sum(sample_tbl_subjects$wcoeff),
    
    
    demo               = c("gender" = "Gender", "area" ="Area", "pop_class" = "Population_Class"),
    
    show_level1 = NULL, #for the contribution tables
    
    x_label = "μg/Kg body weight", # somtime I will give the option to the user to change it
    y_label = "Population", # this might be `sample`? in case of non weighted
    
    exp_label = "μg/Kg body weight",
    substance_info = sample_substance_info
  )
  
  
  datasets <- rv(
    
    tbl_consumption = sample_tbl_consumption,
    tbl_subjects    = sample_tbl_subjects,
    tbl_merged      = sample_tbl_merged,
    tbl_exposure    = sample_tbl_exposure,
    
    occurrence_l2   = sample_occurrence_l2,
    occurrence_l3   = sample_occurrence_l3,
    
    #dataset_info    =  sample_dataset_info,
    consumption_file  = sample_dataset_info$dataset[1],
    occurrence_file   = sample_dataset_info$dataset[2]
  )
  
  consumption_file_info <- rv(
    
    name     = NULL,
    size     = NULL,
    type     = NULL,
    datapath = NULL
    
  )
  
  occurrence_file_info <- rv(
    
    name     = NULL,
    size     = NULL,
    type     = NULL,
    datapath = NULL,
    
    sheets_names = NULL
    
    
  )
  
  
  valid_consumption_file <- reactiveVal(FALSE, "valid_consumption")
  valid_occurrence_file  <- reactiveVal(FALSE, "valid_occurrence")
  
  
  # Exposure Statistics ####
  
  tbl_exposure_stats <- reactive({
    
    req(input$digits_exposure)
    
    digits  <- input$digits_exposure
    
    tbl_exposure() %>% 
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
  
  output$stats_label <- renderUI({
    
    s_size <- rv$sample_size
    p_size <- rv$pop_size
    
    p('Sample size:', s_size, 
      br(), 
      'Population size: ',prettyNum(p_size, big.mark = ",")
    )
    
  })
  
  tbl_exposure_statsDemo <- reactive({
    
    
    req(input$slct_scenario_exposureDemo,
        input$slct_demo
    )
    
    pct.digits <- input$pct.digits_exposureDemo
    
    tbl_exposure() %>% 
      tidyr::pivot_longer(
        cols = starts_with("subExp_"),
        names_to = "scenario",
        values_to = "exposure"
      ) %>% 
      dplyr::group_by(scenario,.data[[input$slct_demo]]) %>% 
      summarise_weighted(ref_value = rv$ref_value) %>% 
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
  
  
  output$substance_info <- renderTable({substance_info()}, rownames = TRUE, colnames = FALSE,  width = "50px")
  
  # output$substance_info <- DT::renderDataTable({
  #   substance_info() %>% 
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
    
    output[[paste0("subInfo_", tab_name)]] <- renderTable({substance_info()}, rownames = TRUE, colnames = FALSE)      
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
    
    ref_value  <- rv$ref_value%||%NA_real_  #NULL ref_value brakes down the plot. Thanks @_ColinFay
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
    
    exp_plot <- pdf_exposure(tbl_exposure(),
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
    exp_plot <- 
      exp_plot +
      labs(
        title = title,
        x     = x_label,
        y     = y_label
      )+
      NULL
    
    
    catch_plotError(exp_plot)
    
    exp_plot
    
  })
  
  output$exposure_pdf <- renderPlot({
    
    exposure_pdf()
    
  })
  
  
  exposure_cdf <- reactive({
    
    ref_value <- rv$ref_value
    
    scenario <- input$slct_scenario_exposure
    var_to_use <- paste0("subExp_",scenario)
    
    title <- glue::glue("Cummulative distribution of exposure at the {scenario} scenario")
    x_label <- rv$x_label
    y_label <- rv$y_label
    
    cdf_plot <- 
      cdf_exposure(tbl_exposure(),
                   var_exp = var_to_use,
                   ref_value = ref_value
      )+
      labs(
        title = title,
        x  = x_label,
        y = y_label
      )+
      NULL
    
    catch_plotError(cdf_plot)
    
    cdf_plot
  })
  
  output$exposure_cdf <- renderPlot({
    
    exposure_cdf()
  })
  
  
  exposure_pdfDemo <-reactive({
    
    # A Ridgline plot
    req(input$slct_scenario_exposureDemo,
        input$slct_demo,
       # input$bandwidthDemo>0
    )
    
    #NULL ref_value brakes down the plot in aes(xintercept= NULL). 
    # see  the %||% function in golem_utils_server.R  Thanks @_ColinFay
    ref_value  <- rv$ref_value%||%NA_real_  
    scenario   <- input$slct_scenario_exposureDemo
    
    var_to_use <- paste0("subExp_",scenario)
    var_group  <- input$slct_demo
    
    title      <- glue::glue("Density distribution of exposure at the {scenario} scenario")
    x_label    <- rv$x_label
    y_label    <- rv$y_label
    
    #digits     <- input$digits_exposureDemo
    #pct.digits <- input$pct.digits_exposureDemo
    
    #bandwidth  <- input$bandwidthDemo
    
    if(sum(tbl_exposure()[[var_to_use]])==0 ){
      
      validate(glue("No exposure at the {scenario} scenario. Unable to create the plot"))
      
    }
    exp_plot <- pdf_exposureDemo(tbl_exposure(),
                                 var_exp = var_to_use,
                                 var_group =  var_group,
                                 #bandwith  = bandwidth,
                                 scale = 1.1,
                                 ref_value= ref_value
    )

        
    # Add the labs
    exp_plot <- 
      exp_plot +
      labs(
        title = title,
        x     = "",
        y     = y_label
      )+
      NULL
    
    catch_plotError(exp_plot)
    
    exp_plot
    
  })
  
  
  output$exposure_pdfDemo <- renderPlot({
    
    exposure_pdfDemo()
  })
  
  exposure_cdfDemo <- reactive({
    
    ref_value <- rv$ref_value
    
    scenario <- input$slct_scenario_exposureDemo
    var_to_use <- paste0("subExp_",scenario)
    var_group <- input$slct_demo
    
    title <- glue::glue("Cummulative distribution of exposure at the {scenario} scenario")
    x_label <- rv$x_label
    y_label <- rv$y_label
    
    if(sum(tbl_exposure()[[var_to_use]])==0 ){
      
      validate(glue("No exposure at the {scenario} scenario. Unable to create the plot"))
      
    }
    
    cdf_exposure(tbl_exposure(),
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
    
    fdx1_filters <- input$slct_fdx_level1
    
    #if(input$hide_water){
      consumption <- 
        #sample_consumption %>% 
        tbl_merged() %>% 
        #filter(foodex_l1_desc != water_level1) %>% 
        filter(foodex_l1_desc %in% fdx1_filters)
    #} else {
     # consumption <- 
        #sample_consumption
     #   tbl_merged()
   # }
    if(nrow(consumption) == 0) {
      validate("Select at least one FoodEx level 1")}
      
    aggr_consumption_by_group(consumption, tbl_subjects(), level_var)
    
    
  })
  
  output$tbl_aggr_consumption <- reactable::renderReactable({
    
    var <- input$slct_food_levelConsumption
    
    var_name <- fdx_names[[var]]
    
    tbl_aggr_consumption() %>% 
      rename({{var_name}} := {{var}}) %>% 
      reactable::reactable(
        filterable = TRUE,
        bordered = TRUE,
        resizable = TRUE,
        defaultPageSize = 20,
        
        columns = list(
          
          N_sample = colDef(show = FALSE),
          pct_sample = colDef(show = FALSE),
          N_pop = colDef(show =FALSE),
          pct_pop = colDef(show  = FALSE),
          consumer_based = colDef("Consumer based",align = "center", format = colFormat(digits = 1)),
          population_based = colDef("Population based",align = "center", format = colFormat(digits = 1)),
          
          Sample = colDef("Sample",align = "center"),
          Population = colDef("Population", align = "center")
          
        ),
        columnGroups = list(
          
          colGroup(name = "Consumers [N(%)]", align = "center", html = FALSE, columns = c("Sample", "Population")),
          colGroup(name = "Consumption (grams)", align = "center", html = FALSE, columns = c("consumer_based", "population_based"))
          
        )
        
      )
    
  })
  
  
  observe({
    if(input$consumptionTabBox == "Table"){
      shinyjs::hide("slct_consumptionType")
    } else {
      shinyjs::show("slct_consumptionType")
    }
    
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
    max_consumption <- max(tbl_aggr_consumption()$consumer_based)
    
    
    if(consumptionType  == "Both"){
      
      p <- 
        tbl_aggr_consumption() %>% 
        slice_max(n= 20, order_by = .data[["consumer_based"]]) %>% 
        tidyr::gather(key, value, consumer_based, population_based) %>% 
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
                          labels = c(consumer_based = "Consumer based", 
                                     population_based = "Population based"
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
    
    catch_plotError(p)
    
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
    
    food_level <- fdx1_levels_contribution[[input$slct_level]]
    
    vars_rename <- 
      paste0("wcoeff_adjusted_refined_exposure_", tolower(scenarios)) %>% 
      purrr::set_names(scenarios)
    
    temp <- 
      tbl_merged() %>% 
      #rename_with(~scenarios, .cols = contains("refined_exposure")) %>%
      rename(vars_rename) %>% 
      group_by(
        across(all_of(food_level))
      ) %>% 
      summarise(
        across(.cols= all_of(scenarios), .fns = ~sum(., na.rm = TRUE))
      ) %>% 
      ungroup() %>% 
      # full_join(
      #   tbl_foodex_desc %>% 
      #     distinct(
      #       across(all_of(food_level))
      #     )
      # ) %>% 
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
   
    
    if("foodex_l3_desc_aggr" %in% food_level){
      tbl_contribution <- rename(tbl_contribution, foodex_l3_desc = foodex_l3_desc_aggr)
    }
    
    tbl_contribution
    
  })
  
  
  contribution_filtered <- reactive({
    
    req(input$slct_scenario_contribution)
    
    tbl_contribution() %>% 
      filter(
        scenario == input$slct_scenario_contribution,
        #contribution >= as.numeric(input$contr_filter)/100
        contribution > input$contr_filter/100
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
    
    
    within_title <- switch (input$slct_level,
                            "Level 2" = "Contribution within Level 1",
                            "Level 3" = "Contribution within Level 2",
                            NULL
    )
    
    table_title <- glue::glue("Food items with greater than {isolate(input$contr_filter)}% contribution\n")
    
    tbl <- 
      contribution_filtered() %>% 
        reactable::reactable(
        # in the Level 1 case
        groupBy = nth(food_level, -2, default = food_level),
        columns = list(
          scenario = colDef(show = FALSE),
          exposure = colDef(name = "Total exposure", 
                            aggregate = "sum",
                            format = colFormat(digits = input$contr_digitsExp),
                            filterable = FALSE,
                            show = FALSE
          ),
          contribution = colDef(name = "Contribution to Total exposure",aggregate = "sum",
                                format = colFormat(percent = TRUE, digits = input$contr_digitsPct),
                                filterable = FALSE,
                                defaultSortOrder = "desc"
                                
          ),
          contr_within = colDef(name = within_title
                                ,show = rv$show_contr
                                ,aggregate = "sum",
                                filterable = FALSE,
                                format = colFormat(percent = TRUE, digits =  input$contr_digitsPct)
          ),
          foodex_l1_desc = colDef(name = "Level 1", show = rv$show_level1)
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
    
    # # add tittle
    # not yet possible in shiny 02/11/2020
    # htmlwidgets::prependContent(tbl,
    #                             h2(class = "title", 
    #                                table_title
    #                                )
    #                             )
    
  })
  
  
  output$tbl_aggr_contribution <- reactable::renderReactable({
    
    tbl_aggr_contribution()
    
  })
  
  output$contr_tbl_title <- renderUI({
    
    pct <- input$contr_filter
    
    tagList(
      
      HTML("<h3>Contribution to the total exposure</h3>"),
      
      HTML(as.character(glue("<h4>Food items with greater than {pct}% contribution</h4>")))
      
    )
    # title <-
    # HTML(as.character(title))
  })
  
  
  contr_graph <- reactive({
    
    top_level <- dplyr::nth(fdx1_levels[[input$slct_level]], -1)
    second_top_level <- dplyr::nth(fdx1_levels[[input$slct_level]], -2, default = "foodex_l1_desc")
    
    
    # Graph peripherals
    title <- glue::glue("Contribution to the Total Exposure ({isolate(input$slct_scenario_contribution)})")
    
    if(isolate(as.numeric(input$contr_filter))== 0) {
      subtitle = ""
    }else {
      subtitle <- glue::glue("Food items with greater than {isolate(input$contr_filter)}% contribution\n")
    }
    
    max_contr <- max(contribution_filtered()$contribution)
    
    values_order <- input$values_order
    
    # The plot
    
    contribution_filtered() %>%
      ggplot(
        aes(
          x = fct_reorder(.data[[top_level]], contribution, .desc=values_order), 
          y = contribution,
          tooltip = percent(contribution,  accuracy = 0.1),
          data_id = .data[[second_top_level]] 
        )
      )+
      ggiraph::geom_col_interactive(width = 0.5, fill = impro_colours[2])+
      coord_flip(ylim = c(0, max_contr*1.15))+
      labs(
        x = "Food Group", # top_level,
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
    
    ggiraph::girafe(ggobj =contr_graph()#,width_svg = 6
                    )
    
  })
  
  output$contr_UI <- renderUI({
    
    ht <- paste0(input$contr_height, "px")
    ggiraph::girafeOutput({
      "contr_graph"
    },height = ht
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
    
    scenario <- input$slct_scenario_drillDown
    
    var_to_use <- paste0("subExp_", scenario)
    
    tbl_exposure() %>% 
      group_by(
        .data[[x]], .data[[y]]
      ) %>% 
      summarise(
        exposure    = mean(!!sym(var_to_use), na.rm = TRUE)
      ) 
    
  })
  
  output$tbl_cross_demoExposure <- renderTable({
    
    tbl_cross_demoExposure() %>% 
      tidyr::spread(.data[[vars_cross()[[2]]]], exposure)
    
  }
  , caption = as.character(h4("Mean exposure (μg/Kg body weight)"))
  , caption.placement = "top"
  )
  
  plot_cross_demoExposure <- reactive({
    
    
    req(length(vars_cross()) == 2)
    
    vars_cross <- vars_cross()
    
    x <- vars_cross[[1]]
    y <- vars_cross[[2]]
    
    scenario <- input$slct_scenario_drillDown
    
    plot_cross <- 
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
        title = glue::glue("Mean exposure at {scenario} scenario across {x} and {y}")
      )
    
    
    catch_plotError(plot_cross)
    
    plot_cross
    
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
  
  
  # Info ####
  
  info_improrisk <- renderUI({info_improrisk})
  
  
  # Observers ####
  
  observeEvent({input$slct_demo
    input$slct_scenario_exposureDemo}
    , 
    {
      # Change title
      
      rv$title_statsDemo <- 
        paste0("Exposure statistics by ", 
               rv$demo[input$slct_demo],
               " at the ",
               input$slct_scenario_exposureDemo,
               " scenario"
        )
      
      # Estimate bandwidth and range
      
      # bw <- calc_bandwidth(tbl_exposure(),
      #                      target  = paste0("subExp_", input$slct_scenario_exposureDemo),
      #                      group   = input$slct_demo
      # )
      
      # updateSliderInput(session,"bandwidthDemo", 
      #                   value  = bw[["mean"]],
      #                   min = round(bw[["low"]], 3),
      #                   max = round(bw[["high"]], 3)
      # )
      
    }, ignoreInit = FALSE
  )
  
  
  
  
  #  DATA ####
  
  tbl_consumption <- reactive({
    
    datasets$tbl_consumption
    
  })
  
  tbl_subjects <- reactive({
    
    datasets$tbl_subjects
    
  })
  
  observeEvent(tbl_subjects(),{
    
    rv$sample_size <- nrow(tbl_subjects())
    
    rv$pop_size <- sum(tbl_subjects()$wcoeff)
    
  })
  
  
  tbl_merged <- reactive({
    
    
    datasets$tbl_merged
    
  })
  
  tbl_exposure <- reactive({
    
    datasets$tbl_exposure %>% 
      relocate(vars_order_1)
  })
  
  
  substance_info <- reactive({
    
    rv$substance_info
  })
  
  
  output$tbl_merged <- DT::renderDataTable({
    
    
    vars_numeric <- 
      tbl_merged() %>% 
      select_if(is.numeric) %>% 
      select(contains("exp")) %>% 
      names()
    
    vars_numeric_ind <- match(vars_numeric, names(tbl_merged()))
    
    tbl_merged() %>% 
      mutate(
        across(c(serial, subjectid, day), as.factor)
      ) %>% 
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
    
    datasets$occurrence_l2
    
  })
  
  
  output$occurrence_l2 <- DT::renderDataTable({
    
    occurrence_l2()
    
  })
  
  occurrence_l3 <- reactive({
    
    datasets$occurrence_l3
    
  })
  
  output$occurrence_l3 <- DT::renderDataTable({
    
    occurrence_l3()
    
  })
  
  dataset_info <- reactive({
    
    data.frame(
      stringsAsFactors = FALSE,
      row.names = c("Consumption",
                    "Occurence"),
      dataset = c(datasets$consumption_file,
                  datasets$occurrence_file)
      
    )
    
  })
  
  
  output$dataset_info <- renderTable({
    dataset_info()
  }
  , rownames = TRUE
  , caption = ""
  )
  
  
  output$tbl_foodex1 <- DT::renderDataTable({
    
    foodex.1
    
  }, filter = "top")
  
  
  output$tbl_exposure <- reactable::renderReactable({
    
    digits <- input$individual_digitsExp
    
    nlab <- glue("<p>N_day total exposure<br>({rv$exp_label})</br></p>")
    dlab <- glue("<p>{rv$exposure_frequency} mean exposure<br>({rv$exp_label})</br></p>")
    
    tbl_exposure() %>% 
      #temp_tbl_exposure() %>% 
      rename(!!!var_names) %>% 
      reactable::reactable(
        filterable = TRUE,
        bordered = TRUE,
        resizable = TRUE,
        defaultPageSize = 20,
        #height = 650,
        columns = list(
          
          nday_lb = colDef("LB", format = colFormat(digits = digits)),
          nday_mb = colDef("MB", format = colFormat(digits = digits)),
          nday_ub = colDef("UB", format = colFormat(digits = digits)),
          
          subExp_LB = colDef("LB", format = colFormat(digits = digits)),
          subExp_MB = colDef("MB", format = colFormat(digits = digits)),
          subExp_UB = colDef("UB", format = colFormat(digits = digits))
          
        ),
        columnGroups = list(
          colGroup(name = nlab, html = TRUE, columns = c("nday_lb", "nday_mb", "nday_ub")),
          colGroup(name = dlab, html = TRUE, columns = c("subExp_LB", "subExp_MB", "subExp_UB"))
          
        )
        
      ) 
    
  })
  
  #temp for download using the module
  foodex1 <- reactive({
    foodex.1
  })
  
  
  # Update DATA -------------------------------------------------------------
  
  
  #> Consumption ####
  # Capture the details of the consumption file
  observeEvent(input$consumption_file,{
    
    consumption_file_info$name = input$consumption_file$name
    consumption_file_info$size = input$consumption_file$size
    #consumption_file_info$type = input$consumption_file$type
    consumption_file_info$type =  tools::file_ext(input$consumption_file$name)
    consumption_file_info$datapath = input$consumption_file$datapath
    
    datasets$consumption_file = consumption_file_info$name
    
    
  })
  
  # temp_tbl_consumption <- reactive({
  #   
  #   req(input$consumption_file)
  #   
  #   data <- 
  #   load_consumption(consumption_file_info$name, 
  #                    consumption_file_info$datapath) 
  #   
  #   # Perform checks
  #   check_varsConsumption(data)
  #   check_fdx1_coding(data)
  #   check_fewRows(data)
  #   
  #   #if ll ok, then enforce column class and return
  #   data %>% 
  #     mutate(
  #       across(all_of(vars_numeric_consumption), as.numeric),
  #       across(all_of(vars_character_consumption), as.character)
  #     )
  #   
  # })
  
  
  cons_progress_UI <- reactive({
    
    validate(
      need(input$consumption_file, "Import the consumption file in .xlsx format")
    )
    
    if(!consumption_file_info$type == "xlsx") {
      
      error_notExcel() 
      validate("Please import an .xlsx file")
    }
    
    # checks
    Consumption <- 
      load_consumption(consumption_file_info$name, 
                       consumption_file_info$datapath) 
    
    # Perform checks
    check_varsConsumption(Consumption)
    check_fdx1_coding(Consumption)
    check_fewRows(Consumption)
    
    #if ll ok, then enforce column class and return
    Consumption <- 
      Consumption %>% 
      mutate(
        across(all_of(vars_numeric_consumption), as.numeric),
        across(all_of(vars_character_consumption), as.character)
      )
    
    datasets$tbl_consumption <- Consumption
    
    # After I update the file
    #since the following triggers calculation
    valid_consumption_file(TRUE)
    
    tagList(
      
      tags$h3("Your Consumption data have been checked and succesfully uploaded",style= "colour: '#3CB371'")
    )
    
  })
  
  
  
  output$cons_progress_UI <- renderUI({
    
    cons_progress_UI()
    
  })
  
  
  observe({
    
    if(valid_consumption_file() == TRUE){
      
      # Consumption changes all datasets
      # observeEvent will execute the following witihin an isolatescope
      
      datasets$tbl_subjects     = create_tbl_subjects(datasets$tbl_consumption)
      
      datasets$tbl_merged       = create_tbl_merged(datasets$tbl_consumption,
                                                    datasets$occurrence_l2,
                                                    datasets$occurrence_l3
      )
      
      datasets$tbl_exposure = create_tbl_exposure(datasets$tbl_merged,
                                                  datasets$tbl_subjects,
                                                  rv$exposure_factor
      )
      
      showModal(modalDialog(
        title = "Datasets are updated",
        "Consumption data is replaced and calculations are done!"
      ))
      
      valid_consumption_file(FALSE)
      shinyjs::hide("accept_consumption")
      
    }
    
    
  })
  
  
  
  # #> Occurrence -----------------------------------------------------------
  
  
  observeEvent(input$occurrence_file,{
    
    occurrence_file_info$name = input$occurrence_file$name
    occurrence_file_info$size = input$occurrence_file$size
    #consumption_file_info$type = input$consumption_file$type
    occurrence_file_info$type =  tools::file_ext(input$occurrence_file$name)
    occurrence_file_info$datapath = input$occurrence_file$datapath
    
    datasets$occurrence_file = occurrence_file_info$name
    
    
  })
  
  
  occur_progress_UI <- reactive({
    
    validate(
      need(input$occurrence_file, "Import the occurrence file")
    )
    
    # Is it an Excel file?
    file_type <- tools::file_ext(input$occurrence_file$name)
    
    if(!file_type == "xlsx") {
      
      error_notExcel()
      validate("Please import an .xlsx file")
    }
    
    
    # Correct sheets?
    check_sheets_occur(input$occurrence_file$datapath)
    
    
    # OK, read and perform checks inside the sheet
    path  = input$occurrence_file$datapath
    
    Level2 <- 
      load_occurrence(path, sheet = "Level2")
    
    Level3 <- 
      load_occurrence(path, sheet = "Level3")
    
    substance_info <- 
      readxl::read_xlsx(path,
                        range = range_subInfo,
                        col_names = FALSE,
                        sheet = "Level2") %>% 
      purrr::set_names(c("row.name","value"))
    
    # checks
    
    check_fdx1_descr(Level2, "level2")
    
    check_fdx1_descr(Level3, "level3")
    
    # not really needed beacuse I use the level 3 fro mappings, but lets
    check_fdx1_descr(Level3, "level2")
    
    #ALL OK (lets say..)
    #show_success_alert("Occurence data are all set")
    
    # Update the occurence relevant files
    datasets$occurrence_l2 <- Level2
    datasets$occurrence_l3 <- Level3
    rv$substance_info      <- substance_info
    
    #  after i Update the files!!!
    # because the below trigger the calculations
    valid_occurrence_file(TRUE)
    
    tagList(
      
      tags$h3("Your Occurrence data have been checked and succesfully uploaded",style= "colour: '#3CB371'")
    )
    
    
  })
  
  # output$occur_progress_UI <- renderUI({
  #   
  #   occur_progress_UI()
  #   
  # })
  
  
  observe({
    
    if(valid_occurrence_file()  == TRUE){
      
      output$occur_progress_UI <- renderUI({
        
        #occur_progress_UI()
        NULL
      })
      
    } else {
      
      output$occur_progress_UI <- renderUI({
        
        occur_progress_UI()
        
      })
      
    }
  },priority = 1)
  
  
  output$imported_occur_l2 <- DT::renderDT({
    
    req(temp_occurrence_l2())
    
    out <- temp_occurrence_l2() %>%
      DT::datatable(
        caption = "Your uploaded occurrence Level 2 table",
        style = "bootstrap"
        , rownames = FALSE
        , options = list(
          #paging = TRUE,
          scrollX = TRUE, scrollY = "500px"
        )
      )
    
    shinyjs::show("accept_occurrence",anim = TRUE, animType = "fade")
    
    out
  })
  
  output$imported_occur_l3 <- DT::renderDT({
    
    req(temp_occurrence_l3())
    
    out <- temp_occurrence_l3() %>%
      DT::datatable(
        caption = "Your uploaded occurrence Level 3 table",
        style = "bootstrap"
        , rownames = FALSE
        , options = list(
          #paging = TRUE,
          scrollX = TRUE, scrollY = "500px"
        )
      )
    
    #shinyjs::show("accept_occurrence",anim = TRUE, animType = "fade")
    
    out
  })
  
  
  output$temp_substance_info <- renderTable({
    
    validate(
      need(input$occurrence_file, "Import the occurrence file"),
      need(occurrence_file_info$type == "xlsx", "Please import an .xlsx file")
    )
    temp_substance_info()
    
  })
  
  
  observe ({
    
    if(valid_occurrence_file()) {
      
      # firt the substance_info  to get the exposure_factor
      
      freq <-
        rv$substance_info %>%
        filter(row.name =="Type") %>% pull(value)
      
      if(freq ==  "DAILY") {
        rv$exposure_factor = 1
        
        rv$exposure_frequency = "DAILY"
      } else{
        
        rv$exposure_factor = 7
        rv$exposure_frequency = "WEEKLY"
        
      }
      
      # I need it as NULL to avoid plotting if no value is given in the occurrence import
      ref <- rv$substance_info$value[3]
      rv$ref_value <- if(not_na(ref)) as.numeric(ref) else NULL
      
      rv$substance_info <-
        data.frame(
          stringsAsFactors = FALSE,
          check.names = FALSE,
          
          row.names = c("Chemical Substance",
                        "Substance Category",
                        "Reference value (μg/Kg b.w.)",
                        "Type of Reference value",
                        "Frequency"),
          values = rv$substance_info$value
        )
      
      # Occurrence changes the following datasets
      datasets$tbl_merged    = create_tbl_merged(datasets$tbl_consumption,
                                                 datasets$occurrence_l2,
                                                 datasets$occurrence_l3
      )
      
      datasets$tbl_exposure = create_tbl_exposure(datasets$tbl_merged,
                                                  datasets$tbl_subjects,
                                                  rv$exposure_factor
      ) 
      
      
      showModal(modalDialog(
        title = "Occurrence datasets are updated",
        "Occurrence data is replaced and calculations are done!"
      ))
      
      valid_occurrence_file(FALSE)
      
      #shinyjs::hide("accept_occurrence")
    }
  }#, ignoreInit = TRUE
  , priority = 0)
  
  
  # Introductions ####
  
  steps <- reactive(
    
    switch(input$tabs,
           
           "exposure" = intro_exposure,
           "exposureDemo" = intro_exposureDemo,
           
           "contribution" = intro_contribution,
           "individual" = intro_individual,
           
           "consumption" = intro_consumption
    )
    
  )
  
  observeEvent(input$help_exposure,{
    
    rintrojs::introjs(session,options = list(steps=steps()))
    
  })
  
  
  # OLD CODE ####
  
  # temp_occurrence_l2 <- reactive({
  #   
  #   req(input$occurrence_file)
  #   
  #   path   = occurrence_file_info$datapath
  #   
  #   data <- 
  #   load_occurrence(path, sheet = "Level2")
  #   
  #   
  #   # checks
  #   
  #   check_fdx1_descr(data, "level2")
  #   
  #   
  #   # If all ok then
  #   
  #   data
  # })
  # 
  # 
  # temp_occurrence_l3 <- reactive({
  #   
  #   req(input$occurrence_file)
  #   
  #   path   = occurrence_file_info$datapath
  #   
  #   load_occurrence(path, sheet = "Level3")
  #   
  # })
  # 
  # temp_substance_info <- reactive({
  #   
  #   req(input$occurrence_file)
  #   
  #   path   = occurrence_file_info$datapath
  #   
  #   
  #   readxl::read_xlsx(path,
  #                     range = range_subInfo,
  #                     col_names = FALSE,
  #                     sheet = "Level2") %>% 
  #     purrr::set_names(c("row.name","value"))
  #   
  # })
  # 
  
  # observeEvent(input$accept_occurrence, {
  #   
  #   # firt the substance_info  to get the exposure_factor
  #   
  #   freq <- 
  #     temp_substance_info() %>% 
  #     filter(row.name =="Type") %>% pull(value)
  #   
  #   if(freq ==  "DAILY") {
  #     rv$exposure_factor = 1
  #     
  #     rv$exposure_frequency = "DAILY"
  #   } else{
  #     
  #     rv$exposure_factor = 7
  #     rv$exposure_frequency = "WEEKLY"
  #     
  #   }
  #   
  #   # I need it as NULL to avoid plotting if no value is given in the occurrence import
  #   ref <- temp_substance_info()$value[3]
  #   rv$ref_value <- if(not_na(ref)) as.numeric(ref) else NULL
  #   
  #   rv$substance_info <- 
  #     data.frame(
  #       stringsAsFactors = FALSE,
  #       check.names = FALSE,
  #       
  #       row.names = c("Chemical Substance", 
  #                     "Substance Category",
  #                     "Reference value (μg/Kg b.w.)",
  #                     "Type of Reference value",
  #                     "Frequency"),
  #       values = temp_substance_info()$value
  #     )
  #   
  #   # Occurrence changes the following datasets
  #   # observeEvent will execute the following witihin an isolatescope
  #   
  #   datasets$occurrence_l2 = temp_occurrence_l2()
  #   
  #   datasets$occurrence_l3 = temp_occurrence_l3()
  #   
  #   datasets$tbl_merged    = create_tbl_merged(datasets$tbl_consumption,
  #                                              datasets$occurrence_l2,
  #                                              datasets$occurrence_l3
  #   )
  #   
  #   datasets$tbl_exposure = create_tbl_exposure(datasets$tbl_merged,
  #                                               datasets$tbl_subjects,
  #                                               rv$exposure_factor
  #   ) %>% 
  #     # relocate(
  #     #   subjectid,gender, area, pop_class, age,weight, wcoeff, cons_days, everything() 
  #     # ) %>% 
  #     {.}
  #   
  #   
  #   showModal(modalDialog(
  #     title = "Occurrence datasets are updated",
  #     "Occurrence data is replaced and calculations are done!"
  #   ))
  #   
  #   shinyjs::hide("accept_occurrence")
  #   
  # }, ignoreInit = TRUE)
  
  
  # observeEvent(input$accept_consumption, {
  #   
  #   # Consumption changes all datasets
  #   # observeEvent will execute the following witihin an isolatescope
  #   
  #   datasets$tbl_consumption  = temp_tbl_consumption()
  #   
  #   
  #   datasets$tbl_subjects     = create_tbl_subjects(datasets$tbl_consumption)
  #   
  #   datasets$tbl_merged       = create_tbl_merged(datasets$tbl_consumption,
  #                                                 datasets$occurrence_l2,
  #                                                 datasets$occurrence_l3
  #   )
  #   
  #   datasets$tbl_exposure = create_tbl_exposure(datasets$tbl_merged,
  #                                               datasets$tbl_subjects,
  #                                               rv$exposure_factor
  #   ) %>% 
  #     # relocate(
  #     #   subjectid,gender, area, pop_class, age,weight, wcoeff, cons_days, everything() 
  #     # ) %>% 
  #     {.}
  #   
  #   showModal(modalDialog(
  #     title = "Datasets are updated",
  #     "Consumption data is replaced and calculations are done!"
  #   ))
  #   
  #   shinyjs::hide("accept_consumption")
  #   
  # }, ignoreInit = TRUE)
  
  
}
