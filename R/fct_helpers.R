



#'Label the reference value
#'@details if the user does not provide ref value then we have NA
#'@param ref_value Numeric. The reference value
#'@noRd
label_ref_value <- function(ref_value, ...){
  
  if(not_null(ref_value)){
    label <- glue::glue("Reference value: {ref_value} μg/Kw b.w.")
    
    stringr::str_wrap(label, ...)
  } else NULL
  
}

#' Calculate bandwidth for the ggridges plot
#' @details Stolen from {ggridges} package
#' @noRd
calc_bandwidth = function(data, target, group) {
  
  xdata <- na.omit(data.frame(x=data[[target]], group=data[[group]]))
  
  xs <- split(xdata$x, xdata$group)
  
  xs_mask <- vapply(xs, length, numeric(1)) > 1
  
  bws <- vapply(xs[xs_mask], bw.nrd0, numeric(1))
  
  mean.bw <- mean(bws, na.rm = TRUE)
  
  bw <- c(
    mean = mean.bw,
    low  = mean.bw/3,
    high = mean.bw*3
  )
}


# Summary Statistics ####
#' Get a tibble of weighted summary statistics
#' @noRd
summarise_weighted <- function(data, ref_value = NULL){
  
  # Note the ref_value  
  if(is.null(ref_value)){
    ref_value <- NA
  }
  
  data %>% 
    dplyr::summarise(
      #N           = dplyr::n(),
      Min         = min(exposure, na.rm = TRUE),
      Max         = max(exposure, na.rm = TRUE),
      Mean        = Hmisc::wtd.mean(exposure, wcoeff),
      SD          = sqrt(Hmisc::wtd.var(exposure, wcoeff)),
      #  For SE see the discussion 
      #R https://stats.stackexchange.com/questions/25895/computing-standard-error-in-weighted-mean-estimation
      
      #SE         = wtd.std*sqrt(sum(wcoeff))/sum(wcoeff),
      P25         = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.25),
      Median      = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.50),
      P75         = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.75),
      P95         = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.95),
      # % above reference value
      pctOver     = sum(wcoeff[exposure>ref_value])/sum(wcoeff)
    )
  
}

#' Create a 'select Scenario input
#' @param tab_name The tab where the input will appear
#' @param label A label for the input
#' @param choices The vector c(""LB" ,"MB" ,"UB")
#' @return A Shiny select input for the Scenario
#' @details This function is called from the server to create
#' an output binding of selecet scenario inputs for each Tab. The output binding creates a UI  (via 'renderUI')
#' See server for the bindig convention names and the inputs id convention names
#' @noRd
slct_scenario <- function(tab_name, label = "Scenario", choices){
  
  inputId =  paste0("slct_scenario_", tab_name)
  
  shinyWidgets::radioGroupButtons(
    
    inputId  = inputId,
    label = label,
    choices = choices,
    selected = choices[2] #MB
    
  )
  
  
}


#'Turn a vector of numerics into percentages
#'@param ...  Arguments passed to scales::percent()
#'@param x A numeric vector
#'@noRd
#'@return A vector of percenages same elngth as x
percent <- function(x, ...){
  scales::label_percent(...)(x)
  
}

#'Histogram for the exposure statistics
#'@param data The Exposure table by subject
#'@param var_exp Variaable that holds the individuaal exposure
#'@param bins How many bins?
#'@param accuracy Rounding digits in percentages
#'@param digits Round digits in exposure values in x - axis
pdf_exposure <- function(data, 
                         var_exp, 
                         bins, 
                         accuracy = 0.1, 
                         #ref_line = NULL,
                         digits = 3
){
  
  
  range_exp <- range(data[[var_exp]])
  
  binsize   <- diff(range_exp)/bins
  
  breaks    <- seq(from = range_exp[1], to = range_exp[2], by =  binsize)
  
  
  data %>% 
    ggplot(aes(.data[[var_exp]]))+
    geom_histogram(
      
      position = "identity"
      , colour= "grey90"
      , alpha  = 1
      , boundary = 0
      , bins = bins
      , breaks = breaks
      , aes(y= ..count../sum(..count..))
      , fill =  impro_colours[2]
    )+
    stat_bin(
      position = "identity"
      , bins = bins
      , breaks = breaks,geom= "text"
      , aes(label = percent(..count../ sum(..count..),
                            accuracy = accuracy
      ),
      y = ..count../sum(..count..)
      )
      , vjust = -0.5
    )+
    scale_x_continuous(breaks = breaks, labels = round(breaks, digits = digits))+
    scale_y_continuous(labels = percent)+
    theme(
      panel.grid.minor.x = element_blank()
    )
  
}


#'Plot of Cummulative exposure distribution
#'@details Plots either single or multigroup CDF depending if you
#'supply a `var_group`
#'@param data The tbl_exposure() for Individuals
#'@param var_exp  String. The variable name for the exposure values. This depends on the scenario
#'@param var_group   String.Is there a grouping in the data? Usually demographic
#'@param ref_value Numeric. The reference value if any


cdf_exposure <- function(data,
                         var_exp,
                         var_group = NULL,
                         ref_value
                         
){
  
  if(is.null(var_group)){
    p <- data %>% 
      ggplot(aes(.data[[var_exp]]))
    
  } else {
    p <- data %>% 
      ggplot(aes(.data[[var_exp]], colour = .data[[var_group]]))
    
  }
  
  p +
    stat_ecdf(pad = FALSE)+
    scale_y_continuous(labels = scales::percent)+
    geom_vline(xintercept = ref_value, linetype = "dashed" )+
    annotate("text", x = ref_value+0.05*ref_value, y = 0.51,
             hjust = 0, label = label_ref_value(ref_value, width  =  16)
    )
  
}


#'  PDF exposure by Group
#'  Compare distribtuion across groups
#'  @param data The data
#'  @param var_exp  String. The exposure variable.Differes depending the scenario
#'  @param var_group String. The grouping variable
#'  @param bandwidth Numeric.The bandwidth for the density plots
#'  @param scale Numeric
#'  @param ref_value Numeric. The reference value
#'  @noRd
pdf_exposureDemo <- function(data,
                             var_exp,
                             var_group,
                             #bandwith,
                             scale = 1.1,
                             ref_value
                             
){
  
  n_group <- length(unique(data[[var_group]]))
  
  data %>% 
    ggplot(
      aes(y = .data[[var_group]], x = .data[[var_exp]])
    )+
    ggridges::geom_density_ridges(fill = impro_colours[2],
                                  #bandwidth =  bandwith, 
                                  stat="density_ridges", 
                                  scale =  scale,
    )+
    scale_x_continuous(expand = c(0,0))+
    geom_vline(aes(xintercept = ref_value), linetype="dotted")+
    annotate("text", x = ref_value+0.05*ref_value, y = n_group-0.5,
             hjust = 0, 
             label= label_ref_value(ref_value,width = 16)
    )
  
  
}






#' Pdf ecosure by group - Freq_poly
#' @noRd
pdf_exposureDemo2 <- function(data,
                              var_exp, 
                              var_group,
                              bins, 
                              #accuracy = 0.1, 
                              #ref_line = NULL,
                              digits = 3
){
  
  range_exp <- range(data[[var_exp]])
  
  binsize   <- diff(range_exp)/bins
  
  breaks    <- seq(from = range_exp[1], to = range_exp[2], by =  binsize)
  
  
  data %>% 
    ggplot(aes(.data[[var_exp]], colour = .data[[var_group]]))+
    geom_freqpoly(
      position = "identity"
      , boundary = 0
      , bins = bins
      , breaks = breaks
      , aes(y= ..count../sum(..count..))
      , pad = TRUE
      
    )+
    scale_x_continuous(breaks = breaks, labels = round(breaks, 3))+
    scale_y_continuous(labels = percent)+
    theme(
      panel.grid.minor.x = element_blank()
    )+
    NULL
  
}

# Mean Consumption ####
#' Aggregate consumption data by food level
#' @param merged The table of merged data
#' @param subjects The table of subjects
#' @param var String. The variable in the data for the food level. This is passed
#' within the tbl_aggr_consumption() reactive. 
#' @return A  tibble. Aggregated consumption by Food Level
#' @details This function is called from the server to create the aggregated table. See tbl_aggr_consumption reactive()
#' @noRd
aggr_consumption_by_group <- function(merged, subjects, var){
  
  
  sample_size = nrow(subjects)
  pop_size = sum(subjects$wcoeff)

  out <- 
    merged %>% 
    mutate(
      across(.data[[var]], as.character)
    ) %>% 
    group_by(
      subjectid, .data[[var]]
    ) %>% 
    summarise(
      ttl_cons = sum(amountfood,  na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    tidyr::complete(.data[[var]], subjectid, fill = list(ttl_cons =  0)) %>% 
    left_join(
      subjects,by = "subjectid"
    ) %>% 
    mutate(daily_cons = ttl_cons/cons_days,
           consumed   = if_else(ttl_cons != 0, 1, 0)
    ) %>% 
    group_by(.data[[var]]) %>% 
    summarise(
      N_sample  = sum(consumed), # number of consumers
      N_pop = sum(consumed * wcoeff),
      #consumers = n_distinct(subjectid[ttl_cons!=0]),
      consumer_based     = sum(daily_cons *  wcoeff)/sum(wcoeff[ttl_cons !=0]),
      population_based   = sum(daily_cons *  wcoeff)/sum(wcoeff, na.rm = TRUE)
    ) %>% 
    mutate_at(
      vars(population_based, consumer_based), ~  round(.,1)
    ) %>% 
    mutate(
      pct_sample = percent(N_sample/sample_size,accuracy = 0.1),
      "Sample" = glue::glue("{N_sample} ({pct_sample}}"),
      
      pct_pop = percent(N_pop/pop_size,accuracy = 0.1),
      "Population" = glue::glue("{N_pop} ({pct_pop}}")
    ) 

    # in case NaNs appear where no consumption is made
  out %>% 
    tidyr::replace_na(list(consumer_based = 0)) 
}


