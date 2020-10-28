# The help/ notes for tables and graphs to be presented vis the {rintrojs} library

# A dataframe that holds the output binding id, and the text to appear


# Exposure TAB  ####

intro_exposure <- 
  data.frame(
    
    element = c(
      
      NA,
      "#subInfo_exposure",
      "#tbl_exposure_stats",
      "#graphs",
      "#exposure_pdf",
      "#scenario_UI_exposure",
      "#exposurePDF"
      
      
    ),
    intro = c( 
      htmltools::HTML("<h3>Welcome to ImproRisk!</h3><p>Hit 'Next' to get a tour</p>"),
      
      "This table shows basic information on the substance",
      
      "Here, we have the exposure statistics. The values
      you see are in μg/Kg of body weight and are 
      weighted by the population weights",
      
      "There are two graphs to see here. One is
     the Probability distribution of the exposure  in the 'PDF' tab
     and the other is the cummultive exposure - in the 'CDF' tab",
      
      "The PDF of the exposure.This a histogram. The exposure estimates for
     each inndividual is 'binned'and the proportion is calculated",
      
      "Select the exposure scenario from here, and the tables and charts
      update accordingly",
      
      "Underneath each plot and table, there is a download button"
      
    ), stringsAsFactors = FALSE
  )

#  ExposureDemo TAB ####


intro_exposureDemo <- 
  data.frame(
    
    element = c(
      
      NA
      ,"#slct_demo"
      ,"#tbl_exposure_statsDemo"
      ,"#graphsDemo"
      ,"#exposure_pdfDemo"
      ,"#scenario_UI_exposureDemo"
      , NA
      
      
    ),
    intro = c( 
      htmltools::HTML("<h3>Explore the exposure by demographic information!</h3><p>Hit 'Next' to get a tour</p>"),
      
      "First, select the demographic",
      
      "Here, we have the exposure statistics. The values
      you see are in μg/Kg of body weight and are 
      weighted by the population weights",
      
      "There are two graphs to see here. One is
      the Probability distribution of the exposure  in the 'PDF' tab
       and the other is the cummultive exposure - in the 'CDF' tab",
      
      "The PDF of the exposure.This a histogram. The exposure estimates for
      each inndividual is 'binned'and the proportion is calculated",
      
      "Select the exposure scenario from here, and the tables and charts
      update accordingly",
      
      "Underneath each plot and table, there is a download button"
      
    ), stringsAsFactors = FALSE
  )
