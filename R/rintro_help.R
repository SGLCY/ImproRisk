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
      htmltools::HTML("<h4>Explore the exposure by demographic information!</h4><p>Hit 'Next' to get a tour</p>"),
      
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

# Contribution ####

intro_contribution <- 
  data.frame(
    
    element = c(
      
      #NA
      "#tbl_aggr_contribution"
      ,"#contribution_panel"
      #, NA
      
      
    ),
    intro = c( 
      #HTML("<h4>Explore the contribution of each food item to the total exposure</h4><p>Hit 'Next'</p>"),
      
      HTML("<p>This table aggregates the contribution of each food item to the <code>Total Exposure</code>, 
      as well as the Contribution within the parent foodex level</p>
           <p>The contribution is weighted by the population weights</p>"),
      
      "You can also view the contribution as a graph"
      
      # HTML("<p>The <code>N_day exposure</code> refers to the Total exposure for the full study period.
      #                 <br>Now depending on the exposure frequency of the substance (see tab 'Exposure'), we also have
      #                 the MEAN <code>WEEKLY</code> or MEAN <code>DAILY</code> exposure</p>")
      
    ), stringsAsFactors = FALSE
  )


# Contribution ####

intro_consumption <- 
  data.frame(
    
    element = c(
      
      #NA
      "#tbl_aggr_consumption"
      ,"#slct_food_levelConsumption"
      #, NA
      
      
    ),
    intro = c( 
      #HTML("<h4>Explore the contribution of each food item to the total exposure</h4><p>Hit 'Next'</p>"),
      
      HTML("<p>This table shows the mean daily consumption in grams for:</p>
      <ul>
<li>Consumer based, by averaging the consumption of those who consumed the food</li>
<li>Population based, by averaging the consumption over the total number of individuals in the survey, irrespective of their zero consumption for that item</li>
</ul>
<p> The calculations are always adjusted by the population weigths<p/>"),
      
      "Select the food level here"
      
    ), stringsAsFactors = FALSE
  )



# Individual Exposure ###

intro_individual <- 
  data.frame(
    
    element = c(
      
      NA
      ,"#tbl_exposure"
      , NA
      
      
    ),
    intro = c( 
      htmltools::HTML("<h4>Explore the exposure estimate for each subject</h4><p>Hit 'Next'</p>"),
      
      "This table lists the participants in the food survey, along with the demographic characteristics",
      
      HTML("<p>The <code>N_day exposure</code> refers to the Total exposure for the full study period.
                      <br>Now depending on the exposure frequency of the substance (see tab 'Exposure'), we also have
                      the MEAN <code>WEEKLY</code> or MEAN <code>DAILY</code> exposure</p>")
      
    ), stringsAsFactors = FALSE
  )