library(shiny)
library(shinydashboard)
library(mapdeck) 
library(colourvalues)
library(tidyverse)
library(googleVis)
library(colourvalues)
library(jsonify)
library(geojsonsf)
library(spatialwidget)
library(googlePolylines)
library(maps)
library(ggplot2)
library(ggmap)
library(ggalt)
library(ggthemes)
library(ggiraph)
library(mapproj)

initialCH4Change = 134

shinyUI(dashboardPage(skin='blue',
  dashboardHeader(
    title = "Assessment of Environmental and Societal Impacts of Methane Reductions",
    titleWidth = 800
  ),
  
  
  dashboardSidebar(width = 300,
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon('align-justify')),

    menuItem("Impacts", tabName = "Impacts", icon = icon('map'))
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
                HTML(
                "<h2>Impacts of Methane Reductions on Climate, Health, Ecosystems and the Economy</h2> 
                <p>This site provides a tool to display analyses from the UN Environment-supported Global Methane Assessment to support decision making regarding methane emissions. Users enter the potential methane emissions reductions (or increases) associated with any action of interest, ranging from individual projects to national or international action plans. The tool then provides quantitative values for multiple impacts of those emissions reductions. These include their effects on climate change and ground-level ozone concentrations, and then via those environmental changes the resulting impacts on human health, agricultural crops and the economy. Results are based upon a set of coordinated modeling studies undertaken using the following models: the CESM2(WACCM6) model developed at the National Center for Atmospheric Research in Boulder, CO, USA; the GFDL AM4.1/ESM4.1 model developed by the National Oceanographic and Atmospheric Administration in Princeton, NJ, USA; the GISS E2.1/E2.1-G model developed by the National Aeronautics and Space Agency in New York, NY, USA; the MIROC-CHASER model developed by the Meteorological Research Institute in Tsukuba, Japan; and the UKESM1 model developed by the UK Meteorological Office, Exeter, UK and the UK academic community.</p> 

<p>We thank our sponsors: UN Environment, the Climate and Clean Air Coalition, NASA. </p>
             "),
             img(src="CCAC_logo.png", width="250px"),
             img(src="nasa.jpg", width="170px"),
             img(src="UNEnvironment_Logo.jpg", width="250px"),
                width = 12),
     
      tabItem(tabName = "Impacts", 
            
              h3("Mt methane reduction"),
              numericInput("obs", "", initialCH4Change, min = -200, max = 1000),            
              fluidRow(
                box(title = "Change in Premature Respiratory Deaths Due to Ozone Exposure", 
                    width = 12, 
                    ggiraphOutput("dMortRespCountry_2040"))
              ),
 
              h4("The above map shows the change in the number of premature deaths due to respiratory illnesses caused by ozone in people age 30 or older. Results are based upon the relationship between ozone exposure and health impacts determined from the American Cancer Society Cancer Prevention Study II that followed more than 660,000 people for 22 years and quantified the increased risk of heart disease, cerebrovascular disease, pneumonia and influenza, chronic obstructive pulmonary disease and lung cancer with increased ozone exposure. Those increased risks are combined with data on public health conditions and population distributions to evaluate worldwide health burdens. Uncertainties in these values stem from both the underlying exposure-response relationships and the ozone response to methane. These vary slightly from country to country, but the 95% confidence interval extends from ~60% lower to 75% higher than the best estimates shown here."),
              fluidRow(
                box(title = "Change in Premature Cardiovascular Deaths Due to Ozone Exposure", 
                    width = 12, 
                    ggiraphOutput("dMortCardCountry_2040"))
              ),
              
              h4("The above map shows the change in the number of premature deaths due to cardiovascular illnesses caused by ozone in people age 30 or older. Results are based upon the relationship between ozone exposure and health impacts determined from the American Cancer Society Cancer Prevention Study II that followed more than 660,000 people for 22 years and quantified the increased risk of heart disease, cerebrovascular disease, pneumonia and influenza, chronic obstructive pulmonary disease and lung cancer with increased ozone exposure. Those increased risks are combined with data on public health conditions and population distributions to evaluate worldwide health burdens. Uncertainties in these values stem from both the underlying exposure-response relationships and the ozone response to methane. These vary slightly from country to country, but the 95% confidence interval extends from ~60% lower to 75% higher than the best estimates shown here."),
              fluidRow(
                box(title = "Change in Premature Respiratory Deaths Due to Ozone Exposure per Million Persons", 
                    width = 12, 
                    ggiraphOutput("dMortRespCountry_capita_2040"))
              ),
 
              h4("The above map shows the change in premature deaths due to cardio illnesses caused by ozone per million persons of age 30 and older. Results are based upon the relationship between ozone exposure and health impacts determined from the American Cancer Society Cancer Prevention Study II that followed more than 660,000 people for 22 years and quantified the increased risk of heart disease, cerebrovascular disease, pneumonia and influenza, chronic obstructive pulmonary disease and lung cancer with increased ozone exposure. Those increased risks are combined with data on public health conditions and population distributions to evaluate worldwide health burdens. Uncertainties in these values stem from both the underlying exposure-response relationships and the ozone response to methane. These vary slightly from country to country, but the 95% confidence interval extends from ~60% lower to 75% higher than the best estimates shown here."),
              fluidRow(
                box(title = "Change in Premature Cardiovascular Deaths Due to Ozone Exposure per Million Persons", 
                    width = 12, 
                    ggiraphOutput("dMortCardCountry_capita_2040"))
              ),
              
              h4("The above map shows the change in the number of premature deaths due to cardiovascular illnesses caused by ozone per million persons of age 30 or older. Results are based upon the relationship between ozone exposure and health impacts determined from the American Cancer Society Cancer Prevention Study II that followed more than 660,000 people for 22 years and quantified the increased risk of heart disease, cerebrovascular disease, pneumonia and influenza, chronic obstructive pulmonary disease and lung cancer with increased ozone exposure. Those increased risks are combined with data on public health conditions and population distributions to evaluate worldwide health burdens. Uncertainties in these values stem from both the underlying exposure-response relationships and the ozone response to methane. These vary slightly from country to country, but the 95% confidence interval extends from ~60% lower to 75% higher than the best estimates shown here."),
              fluidRow(
                box(title = "Valuation of Reduced Risk of Death Due to Ozone Exposure", 
                    width = 12, 
                    ggiraphOutput("dMortCountry_VSL_2040"))
              ),
 
              h4("The above map shows the monetized value of reduced risk of premature death due to respiratory plus cardiovascular illnesses caused by ozone in persons of age 30 and older. Results are based upon the relationship between ozone exposure and health impacts determined from the American Cancer Society Cancer Prevention Study II that followed more than 660,000 people for 22 years and quantified the increased risk of heart disease, cerebrovascular disease, pneumonia and influenza, chronic obstructive pulmonary disease and lung cancer with increased ozone exposure. Those increased risks are combined with data on public health conditions and population distributions to evaluate worldwide health burdens. Valuation of reduced risk is based upon willingness to pay data, adjusted to local income levels (all using 2018 USD). Uncertainties in these values stem from both the underlying exposure-response relationships and the ozone response to methane. These vary slightly from country to country, but the 95% confidence interval extends from ~60% lower to 75% higher than the best estimates shown here."),
                fluidRow(
                box(title = "Change in Yield of Wheat due to Climate and Ozone Response to Methane", 
                    width = 12,
                    ggiraphOutput("Wheat_kt"))
              ),
             h4("The above map shows the change in yield of wheat in response to the input methane emissions changes. Values are based on the multi-model mean of the participating models' temperature, precipitation and ozone responses along with a small contribution from CO2 fertilization. Additional analyses demonstrated that ozone responses are approximately linearly proportional to methane emissions changes, so that these interpolated results are accurate for current background atmospheric conditions."),
             fluidRow(
              
                box(title = "Change in Asthma-related Emergency Room Visits due to Ozone Exposure", 
                    width = 12,
                    ggiraphOutput("ozoneAsthmaER_2040"))
              ),
             h4("The above map shows the change in ground-level ozone in response to the input methane emissions changes. Values are based on the multi-model mean of the five participating models. Additional analyses demonstrated that ozone responses are approximately linearly proportional to methane emissions changes, so that these interpolated results are highly accurate for current background atmospheric conditions."),
              fluidRow(
              
                box(title = "Valuation of Change in Asthma-related Emergency Room Visits due to Ozone Exposure", 
                    width = 12,
                    ggiraphOutput("ozoneAsthmaERCost_2040"))
              ),
             h4("The above map shows the valuation of change in ground-level ozone in response to the input methane emissions changes. Values are given in 2018 USD, and are based on the multi-model mean of the five participating models. Additional analyses demonstrated that ozone responses are approximately linearly proportional to methane emissions changes, so that these interpolated results are highly accurate for current background atmospheric conditions."),
              fluidRow(
              
                box(title = "Change in Ground-level Ozone Concentration (ppb)", 
                    width = 12,
                    ggiraphOutput("ozoneCountry_2040"))
              ),
             h4("The above map shows the change in asthma-related emergency room visits due to changes in ground-level ozone in response to the input methane emissions changes. Values are based on the multi-model mean of the five participating models. Hospital data is only available for a limited number of countries."),
              fluidRow(
              
                box(title = "Change in Surface Temperature (C)", 
                    width = 12,
                    ggiraphOutput("dSAT_2040"))
              ),
             h4("The above map shows the change in national average annual average surface temperature in response to the input methane emissions changes. Values are based on the multi-model mean of the models that performed climate simulations, with results presented only when robust, statistically significant values are found over at least 50% of a nation's area."),
 
              )
    
      
    )
    )
  )
  
)


