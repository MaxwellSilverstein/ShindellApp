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
  
  
  dashboardSidebar(width = 150,
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

              h3("Climate and Ozone Responses"),
              fluidRow(
                box(title = "Avoided Warming", 
                    width = 6,
                    ggiraphOutput("dSAT_2040")),              
                box(title = "Reduction in Ground-level Ozone Concentration", 
                    width = 6,
                    ggiraphOutput("ozoneCountry"))
              ),
             h4("The above maps show the change in national average annual average surface temperature and surface ozone (parts per billion; ppb) in response to the input methane emissions changes. For temperature, values are based on the multi-model mean of the models that performed climate simulations, with results presented only when robust, statistically significant values are found over at least 50% of a nation's area. For ozone, values are based on the multi-model mean of the five participating models."),

              h3("Human Health Responses"),
              fluidRow(
                box(title = "Reduced Premature Respiratory Deaths Due to Ozone Exposure", 
                    width = 6, 
                    ggiraphOutput("dMortRespCountry")),
                box(title = "Reduced Premature Respiratory Deaths Due to Ozone per Million Persons", 
                    width = 6, 
                    ggiraphOutput("dMortRespCountry_capita"))
              ),
              h4("The above maps show the change in premature deaths due to respiratory illnesses caused by ozone in people age 30 or older. Results are based upon the relationship between ozone exposure and health impacts determined from the American Cancer Society Cancer Prevention Study II that followed more than 660,000 people for 22 years and quantified the increased risk of heart disease, cerebrovascular disease, pneumonia and influenza, chronic obstructive pulmonary disease and lung cancer with increased ozone exposure. Those increased risks are combined with data on public health conditions and population distributions to evaluate worldwide health burdens. Uncertainties in these values stem from both the underlying exposure-response relationships and the ozone response to methane. These vary slightly from country to country, but the 95% confidence interval extends from ~60% lower to 75% higher than the best estimates shown here."),
              fluidRow(
                box(title = "Reduced Premature Cardiovascular Deaths Due to Ozone Exposure", 
                    width = 6, 
                    ggiraphOutput("dMortCardCountry")),
                box(title = "Reduced Premature Cardiovascular Deaths Due to Ozone per Million Persons", 
                    width = 6, 
                    ggiraphOutput("dMortCardCountry_capita"))
              ),             
              h4("The above maps show the change in the number of premature deaths due to cardiovascular illnesses caused by ozone in people age 30 or older. Results are based upon the relationship between ozone exposure and health impacts determined from the American Cancer Society Cancer Prevention Study II that followed more than 660,000 people for 22 years and quantified the increased risk of heart disease, cerebrovascular disease, pneumonia and influenza, chronic obstructive pulmonary disease and lung cancer with increased ozone exposure. Those increased risks are combined with data on public health conditions and population distributions to evaluate worldwide health burdens. Uncertainties in these values stem from both the underlying exposure-response relationships and the ozone response to methane. These vary slightly from country to country, but the 95% confidence interval extends from ~60% lower to 75% higher than the best estimates shown here."),
             fluidRow(
                box(title = "Reduced Asthma-related Emergency Room Visits due to Ozone Exposure", 
                    width = 6,
                    ggiraphOutput("ozoneAsthmaER")),
                box(title = "Reduced Hospital Admissions for Persons 65 and over due to Ozone", 
                    width = 6,
                    ggiraphOutput("OzRespHosp65"))
              ),
             h4("The above maps show the reductions in asthma-related emergency room visits and respiratory-related hospital admissions in elderly people due to changes in ground-level ozone in response to the input methane emissions changes. Values are based on the multi-model mean of the five participating models. Hospital data is only available for a limited number of countries."),

              h3("Agricultural Responses"),
                fluidRow(
                box(title = "Increase in Yield of Wheat due to Climate and Ozone Response to Methane", 
                    width = 6,
                    ggiraphOutput("Wheat_kt")),
                box(title = "Increase in Yield of Rice due to Climate and Ozone Response to Methane", 
                    width = 6,
                    ggiraphOutput("Rice_kt"))
              ),
             h4("The above maps show the increase in yield of wheat and rice in response to the input methane emissions changes. Values are based on the multi-model mean of the participating models' temperature, precipitation and ozone responses along with a small contribution from CO2 fertilization. Additional analyses demonstrated that ozone responses are approximately linearly proportional to methane emissions changes, so that these interpolated results are accurate for current background atmospheric conditions."),
                fluidRow(
                box(title = "Increase in Yield of Maize due to Climate and Ozone Response to Methane", 
                    width = 6,
                    ggiraphOutput("Maize_kt")),
                box(title = "Increase in Yield of Soy due to Climate and Ozone Response to Methane", 
                    width = 6,
                    ggiraphOutput("Soy_kt"))
              ),
             h4("The above maps show the increase in yield of maize (corn) and soybeans in response to the input methane emissions changes. Values are based on the multi-model mean of the participating models' temperature, precipitation and ozone responses along with a small contribution from CO2 fertilization. Additional analyses demonstrated that ozone responses are approximately linearly proportional to methane emissions changes, so that these interpolated results are accurate for current background atmospheric conditions."),

              h3("Economic Valuation"),
              fluidRow(
                box(title = "Valuation of Reduced Risk of Death Due to Ozone Exposure", 
                    width = 12, 
                    ggiraphOutput("dMortCountry_VSL"))
              ),
              h4("The above map shows the monetized value of reduced risk of premature death due to respiratory plus cardiovascular illnesses caused by ozone in persons of age 30 and older. Results are based upon the relationship between ozone exposure and health impacts determined from the American Cancer Society Cancer Prevention Study II that followed more than 660,000 people for 22 years and quantified the increased risk of heart disease, cerebrovascular disease, pneumonia and influenza, chronic obstructive pulmonary disease and lung cancer with increased ozone exposure. Those increased risks are combined with data on public health conditions and population distributions to evaluate worldwide health burdens. Valuation of reduced risk is based upon willingness to pay data, adjusted to local income levels (all using 2018 USD). Uncertainties in these values stem from both the underlying exposure-response relationships and the ozone response to methane. These vary slightly from country to country, but the 95% confidence interval extends from ~60% lower to 75% higher than the best estimates shown here."),
                fluidRow(
                box(title = "Valuation of Increase in Crop Yields due to Climate and Ozone Response to Methane", 
                    width = 12,
                    ggiraphOutput("Crops_USD"))
              ),
             h4("The above maps show the valuation of the increases in yield of wheat, rice, maize (corn) and soybeans in response to the input methane emissions changes. Values are based on the multi-model mean of the participating models' temperature, precipitation and ozone responses along with a small contribution from CO2 fertilization to establish yield changes, which are then valued based on world commodity prices. Additional analyses demonstrated that ozone responses are approximately linearly proportional to methane emissions changes, so that these interpolated results are accurate for current background atmospheric conditions."),
                fluidRow(
                box(title = "Valuation of Reduced Asthma-related Emergency Room Visits due to Ozone", 
                    width = 6,
                    ggiraphOutput("ozoneAsthmaERCost")),
                box(title = "Valuation of Reduced Hospital Admissions for Persons 65 and over", 
                    width = 6,
                    ggiraphOutput("OzRespHosp65Cost"))
              ),
             h4("The above maps show the valuation of reductions in asthma-related emergency room visits and respiratory-related hospital admissions in elderly people due to changes in ground-level ozone in response to the input methane emissions changes. Values are based on the multi-model mean of the five participating models. Hospital data is only available for a limited number of countries."),
    
      )
    )
    )
  )
  
)


