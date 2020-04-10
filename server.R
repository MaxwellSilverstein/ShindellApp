# Read files into global memory
countries_mmm <-read.csv("csvData/MMMresults.csv")
initialO3 <- read.csv("csvData/InitialO3.csv")
initial_resp<-read.csv("csvData/Initial_Resp.csv")
initial_cardio<-read.csv("csvData/Initial_Cardio.csv")
allagePop <-read.csv("csvData/AllAgePopulation.csv")
surfaceTemp <-read.csv("csvData/Country_Level_Temp_Change_MMM_1StandardErrors_50%_ValidDataEachCountry.csv")
nationalVSL <-read.csv("csvData/VSL2018USD.csv")
asthmaERV <-read.csv("csvData/AsthmaEV4Mar2020.csv")
RespHosp65 <-read.csv("csvData/RespHA_delta3minus4_19Mar2020.csv")
wheatYieldkt <-read.csv("csvData/wheat_countryvalue_CropYieldsLoss_03202020.csv")
riceYieldkt <-read.csv("csvData/rice_countryvalue_CropYieldsLoss_03202020.csv")
soyYieldkt <-read.csv("csvData/soy_countryvalue_CropYieldsLoss_03202020.csv")
maizeYieldkt <-read.csv("csvData/maize_countryvalue_CropYieldsLoss_03202020.csv")

shinyServer(function(input, output){

cat("\nEXECUTION ", format(Sys.time(), "%a %b %d %X %Y"), "\n", file=stderr())
  #Environment variables 
  EpiTMREL = 26.3
  EpiBeta = .01133
  EpiBetaCard = 0.00296
  #text render
  n = renderText({input$obs})
  output$caption <- renderText({
    input$obs
  })

  # Ozone Delta Map

  df<-data.frame(countries_mmm["Country"],countries_mmm["ANN_MDA8Sim2.Sim1.Diff."])
  dfO3 <-data.frame(initialO3["Country"],initialO3["MMM.Initial.O3..ppb."])
  output$ozoneCountry <- renderggiraph({
    world <- map_data("world")
    ozoneDF<-setNames(data.frame(df[1],df[2]*-1*input$obs*(1/134)),c("Country","ppb"))
    map.world_joined <- left_join(world, ozoneDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = ppb, tooltip
=sprintf("%s<br/>%s",region,ppb)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  # Attempt to define health-related variables (AF) for use in all functions

  #newO3 <- reactive({pmax(0,df[2]*input$obs*(1/134)-EpiTMREL+dfO3[2])})
  #AttFracGlb <- reactive({1-exp(newO3*-1*EpiBeta)})

  # Delta Total Resp Mortality Map
  
  dfResp <- data.frame(initial_resp["PopTimesBaseMort"],initial_resp["InitialMort"])
  output$dMortRespCountry <-renderggiraph({    

    newO3 <- pmax(0,df[2]*input$obs*(1/134)-EpiTMREL+dfO3[2])
    AttFrac <- 1-exp(newO3*-1*EpiBeta)
    deathCol <- ceiling(dfResp[2]-AttFrac*dfResp[1])
    deathFram <- data.frame(df[1],deathCol)
    dfDeaths <- setNames(deathFram,c("Country","Avoided_Deaths"))
    
    world <- map_data("world")
    map.world_joined <- left_join(world, dfDeaths, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = Avoided_Deaths, tooltip=
sprintf("%s<br/>%s",region,Avoided_Deaths)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map() 
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Per Capita Resp Mortality Map
  
  dfPOP <- data.frame(allagePop["Country"],allagePop["Population"])
  output$dMortRespCountry_capita <-renderggiraph({

    newO3 <- pmax(0,df[2]*input$obs*(1/134)-EpiTMREL+dfO3[2])
    AttFrac <- 1-exp(newO3*-1*EpiBeta)
    deathCol <- ceiling((dfResp[2]-AttFrac*dfResp[1])/(dfPOP[2]/1000000))
    deathFram <- data.frame(df[1],deathCol)
    dfDeaths <- setNames(deathFram,c("Country","Avoided_Deaths"))
    
    world <- map_data("world")
    map.world_joined <- left_join(world, dfDeaths, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = Avoided_Deaths, tooltip=
sprintf("%s<br/>%s",region,Avoided_Deaths)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map() 
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Total Cardio Mortality Map
  
  dfCard <- data.frame(initial_cardio["PopTimesBaseMort"],initial_cardio["InitialMort"])
  output$dMortCardCountry <-renderggiraph({    

    newO3 <- pmax(0,df[2]*input$obs*(1/134)-EpiTMREL+dfO3[2])
    AttFrac <- 1-exp(newO3*-1*EpiBetaCard)
    deathCol <- ceiling(dfCard[2]-AttFrac*dfCard[1])
    deathFram <- data.frame(df[1],deathCol)
    dfDeaths <- setNames(deathFram,c("Country","Avoided_Deaths"))
    
    world <- map_data("world")
    map.world_joined <- left_join(world, dfDeaths, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = Avoided_Deaths, tooltip=
sprintf("%s<br/>%s",region,Avoided_Deaths)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map() 
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Per Capita Card Mortality Map
  
  dfPOP <- data.frame(allagePop["Country"],allagePop["Population"])
  output$dMortCardCountry_capita <-renderggiraph({
    
    newO3 <- pmax(0,df[2]*input$obs*(1/134)-EpiTMREL+dfO3[2])
    AttFrac <- 1-exp(newO3*-1*EpiBetaCard)
    deathCol <- ceiling((dfCard[2]-AttFrac*dfCard[1])/(dfPOP[2]/1000000))
    deathFram <- data.frame(df[1],deathCol)
    dfDeaths <- setNames(deathFram,c("Country","Avoided_Deaths"))
    
    world <- map_data("world")
    map.world_joined <- left_join(world, dfDeaths, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = Avoided_Deaths, tooltip=
sprintf("%s<br/>%s",region,Avoided_Deaths)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map() 
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Valuation of Reduced Mortality Map
  
  dfVSL <- data.frame(nationalVSL["Country"],nationalVSL["VSLmillionsUSD2018"])
  output$dMortCountry_VSL <-renderggiraph({

    newO3 <- pmax(0,df[2]*input$obs*(1/134)-EpiTMREL+dfO3[2])
    AttFracR <- 1-exp(newO3*-1*EpiBeta)
    AttFracC <- 1-exp(newO3*-1*EpiBetaCard)
    deathResp <- dfResp[2]-AttFracR*dfResp[1]
    deathCard <- dfCard[2]-AttFracC*dfCard[1]
    deathCol<-ceiling((deathResp[1]+deathCard[1])*dfVSL[2])
    deathFram <- data.frame(df[1],deathCol)
    dfDeaths <- setNames(deathFram,c("Country","Millions_USD"))
    
    world <- map_data("world")
    map.world_joined <- left_join(world, dfDeaths, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = Millions_USD, tooltip=
sprintf("%s<br/>%s",region,Millions_USD)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map() 
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Asthma-related ER visits Map

  dfAsthmaER <- data.frame(asthmaERV["Country"],asthmaERV["CasesMEANper10Mt"],asthmaERV["CostsMEAN2018USDperkt"])
  output$ozoneAsthmaER <-renderggiraph({

    asthmaDF<-setNames(data.frame(df[1], ceiling(dfAsthmaER[2]*input$obs*(17/134))),c("Country","Avoided_Visits"))
    # data is outdated and assumed 170 Mt methane between sims 1 and 2, really 134, so adjustment here * 170/134 (plus 1/10)

    world <- map_data("world")
    map.world_joined <- left_join(world, asthmaDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = Avoided_Visits, tooltip
=sprintf("%s<br/>%s",region, Avoided_Visits)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Costs Asthma-related ER visits Map

  #dfAsthmaER <- data.frame(asthmaERV["Country"],asthmaERV["CasesMEANper10Mt"],asthmaERV["CostsMEAN2018USDperkt"])
  output$ozoneAsthmaERCost <-renderggiraph({

    asthmaCostDF<-setNames(data.frame(df[1], ceiling(dfAsthmaER[3]*input$obs*(1.7/1.34))),c("Country","thousands_USD"))

    world <- map_data("world")
    map.world_joined <- left_join(world, asthmaCostDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = thousands_USD, tooltip
=sprintf("%s<br/>%s",region, thousands_USD)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Respiratory Hospitalizations Map

  dfRespHosp <- data.frame(RespHosp65["Country"], RespHosp65["CasesMEANper10Mt"], RespHosp65["CostsMEAN2018USDperkt"])
  output$OzRespHosp65 <-renderggiraph({

    RespHospDF<-setNames(data.frame(df[1], ceiling(dfRespHosp[2]*input$obs*(17/134))),c("Country","Avoided_Admissions"))
    # data is outdated and assumed 170 Mt methane between sims 1 and 2, really 134, so adjustment here * 170/134 (plus 1/10)

    world <- map_data("world")
    map.world_joined <- left_join(world, RespHospDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = Avoided_Admissions, tooltip
=sprintf("%s<br/>%s",region, Avoided_Admissions)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Costs Respiratory Hospitalizations Map

  #dfRespHosp <- data.frame(RespHosp65["Country"], RespHosp65["CasesMEANper10Mt"], RespHosp65["CostsMEAN2018USDperkt"])
  output$OzRespHosp65Cost <-renderggiraph({

    RespHospCostDF<-setNames(data.frame(df[1], ceiling(dfRespHosp[3]*input$obs*(1.7/1.34))),c("Country","thousands_USD"))

    world <- map_data("world")
    map.world_joined <- left_join(world, RespHospCostDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = thousands_USD, tooltip
=sprintf("%s<br/>%s",region, thousands_USD)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Wheat yield Map

  dfWheatYield <- data.frame(wheatYieldkt["Country"], wheatYieldkt["kt"])
  output$Wheat_kt <-renderggiraph({

    wheatDF<-setNames(data.frame(df[1], ceiling(dfWheatYield[2]*input$obs*(1/13.4))),c("Country","ktonnes"))

    world <- map_data("world")
    map.world_joined <- left_join(world, wheatDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = ktonnes, tooltip
=sprintf("%s<br/>%s",region, ktonnes)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Rice yield Map

  dfRiceYield <- data.frame(riceYieldkt["Country"], riceYieldkt["kt"])
  output$Rice_kt <-renderggiraph({

    riceDF<-setNames(data.frame(df[1], ceiling(dfRiceYield[2]*input$obs*(1/13.4))),c("Country","ktonnes"))

    world <- map_data("world")
    map.world_joined <- left_join(world, riceDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = ktonnes, tooltip
=sprintf("%s<br/>%s",region, ktonnes)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Maize yield Map

  dfMaizeYield <- data.frame(maizeYieldkt["Country"], maizeYieldkt["kt"])
  output$Maize_kt <-renderggiraph({

    maizeDF<-setNames(data.frame(df[1], ceiling(dfMaizeYield[2]*input$obs*(1/13.4))),c("Country","ktonnes"))

    world <- map_data("world")
    map.world_joined <- left_join(world, maizeDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = ktonnes, tooltip
=sprintf("%s<br/>%s",region, ktonnes)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Soy yield Map

  dfSoyYield <- data.frame(soyYieldkt["Country"], soyYieldkt["kt"])
  output$Soy_kt <-renderggiraph({

    soyDF<-setNames(data.frame(df[1], ceiling(dfSoyYield[2]*input$obs*(1/13.4))),c("Country","ktonnes"))

    world <- map_data("world")
    map.world_joined <- left_join(world, soyDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = ktonnes, tooltip
=sprintf("%s<br/>%s",region, ktonnes)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  # Delta Crop cost Map

  output$Crops_USD <-renderggiraph({

    cropDF<-setNames(data.frame(df[1], ceiling(input$obs*(1/13.4)*(dfWheatYield[2]*.210+dfRiceYield[2]*.421+dfMaizeYield[2]*.164+dfSoyYield[2]*.394))),c("Country","millions_USD"))

    world <- map_data("world")
    map.world_joined <- left_join(world, cropDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = millions_USD, tooltip
=sprintf("%s<br/>%s",region, millions_USD)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })


  # Delta Surface Temperature Map

  dfSAT <- data.frame(surfaceTemp["Country"],surfaceTemp["NationalAverageC"])
  output$dSAT_2040 <-renderggiraph({

    dfSAT[2][dfSAT[2]==0] <- NA
    satDF<-setNames(data.frame(df[1],dfSAT[2]*input$obs*(1/134)),c("Country","Degrees_C"))

    world <- map_data("world")
    map.world_joined <- left_join(world, satDF, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = Degrees_C, tooltip
=sprintf("%s<br/>%s",region, Degrees_C)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

})

