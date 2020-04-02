# Read files into global memory
countries_mmm <-read.csv("csvData/MMMresults.csv")
initialO3 <- read.csv("csvData/InitialO3.csv")
initial_resp<-read.csv("csvData/Initial_Resp.csv")
initial_cardio<-read.csv("csvData/Initial_Cardio.csv")
allagePop <-read.csv("csvData/AllAgePopulation.csv")
surfaceTemp <-read.csv("csvData/Country_Level_Temp_Change_MMM_1StandardErrors_50%_ValidDataEachCountry.csv")
nationalVSL <-read.csv("csvData/VSL2018USD.csv")
asthmaERV <-read.csv("csvData/AsthmaEV4Mar2020.csv")
wheatYieldkt <-read.csv("csvData/wheat_countryvalue_CropYieldsLoss_03202020.csv")

shinyServer(function(input, output){

cat("\nEXECUTION ", format(Sys.time(), "%a %b %d %X %Y"), "\n", file=stderr())
#cat("\n\nAAAAAAAAAAAAAAAAAAAA\n\n", file=stderr())
  #Environment variablesmor 
  EpiTMREL = 26.3
  EpiBeta = .01133
  EpiBetaCard = 0.00296
  #text render
  n = renderText({input$obs})
  output$caption <- renderText({
    input$obs
  })
  df<-data.frame(countries_mmm["Country"],countries_mmm["ANN_MDA8Sim2.Sim1.Diff."])
  dfO3 <-data.frame(initialO3["Country"],initialO3["MMM.Initial.O3..ppb."])
  #Ozone Delta Map

  output$ozoneCountry_2040 <- renderggiraph({
    world <- map_data("world")
    ozoneDF<-setNames(data.frame(df[1],df[2]*-1*input$obs*(1/134)),c("Country","OzoneReduction"))
    map.world_joined <- left_join(world, ozoneDF, by = c('region' = 'Country'))
    # using width="auto" and height="auto" to
    # automatically adjust the map size
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = OzoneReduction, tooltip
=sprintf("%s<br/>%s",region,OzoneReduction)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Surface Temperature Map

  dfSAT <- data.frame(surfaceTemp["Country"],surfaceTemp["NationalAverageC"])
  output$dSAT_2040 <-renderggiraph({
    world <- map_data("world")
    satDF<-setNames(data.frame(df[1],dfSAT[2]*input$obs*(1/134)),c("Country","AvoidedWarming"))
    map.world_joined <- left_join(world, satDF, by = c('region' = 'Country'))
    # using width="auto" and height="auto" to
    # automatically adjust the map size
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = AvoidedWarming, tooltip
=sprintf("%s<br/>%s",region,AvoidedWarming)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Wheat yield Map

  dfWheatYield <- data.frame(wheatYieldkt["Country"], wheatYieldkt["kt"])
  output$Wheat_kt <-renderggiraph({
    world <- map_data("world")
    wheatDF<-setNames(data.frame(df[1], dfWheatYield[2]*input$obs*(1/13.4)),c("Country","ktonnes"))
    map.world_joined <- left_join(world, wheatDF, by = c('region' = 'Country'))
    # using width="auto" and height="auto" to automatically adjust the map size
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = ktonnes, tooltip
=sprintf("%s<br/>%s",region, ktonnes)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Wheat cost Map

  output$Wheat_USD <-renderggiraph({
    world <- map_data("world")
    wheatDF<-setNames(data.frame(df[1], dfWheatYield[2]*input$obs*(1/13.4)*210),c("Country","thousands_USD"))
    map.world_joined <- left_join(world, wheatDF, by = c('region' = 'Country'))
    # using width="auto" and height="auto" to automatically adjust the map size
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = thousands_USD, tooltip
=sprintf("%s<br/>%s",region, thousands_USD)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Asthma-related ER visits Map

  dfAsthmaER <- data.frame(asthmaERV["Country"],asthmaERV["CasesMEANper10Mt"],asthmaERV["CostsMEAN2018USDperkt"])
  output$ozoneAsthmaER_2040 <-renderggiraph({
    world <- map_data("world")
    asthmaDF<-setNames(data.frame(df[1], dfAsthmaER[2]*input$obs*(17/134)),c("Country","AvoidedVisits"))
    # data is outdated and assumed 170 Mt methane between sims 1 and 2, really 134, so adjustment here * 170/134 (plus 1/10)
    map.world_joined <- left_join(world, asthmaDF, by = c('region' = 'Country'))
    # using width="auto" and height="auto" to automatically adjust the map size
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = AvoidedVisits, tooltip
=sprintf("%s<br/>%s",region, AvoidedVisits)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Costs Asthma-related ER visits Map

  #dfAsthmaER <- data.frame(asthmaERV["Country"],asthmaERV["CasesMEANper10Mt"],asthmaERV["CostsMEAN2018USDperkt"])
  output$ozoneAsthmaERCost_2040 <-renderggiraph({
    world <- map_data("world")
    asthmaDFcost<-setNames(data.frame(df[1], dfAsthmaER[3]*input$obs*(1.7/1.34)),c("Country","thousands_USD"))
    map.world_joined <- left_join(world, asthmaDFcost, by = c('region' = 'Country'))
    # using width="auto" and height="auto" to
    # automatically adjust the map size
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = thousands_USD, tooltip
=sprintf("%s<br/>%s",region, thousands_USD)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map()
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Total Resp Mortality Map
  
  dfResp <- data.frame(initial_resp["PopTimesBaseMort"],initial_resp["InitialMort"])
  #avoided deaths
  output$dMortRespCountry_2040 <-renderggiraph({    
    #below sets up the two columns of the truth statement for Mean AF
    meanAF<-data.frame((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2]))
    tempExpFrame<-data.frame(1-exp((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2])*-1*EpiBeta))
    #below is the Mean AF
    meanAF[2] <- ifelse(meanAF[1]<0,0,tempExpFrame[1])
    deathCol<-ceiling(-1*data.frame(meanAF[2])*dfResp[1]+dfResp[2])
    deathFram <- data.frame(df[1],deathCol)
    dfDeaths <- setNames(deathFram,c("Country","AvoidedDeaths"))
    
    #plot everything below
    world <- map_data("world")
    map.world_joined <- left_join(world, dfDeaths, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = AvoidedDeaths, tooltip=
sprintf("%s<br/>%s",region,AvoidedDeaths)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato",na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map() 
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Total Cardio Mortality Map
  
  dfCard <- data.frame(initial_cardio["PopTimesBaseMort"],initial_cardio["InitialMort"])
  #avoided deaths
  output$dMortCardCountry_2040 <-renderggiraph({    
    #below sets up the two columns of the truth statement for Mean AF
    meanAFc<-data.frame((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2]))
    tempExpFrame<-data.frame(1-exp((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2])*-1*EpiBetaCard))
    #below is the Mean AF
    meanAFc[2] <- ifelse(meanAFc[1]<0,0,tempExpFrame[1])
    deathCol<-ceiling(-1*data.frame(meanAFc[2])*dfCard[1]+dfCard[2])
    deathFram <- data.frame(df[1],deathCol)
    dfDeaths <- setNames(deathFram,c("Country","AvoidedDeaths"))
    
    #plot everything below
    world <- map_data("world")
    map.world_joined <- left_join(world, dfDeaths, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = AvoidedDeaths, tooltip=
                                                  sprintf("%s<br/>%s",region,AvoidedDeaths)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map() 
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Per Capita Resp Mortality Map
  
  dfPOP <- data.frame(allagePop["Country"],allagePop["Population"])
  output$dMortRespCountry_capita_2040 <-renderggiraph({
    
    #below sets up the two columns of the truth statement for Mean AF
    meanAF<-data.frame((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2]))
    tempExpFrame<-data.frame(1-exp((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2])*-1*EpiBeta))
    #below is the Mean AF
    meanAF[2] <- ifelse(meanAF[1]<0,0,tempExpFrame[1])
    deathCol<-ceiling((-1*data.frame(meanAF[2])*dfResp[1]+dfResp[2])/(dfPOP[2]/1000000))
    deathFram <- data.frame(df[1],deathCol)
    dfDeaths <- setNames(deathFram,c("Country","AvoidedDeaths"))
    
    #plot everything below
    world <- map_data("world")
    map.world_joined <- left_join(world, dfDeaths, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = AvoidedDeaths, tooltip=
sprintf("%s<br/>%s",region,AvoidedDeaths)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map() 
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Per Capita Cardio Mortality Map
  
  #avoided deaths
  output$dMortCardCountry_capita_2040 <-renderggiraph({    
    #below sets up the two columns of the truth statement for Mean AF
    meanAFc<-data.frame((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2]))
    tempExpFrame<-data.frame(1-exp((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2])*-1*EpiBetaCard))
    #below is the Mean AF
    meanAFc[2] <- ifelse(meanAFc[1]<0,0,tempExpFrame[1])
    deathCol<-ceiling((-1*data.frame(meanAFc[2])*dfCard[1]+dfCard[2])/(dfPOP[2]/1000000))
    deathFram <- data.frame(df[1],deathCol)
    dfDeaths <- setNames(deathFram,c("Country","AvoidedDeaths"))
    
    #plot everything below
    world <- map_data("world")
    map.world_joined <- left_join(world, dfDeaths, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = AvoidedDeaths, tooltip=
                                                  sprintf("%s<br/>%s",region,AvoidedDeaths)))
    gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map() 
    ggiraph(code = print(gg), width_svg=10)
  })

  #Delta Valuation of Reduced Mortality Map
  
  dfVSL <- data.frame(nationalVSL["Country"],nationalVSL["VSLmillionsUSD2018"])
  output$dMortCountry_VSL_2040 <-renderggiraph({
    
    #below sets up the two columns of the truth statement for Mean AF
    meanAF<-data.frame((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2]))
    meanAFc<-data.frame((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2]))
    tempExpFrame<-data.frame(1-exp((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2])*-1*EpiBeta))
    #below is the Mean AF for Resp
    meanAF[2] <- ifelse(meanAF[1]<0,0,tempExpFrame[1])
    tempExpFrameC<-data.frame(1-exp((data.frame((df[2]*input$obs*(1/134))-EpiTMREL)+dfO3[2])*-1*EpiBetaCard))
    #below is the Mean AF for Cardio
    meanAFc[2] <- ifelse(meanAFc[1]<0,0,tempExpFrameC[1])
    deathResp<-(-1*data.frame(meanAF[2])*dfResp[1])+dfResp[2]
    deathCard<-(-1*data.frame(meanAFc[2])*dfCard[1])+dfCard[2]
    deathCol<-ceiling((deathResp[1]+deathCard[1])*dfVSL[2])
    deathFram <- data.frame(df[1],deathCol)
    dfDeaths <- setNames(deathFram,c("Country","MillionsUSD"))
    
    #plot everything below
    world <- map_data("world")
    map.world_joined <- left_join(world, dfDeaths, by = c('region' = 'Country'))
    gg<-ggplot() + geom_polygon_interactive(data = map.world_joined, 
                                            aes(x = long, y = lat, group = group, fill = MillionsUSD, tooltip=
sprintf("%s<br/>%s",region,MillionsUSD)))
     gg<-gg+ scale_fill_gradient(low = "grey95", high = "tomato", na.value="white")
    gg<-gg+ coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    gg<-gg+theme_map() 
    ggiraph(code = print(gg), width_svg=10)
  })
})
#geom_sf will work for netcdf files likely.



