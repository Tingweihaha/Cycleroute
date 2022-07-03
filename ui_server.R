library(s2)
library(geosphere)
library(ggmap)
library(shiny)
library(shinyTime)
library(ggplot2)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(fontawesome)
library(mapsapi)
library(xml2)
library(summaryBox)
library(plotly)
library(googleway)
library(sf)
library(XML)
library(tidyr)
library(curl)
library(rvest)
library(dplyr)
library(stringr)
library(png)
library(patchwork)
library(egg)
library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)
library(shinydashboard)

theme <- bslib::bs_theme(version = 4)

# define UI
ui <- dashboardPage(skin='red', 
  dashboardHeader(title="Plan Your Route"), 
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Map and Route Details', tabName='map', icon=icon("globe", lib = "glyphicon")), 
      menuItem('Weather Forcast', tabName='weather', icon=icon("cloud", lib = "glyphicon")), 
      menuItem('Summary', tabName='summary', icon=icon("signal", lib = "glyphicon"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab 1: map and elevation
      tabItem(tabName = 'map', 
              fluidRow(
                column(width=8, 
                       leafletOutput(outputId = "map", height=500), 
                       br(),
                       plotlyOutput(outputId = "plot"),
                       br(),
                       infoBoxOutput(outputId = "Instruction", width=12)), 
                column(width=4, 
                       box(#width=NULL, height=NULL, 
                         textInput(inputId = 'origin', label = h3('Origin: Where you want to start your journey'), value = ""),
                         hr(), 
                         
                         textInput(inputId = 'destination', label = h3('Destination: Where you want to stop your journey'), value = ""),
                         hr(), 
                         
                         checkboxGroupInput(inputId = 'checkGroup', label = h3("Amenities you want to visit en route"), 
                                            choices = list(
                                              "cafe"="cafe",
                                              "convenience_stores"="convenience_stores",
                                              "gas_stations"="gas_stations",
                                              "meal_takeaway"="meal_takeaway", 
                                              "park"="park",
                                              "shopping_mall"="shopping_mall",
                                              "tourist_attraction"="tourist_attraction"), 
                         ),
                         hr(),
                        
                         h3("How long you plan to finish your journey (in hours & mins)"),
                         fluidRow(
                           column(5,
                                  numericInput(inputId = "NumHours", label = "No. of Hours", value = 0, min = 0, max = 24)),
                           column(5,
                                  numericInput(inputId = "NumMins", label = "No. of Minutes", value = 0, min = 0, max = 59))),
                         textInput(inputId = 'weight', label = h3('Your weight is (in kilograms)'), value = ""), submitButton("Confirm"),
                         hr(),
                          
                         width = 12)
              ))
    ),
    
    # Tab 2: weather forecast
    tabItem(tabName='weather', 
            fluidRow(
              infoBoxOutput(outputId = "Reminder", width=12),
              plotOutput(outputId = "uv"),
              plotOutput(outputId = 'temperature'), 
              plotOutput(outputId = 'humid.rain'))),
    
    # Tab 3: summary
    tabItem(tabName='summary', 
            fluidRow(
              infoBoxOutput(outputId='heythere', width=12), 
              br(),
              infoBoxOutput(outputId = "origin"), 
              infoBoxOutput(outputId = "distance"), 
              infoBoxOutput(outputId = "speed"), 
              infoBoxOutput(outputId = "destination"), 
              infoBoxOutput(outputId = "duration"), 
              infoBoxOutput(outputId = "calories"), 
              infoBoxOutput(outputId = "weight"), 
              infoBoxOutput(outputId = "amenities", width=8)))
    

    )
  )
)

# define server logic
server <- function(input, output, session){
  
  m <- leaflet() %>%
    addTiles()
  
  trigger.map.and.elevation <- reactive({
    
    origin.input <- input$origin
    destination.input <- input$destination
    checkGroup.input <- input$checkGroup
    
    # function to get the nearest location when supplied a vector of long and lat
    nearest <- function(curr.long.lat, next.loc){ # curr.long.lat is a vector of the current location, next.loc is a character of the next target location e.g. "gas stations"
      
      # read csv of next.loc
      next_loc <- read.csv(paste0(getwd(),
                                  "/Data Files/",
                                  gsub(" ",
                                       "",
                                       paste(gsub(" ",
                                                  "_",
                                                  next.loc),
                                             ".csv")),
                                  sep = ""))
      
      # get the long and lat of closest point identified
      curr.long.latv <- s2_lnglat(curr.long.lat[1],
                                  curr.long.lat[2]) 
      target.loc.long.lat <- s2_lnglat(next_loc$geometry.location.lng,
                                       next_loc$geometry.location.lat)
      closest.long.lat <- s2_closest_feature(curr.long.latv,
                                             target.loc.long.lat)
      closest.coordinate <- c(next_loc[closest.long.lat, "geometry.location.lng"],
                              next_loc[closest.long.lat, "geometry.location.lat"])
      
      print("closest coordinate in nearest")
      print(closest.coordinate)

      # get the name and details of the place
      placedetails <- paste(sep = "<br/>",
                            '<strong>', next_loc[closest.long.lat, "name"], '</strong>',
                            paste("Address:" ,next_loc[closest.long.lat, "vicinity"]),
                            paste0("Rating: ", next_loc[closest.long.lat, "rating"], "/5.0"))
      
      place_info <- list(placedetails,
                         as.numeric(closest.coordinate))

      return (place_info) # returns a list with place name and a vector of the long and lat of the closest next location, to be supplied to the mp_directions function
    }
    
    # create matrix of intermediate locations
    nearest.loc.mat <- function(origin, ...){
      
      locations <- unlist(list(...))
      output <- list()
      output.rownames <- c()
      
      if (length(locations) != 0){
        
        for (i in 1:length(locations)){

          inter <- c(origin[i, ])
          closest.amen <- nearest(inter, locations[i])
          closest.amen.long.lat <- closest.amen[2]
          closest.amen.name <- closest.amen[1]
          output.rownames <- c(output.rownames, closest.amen.name)
          output <- append(output, closest.amen.long.lat)

        }
        
        output <- matrix(unlist(output),
                         ncol = 2,
                         byrow = T)
        
        colnames(output) <- c("longitude", "latitude")
        rownames(output) <- output.rownames
        
      } else {
        
        output <- 0
        
      }
      
      return (output)
    }
    
    # final run function
    intermediate.loc.mat <- function(origin, destination, ...){
      
      locations <- list(...)
      n <- length(unlist(locations))
      search_points <- gcIntermediate(origin, destination, n)
      output <- nearest.loc.mat(search_points, locations)

      return (output)
    }
    
    # define origin
    origin <- mp_geocode(origin.input,
                         region = 'sg',
                         postcode = NULL,
                         key = key,
                         quiet = F,
                         timeout = 10)
    
    origin <- mp_get_points(origin)
    origin <- c(origin$pnt$`1`[1], origin$pnt$`1`[2])
    
    # define destination
    destination <- mp_geocode(destination.input,
                              region = 'sg',
                              postcode = NULL,
                              key = key,
                              quiet = F,
                              timeout = 10)
    
    destination <- mp_get_points(destination)
    destination <- c(destination$pnt$`1`[1], destination$pnt$`1`[2])
    
    # define intermediate points
    if (is.null(origin) | is.null(destination)){
      
      waypoints <- 0
      
    } else {
      
      waypoints <- intermediate.loc.mat(origin, destination, checkGroup.input)
  
    }

    # optimise route based on identified points
    if (is.null(origin) | is.null(destination)){
      
      d <- 0
      
    } else if (length(checkGroup.input) == 0){
      
      doc <- mp_directions(origin = origin,
                           destination = destination,
                           alternatives = F,
                           key = key,
                           mode = "bicycling",
                           quiet = F)
      
      doc <- as_xml_document(doc)
      d <- mp_get_routes(doc)
      segments <- mp_get_segments(doc)
      waypoints <- 0
      
    } else {
      
      print("waypoints in else")
      print(waypoints)
      doc <- mp_directions(origin = origin,
                           waypoints = waypoints,
                           destination = destination,
                           alternatives = F,
                           key = key,
                           mode = "bicycling",
                           quiet = F)
      
      doc <- as_xml_document(doc)
      d <- mp_get_routes(doc)
      segments <- mp_get_segments(doc)
      
    }
    
    # plot route on leaflet
    if (is.null(origin) | is.null(destination)){
      
      m <- 0
      
    } else {

      instructions <- data.frame(segments$instructions)
      coords <- data.frame(st_coordinates(segments$geometry)) %>% group_by(L1) %>% slice_head()
      instructions.final <- cbind(coords, instructions) %>% select(-L1)
      
      m <- addPolylines(m,
                        data = d,
                        opacity = 1, weight = 4, color = '#1362e8') %>%
        addCircleMarkers(data = instructions.final,
                         lng = ~X, lat = ~Y,
                         popup = ~segments.instructions,
                         color = "#8b0000", radius = 5)
    }

    # add markers
    addLocationMarkers <- function(map, origin.v, origin.c, waypoints.m, destination.v, destination.c){
      
      iconSet <- awesomeIconList(
        home = makeAwesomeIcon(text = fa("home"), iconColor = "white", markerColor = "green"),
        dest = makeAwesomeIcon(text = fa("circle"), iconColor = "white", markerColor = "green"),
        cafe = makeAwesomeIcon(icon = 'coffee', library = 'ion', iconColor = 'white', markerColor = 'brown'),
        meal_takeaway = makeAwesomeIcon(icon = 'cutlery', library = 'glyphicon', iconColor = 'rgb(192, 255, 0)', markerColor = 'darkpurple'),
        convenience_stores = makeAwesomeIcon(icon = 'cart-arrow-down', library = 'fa', iconColor = 'white', markerColor = 'green'),
        park = makeAwesomeIcon(icon = 'tree', library = 'fa', iconColor = 'purple', markerColor = 'yellow'),
        shopping_mall = makeAwesomeIcon(icon = 'shopping-cart', library = 'fa', iconColor = 'white', markerColor = 'pink'),
        tourist_attraction = makeAwesomeIcon(icon = 'industry', library = 'fa', iconColor = 'white', markerColor = 'yellow'),
        gas_stations = makeAwesomeIcon(icon = 'fire', library = 'fa', iconColor = 'black', markerColor = 'orange'),
        train_station = makeAwesomeIcon(icon = 'train', library = 'fa', iconColor = 'white', markerColor = 'darkpurple')
      )
      
      if (length(waypoints.m) == 1){
        
        m <- rbind(origin.v,
                   destination.v)

        colnames(m) <- c("longitude", "latitude")
        
      } else {
        
        m <- rbind(rbind(origin.v,
                         waypoints.m),
                   destination.v)
        
      }
      
      # change the row names of origin and destination
      rownames(m)[1] <- paste0('<strong>', "Start: ", '</strong>',
                               origin.c)
      rownames(m)[nrow(m)] <- paste0('<strong>', "Destination: ", '</strong>',
                                     destination.c)
      
      m <- asplit(m, 2)
      
      longitudes <- m$longitude
      latitudes <- m$latitude
      
      for (i in 1:length(longitudes)){
        if (i == 1){
          
          map <- map %>%
            addAwesomeMarkers(lng = longitudes[i],
                              lat = latitudes[i],
                              popup = names(longitudes[i]),
                              icon = iconSet["home"])
          
        } else if (i == length(longitudes)) {
          
          map <- map %>%
            addAwesomeMarkers(lng = longitudes[i],
                              lat = latitudes[i],
                              popup = names(longitudes[i]),
                              icon = iconSet["dest"])
          
        } else {
          
          map <- map %>%
            addAwesomeMarkers(lng = longitudes[i],
                              lat = latitudes[i],
                              popup = names(longitudes[i]),
                              icon = iconSet[checkGroup.input[i - 1]])
        }
      }
      
      return (map)
    }
    
    route_points <- function(doc){
      #this function takes in a xml doc of a route
      segments <- mp_get_segments(doc)
      distances <- segments$distance_m
      
      segments_df <- as.data.frame(st_coordinates(segments$geometry))
      
      
      segments_dist_df <- segments_df %>% group_by(L1) %>% summarise(Count = n())
      
      segments_dist_df <- bind_cols(segments_dist_df, distances)
      
      colnames(segments_dist_df) <- c("L1", "Count", "Cumulative_Distance")
      
      segments_dist_df$indiv_dist <- round(segments_dist_df$Cumulative_Distance / segments_dist_df$Count, 2)
      
      route_points <- as.data.frame(st_coordinates(segments$geometry))
      
      route_points <- left_join(route_points, segments_dist_df, by = 'L1') %>% 
        mutate(Total_Distance_metres = round(cumsum(indiv_dist), 0))
      
      colnames(route_points) <- c("lon", "lat", "L1", "Count", "Cumulative_Distance", "Indiv_Dist", "Total_Distance_metres")
      
      return(route_points)
    }
    
    # Elevation function
    elevation_fn <- function(df){
      # df MUST have columns with Latitude and Longitude column names!!!
      # df <- route_points
      # check for empty df
      # df <- route_points
      if (nrow(df) == 0){
        
        return(FALSE)
        
      } else {
        
        df <- df[, c("lon", "lat")] 
        elevation_df <- data.frame() #empty df
        
        #google elevation takes in max 500 points
        #find the number of times we need to get 500 elevations
        
        count <- ceiling(nrow(df) / 200)
        nrow_initial_df <- nrow(df)
        initial_row <- 1
        max_row <- ifelse(nrow(df) >= 200, 200, nrow(df))
        
        for (i in 1:count){
          
          rows_to_elevate <- df[initial_row:max_row, ]
          df_sliced <- df[(max_row + 1):nrow(df), ]
          
          elevation_data <- google_elevation(df_locations = rows_to_elevate,
                                             location_type = "path",
                                             samples = nrow(rows_to_elevate),
                                             simplify = T,
                                             key = key)
          Sys.sleep(0.1)
          
          elevation_to_bind <- as.data.frame(round(elevation_data$results$elevation, 1))
          elevation_df <- bind_rows(elevation_df, elevation_to_bind)
          
          initial_row <- max_row + 1
          max_row <- max_row + 200
          max_row <- ifelse(max_row >= nrow_initial_df,
                            nrow_initial_df,
                            max_row)
          
          i <- i + 1
        }
      }
      
      elevation_df$point_number <- seq.int(nrow(elevation_df)) # creates a column of row numbers in the df
      return(elevation_df)
    }
    
    # functions to derive route df and main df.
    # route df is used to plot elevation data,
    # main df is used to plot geom_points on elevation data.
    
    # function to get df used to plot elevation
    route_df_to_plot <- function(route_points){
      
      #this function takes in the route_points df and returns the df that should be used for plotting
      route_points$point_number <- seq.int(nrow(route_points)) #return row number in a column
      elevation_df <- elevation_fn(route_points)
      Sys.sleep(0.1)
      route_df <- inner_join(elevation_df,
                             route_points,
                             by = 'point_number')
      #route_df <- left_join(route_df, main_points, by='point_number')
      colnames(route_df) <- c("elevation", "point_number", "lon", "lat", "L1", "Count", 
                              "Cumulative_Distance", "Indiv_Dist", "Total_Distance_metres")  
      
      return(route_df)
    }
    
    # function to get df used to plot main names
    main_df_to_plot <- function(route_df, origin, intermediates, destination){
      if (intermediates == 0){
        
        main_points <- data.frame(rbind(origin,
                                        destination))
        main_points$name <- rownames(main_points)
        point_number_vector <- c(1, nrow(route_df))
        
      } else {
        
        main_points <- data.frame(rbind(rbind(origin,
                                              intermediates),
                                        destination))
        main_points$name <- rownames(main_points)
        point_number_vector <- c()
        route_df.lnglat <- s2_lnglat(route_df$lon, route_df$lat) # convert points to s2 object
        
        for (i in 1:nrow(main_points)){
          
          main.point.to.search <- s2_lnglat(main_points$lon[[i]], main_points$lat[[i]])
          point_number <- as.integer(s2_closest_feature(main.point.to.search, route_df.lnglat))
          point_number_vector <- append(point_number_vector, point_number)
          
        }
      }
      
      main_points <- bind_cols(main_points, point_number_vector)
      colnames(main_points) <- c("lon", "lat", "name", "point_number")
      main_points <- left_join(main_points, route_df, by = 'point_number')
      
      return(main_points)
    }
    
    if (is.null(origin) | is.null(destination)){
      
      route_df <- 0
      
    } else {
      route_points <- route_points(doc)
      route_df <- route_df_to_plot(route_points)
      main_df <- main_df_to_plot(route_df, origin, waypoints, destination)
      main_df[1, 'Total_Distance_metres'] <- 0 # set origin distance to 0
      
      route_df <- left_join(route_df,
                            main_df[, c('point_number', 'name')],
                            by = "point_number")
      route_df[is.na(route_df$name), ]$name <- ''
      
      route_df[1, ]$Total_Distance_metres <- 0
      
      slider_formatter <- function(x){
        dplyr::case_when(
          x < 1e3 ~ paste0(as.character(x), "M"),
          x < 1e5 ~ paste0(as.character(round(x/1e3, 1)), "KM"),
          TRUE ~ paste0(as.character(round(x/1e3)), "KM")
        )
      }
      
      route_df <- route_df %>% mutate(levels_formatted = slider_formatter(Total_Distance_metres))
    }
    
    if (is.null(origin) | is.null(destination)){
      final.map <- 0
    } else {
      final.map <- addLocationMarkers(m, origin, origin.input, waypoints, destination, destination.input)
    }
    
    plot_list <- list(waypoints, # for summary
                      final.map, # for main leaflet plot
                      route_df # for elevation plot
    )
    
    return (plot_list)
    
    }
  )
  
  url <- "https://weather.com/weather/today/l/1.29,103.85?par=google"
  page <- read_html(url)
  default_site <- "https://weather.com"
  hourly_href <- page %>% html_nodes(".styles--active--3X9QA+ .Button--default--3zkvy") %>% html_attr("href")
  hourly_url <- paste0(default_site, hourly_href)
  
  # reading hourly page
  hourly_page <- read_html(hourly_url)
  
  # Timing (exclude date)
  hourly_data_hr <- hourly_page %>% html_nodes(".DetailsSummary--daypartName--2FBp2") %>% html_text()
  
  # temperature
  hourly_data_temp <- hourly_page %>% html_nodes(".DetailsSummary--tempValue--1K4ka") %>% html_text()
  
  # hourly chance of rain
  hourly_data_rain <- hourly_page %>% html_nodes(".DetailsSummary--precip--1ecIJ") %>% html_text()
  
  # Weather forecast
  hourly_data_w <- hourly_page %>% html_nodes(".DetailsSummary--extendedData--365A_") %>% html_text()
  hourly_data_w <- hourly_data_w[seq(1, length(hourly_data_w), by = 2)]
  
  # Block data
  hourly_data_s <- hourly_page %>% html_nodes(".DetailsTable--DetailsTable--142jU") %>% html_text2()
  
  # Splitting data by "\n"
  col_data <- strsplit(hourly_data_s, split = "\\n")
  
  # colnames
  col_names <- col_data[[1]][seq(1, length(unlist(col_data[1])), by =2)]
  
  # col data
  col_data2 <- lapply(col_data, function(x) x[seq(0, length(unlist(col_data[1])), by = 2)])
  
  df <- as.data.frame(t(as.data.frame(col_data2)))
  colnames(df) <- col_names
  rownames(df) <- NULL
  
  # formatting data
  for (i in 1:ncol(df)){
    if (colnames(df)[i] != "UV Level"){
      
      df[ , i] <- readr::parse_number(df[ , i])
      
    } else {
      
      df[ , i] <- str_split_fixed(df[ , i], "UV Index", n = 2)
      df[ , i] <- df[ , i][, 2]
      
    }
  }
  
  df_1 <- data.frame(Hours = hourly_data_hr,
                     Temperature = as.integer((readr::parse_number(hourly_data_temp) - 32) * (5/9)),
                     `Chance of Rain` = readr::parse_number(hourly_data_rain),
                     Status = hourly_data_w)
  
  # merging both files
  df <- cbind(df_1, df)
  
  # date
  date_data <- hourly_page %>% html_nodes(".HourlyForecast--longDate--1tdaJ") %>% html_text()
  
  count <- 1
  
  for (i in 1:nrow(df)){
    
    if((df[i, "Hours"] == "12 am" & i > 1)){
      count <- count + 1
    }
    
    # Add date
    df[i, "Date"] <- date_data[count]
  }
  
  # convert to degree celsius
  df$`Feels-Like Temperature` <- as.integer((df$`Feels-Like Temperature` - 32) * (5/9))
  
  df <- df %>% separate(col = "UV Level",
                        into = c("UV.score", "dummy", "max"),
                        sep = " ",
                        fill = "right") %>% mutate(UV.score = ifelse(UV.score == "Extreme", 11, UV.score))
  
  final.weather.df <- df[1:10,]
  final.weather.df$dummy <- ""
  final.weather.df$Hours <- factor(final.weather.df$Hours, levels = final.weather.df[["Hours"]])
  
  # font size for weather forecast plots
  geom.text.size = 5.5
  theme.size = (14/5) * geom.text.size
  
  # map
  output$map <- renderLeaflet(
    {
      if (input$origin == "" | input$destination == ""){
        
        m <- leaflet() %>% setView(lat = 1.356660, lng = 103.835381,
                                   zoom = 12) %>% 
          addTiles() %>%
          addMarkers(lat = 1.356660, lng = 103.835381, label = "Singapore")
        
      } else {
        
        main.map <- trigger.map.and.elevation()
        
        m <- main.map[[2]]
        
        if (class(m) == "numeric"){
          
          m <- leaflet() %>% setView(lat = 1.356660, lng = 103.835381,
                                     zoom = 12) %>% 
            addTiles() %>%
            addMarkers(lat = 1.356660, lng = 103.835381, label = "Singapore")

        }
      }
      
      return (m)
    }
  )
  
  
  output$plot <- renderPlotly(
    {
      if (input$origin == "" | input$destination == ""){
        
        fig <- plotly_empty() %>%
          layout(title = "Please input your starting point and destination!",
                 plot_bgcolor = "#ECF0F5",
                 paper_bgcolor = "#ECF0F5")
        
        return (fig)
        
      } else {
        
        accumulate_by <- function(dat, var) {
          var <- lazyeval::f_eval(var, dat)
          lvls <- plotly:::getLevels(var)
          dats <- lapply(seq_along(lvls),
                         function(x) { cbind(dat[var %in% lvls[seq(1, x)], ],
                                             frame = lvls[[x]])
                         }
          )
          
          dplyr::bind_rows(dats)
          
        }
        
        route_df <- trigger.map.and.elevation()[[3]]
        
        if (class(route_df) == "numeric"){
          
          print(route_df)
          fig <- plotly_empty() %>%
            layout(title = "Please input valid place names!",
                   plot_bgcolor = "#ECF0F5",
                   paper_bgcolor = "#ECF0F5")
          
          return (fig)
          
        } else {
          
          fig <- plot_ly(data = route_df,
                         x = ~Total_Distance_metres, 
                         y = ~elevation, 
                         type = 'scatter', 
                         mode = 'lines', 
                         line = list(color = 'rgba(255, 168, 108, 1)'),
                         fill = 'tozeroy',
                         fillcolor = list(color = 'rgba(255, 168, 108, 0.3)'),
                         text = ~levels_formatted, 
                         hovertemplate = paste(
                           "<b>Distance</b>: %{x}m <br>",
                           "<b>Elevation</b>: %{y}m <br>",
                           "<extra></extra>")) %>%
            layout(hovermode = "x unified",
                   plot_bgcolor = "#ECF0F5",
                   paper_bgcolor = "#ECF0F5")
          
          fig <- fig %>%
            layout(title = "Elevation along your journey",
                   yaxis = list(title = "Elevation", 
                                zeroline = F,
                                tickprefix = ""),
                   xaxis = list(title = "Distance", 
                                tickprefix = "",
                                zeroline = F, 
                                showgrid = F
                   )
            )
          
          fig %>% animation_opts(frame = 100, 
                                 transition = 0, 
                                 redraw = F
          )
          
          return (fig)
          
          }
      }
    }
  )
  
  output$Instruction <- renderInfoBox(
    {       
      fluidRow(
        infoBox("Instructions:", 
                value ="Hover over the graph to view exact distances and elevations points within your route!",
                width = 12, color = "navy", icon=icon("book", lib="glyphicon"))
      )
    }
  )
  
  output$heythere <- renderInfoBox({
    infoBox("Heythere", "Change the duration of your trip if you'd like to!",color = "navy", icon=icon("edit", lib="glyphicon"))
  })
  
  output$origin <- renderInfoBox({
    infoBox("Origin of Trip",input$origin, color = "light-blue", icon=icon("map-marker", lib="glyphicon"))
  })
  
  output$destination <- renderInfoBox({
    infoBox("Destination of Trip",input$destination, color = "light-blue", icon=icon("flag", lib="glyphicon"))
  })
  
  output$distance <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    route_df <- data[[3]]
    
    distance <- route_df[nrow(route_df), "Total_Distance_metres"]
    
    infoBox("Distance of Trip",ifelse(distance>1000, paste(distance/1000, 'km'), paste(distance, 'meters')), 
            color = "green", icon=icon("transfer", lib="glyphicon"))
  })
  
  output$duration <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    route_df <- data[[3]]
    
    distance <- route_df[nrow(route_df), "Total_Distance_metres"]
    
    duration <- (input$NumHours*60 + input$NumMins)*60
    
    infoBox("Duration of Trip",paste(duration%/%3600, 'Hrs', duration%%3600/60, 'Mins'), color = "green", icon=icon("time", lib="glyphicon"))
  })
  
  output$speed <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    route_df <- data[[3]]
    
    distance <- route_df[nrow(route_df), "Total_Distance_metres"]
    duration <- (input$NumHours*60 + input$NumMins)*60
    weight <- input$weight
    speed <- distance/duration
  
    infoBox("Recommended Speed for Your Trip", p(p(paste(round(speed * 3.6, 2), 'Km/hr'))), color = "teal", icon=icon("forward", lib="glyphicon"))
  })
  
  output$calories <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    route_df <- data[[3]]
    
    distance <- route_df[nrow(route_df), "Total_Distance_metres"]
    duration <- (input$NumHours*60 + input$NumMins)*60
    weight <- input$weight
    speed <- distance/duration
    mph <- speed * 2.236936
    
    MET <- dplyr::case_when(mph < 10 ~ 4,
                            mph < 12 ~ 6.8,
                            mph < 14 ~ 8,
                            mph < 16 ~ 10,
                            mph >= 16 ~ 12)
    
    calories <- MET * as.numeric(weight) * as.numeric(duration/3600)
    
    infoBox("Estimated Calories Burnt", paste(round(calories,0), "cals"), color = "teal", icon=icon("fire", lib="glyphicon"))
  })
  
  
  output$weight <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    route_df <- data[[3]]
    
    distance <- route_df[nrow(route_df), "Total_Distance_metres"]
    duration <- (input$NumHours*60 + input$NumMins)*60
    weight <- input$weight
    speed <- distance/duration
    mph <- speed * 2.236936
    
    MET <- dplyr::case_when(mph < 10 ~ 4,
                            mph < 12 ~ 6.8,
                            mph < 14 ~ 8,
                            mph < 16 ~ 10,
                            mph >= 16 ~ 12)
    
    calories <- MET * as.numeric(weight) * as.numeric(duration/3600)
    weightloss <- calories/7700
    
    infoBox("Weight lost", paste(round(weightloss, 3), "kilograms"), color = "light-blue", icon=icon("scale", lib="glyphicon"))
  })
  
  
  output$amenities <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    
    amenities <- strsplit(rownames(way.points), split = "<br/>")
    
    amenity.c <- ''
    
    for (amenity in amenities){
      amenity.c <- p(amenity.c,
                     p(amenity[[2]]))
    }
    
    infoBox("Amenities passed by", amenity.c, width=NULL, color = "green", icon=icon("screenshot", lib="glyphicon"))
  })
  
  output$Reminder <- renderInfoBox(
    {       
      infoBox("This is a 10-hr weather forecast!", 
              value = p(p("1) Avoid high UV levels for better health!"),
                        p("2) Plan your trip for some other time if the weather in the coming few hours is forecasted to be undesirable!"),
                        width=12, color = "navy", icon=icon("th-list", lib="glyphicon")),
      )
    }
  )
  
  output$humid.rain <- renderPlot(
    {
      humid.rain <- ggplot(final.weather.df) +
        geom_area(aes(x = Hours, y = Chance.of.Rain), stat = "identity", group = 1, alpha = 0.5, fill = "#69b3a2") +
        geom_line(aes(x = Hours, y = Chance.of.Rain, linetype = 'Chance of Rain'), stat = "identity", group=1) +
        geom_point(aes(x = Hours, y = Chance.of.Rain), stat = "identity", group = 1, size = 2) +
        geom_area(aes(x = Hours, y = Humidity), stat = "identity", group = 1, alpha = 0.3, fill = "paleturquoise1") +
        geom_line(aes(x = Hours, y = Humidity, linetype = 'Humidity'), stat = "identity", group = 1) +
        geom_point(aes(x = Hours, y = Humidity), stat = "identity", group = 1, size = 2) +
        geom_vline(xintercept = final.weather.df$Hours,
                   linetype = "longdash",
                   alpha = 0.3,
                   colour = "grey") +
        geom_text(aes(x = Hours, y = Chance.of.Rain, label = Chance.of.Rain), position = position_dodge(0.9), vjust = -1, size = geom.text.size) +
        geom_text(aes(x = Hours, y = Humidity, label = Humidity), position = position_dodge(0.9), vjust = -1, size = geom.text.size) +
        ylim(c(0, 100)) +
        theme_classic() +
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "bottom",
              axis.text = element_text(size = theme.size, colour = "black"),
              legend.text = element_text(size = theme.size - 2, colour = "black"),
              axis.title = element_text(size = theme.size, colour = "black")) +
        labs(x = "Hour",
             y = "Humidity and Chance of Rain (%)",
             linetype = "") 
      
      return (humid.rain)
    }
  )
  
  output$temperature <- renderPlot(
    {
      status.v <- final.weather.df[, "Status"]
      
      temperature <- ggplot(final.weather.df) +
        geom_line(aes(x = Hours, y = Temperature, linetype = "Actual Temperature"), stat = "identity", group = 1) +
        geom_point(aes(x = Hours, y = Temperature), stat = "identity", group = 1) +
        geom_line(aes(x = Hours, y = `Feels-Like Temperature`, linetype = 'Feels-like Temperature'), stat = "identity", group = 1) +
        geom_point(aes(x = Hours, y = `Feels-Like Temperature`), stat = "identity", group = 1) +
        geom_vline(xintercept = final.weather.df$Hours,
                   linetype = "longdash",
                   alpha = 0.3, colour = "grey") +
        geom_text(aes(x = Hours, y = Temperature, label = Temperature), position = position_dodge(0.9), vjust = -1, size = geom.text.size) +
        geom_text(aes(x = Hours, y = `Feels-Like Temperature`, label = `Feels-Like Temperature`), position = position_dodge(0.9), vjust = -1, size = geom.text.size) +
        theme_classic() +
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank(),
              legend.position = "bottom",
              axis.text = element_text(size = theme.size, colour = "black"),
              legend.text = element_text(size = theme.size - 2, colour = "black"),
              axis.title.y = element_text(size = theme.size, colour = "black"),
              axis.title.x = element_blank()) +
        ylim(min(c(final.weather.df$Temperature, final.weather.df$`Feels-Like Temperature`)) - 1,
             max(c(final.weather.df$Temperature, final.weather.df$`Feels-Like Temperature`)) + 4) +
        ylab("Temperature (Degree Celsius)") +  
        labs(linetype = '')
      
      for (i in 1:length(status.v)){
        weather <- as.character(status.v[i])
        weather.icon <- readPNG(paste0(getwd(),
                                       "/images/",
                                       weather,
                                       ".png"),
                                native = T)
        factor = i/10
        left_interval = 0.115
        right_interval = 0.02
        temperature <- temperature +
          inset_element(p = weather.icon,
                        left = ifelse((i == 1),
                                      factor - 0.098,
                                      ifelse((i > 5),
                                              factor - 0.125,
                                              factor - left_interval)),
                        bottom = 0.82,
                        right = factor + right_interval,
                        top = 1)
      }
      return (temperature)
      
      }
    )
  
  output$uv <- renderPlot(
    {
      uv <- ggplot(final.weather.df) +
        geom_point(aes(x = Hours, y = dummy, size = readr::parse_number(UV.score), color = readr::parse_number(UV.score), fill = readr::parse_number(UV.score)), shape = 16) + 
        scale_color_continuous(low = "violetred1", high = "violetred4", breaks = c(0:11)) +
        scale_size(breaks = c(0:11), range = c(1,30)) +
        geom_text(aes(x = Hours, y = dummy, label = UV.score), position = position_dodge(0.9), vjust = -3.5, size = geom.text.size) +
        theme_classic() +
        theme(axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank(),
              plot.margin = unit(c(3.5,1,3.5,1), 'cm'),
              legend.position = "none",
              axis.text = element_text(size = theme.size, colour = "black"),
              legend.text = element_text(size = theme.size - 2, colour = "black"),
              axis.title = element_text(size = theme.size, colour = "black")) +
        geom_vline(xintercept = final.weather.df$Hours,
                   linetype = "longdash",
                   alpha = 0.3,
                   colour = "grey") +
        ylab("UV Score\n(out of 10)")
      
      return (uv)
      }
    )
}

shinyApp(ui = ui, server = server)
