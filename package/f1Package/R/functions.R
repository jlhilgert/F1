getDriverStandings <- function(currentRound = "current", season = year) {

  if(currentRound == "current") {
    url <- "http://ergast.com/api/f1/current/driverStandings"
  } else {
    url <- paste("http://ergast.com/api/f1/", season, "/", currentRound,  "/driverStandings", sep = "")
  }

  temp <- xmlToList(rawToChar(GET(url)$content))

  round <- as.numeric(temp$StandingsTable$StandingsList$.attrs["round"])

  driver_standings <- data.frame()
  for(i in 1:(length(temp$StandingsTable$StandingsList)-1)) {
    driver_temp <- temp$StandingsTable$StandingsList[[i]]

    row_temp <- cbind(
      bind_rows(driver_temp$Driver[1:length(driver_temp$Driver)-1]),
      bind_rows(driver_temp$Driver$.attrs),
      bind_rows(driver_temp$Constructor[1:length(driver_temp$Constructor)-1]) %>%
        rename(Constructor = Name, TeamCountry = Nationality),
      bind_rows(driver_temp$Constructor$.attrs) %>%
        rename(TeamUrl = url),
      bind_rows(driver_temp$.attrs)
    )

    driver_standings <- rbind(driver_standings, row_temp)
  }

  driver_standings <- as.data.frame(driver_standings)

  driver_standings[,13:16] <- as.numeric(unlist(driver_standings[,13:16]))

  driver_standings <- driver_standings %>%
    mutate(FullName = paste(GivenName,FamilyName)) %>%
    relocate(FullName, .after = FamilyName) %>%
    mutate(ColorCode =
             ifelse(
               Constructor == "Ferrari", "#DC0000",
               ifelse(
                 Constructor == "Mercedes", "#00D2BE",
                 ifelse(
                   Constructor == "Red Bull", "#0600EF",
                   ifelse(
                     Constructor == "Alpine F1 Team", "#0090FF",
                     ifelse(
                       Constructor == "Haas F1 Team", "#999999",
                       ifelse(
                         Constructor == "Aston Martin", "#006F62",
                         ifelse(
                           Constructor == "AlphaTauri", "#2B4562",
                           ifelse(
                             Constructor == "McLaren", "#FF8700",
                             ifelse(
                               Constructor == "Alfa Romeo", "#900000",
                               ifelse(
                                 Constructor == "Williams", "#005AFF", NA
                               )
                             )
                           )
                         )
                       )
                     )
                   ))
               ))
    )
  return(driver_standings)
}

plotLapTimes <- function(currentRound) {
  p1 <- ggplotly(
    ggplot(lap_list %>% filter(Round == currentRound), aes(x = lap, y = position, fill = code, color = Constructor)) +
      geom_line() +
      scale_y_continuous(trans = "reverse", breaks = unique(lap_list$position[lap_list$Round == currentRound])) +
      constructer_scale_colors +
      labs(x = "Lap", y = "Position", color = "Driver")
  )

  p2 <- ggplotly(
    ggplot(lap_list %>% filter(Round == currentRound), aes(x = lap, y = timeSecs, fill = code, color = Constructor)) +
      geom_line() +
      constructer_scale_colors +
      labs(x = "Lap", y = "Lap time (sec)", color = "Driver")
  )

  return(list(p1, p2))
}

getConstructerScaleColors <- function() {
  constructer_list <- unique(driver_standings %>% select(Constructor, ColorCode))

  constructer_scale_colors <- scale_colour_manual(
    values = c(
      "Red Bull" = constructer_list$ColorCode[constructer_list$Constructor == "Red Bull"],
      "Ferrari" = constructer_list$ColorCode[constructer_list$Constructor == "Ferrari"],
      "Mercedes" = constructer_list$ColorCode[constructer_list$Constructor == "Mercedes"],
      "McLaren" = constructer_list$ColorCode[constructer_list$Constructor == "McLaren"],
      "Alpine F1 Team" = constructer_list$ColorCode[constructer_list$Constructor == "Alpine F1 Team"],
      "Alfa Romeo" = constructer_list$ColorCode[constructer_list$Constructor == "Alfa Romeo"],
      "Haas F1 Team" = constructer_list$ColorCode[constructer_list$Constructor == "Haas F1 Team"],
      "AlphaTauri" = constructer_list$ColorCode[constructer_list$Constructor == "AlphaTauri"],
      "Aston Martin" = constructer_list$ColorCode[constructer_list$Constructor == "Aston Martin"],
      "Williams" = constructer_list$ColorCode[constructer_list$Constructor == "Williams"]
    )
  )

  return(constructer_scale_colors)
}

plotGPMap <- function(round) {
  if(round == "Season") {
    lat <- 10
    long <- 20
    zoom <- 2
    weight <- 8
    GP_df <- grandPrixRounds
  } else {
    lat <- grandPrixRounds$lat[grandPrixRounds$Round == round]
    long <- grandPrixRounds$long[grandPrixRounds$Round == round]
    zoom <- 5
    weight <- 20
    GP_df <- grandPrixRounds[grandPrixRounds$Round == round,]
  }

  map <- leaflet() %>%
    setView(lat = lat, lng = long, zoom = zoom) %>%
    addTiles(group = "Map")  %>%
    addCircles(data = GP_df, lat = ~lat, lng = ~long, weight = weight,fillOpacity = 0.8, color = "red", popup = ~paste("Round ", Round, " - ", Locality, ", ", Country,  sep = ""), label = ~RaceName, group = "Grand Prix") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
    addProviderTiles("Stamen.Watercolor", group = "Watercolor") %>%
    addProviderTiles("CartoDB.DarkMatterNoLabels", group = "Dark") %>%
    addProviderTiles("Stamen.TerrainLabels", group = "Labels") %>%
    addLayersControl(
      baseGroups = c("Satelite","Map", "Dark", "Watercolor"),
      overlayGroups = c("Labels", "Grand Prix"),
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    )

  return(map)
}

