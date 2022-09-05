library(httr)
library(XML)
library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(phyloseq)
library(data.table)
library(plotly)

Sys.setenv(LANG = "en")

#### OLD ####

lapDF <- data.frame()
for (i in 1:100) {
  url <- paste("http://ergast.com/api/f1/2022/last/laps/",i, sep = "")
  
  raw <- GET(url)
  content <- rawToChar(raw$content)
  temp <- xmlToList(content)
  
  if(class(temp[["RaceTable"]][["Race"]]) != "NULL"){
    lap <- temp[["RaceTable"]][["Race"]][["LapsList"]][["Lap"]]
    
    tempDF <- data.frame()
    for(j in 1:(length(lap)-1)) {
      ind_lap <- as.data.frame(t(lap[[j]]))
      ind_lap$Min <- str_split(ind_lap$time, ":")[[1]][1]
      ind_lap$Sec <- str_split(ind_lap$time, ":")[[1]][2]
      tempDF <- rbind(tempDF, ind_lap)
    }
    
    lapDF <- rbind(lapDF, tempDF)
    
    Sys.sleep(0.2)
    
  }
  
  if(i == 100) {
    lapDF <- lapDF %>%
      mutate(Min = as.numeric(Min), Sec = as.numeric(Sec), lap = as.numeric(lap)) %>%
      mutate(lap_time = duration(minute = Min, second = Sec))
  }
}


lapDF <- lapDF %>%
  mutate(lap_time = duration(minute = Min, second = Sec))

lapDF_min <- lapDF %>%
  group_by(driverId) %>%
  summarise(lap_time = min(lap_time))

driver_order <- lapDF_min$driverId[order(lapDF_min$lap_time,decreasing = T)]

lapDF$driverId <- factor(lapDF$driverId, levels=driver_order)

ggplot(lapDF, aes(lap, driverId, fill= lap_time)) + 
  geom_tile() +
  scale_fill_gradient2(low="red", mid = "yellow", high="blue", midpoint = (mean(lapDF$lap_time) + mean(lapDF$lap_time)/4)) +
  labs(x = "Lap", y = "Drivers", fill = "Seconds") +
  theme_ipsum()
  
#### NEW ####

# Driver info (all time) - UNFINISHED

url <- "http://ergast.com/api/f1/drivers?limit=1000"

temp <- xmlToList(rawToChar(GET(url)$content))

test <- bind_rows(temp$DriverTable)

driver_data <- data.frame()
for(i in 1:length(temp$DriverTable)) {
  driver_temp <- temp$DriverTable[i]$Driver[1:4]
  
  if("code" %in% names(temp$DriverTable[i]$Driver$.attrs)) {
    row_temp <- cbind(
      bind_rows(driver_temp),
      bind_rows(temp$DriverTable[i]$Driver$.attrs)
    )
  } else {
    row_temp <- cbind(
      bind_rows(driver_temp),
      bind_rows(temp$DriverTable[i]$Driver$.attrs)
    ) %>%
      mutate(code = NA) %>%
      relocate(code, .after = driverId)
  }

  driver_data <- rbind(driver_data, row_temp)
}

# Driver info this season

url <- "http://ergast.com/api/f1/current/drivers"

temp <- xmlToList(rawToChar(GET(url)$content))

driver_data <- data.frame()
for(i in 1:length(temp$DriverTable)) {
  driver_temp <- temp$DriverTable[i]$Driver[1:4]
  
  row_temp <- cbind(
    bind_rows(driver_temp),
    bind_rows(temp$DriverTable[i]$Driver$.attrs)
  )
  
  driver_data <- rbind(driver_data, row_temp)
}

# Driver Standings

url <- "http://ergast.com/api/f1/current/driverStandings"

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

# Grand Prix Rounds

url <- "http://ergast.com/api/f1/current"

temp <- xmlToList(rawToChar(GET(url)$content))

grandPrixRounds <- data.frame()
for(i in 1:(length(temp$RaceTable)-1)) {
  circuit_temp <- temp$RaceTable[i]
  
  row_temp <- cbind(
    as.numeric(circuit_temp$Race$.attrs["round"]),
    bind_rows(circuit_temp$Race[c(1,3)]),
    circuit_temp$Race$Circuit$CircuitName,
    bind_rows(circuit_temp$Race$Circuit$Location[1:2]),
    bind_rows(circuit_temp$Race$Circuit$Location$.attrs)
  ) %>%
    rename(
      CircuitName = "circuit_temp$Race$Circuit$CircuitName",
      Round = "as.numeric(circuit_temp$Race$.attrs[\"round\"])"
      )
  
  grandPrixRounds <- rbind(grandPrixRounds, row_temp)
}

grandPrixRounds <- grandPrixRounds %>%
  mutate(pointsFirst = ifelse(
    Locality %in% c(
      "Imola",
      "Spielberg",
      "São Paulo"
    ),
    32,
    26
  )) %>%
  mutate(pointsSecond = ifelse(
    Locality %in% c(
      "Imola",
      "Spielberg",
      "São Paulo"
    ),
    25,
    20
  )) %>%
  mutate(pointsThird = ifelse(
    Locality %in% c(
      "Imola",
      "Spielberg",
      "São Paulo"
    ),
    21,
    15
  ))

pointsLeft <- sum(grandPrixRounds$pointsFirst[grandPrixRounds$Round > round])
pointsSecond <- sum(grandPrixRounds$pointsSecond[grandPrixRounds$Round > round])
pointsThird <- sum(grandPrixRounds$pointsThird[grandPrixRounds$Round > round])

prospectTable <- driver_standings %>%
  select(PermanentNumber,FullName,code,Constructor,position,points) %>%
  mutate(distToFirst = driver_standings$points[driver_standings$position==1] - points) %>%
  mutate(pointsMaximiz = pointsLeft - distToFirst) %>%
  mutate(pointsVsSecond = ifelse(
    position == 1, NA, pointsLeft - (distToFirst+pointsSecond)
  )) %>%
  mutate(pointsVsThird = ifelse(
    position == 1, NA, pointsLeft - (distToFirst+pointsThird)
  ))

"http://ergast.com/api/f1/2008/5/constructorStandings"

# Lap lists

url <- "https://ergast.com/api/f1/current/5/laps/70"

temp <- xmlToList(rawToChar(GET(url)$content))

current_round <- 1

lap_list <- data.frame()
for(j in 1:round) {
  lap_list_temp <- data.frame()
  
  print(paste("Round", j, "of", round, sep = " "))
  
  for(i in 1:100) {
    url <- paste("https://ergast.com/api/f1/current/", j, "/laps/", i, sep = "")
    
    temp <- xmlToList(rawToChar(GET(url)$content))
    
    if(is.null(temp$RaceTable$Race$LapsList)) {
      break
    } else {
      lap_temp <- bind_rows(temp$RaceTable$Race$LapsList[[1]]) %>%
        select(-number)
      lap_list_temp <- rbind(lap_list_temp,lap_temp)
      
      print(paste("Lap", i))
      Sys.sleep(0.5)
    }
  }
  
  lap_list_temp$Round <- j
  
  lap_list <- rbind(lap_list, lap_list_temp)
}

lap_list <- lap_list %>%
  drop_na()

timeSecs_temp <- c()
for (i in 1:length(lap_list$time)) {
  timeSecs_temp <-c(timeSecs_temp,
  as.numeric(str_split(lap_list$time[i], ":")[[1]][1]) * 60 + as.numeric(str_split(lap_list$time[i], ":")[[1]][2])
  )
}

lap_list <- lap_list %>%
  mutate(position = as.numeric(position),lap = as.numeric(lap), timeSecs = timeSecs_temp) %>%
  left_join(
    driver_standings %>%
      select(driverId, Constructor, code)
  )

saveRDS(lap_list, "F1_lap_history2022.RDS")

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

p1 <- ggplotly(
  ggplot(lap_list %>% filter(Round == 1), aes(x = lap, y = position, fill = code, color = Constructor)) +
    geom_line() +
    scale_y_continuous(trans = "reverse", breaks = unique(lap_list$position[lap_list$Round == 1])) +
    constructer_scale_colors +
    labs(x = "Lap", y = "Position", color = "Driver")
)

p2 <- ggplotly(
  ggplot(lap_list %>% filter(Round == 1), aes(x = lap, y = timeSecs, fill = code, color = Constructor)) +
    geom_line() +
    constructer_scale_colors +
    labs(x = "Lap", y = "Lap time (sec)", color = "Driver")
)

test <- list(p1,p2)
test[1]
