if(!file.exists("data/F1_lap_history2022.RDS")) {
  lap_list <- data.frame()
  max_round <- 1
} else {
  lap_list <- readRDS("data/F1_lap_history2022.RDS")
  max_round <- (max(lap_list$Round)+1)
}

if(max_round <= latestRound) {
  for(j in max_round:round_var) {
    lap_list_temp <- data.frame()
    
    print(paste("Round", j, "of", round_var, sep = " "))
    
    for(i in 1:100) {
      url <- paste("https://ergast.com/api/f1/current/", j, "/laps/", i, sep = "")
      
      temp <- rawToChar(GET(url)$content) #while temp3 == "Unable to select database"
      
      while(temp == "Unable to select database") {
        temp <- rawToChar(GET(url)$content)
      }
      
      temp <- xmlToList(temp)
      
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
  }
}
  
   lap_list_temp <- lap_list_temp %>%
    drop_na()
  
  timeSecs_temp <- c()
  
  for (i in 1:length(lap_list_temp$time)) {
    timeSecs_temp <-c(timeSecs_temp,
                      as.numeric(str_split(lap_list_temp$time[i], ":")[[1]][1]) * 60 + as.numeric(str_split(lap_list_temp$time[i], ":")[[1]][2])
    )
  }
  
  lap_list_temp <- lap_list_temp %>%
    mutate(position = as.numeric(position),lap = as.numeric(lap), timeSecs = timeSecs_temp) %>%
    left_join(
      driver_standings %>%
        select(driverId, Constructor, code)
    )
  
  lap_list <- rbind(lap_list, lap_list_temp)
  
  lap_list <- lap_list[!duplicated(lap_list), ]
  
  saveRDS(lap_list, "data/F1_lap_history2022.RDS")

