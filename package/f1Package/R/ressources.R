f1_reqPackages <- c("httr", "XML","stringr","dplyr","tidyr","ggplot2","data.table","plotly","leaflet","lubridate")

lap_list <- readRDS("data/F1_lap_history2022.RDS")
driverResult <- readRDS("data/driverResult2022.RDS")
quali_df <- readRDS("data/quali_results.RDS")

latest_round <- as.numeric(xmlToList(rawToChar(GET("http://ergast.com/api/f1/current/driverStandings")$content))$StandingsTable$StandingsList$.attrs["round"])

