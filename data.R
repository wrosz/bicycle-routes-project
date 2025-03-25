# Najpopularniejsze trasy rowerowe w New Jersey w zaleznosci od czasu w latach 2016 - 2020


# pobranie potrzebnych bibliotek i plikow

library(data.table)
library(leaflet)

csv_files <- list.files("data", full.names = TRUE)

data <- lapply(csv_files, FUN = fread, check.names = TRUE)

names(data) <- sapply(list.files(path = "data"), FUN = substr, 4, 9)



# przygotowanie ramek danych do pracy

all_routes <- data # wszystkie przejazdy ze stacji A do B w latach 2016 - 2020
for (i in 1:60){
  dt <- data[[i]]
  
  # uzgodnienie nazw kolumn
  if ("start.station.id" %in% colnames(dt)){
    setnames(dt, colnames(dt), colnames(data[[1]]))
    
  }
  if ("start_station_id" %in% colnames(dt)){
    setnames(dt, c("start_station_id", "end_station_id",
                   "start_station_name", "end_station_name",
                   "start_lat", "end_lat",
                   "start_lng", "end_lng"),
             c("Start.Station.ID", "End.Station.ID",
               "Start.Station.Name", "End.Station.Name",
               "Start.Station.Latitude", "End.Station.Latitude",
               "Start.Station.Longitude", "End.Station.Longitude"))
  }
  
  # wybor potrzebnych kolumn
  dt <- dt[ , .(Start.Station.ID, End.Station.ID,
                Start.Station.Name, End.Station.Name,
                Start.Station.Latitude, End.Station.Latitude,
                Start.Station.Longitude, End.Station.Longitude)]
  
  # dodanie kolumn z miesiacem, rokiem i kwartalem, ktorych dotyczy dana ramka
  dt[ , ':='(Month = as.numeric(substr(names(all_routes[i]), 5, 6)),
              Year = as.numeric(substr(names(all_routes[i]), 1, 4)))]
  dt[ , Quarter := ifelse(
    Month <= 3, 1, ifelse(
      Month <= 6, 2, ifelse(
        Month <= 9, 3, 4)))]

  all_routes[[i]] <- dt
}
all_routes <- rbindlist(all_routes)


stations_data <- rbind(  # dane o wszystkich dzialajacych stacjach
  all_routes[ , .(Station.ID = Start.Station.ID,
                  Station.Name = Start.Station.Name,
                  Station.Latitude = Start.Station.Latitude,
                  Station.Longitude = Start.Station.Longitude)],
  all_routes[ , .(Station.ID = End.Station.ID,
                  Station.Name = End.Station.Name,
                  Station.Latitude = End.Station.Latitude,
                  Station.Longitude = End.Station.Longitude)])

stations_data <- stations_data[!duplicated(Station.ID)]

all_routes <- all_routes[ , .(Start.Station.ID, End.Station.ID, Month, Quarter, Year)]



# funkcje

# grupuje ile przejazdow pomiedzy stacja A bylo w danym miesiacy/kwartale/roku
group_routes <- function(years, by, combine_years){
  stopifnot(
    years %in% 2016:2020,
    by %in% c('m', 'q', 'y'),
    is.logical(combine_years),
    !(combine_years == TRUE & by == 'y'),
    (length(by) == 1 & length(combine_years) == 1)
  )
  dt <- all_routes[Year %in% years, ]
  if (by == 'y'){
    dt <- dt[, .(Count = .N), by = .(Year, Start.Station.ID, End.Station.ID)]
  }
  if (by == 'q'){
    if (combine_years){
      dt <- dt[ , .(Count = .N), by = .(Quarter, Start.Station.ID, End.Station.ID)]
    }
    else{
      dt <- dt[ , .(Count = .N), by = .(Year, Quarter, Start.Station.ID, End.Station.ID)]
    }
  }
  if (by == 'm'){
    if (combine_years){
      dt <- dt[ , .(Count = .N), by = .(Month, Start.Station.ID, End.Station.ID)]
    }
    else{
      dt <- dt[ , .(Count = .N), by = .(Year, Month, Start.Station.ID, End.Station.ID)]
    }
  }
  dt
}

# najpopularniejsze trasy w danym okresie czasu i ile najpopularniejszych ma wyswietlic
popular_routes_dt <- function(years = NULL, by, combine_years, s_month = NULL, s_quarter = NULL, s_year = NULL){ 
  if (combine_years == FALSE){
    dt <- group_routes(2016:2020, by, FALSE)
    dt <- dt[Year == s_year, ]
  }
  else{
    dt <- group_routes(years, by, combine_years)
  }
  
  if (by == 'm'){
    dt <- dt[Month == s_month, ]
  }
  if (by == 'q'){
    dt <- dt[Quarter == s_quarter, ]
  }
  
  
  dt <- merge.data.table(dt, stations_data, by.x = 'Start.Station.ID', by.y = 'Station.ID',
                         all.x = TRUE, all.y = FALSE)
  dt <- merge.data.table(dt, stations_data, by.x = 'End.Station.ID', by.y = 'Station.ID',
                         all.x = TRUE, all.y = FALSE,
                         suffixes = c('.Start', '.End'))
  setnames(dt,
           c('Station.Name.Start', 'Station.Latitude.Start', 'Station.Longitude.Start',
             'Station.Name.End', 'Station.Latitude.End', 'Station.Longitude.End'),
           c('Start.Station.Name', 'Start.Station.Latitude', 'Start.Station.Longitude',
             'End.Station.Name', 'End.Station.Latitude', 'End.Station.Longitude'))
  
  setorder(dt, -Count)
  dt
}


# pokaz mape z naniesionymi trasami z popular_routes_dt
render_map <- function(dt, limit = 10, show_markers = TRUE){
  stopifnot(
    limit %in% 1:50,
    is.logical(show_markers),
    (length(limit == 1) & length(show_markers == 1))
  )
  
  weight_factor <- 13 / dt[1, Count]
  # weight_factor <- 13 / 1000      dla animacji animation_month_screenshots
  # weight_factor <- 13 / 8000      dla animacji animation_quarters_screenshots
  
  rmap <- leaflet() %>% addTiles()
  for (i in 1:limit){
    
    # jezeli trasa w druga strone juz jest uwzgledniona na mapie
    if (i > 1 & any(
      (dt[i, Start.Station.ID] %in% dt[1:(i - 1), End.Station.ID]) & (dt[i, End.Station.ID] %in% dt[1:(i - 1), Start.Station.ID]))){
    #if (i > 1 & all(dt[i, c(End.Station.ID, Start.Station.ID)] %in% dt[1:(i - 1), c(Start.Station.ID, End.Station.ID)])){
      j <- which(dt$Start.Station.ID == dt[i, End.Station.ID] & dt$End.Station.ID == dt[i, Start.Station.ID])[1]
      route_data <- sprintf("Stacja poczatkowa: %s<br>Stacja koncowa: %s<br>Liczba przyjazd?w: %s<br>Pozycja w rankingu: %s
                              <br><br>
                              Stacja poczatkowa: %s<br>Stacja koncowa: %s<br>Liczba przyjazd?w: %s<br>Pozycja w rankingu: %s
                              <br><br>
                              Razem: %s",
                            dt[j, Start.Station.Name], dt[j, End.Station.Name], dt[j, Count], j,
                            dt[i, Start.Station.Name], dt[i, End.Station.Name], dt[i, Count], i,
                            dt[j, Count] + dt[i, Count])
    }
    
    # jezeli takiej trasy jeszcze nie ma na mapie
    else{
      route_data <- sprintf("Stacja poczatkowa: %s<br>Stacja koncowa: %s<br>Liczba przyjazd?w: %s<br>Pozycja w rankingu: %s
                              <br><br>",
                            dt[i, Start.Station.Name], dt[i, End.Station.Name], dt[i, Count], i)
    }
    
    # jezeli stacja poczatkowa == stacja koncowa
    if (dt[i, Start.Station.ID] == dt[i, End.Station.ID]){
      rmap <- rmap %>%
        addCircleMarkers(
          lng = dt[i, Start.Station.Longitude],
          lat = dt[i, Start.Station.Latitude],
          weight = dt[i, Count] * weight_factor,
          radius = 15,
          popup = route_data)
    }
    
    # jezeli stacje poczatkowa i koncowa sa rozne
    else{
      rmap <- rmap %>%
        addPolylines(
          lng = dt[i, c(Start.Station.Longitude, End.Station.Longitude)],
          lat = dt[i, c(Start.Station.Latitude, End.Station.Latitude)],
          weight = dt[i, Count] * weight_factor,
          popup = route_data)
    }
    
  }
  
  # pokazanie markerow lokalizacji stacji
  if (show_markers == TRUE){
    rmap <- rmap %>%
      addMarkers(
        lng = unique(c(dt[1:limit, Start.Station.Longitude],
                       dt[1:limit, End.Station.Longitude])),
        lat = unique(c(dt[1:limit, Start.Station.Latitude],
                       dt[1:limit, End.Station.Latitude])),
        popup = unique(c(dt[1:limit, Start.Station.Name],
                         dt[1:limit, End.Station.Name])))
  }  
  
  rmap
}

table_to_display <- function(dt){
  dt_disp <- dt[ , .(Count, Start.Station.Name, End.Station.Name)]
  setnames(dt_disp, c('Count', 'Start.Station.Name', 'End.Station.Name'), c('Liczba przejazd?w', 'Stacja poczatkowa', 'Stacja koncowa'))
  dt_disp
}


