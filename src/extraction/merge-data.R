merge_data <- function(data_bixi, data_stations, points_stations)
{
  data_stations[, quartier := points_stations$quartier]
  setkey(data_stations, code)
  setkey(data_bixi, start_station_code)
  data_bixi[data_stations, start_quartier := quartier]
  setkey(data_bixi, end_station_code)
  data_bixi[data_stations, end_quartier := quartier]
  
}