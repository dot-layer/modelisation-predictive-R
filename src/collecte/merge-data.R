merge_data <- function(data_bixi, data_stations, points_stations)
{
  data_bixi[, `:=`(start_date_time = as.POSIXct(lubridate::fast_strptime(start_date, format = "%Y-%m-%d %H:%M")))]
  
  data_bixi[, `:=`(start_date = as.Date(start_date_time))]
  
  data_stations[, quartier := points_stations$quartier]
  setkey(data_stations, code)
  setkey(data_bixi, start_station_code)
  data_bixi[data_stations, start_quartier := quartier]
}