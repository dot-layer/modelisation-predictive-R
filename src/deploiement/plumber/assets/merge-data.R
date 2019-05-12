merge_data <- function(data_bixi, data_stations)
{
  data_bixi[, `:=`(start_date_time = as.POSIXct(lubridate::fast_strptime(start_date, format = "%Y-%m-%d %H:%M")))]
  data_bixi[, `:=`(start_date = as.Date(start_date_time))]
  setkey(data_bixi, start_station_code)
  data_bixi[data_stations, start_quartier := quartier]
  return(data_bixi)
}