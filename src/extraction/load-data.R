

load_data <- function(path_save_data) {
  
  if (!file.exists(paste0(path_save_data, "data_bixi.csv"))){
    data <- list()
    months <- c(4:11)
    x <- 1
    for (month in months){
      month <- formatC(month, width = 2, format = "d", flag = "0")
      path <- paste0("https://s3.ca-central-1.amazonaws.com/jeremiedb/share/dot-layer/R-Quebec/BixiMontrealRentals2017", "/", "OD_2017-", month, ".csv")
      data[[x]] <- fread(path)
      x <- x + 1
    }
    
    data_bixi <- rbindlist(data, use.names = TRUE, fill = FALSE)
    fwrite(data_bixi, paste0(path_save_data, "data_bixi.csv"))
  } else {
    data_bixi <- fread(paste0(path_save_data, "data_bixi.csv"))
  }
  
  if (!file.exists(paste0(path_save_data, "data_stations.csv"))){
    data_stations <- fread("https://s3.ca-central-1.amazonaws.com/jeremiedb/share/dot-layer/R-Quebec/BixiMontrealRentals2017/Stations_2017.csv")
    fwrite(data_stations, paste0(path_save_data, "data_stations.csv"))
  } else {
    data_stations <- fread(paste0(path_save_data, "data_stations.csv"))
  }
  
  data_bixi[, `:=`(start_date_time = as.POSIXct(fast_strptime(start_date, format = "%Y-%m-%d %H:%M")),
                   end_date_time = as.POSIXct(fast_strptime(end_date, format = "%Y-%m-%d %H:%M")))]
  
  data_bixi[, `:=`(start_date = as.Date(start_date_time),
                   end_date = as.Date(end_date_time))]
  
  # Merger de l'info sur les quartiers
  path_geo <- paste0(path_save_data, "LIMADMIN.shp")
  shape_file <- read_sf(dsn=path.expand(path_geo))
  
  points_stations <- data.frame(
    x = data_stations$longitude,
    y = data_stations$latitude
  )
  
  points_stations_sf <- do.call("st_sfc", c(lapply(1:nrow(points_stations), function(i) {st_point(as.numeric(points_stations[i,]))}), list("crs" = 4326)))
  
  pnts_trans <- st_transform(points_stations_sf, 2163)
  shape_file_trans <- st_transform(shape_file, 2163)
  
  points_stations$quartier <- apply(st_intersects(shape_file_trans, pnts_trans, sparse = FALSE), 2, 
                                    function(col) { 
                                      if (length(shape_file_trans[which(col), ]$NOM)>0){
                                        shape_file_trans[which(col), ]$NOM 
                                      }else{
                                        NA_character_
                                      }
                                    })
  
  data_stations[, quartier := points_stations$quartier]
  setkey(data_stations, code)
  setkey(data_bixi, start_station_code)
  data_bixi[data_stations, start_quartier := quartier]
  setkey(data_bixi, end_station_code)
  data_bixi[data_stations, end_quartier := quartier]
  
  data_bixi
  
}