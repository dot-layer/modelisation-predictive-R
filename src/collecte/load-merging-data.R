load_merging_data <- function(path_save_data)
{
  if (!file.exists(paste0(path_save_data, "data_stations.csv"))){
    data_stations <- fread("https://s3.ca-central-1.amazonaws.com/jeremiedb/share/dot-layer/R-Quebec/BixiMontrealRentals2017/Stations_2017.csv")
    fwrite(data_stations, paste0(path_save_data, "data_stations.csv"))
  } else {
    data_stations <- fread(paste0(path_save_data, "data_stations.csv"))
  }
  
  # Merger de l'info sur les quartiers
  path_geo <- paste0(path_save_data, "LIMADMIN.shp")
  if (!file.exists(path_geo)){
    url <- "https://s3.ca-central-1.amazonaws.com/jeremiedb/share/dot-layer/R-Quebec/LIMADMIN/LIMADMIN.shp"
    download.file(url, path_geo)
    shape_file <- read_sf(dsn=path.expand(path_geo))
  } else {
    shape_file <- read_sf(dsn=path.expand(path_geo))
  }
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

  list(points_stations = points_stations,
       data_stations = data_stations)
}