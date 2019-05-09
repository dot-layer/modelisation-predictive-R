load_merging_data <- function(path_save_data)
{
  AWS_URL <- "https://s3.ca-central-1.amazonaws.com/jeremiedb/share/dot-layer/R-Quebec"
  
  # Merger de l'info sur les stations
  path_data_stations <- paste0(path_save_data, "data_stations.csv")
  
  if (!file.exists(path_data_stations)){
    data_stations <- fread(paste0(AWS_URL, "/BixiMontrealRentals2017/Stations_2017.csv"))
    fwrite(data_stations, path_data_stations)
  } else {
    data_stations <- fread(path_data_stations)
  }
  
  # Merger de l'info sur les quartiers
  path_geo <- paste0(path_save_data, "LIMADMIN")
  
  if (!file.exists(path_geo)){
    purrr::walk(c(".shx", ".shp", ".prj", ".dbf"),
                ~ download.file(url = paste0(AWS_URL, "/LIMADMIN/LIMADMIN", .x),
                                destfile = paste0(path_geo, .x),
                                method = "auto",
                                mode = "wb"))
    shape_file <- read_sf(dsn=path.expand(paste0(path_geo, ".shp")))
    write_sf(shape_file, path_geo)
  } else {
    shape_file <- read_sf(dsn=path.expand(paste0(path_geo, ".shp")))
  }
  
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