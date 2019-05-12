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
  extension_list <- c(".shx", ".shp", ".prj", ".dbf")
  
  if (! all(purrr::map_lgl(extension_list, ~ file.exists(paste0(path_geo, .x))))){
    purrr::walk(extension_list,
                ~ download.file(url = paste0(AWS_URL, "/LIMADMIN/LIMADMIN", .x),
                                destfile = paste0(path_geo, .x),
                                method = "auto",
                                mode = "wb"))
    
  }
  
  shape_file <- read_sf(dsn=path.expand(paste0(path_geo, ".shp")))
  
  points_stations <- data.frame(
    x = data_stations$longitude,
    y = data_stations$latitude
  )
  
  points_stations_sf <- do.call("st_sfc", c(lapply(1:nrow(points_stations), function(i) {st_point(as.numeric(points_stations[i,]))}), list("crs" = 4326)))
  
  pnts_trans <- st_transform(points_stations_sf, 2163)
  shape_file_trans <- st_transform(shape_file, 2163)
  matrice_intersection <- st_intersects(shape_file_trans, pnts_trans, sparse = FALSE)
  
  extraire_nom_quartier <- function(col) {
    nom_quartier <- shape_file_trans[which(col), ]$NOM
    
    if (length(nom_quartier) > 0){
      nom_quartier
    }else{
      NA_character_
    }
  }
  
  points_stations$quartier <- apply(matrice_intersection, 2, extraire_nom_quartier)
  
  points_stations <- as.data.table(points_stations)
  setnames(points_stations, old = c("x", "y"), new = c("longitude", "latitude"))
  setkeyv(points_stations, c("longitude", "latitude"))
  setkeyv(data_stations, c("longitude", "latitude"))
  data_stations <- points_stations[data_stations]
  setkey(data_stations, code)

  return(list(data_stations = data_stations))
}