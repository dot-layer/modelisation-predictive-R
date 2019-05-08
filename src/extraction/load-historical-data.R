load_historical_data <- function(path_save_data) {
  
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
  
  data_bixi[, `:=`(start_date_time = as.POSIXct(fast_strptime(start_date, format = "%Y-%m-%d %H:%M")),
                   end_date_time = as.POSIXct(fast_strptime(end_date, format = "%Y-%m-%d %H:%M")))]
  
  data_bixi[, `:=`(start_date = as.Date(start_date_time),
                   end_date = as.Date(end_date_time))]
  
  data_bixi
}