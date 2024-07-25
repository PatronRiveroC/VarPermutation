load_data <- function(csv_dir, tif_dir) {
	csv_data <- read.csv(csv_dir, header = TRUE, sep = ",", row.names = NULL)
	tif_files <- list.files(tif_dir, pattern = ".tif", full.names = TRUE)
	raster_stack <- stack(tif_files)
	extracted_data <- extract(raster_stack, cbind(csv_data$Long, csv_data$Lat), df = TRUE)
	extracted_data <- na.omit(extracted_data)
	extracted_data <- extracted_data[-1]
	return(extracted_data)
}
