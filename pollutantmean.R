pollutantmean <- function(directory, pollutant, id = 1:332) {
  full_data <- NULL
  for(name in id) {
    file_name <- paste('000', as.character(name), sep='')
    file_path <- paste(directory,'/',substr(file_name,nchar(file_name)-2,nchar(file_name)), ".csv", sep='')
    file_data <- read.csv(file_path, stringsAsFactors = FALSE)
    full_data <- rbind(full_data, file_data)
  }
  pol_vector <- full_data[pollutant]
  mean(pol_vector[!is.na(pol_vector)])
}
