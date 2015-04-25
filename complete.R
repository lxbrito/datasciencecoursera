complete <- function(directory, id = 1:332) {
  nobs <- vector('integer', length(id))
  for(i in 1:length(id)) {
    file_name <- paste('000', as.character(id[i]), sep='')
    file_path <- paste(directory,'/',substr(file_name,nchar(file_name)-2,nchar(file_name)), ".csv", sep='')
    file_data <- read.csv(file_path, stringsAsFactors = FALSE)
    nobs[i] <- sum(complete.cases(file_data))
  }
  data.frame(id, nobs)
}