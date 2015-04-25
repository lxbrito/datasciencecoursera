corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names=TRUE)
  ret <- vector('numeric',0)
  for(file in files) {
    data <- read.csv(file)
    cc <- complete.cases(data)
    if(sum(cc) <= threshold) next
    cr <- cor(data$nitrate[cc], data$sulfate[cc])
    ret <- c(ret, cr)
  }
  ret
}