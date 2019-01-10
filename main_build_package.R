library(httr)
library(devtools)
# install.packages("roxygen2")
library(roxygen2)

setwd("./katharos")
document()

setwd("..")
install("katharos")
library("katharos")
