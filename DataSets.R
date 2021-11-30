library(dplyr)

GetBenchmarkData <- function(name, standardized = TRUE) {
  X <- NULL
  y <- NULL
  switch(name,
         colonCA = {
           # See the source about the use of dependent variables. Features 7 should not be used as dependent or predictive.
           X <- tExpressionData[,c(50,66,106,493,515,682,1522,1887,1920,1999)]
           y <- factor(colonCA@phenoData@data[["class"]])
          
         },
         GSE18842 = {
           # https://sci2s.ugr.es/keel/dataset.php?cod=183
           X <- tExpressionDataLung[,c(256, 1547, 1815, 10462, 10699,10740,12404,12663,12810,14089)]
           y <- ClassLungFactor
         },
         GSE24514 = {
           library(mlbench)
           data("PimaIndiansDiabetes", package="mlbench")
           X <- tExpressionDataColon[,c(759,2387,2530,2783,3812,4168,5477,5634,9175, 12426)]
           y <- classColon
         }, {
           # Name is not a correct abbreviation of dataset
           return(warning("Dataset not found, try a different name"))
         })
  if (standardized){
    X <- scale(X)
  }
  X <- as.matrix(X)
  colnames(X) <- 1:ncol(X)
  levels(y) <- c(0, 1)
  y <- as.numeric(y) - 1
  return(list(X, y))
}