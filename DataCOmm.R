rm(list=ls()) #Clears Workspace (Variables and Loaded Data)
cat("\014") #Clears Consol
#dev.off() works to clear plots, but can't be run in program

library(tidyverse)
library(haven)

stataFile <- read_dta(file = "C:/Users/glide/OneDrive/Desktop/DATACOM_NEW.dta")

#parent tens digit 1 or 2
myfunction <- function(vector, iterations){
  if(missing(iterations)){
    numTimes = 500
  }else{numTimes = iterations}
  trueMean = mean(vector)
  vectorLength = length(vector)
  sampleMean = (rep(0, numTimes))
  for(i in 1:numTimes){
    sampleMean[i] = mean(sample(vector,vectorLength, replace = TRUE))
  }
  hist(sampleMean)
}

momLength = length(momrule)
momPosative = (rep,(0, momLength))
if()