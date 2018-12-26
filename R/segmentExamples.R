setwd("C:\\R_projects\\segmentation")



#### get previous data produced 

seg.raw <- read.csv("http://goo.gl/qw303p")
seg.df <- seg.raw[,-7] ## remove already marked segments

summary(seg.df)

## clustering

## if we have an example cluster already we can create function to check differences between clusters at high level

seg.sum <- function(data,groups){
  aggregate(data, list(groups), function(x)  mean(as.numeric(x)))
  
}


## or dplyr

seg.sum <- function(data,groups){
  aggregate(data, by = list(groups), function(x)  mean(as.numeric(x)))
  
}

seg.sum(seg.df,seg.raw$Segment) ## nb categorical data is here treated as numeric so take care in interpretaion

list(seg.raw$Segment)
