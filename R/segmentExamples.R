setwd("C:\\R_projects\\segmentation")
rm(list = ls())



#### get previous data produced 

seg.raw <- read.csv("http://goo.gl/qw303p")
seg.df <- seg.raw[,-7] ## remove already marked segments

## NB here we have left in whether a person subscribes to a service or not. For LBBD we may wish to add this high level
# service variable rather that the cost variable - or several CSC, ASC, etc, or leave out?  

summary(seg.df)

## clustering

## if we have an example cluster already we can create function to check differences between clusters at high level

seg.sum <- function(data,groups){
  aggregate(data, list(groups), function(x)  mean(as.numeric(x)))
  
}


seg.sum(seg.df,seg.raw$Segment) ## nb categorical data is here treated as numeric so take care in interpretaion

# 11.3.2

#################################################
##  Hierarchical Clustering: hclust() Basics ####
#################################################


# simple distnce based agglomeartive
# only for numeric data ....
## so we need to use the daisy function from the cluster package
library(cluster)
seg.dist <- daisy(seg.df) # works with mixed data types creates a dist matrix

class(seg.dist)

as.matrix(seg.dist)[1:10,1:10]

##create hieracrchy using hclust on the matrix
# useing here a complets linkage which starts at individuals ad goes to 1 cluster so BEWARE

seg.hc <- hclust(seg.dist, method = "complete")

plot(seg.hc)# shows every cluster down to individuals so with latge data becomes too much!!

## can be coerced to a dendrogram and cut for just one branch

plot(cut(as.dendrogram(seg.hc), h = 0.5)$lower[[1]]) # cut at height 0.5 and select lower part of first branch

seg.df[c(128,137),] ##  very similar
seg.df[c(128,173),] # quite similar
seg.df[c(128,89),] # less similar

## There is a method that test for how well the dendgram fits the ditsnace matrix : Cophentic correlation coef (CPCC)

cor(cophenetic(seg.hc), seg.dist) # 
# [1] 0.7682436 quite strong

## number of clusters. Look at heights
# k captures whare to cut for k segments


plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red") # here we specify 4 segments # which seems reasonable

seg.hc.segment <- cutree(seg.hc, k = 4) # so cut tree here 

seg.sum(seg.df, seg.hc.segment) ## use custom summary function defined above

## note that here the segments have split groups 1 and two into men and women## does this answer the business question?
#maybe not

## plot below shows two target groups ( non subscribers ) ==== Men are one women are another === pretty useless here

plot(jitter(as.numeric(seg.df$gender)) ~ jitter(as.numeric(seg.df$subscribe)), col=seg.hc.segment, yaxt="n", xaxt="n", ylab="", xlab="")
### jitter has been used to seperate the points
#col has been used to colour plot by segment
# yaxt etc just sets the axis scales and titles to blank

axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes")) # label axis x
axis(2, at=c(1, 2), labels=levels(seg.df$gender)) # label axis y based on gender

## so here hclust didnt reall help anwer the question - has found paths of "least resistance" ie Male and Female


# 11.3.3

#################################################
##  Means based Clustering : K- Means ###########
#################################################

### attempts to find groups that are most compact in terms of the mean sum of squres away from the centroid of each group
# as uses means it needs numerical data - we have mixed data. 
# Is not an ideal solution, and should be done with care, but we can binaries our categories to 1 and 0
# This example alrady uses binary categories but we can also create dummy variables where we turn levels into seperate yes no 1,0 variables.

## HOWEVER this is often not satisfactory and is so needs caution to be exercised## consider ROCK type linked clustering instead
# but lets give it a go :)

# recode variables to 1,0 numrerics where categorical
seg.df.num <- seg.df
seg.df.num$gender <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)

 
set.seed(96743) # for reproducibilty
seg.k <- kmeans(seg.df.num, centers=4) # set 4 clusters

# use summary fuction we created

seg.sum(seg.df.num, seg.k$cluster) ## notice here we no longer have the M/F Split

boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster") # nice spit on income

## visualise using a PCA

#(see Chap. 8 to review principal component analysis and plotting.)
#We use clusplot from the cluster package with arguments to color the groups,
#shade the ellipses for group membership, label only the groups (not the individual points) with labels=4,
# and omit distance lines between groups (lines=0):
  

library(cluster)
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main="K-means cluster plot")


