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

seg.summ <- function(data,groups){
  aggregate(data, list(groups), function(x)  mean(as.numeric(x)))
  
}


seg.summ(seg.df,seg.raw$Segment) ## nb categorical data is here treated as numeric so take care in interpretaion

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

seg.summ(seg.df, seg.hc.segment) ## use custom summary function defined above

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

table(seg.raw$Segment,seg.hc.segment) ## preety rubbish actually

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

summary(seg.df.num)

 
set.seed(3) # for reproducibilty
seg.k <- kmeans(seg.df.num, centers=4) # set 4 clusters

# use summary fuction we created

seg.summ(seg.df.num, seg.k$cluster) ## notice here we no longer have the M/F Split

boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster") # nice spit on income

## visualise using a PCA

#(see Chap. 8 to review principal component analysis and plotting.)
#We use clusplot from the cluster package with arguments to color the groups,
#shade the ellipses for group membership, label only the groups (not the individual points) with labels=4,
# and omit distance lines between groups (lines=0):
  

library(cluster)
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main="K-means cluster plot")

# 11.3.4

#################################################
##  Model Based Clustering mClust     ###########
#################################################

## Model based clustering works on priciple that there are sub groups within the datsets that are distict groups with 
#different meansd and variance ( or SD)
# also known as MIXTURE MODELS which refelct a mixture of obs from DIFFERENT populations each with differet gausian ( normal distributions)

### again need numeric data

## ## note the book gets different results due to change in mclust code between 4.4 and 5


# install earlier versions of mclust if needed
# library(versions)
# install.versions(c('mclust'), c("4.4"))

library(mclust)
sessionInfo()

###
# convert factor variables to numeric (mclust requires). OK b/c all are binary.
# these lines are the same as above for k-means [not repeated in book]
seg.df.num <- seg.df
seg.df.num$gender    <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome   <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
summary(seg.df.num)
###


## ## note the book gets different results due to change in mclust code between 4.4 and 5.4

mclust.options(hcUse = "PCS") # specified a specific mclust model to get around this . think this may have been 4.4 default

# fit the model
seg.mc <- Mclust(seg.df.num)
summary(seg.mc)

# what if we estimate 4 clusters?
seg.mc4 <- Mclust(seg.df.num, G=4)
summary(seg.mc4)

# compare the two models
BIC(seg.mc, seg.mc4)

# examine the 3-cluster model
seg.summ(seg.df, seg.mc$class)


# plot the 3-cluster model
library(cluster)
clusplot(seg.df, seg.mc$class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Model-based cluster plot")





# and also shows Log Likelihoods and so models can be compared 
# force 4 segments

seg.mc4 <- Mclust(seg.df.num,  G = 4) # G is number of segments
summary(seg.mc4)#

# compare mdels using BIC (Bayesian Information Criterion)

BIC(seg.mc,seg.mc4) ### compares our two modedls. ### the lower the BIC score the better# hERE THE 3 CLUSTER MODEL IS MUCH BETTER  ACCORDING
# TO THE DATA

seg.summ(seg.df, seg.mc$class) # have a look and plot
clusplot(seg.df, seg.mc$class, color=TRUE, shade=TRUE, labels=4, lines=0, main="MCLUST mixture cluster plot")
clusplot(seg.df, seg.mc4$class, color=TRUE, shade=TRUE, labels=4, lines=0, main="MCLUST mixture cluster plot")


#################################################
##  poLCA: Latent class analysis      ###########
#################################################

#### This technique only uses categorical variables. So we do the opposite at classify our numeric vars

#### poLCA
seg.df.cut <- seg.df
seg.df.cut$age    <- factor(ifelse(seg.df$age < median(seg.df$age), 1, 2)) # split age var into above and below median age
seg.df.cut$income <- factor(ifelse(seg.df$income < median(seg.df$income), # split income var into above and below median income
                                   1, 2))
seg.df.cut$kids   <- factor(ifelse(seg.df$kids < median(seg.df$kids), 1, 2)) # split kid number var into above and below median kids
summary(seg.df.cut)


# here we dont ask for polca do look at more complex models with covariates we just want to see effect of cluster membership alone
  # create a model formula

seg.f <- with(seg.df.cut, cbind(age, gender, income, kids, ownHome, subscribe)~1) # We use with() to save typing, and∼1 to specify a formula with intercepts only:


# fit the model
library(poLCA)
set.seed(02807)
seg.LCA3 <- poLCA(seg.f, data=seg.df.cut, nclass=3)
seg.LCA4 <- poLCA(seg.f, data=seg.df.cut, nclass=4)

seg.LCA4$bic
seg.LCA3$bic

#The 3-cluster model shows a lower BIC by 32 and thus a substantially stronger fit to the data (see Table 11.1).
#As we’ve seen, that is not entirely conclusive as to business utility,
#so we also examine some other indicators such as the quick summary function and cluster plots:

# examine the solutions
# 3 clusters
seg.summ(seg.df, seg.LCA3$predclass)
table(seg.LCA3$predclass)

clusplot(seg.df, seg.LCA3$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=3)")


# 4 clusters
seg.summ(seg.df, seg.LCA4$predclass)
table(seg.LCA4$predclass)

clusplot(seg.df, seg.LCA4$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=4)")



# compare 3-cluster and 4-cluster solutions
table(seg.LCA3$predclass, seg.LCA4$predclass) # this gives us an idea of similarity between the two model, but is error prone


# instead we turn to mclust again and map the classes from each model 
library(mclust)
mapClass(seg.LCA3$predclass, seg.LCA4$predclass) # this shows the best matches between different segments in the two models
adjustedRandIndex(seg.LCA3$predclass, seg.LCA4$predclass) # this shows how closely they correlate

# compare random assignment to LCA4
set.seed(11021)
random.data <- sample(4, length(seg.LCA4$predclass), replace=TRUE) ## here we set a total random index to compare
adjustedRandIndex(random.data, seg.LCA4$predclass) # can see there is no correlation


# compare to known segments
table(seg.raw$Segment, seg.LCA4$predclass)
adjustedRandIndex(seg.raw$Segment, seg.LCA4$predclass) ## better than random to actual known segments


################################################
### USING ROCK CUSTERING ## NOT PART OF BOOK ###
################################################

library(cba)

?rockCluster

### example from paper
data(Votes)
x <- as.dummy(Votes[-17])
rc <- rockCluster(x, n=2, theta=0.73, debug=TRUE)
print(rc)
rf <- fitted(rc)
table(Votes$Class, rf$cl)
## Not run: 
### large example from paper
data("Mushroom")
x <- as.dummy(Mushroom[-1])
rc <- rockCluster(x[sample(dim(x)[1],1000),], n=10, theta=0.8)
print(rc)
rp <- predict(rc, x)
table(Mushroom$class, rp$cl)

## End(Not run)
### real valued example
gdist <- function(x, y=NULL) 1-exp(-dist(x, y)^2)
xr <- matrix(rnorm(200, sd=0.6)+rep(rep(c(1,-1),each=50),2), ncol=2)
rcr <- rockCluster(xr, n=2, theta=0.75, fun=gdist, funArgs=NULL)
print(rcr)


### 
x <- as.integer(sample(3,10,rep=TRUE))
x1 <- as.dummy(x)
is.na(x) <- c(3,5)
as.dummy(x)
x <- as.data.frame(x)
x <- as.dummy(x)

### book example
str()

seg.df.num$age <- as.integer(seg.df.num$age)
seg.df.num$income <- as.integer(seg.df.num$income)
seg.df.num[] <- lapply(seg.df.num, function(x) as.factor(as.numeric(x)))


seg.df.rockmat <- as.dummy(seg.df.num)


seg.rock <- rockCluster(seg.dist, n=4, theta=0.05)

clusplot(seg.df, seg.rock$cl, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="ROCK plot (K=4)")


seg.summ(seg.df, seg.rock$cl)
table(seg.rock$cl)


table(seg.raw$Segment, seg.rock$cl)

#################################################################################
#################################################################################
## strangely works well with dist matrix from daisy ## but guess will scale badly

seg.dist.2 <- as.matrix(daisy(seg.df))  # may need to create a sample from bigger datasets
seg.rock.dist <- rockCluster(seg.dist.2, n=4, theta=0.3)

clusplot(seg.df, seg.rock.dist$cl, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="ROCK plot (K=4)") ## PCA doesnt look good BUT

seg.summ(seg.df, seg.rock.dist$cl)
table(seg.rock.dist$cl)

table(seg.raw$Segment, seg.rock.dist$cl)# nice match to allocated segments

# compare to known segments
adjustedRandIndex(seg.raw$Segment, seg.rock.dist$cl) ## better than random to actual known segments
# actually best result so far!!!!!








# test with sample 

##################################################################################
##################################################################################
##################################################################################


# try all factors ## not that good !!! ###  think may be probs with binary vars
seg.df.factor <- seg.df
seg.df.factor$age <- as.factor(round(seg.df.factor$age,-1))
seg.df.factor$income <- as.factor(round(seg.df.factor$income,-3))
seg.df.factor$gender <- as.factor(seg.df.factor$gender)
seg.df.factor$kids <- as.factor(seg.df.factor$kids)
seg.df.factor$subscribe <- as.factor(seg.df.factor$subscribe)

str(seg.df.factor)
table(seg.df.factor$income)

seg.df.factor.mat <- as.dummy(seg.df.factor)
str(seg.df.factor.mat)


seg.rock <- rockCluster(seg.df.factor.mat, n=4, theta=0.001)

clusplot(seg.df, seg.rock$cl, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="ROCK plot (K=4)")

seg.summ(seg.df, seg.rock$cl)
table(seg.rock$cl)

table(seg.raw$Segment, seg.rock$cl)

seg.rock.fitted <- fitted(seg.rock)
table(seg.raw$Segment, seg.rock.fitted$cl)










  