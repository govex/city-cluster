## The content of this project is licensed under the Creative Commons Attribution 4.0 International (CC BY 4.0).
## https://creativecommons.org/licenses/by/4.0/

## This script uses demographic, economic, and geographic data to identify cities that are similar to one another.
## Analytical methods include general data exploration and hierarchical clustering.

## This file is also designed to be a tutorial for users wanting to replicate and maniupulate the analysis.

## First, download city indicator dataset. This includes data from the American Community Survey, State and Local Government Finance Survey, and the Government Employment and Payroll Survey. 
## https://drive.google.com/open?id=0B9FLZ57ziQq5UVZrQnJOYkFpMHc

## Load data
data <- read.csv("~/Downloads/city_indicator_dataset.csv")

## Run basic descriptive statistics on the dataset.
## This function shows the mean, median, etc, of the observations in each column of the dataset.
summary(data)


## Scale the features by min max range to compress the feature distances to between 0 and 1
## Based on http://stackoverflow.com/questions/5665599/range-standardization-0-to-1-in-r
mmscalar <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

## Set weight factors
low_weight = 5
med_weight = 10
high_weight = 50

## Assign variable names to dataset columns.
## Categorical variables (names, regions, etc.) should be stored as factors.
## Multiple variables by desired weight factor.
## This model is weighted to boost size and shape variables. 
x0 <- as.character.factor(data$CityName)
x1 <- mmscalar(data$X2013pop) * high_weight
x2 <- mmscalar(data$Growth) * high_weight
x3 <- mmscalar(data$Median.Income) * low_weight
x4 <- mmscalar(data$Poverty) * low_weight
x5 <-  mmscalar(data$Nonwhite) * low_weight
x6 <-  mmscalar(data$Land.Area.Sq.M) * high_weight
x7 <-  mmscalar(data$Density_pop_sq_km) * high_weight
x8 <-(as.factor(data$State_Capital))

## Convert region categorical variable into dummy variable
library(dummies)
x9 <-(as.factor(data$Region))
x9 <- dummy(data$Region, data = NULL, fun = as.factor)
x9 <- as.data.frame(x9)
x9 <- x9[,-3]

## Combine variables into dataframe
x.df <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)


## Cluster Models
## The following functions compute the hierarchical clustering models (agglomerative and divisive)

library(cluster)
x.diana <- diana(x.df)
x.agnes <- agnes(x.df)


## Plot Dendrograms to compute differences between the two clustering models. 
library(ggplot2)
library(dendextend)

dend1 <- as.dendrogram(x.diana)
dend2 <- as.dendrogram(x.agnes)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = TRUE,
           common_subtrees_color_lines = TRUE,
           common_subtrees_color_branches = TRUE,
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

## Plot the hierarchical clusters to identify similar cities.
## Reduce font size
par(cex=.3)

plot(x.agnes, labels = data$City)


plot(x.diana, labels = data$City)

