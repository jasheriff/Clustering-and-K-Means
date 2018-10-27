# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))
install.packages("cluster")
install.packages("rattle.data")
install.packages("NbClust")

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)
View(wine)
str(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function.
# We are creating a training set using "Type" as the test variable, and the rest as training set. 
data<- wine[ , 2:14]
str(data)
#ANOTHER WAY
# data <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

# (WE WANT CLUSTERS WITH RELATIVELY LOW SUMS OF SQUARES TO NEXT CLUSTER #)

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest? (4. slope stabalizes) or 15 (concave, but seems like a lot of clusters)  
#   * Why does this method work? What's the intuition behind it? We want clusters to be meaningful. 
#        A good number of clusters will clearly reduce the sum of squares, when comparing to an (n-1) number of clusters
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# 4. This is the fewest number of clusters with the lowest number of criteria. 
# Would also consider 3 clusters, but it seems like there is a significant improvement between 3 and 4.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# fit.km <- kmeans( ... )
fit.km3 <- kmeans(data, 3)
str(fit.km3)
fit.km3$size
fit.km3$centers
aggregate(data, by = list(cluster = fit.km3$cluster), mean)



# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering? 
# wine$Type is the test set. data (wine without Type) is training set
#ct stands for cross tabulation 

ct.km <- table(wine$Type, fit.km3$cluster)
ct.km 
sum(wine$Type == 1)
sum(wine$Type == 2)
sum(wine$Type == 3)
#The output table shows that we correctly predicted 
#1  2  3
#1 30  1 28
#2  9 62  0
#3 11 37  0

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

#clusplot( ... )
