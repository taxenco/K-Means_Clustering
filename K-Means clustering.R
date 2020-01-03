install.packages('reshape2')
install.packages('factoextra')
library(reshape2)
library(factoextra)
setwd('C:/Users/carlo/Desktop/Data science/ASDM/K-Means clustering')
  crimes  <-read.csv("crimes.csv", sep = ",", header= TRUE)
  
  # Inspect the dataset 
  
  names(crimes)
  head(crimes)
  tail(crimes)
  summary(crimes)
  str(crimes)
  nrow(crimes)
  ncol(crimes)
  dim(crimes)
  crimes<-crimes[,c(3,7,9)]
  head(crimes)
  
# Data wrangling
  crimes_pivot<-dcast(crimes, Force.Name ~ Offence.Subgroup, sum,  value.var ="Number.of.offences")
  head(crimes_pivot)
  rownames(crimes_pivot) <-crimes_pivot[,1] # Set valuesin Force.Namecolumn as rownames 
  crimes_pivot[,1] <-NULL
  head(crimes_pivot)
  
  # Normalising the dataset. 
  normalise <-function(df){return(((df-min(df)) /(max(df)-min(df))*(1-0))+0)}
  Force.Name<-rownames(crimes_pivot)
  crimes_pivot_n<-as.data.frame(lapply(crimes_pivot,normalise))
  rownames(crimes_pivot_n)<-Force.Name
  head(crimes_pivot_n)
  
  # Assessing clustering tendency
  
  tendency <-get_clust_tendency(crimes_pivot_n, 30, graph = TRUE)
  tendency$hopkins_stat # If the value of Hopkins is < 0.5, then it is unlikely that exist statistically significant clusters.However, if the value is > 0.5 is likely that exist statistically significant clusters 
  
  # Choosing right number of expected clusters (k)?
  
  fviz_nbclust(crimes_pivot_n, kmeans, method = "wss") 
  
  # Perform k-means clustering 
  set.seed(123)
  km.fit <-kmeans(crimes_pivot_n, 3, nstart = 30)
  km.fit$cluster
  km.fit$size
  fviz_cluster(km.fit,crimes_pivot_n)# Visualize clusters 
  
  # k-means  clustering,  butwithout "Metropolitan  Police
  
  crimes_pivot_n2<-subset(crimes_pivot_n, rownames(crimes_pivot_n)!="Metropolitan Police")
  set.seed(123)
  km.fit2 <-kmeans(crimes_pivot_n2, 3, nstart = 30)
  km.fit2$cluster
  km.fit2$size
  fviz_cluster(km.fit2,crimes_pivot_n2)# Visualize clusters 
  
  # k-means  clustering,  butwithout "Metropolitan Police, Greater Manchester, West Midlands and West Yorkshire' 
  
  crimes_pivot_n3<-subset(crimes_pivot_n, !(rownames(crimes_pivot_n) %in% c("Metropolitan Police","Greater Manchester","West Midlands","West Yorkshire")))
  set.seed(123)
  km.fit3 <-kmeans(crimes_pivot_n3, 3, nstart = 30)
  km.fit3$cluster
  km.fit3$size
  fviz_cluster(km.fit3,crimes_pivot_n3)
  fviz_cluster(km.fit3,crimes_pivot_n3,ellipse.type = "norm")