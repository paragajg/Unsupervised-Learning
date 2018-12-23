## load packages
library(ggplot2) # will be used for plotting complex and customisable graphs
library(reshape2) # has useful functions which can be used for restructuring the data
## Load data
bank <- read.csv("bank-full.csv", header=T,sep=";")


bank <- subset(bank, contact %in% c("cellular", "telephone"))
bank$contact <- as.factor(as.character(bank$contact))

# Balance
p <- qplot(balance, data = bank, geom = "density", alpha = I(1/2))
p <- p + ggtitle("Histogram of Balance")
p

## The balance column does not appear to follow a normal distribution. There are some outliers that add noise to the variable so there is a need to remove such values

#Using the interquartile rule, we proceed with the exclusion of all values that lie above the third quartile plus 1.5 times the IQR, and below the first quartile minus 1.5 times the IQR (-2067.5, 3664.5).
summary(bank$balance)
IQR(bank$balance)

bank <- subset(bank, balance>-2067.5 & balance<3664.5)

p <- qplot(balance, data = bank,
           geom = "density", alpha = I(1/2))
p <- p + ggtitle("Histogram of Balance")
p

## Feature selections -We now eliminate columns that we don´t want to include in the analysis (all marketing campaign columns)
p <- ggplot(bank, aes(x=age, y=balance))
p <- p + geom_point()
p

## Scaling of features - Let´s standardize those two columns by substracting their respective means and dividing by their standard deviations.

feat.scaled <- scale(bank[,c("age","balance")])

## Build Kmeans clusters
set.seed(15555)
pclusters <- kmeans(feat.scaled, 4, nstart=20, iter.max=100)

groups <- pclusters$cluster
clusterDF <- cbind(as.data.frame(feat.scaled), Cluster=as.factor(groups))

plot(feat.scaled[, c("balance", "age")],col = pclusters$cluster)

bank_new <-bank[,c("age","balance","duration","pdays")]
bank_new$Clusters <- pclusters$cluster
bank_new$Clusters <- as.factor(bank_new$Clusters)

## Profile the clusters
df.m <- melt(bank_new, id.var = "Clusters")
## melt is a function from reshape package used for changing the structure of data. In the current example bank_new has a wide format data structure and df.m has long format data structure. For plotting multiple box plots across Clusters in ggplot we need long format of data.

p <- ggplot(data = df.m, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Clusters))
p + facet_wrap( ~ variable, scales="free")
