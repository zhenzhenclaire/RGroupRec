 bbitemPath <- "/home/claire/IdeaProjects/groupMSTRec/data/clustering/itemClusteringData"
itemFile <- read.csv(itemPath, header=TRUE, sep = ",")
# Escape the first col-id
item <- itemFile[,-1]

# 数据的中心化是指数据集中的各项数据减去数据集的均值。
# 数据的标准化是指中心化之后的数据在除以数据集的标准差，即数据集中的各项数据减去数据集的均值再除以数据集的标准差。
# scale方法中的两个参数center和scale的解释：1.center和scale默认为真,即T或者TRUE 2.center为真表示数据中心化 3.scale为真表示数据标准化

# Processing numeric attribute: review_count & stars
# 限定输出小数点后数字的位数为3位
options(digits=3)
review_count <- scale(item$review_count, center = T,scale = T)
stars <- scale(item$stars, center = T,scale = T)

# Processing category & boolean attributes
state <- as.numeric(as.character(fun.state(item$state)))
alcohol <- as.numeric(as.character(fun.alco(item$Alcohol)))
noise <- as.numeric(as.character(fun.noise(item$Noise.Level)))
caters <- as.numeric(as.character(fun.bool(item$Caters)))
seating <- as.numeric(as.character(fun.bool(item$Outdoor.Seating)))
kids <- as.numeric(as.character(fun.bool(item$Good.for.Kids)))
groups <- as.numeric(as.character(fun.bool(item$Good.For.Groups)))

newItem <- cbind(review_count,state,stars,caters,noise,seating,alcohol,kids,groups)

# Make a summary to the item data
# summary(itemFile)
# summary(item)
# sum(complete.cases(newItem))

# k-means clustering
ks <- kmeans(newItem,centers=250,200)
itemClusters <- ks$cluster

# Normalize clustering result to <business_id, clusterId>
business_id <- as.character(itemFile$business_id)
itemClusters <- as.character(ks$cluster)
result <- cbind(business_id,itemClusters)

# Save cluster result to file
write.csv(result,"itemSummary/itemClusteringResult",row.names = FALSE)


# dissE <- daisy(newItem)
# sk <- silhouette(ks$cluster, dissE)
# plot(sk)
# pdf('my_nice_plot.pdf')
# plot(sk)
# dev.off()

############### I'm functions ##########################
fun.bool <- function(c){
  res <-vector(length=length(c))
  cName <- c("true","false");
  cPortion <- c(length(which(c=="true")),length(which(c=="false")))/length(c);
  matrix <- cbind(cName,cPortion);
  for(i in 1:length(c)){
    if (c[i]%in%cName) {
      res[i] <- matrix [which(matrix[,1]==c[i]),2]
    }
    else{
      res[i] <- 27/14710;
    }
  }
  return(res);
}

fun.noise <- function(noise){
  res <-vector(length=length(noise)) 
  cName <- c("average","loud","quiet","very_loud");
  cPortion <- c(length(which(noise=="average")),length(which(noise=="loud")),length(which(noise=="quiet")),length(which(noise=="very_loud")))/length(noise);
  noiseMatrix <- cbind(cName,cPortion);
  for(i in 1:length(noise)){
    if (noise[i]%in%cName) {
      res[i] <- noiseMatrix [which(noiseMatrix[,1]==noise[i]),2]
    }
  }
  return(res);
}

fun.alco <- function(alcohol){
  res <-vector(length=length(alcohol)) 
  cName <- c("beer_and_wine","full_bar","none");
  cPortion <- c(length(which(alcohol=="beer_and_wine")),length(which(alcohol=="full_bar")),length(which(alcohol=="none")))/length(alcohol);
  alcoMatrix <- cbind(cName,cPortion);
  for(i in 1:length(alcohol)){
    if (alcohol[i]%in%cName) {
      res[i] <- alcoMatrix [which(alcoMatrix[,1]==alcohol[i]),2]
    }
  }
  return(res);
}

fun.state <- function(states){
  res <-vector(length=length(states)) 
  cName <- c("AZ","NV","NC","QC","PA","WI");
  cPortion <- c(length(which(states=="AZ")),length(which(states=="NV")),length(which(states=="NC")),length(which(states=="QC")),length(which(states=="PA")),length(which(states=="WI")))/length(states);
  stateMatrix <- cbind(cName,cPortion);
  for(i in 1:length(states)){
    if (states[i]%in%cName) {
      res[i] <- stateMatrix [which(stateMatrix[,1]==states[i]),2]
    }
    else{
      res[i] <- 0.0554
    }
  }
  return(res);
}



