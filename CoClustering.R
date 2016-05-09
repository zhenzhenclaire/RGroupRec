library(hash)
library(kernlab)
library(mlbench)

options(stringsAsFactors=FALSE)

# Prepare three triple <clusteredUser, clusteredItem, avgRating>
ratingFile <- read.csv("/home/claire/IdeaProjects/groupMSTRec/data/ratingFile",header = FALSE)
userId <- ratingFile[,1]
itemId <- ratingFile[,2]

# userCluster <- read.csv("userClusteringResult",header = TRUE)
# userHash <- hash(userCluster$user_id,userCluster$userClusters)
# 
# itemCluster <- read.csv("itemClusteringResult",header = TRUE)
# itemHash <- hash(itemCluster$business_id,itemCluster$itemClusters)
# replacedUser <- fun.replace(userId,userCluster)
# replacedItem <- fun.replace(itemId,itemCluster)

# replacedUser <- fun.hashReplace(userId,userHash)
# replacedItem <- fun.hashReplace(itemId,itemHash)
# newRating <- cbind(replacedUser,replacedItem,ratingFile$V3)

newRating <- read.csv("/home/claire/RProjects/GroupRec/newRatingFile",header = FALSE)

n <- fun.combine(newRating)
cname <- c("userId","itemId","rating")
colnames(n) <- cname

# Change rating triple to a matrix 
ratingXtabs <- xtabs(rating~userId+itemId, data=n)
ratingMatrix <- matrix(data = ratingXtabs,nrow = 50,ncol = 250)
tRatingMatrix <- t(ratingMatrix)

mmatrix<-matrix(0,50,50)
nmatrix<-matrix(0,250,250)

mratingMatrix<-cbind(ratingMatrix,mmatrix)
nratingMatrix<-cbind(nmatrix,tRatingMatrix)
rating<-rbind(mratingMatrix,nratingMatrix)

# Start to co-clustering
sc <- specc(rating, centers = 50)
mean(sc@withinss)


scData <- sc@.Data
userCl <- scData[1:50]
itemCl <- scData[51:300]

# Save userClusterId(itemClusterId) and corresponding mixClusterId
table(userCl)
table(itemCl)
write.csv(userCl,"data/usercl_mixcl",row.names = TRUE)
write.csv(itemCl,"data/itemcl_mixcl",row.names = TRUE)

# Load user(item) and userClusterId(itemClusterId)
user_cl <- read.table("data/userClusteringResult",header = TRUE,sep = ",")
item_cl <- read.table("data/itemClusteringResult",header = TRUE,sep = ",")

# Add avg_rating of user(item) to user_cl
userPath <- "/home/claire/IdeaProjects/groupMSTRec/data/clustering/userClusteringData"
userFile <- read.csv(userPath, header=TRUE, sep = ",")
userAvgRating <- cbind(userFile$user_id,userFile$average_stars)
colnames(userAvgRating) <- c("user_id","avg_stars")
user_cl <- merge(user_cl,userAvgRating,by.x = "user_id",by.y = "user_id")


itemPath <- "/home/claire/IdeaProjects/groupMSTRec/data/clustering/itemClusteringData"
itemFile <- read.csv(itemPath, header=TRUE, sep = ",")
itemAvgRating <- cbind(itemFile$business_id,itemFile$stars)
colnames(itemAvgRating) <- c("business_id","stars")
item_cl <- merge(item_cl,itemAvgRating,by.x = "business_id",by.y = "business_id")

# Change stars to interger
userstars <- as.numeric(user_cl$avg_stars)
user_cl <- user_cl[,-3]
user_cl <- cbind(user_cl,userstars)

itemstars <- as.numeric(item_cl$stars)
item_cl <- item_cl[,-3]
item_cl <- cbind(item_cl,itemstars)

# Count avg rating of userCluster(itemCluster) 
itemClAvg <- tapply(item_cl$itemstars,item_cl$itemClusters, mean)
itemClAvg <- cbind(c(1:length(itemClAvg)),itemClAvg)
colnames(itemClAvg) <- c("itemcl","avgstars")

userClAvg <- tapply(user_cl$userstars,user_cl$userClusters, mean)
userClAvg <- cbind(c(1:length(userClAvg)),userClAvg)
colnames(userClAvg) <- c("usercl","avgstars")

write.csv(userClAvg,"data/usercl_avg",row.names = FALSE)
write.csv(itemClAvg,"data/itemcl_avg",row.names = FALSE)
          
write.csv(user_cl,"data/user_cl",row.names = FALSE)
write.csv(item_cl,"data/item_cl",row.names = FALSE)

usercl_mixcl <- read.table("data/usercl_mixcl",header = TRUE,sep = ",")
itemcl_mixcl <- read.table("data/itemcl_mixcl",header = TRUE,sep = ",")

user_usercl_mixcl <- merge(usercl_mixcl,user_cl,by.x="userClusters",by.y = "userClusters")
item_itemcl_mixcl <- merge(itemcl_mixcl,item_cl,by.x="itemClusters",by.y = "itemClusters")


# Save userId(itemId), userClusterId(itemClusterId) and corresponding mixClusterId
write.csv(user_usercl_mixcl,"data/user_usercl_mixcl",row.names = FALSE)
write.csv(item_itemcl_mixcl,"data/item_usercl_mixcl",row.names = FALSE)
############### I'm functions ##########################

fun.replace <- function(id, cluster){
  res <-vector(length=length(id))
  for(i in 1:length(id)){
    if (id[i]%in%cluster[,1]) {
      res[i] <- cluster[which(cluster[,1]==id[i]),2];
    }
  }
  return(res);
}

fun.hashReplace <- function(id,hash){
  res <-vector(length=length(id));
  for(i in 1:length(id)){
    c <- id[i];
    if(has.key(c,hash)==TRUE){
      val <- as.list(hash[c])[1]
      res[i] <- val;
    } 
  }
  return(res);
}

fun.combine <- function(newRating){
  re <- aggregate(newRating[,3],newRating[,c(1,2)],mean);
  return(re);
  # with(newRating,tapply(newRating[,3],c(newRating[,1],newRating[,2]),mean))
}