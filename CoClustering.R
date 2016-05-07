library(hash)
library(kernlab)
library(kknn)

options(stringsAsFactors=FALSE)

# Prepare three triple <clusteredUser, clusteredItem, avgRating>
ratingFile <- read.csv("/home/claire/IdeaProjects/groupMSTRec/data/ratingFile",header = FALSE)
userId <- ratingFile[,1]
itemId <- ratingFile[,2]

userCluster <- read.csv("userClusteringResult",header = TRUE)
userHash <- hash(userCluster$user_id,userCluster$userClusters)

itemCluster <- read.csv("itemClusteringResult",header = TRUE)
itemHash <- hash(itemCluster$business_id,itemCluster$itemClusters)
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
sc <- specc(rating, centers = 20)

sc2 <- specClust(rating,centers = 20)

# plot(rating, pch = (23 - 2 * sc))

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