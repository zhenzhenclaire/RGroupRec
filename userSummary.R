userPath <- "/home/claire/IdeaProjects/groupMSTRec/data/clustering/userClusteringData"
userFile <- read.csv(userPath, header=TRUE, sep = ",")
user <- userFile[,-1]
#summary(user)
options(digits=3)

funny <- scale(user$funny, center = T,scale = T)
cool <- scale(user$cool, center = T,scale = T)
review_count <- scale(user$review_count, center = T,scale = T)
fans <- scale(user$fans, center = T,scale = T)
average_stars <- scale(user$average_stars, center = T,scale = T)

newUser <- cbind(funny,cool,review_count,fans,average_stars)
norm.user <- data.frame(funny = min.max.norm(funny),
                        cool = min.max.norm(cool),
                        review_count = min.max.norm(review_count),
                        fans = min.max.norm(fans),
                        average_stars = min.max.norm(average_stars))

ks <- kmeans(norm.user,centers=50,200)
userClusters <- ks$cluster

# Normalize clustering result to <business_id, clusterId>
user_id <- as.character(userFile$user_id)
userClusters <- as.character(ks$cluster)
result <- cbind(user_id,userClusters)

# Save cluster result to file
write.csv(result,"userClusteringResult",row.names = FALSE)

############### I'm functions ##########################
min.max.norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}