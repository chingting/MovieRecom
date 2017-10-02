#load data
setwd("C:\\Users\\ellen\\Desktop\\again")
movies2 <- read.csv("movies.csv", header = TRUE, stringsAsFactors=FALSE)
ratings2 <- read.csv("ratings100k.csv", header = TRUE)
library(dplyr)

ratings[]

a <- unique(movies2$movieId)
a1 <- as.data.frame(a)
colnames(a1)="movieId"
b <- unique(ratings2$movieId) 
b1 <- as.data.frame(b)
colnames(b1)="movieId"
newa <- inner_join(a1, b1['movieId'], by = 'movieId')
newb <- inner_join(b1, a1['movieId'], by = 'movieId')
movies <- inner_join(movies2, newa['movieId'], by = 'movieId')
ratings <- inner_join(ratings2, newb['movieId'], by = 'movieId')
write.csv(movies,file="C:\\Users\\ellen\\0930\\moviesclean.csv",row.names = FALSE)
write.csv(ratings,file="C:\\Users\\ellen\\0930\\ratingsclean.csv",row.names = FALSE)
#一開始先MATCH兩邊一樣的MOVIEiD
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aid <- unique(movies$userId) 
bid <- unique(ratings2$userId) 

setwd("C:\\Users\\ellen\\0930")
movies2 <- read.csv("moviesclean.csv", header = TRUE, stringsAsFactors=FALSE)
ratings2 <- read.csv("ratingsclean.csv", header = TRUE)
library(dplyr)
ratings <- read.csv("ratings100k.csv", header = TRUE)

bi <- unique(ratings2$userId) 
bi1 <- as.data.frame(bi)
colnames(bi1)="userId"
newbi <- inner_join(ratings, bi1['userId'], by = 'userId')
write.csv(newbi,file="C:\\Users\\ellen\\0930\\ratingstomovies.csv",row.names = FALSE)
#選完30部電影後，看這30部是哪個USER選的
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:\\Users\\ellen\\0930")
movies2 <- read.csv("movies.csv", header = TRUE)
ratings2 <- read.csv("ratingstomovies.csv", header = TRUE)
library(dplyr)
ri <- unique(ratings2$movieId) 
ri1 <- as.data.frame(ri)
colnames(ri1)="movieId"

mi <- unique(movies2$movieId) 
mi1 <- as.data.frame(mi)
colnames(mi1)="movieId"
newbi <- inner_join(ri1, mi1['movieId'], by = 'movieId')
newbi2 <- inner_join(mi1, ri1['movieId'], by = 'movieId')
final <- inner_join(movies2, newbi['movieId'], by = 'movieId')
write.csv(final,file="C:\\Users\\ellen\\0930\\moviesfinal.csv",row.names = FALSE)





# a <- unique(movies2$movieId)
# b <- unique(ratings2$movieId)  
# ar <- movies2[which((a %in% b) == FALSE),]  
# br <- ratings2[which((b %in% a) == FALSE),]  
# #movies <- inner_join(movies2, ar['movieId'], by = 'movieId')
# #ratings <- inner_join(ratings2, br['movieId'], by = 'movieId')
# movies <- anti_join(movies2, ar['movieId'])
# ratings <- inner_join(ratings2, movies['movieId'])
# write.csv(movies,file="C:\\Users\\ellen\\shinytest1\\movies0929.csv",row.names = FALSE)
# write.csv(ratings,file="C:\\Users\\ellen\\shinytest1\\ratings0929.csv",row.names = FALSE)
# 
# a1 <- unique(movies$movieId)
# b1 <- unique(ratings$movieId) 

library(reshape2)
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

##################
#Model Creation###
##################

library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
#ratingmat_norm <- normalize(ratingmat)

#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat, n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

# #Obtain Top-10 recommendations
# recom_result <- matrix(0,671,10)
# for (j in c(1:671)){
#   for (i in c(1:10)){
#     recom_result[j,i] <- as.integer(recom_list[[j]][i])
#   }
# }
# recom_result <- as.data.frame(recom_result)
# recom_result[] <-  lapply(recom_result, function(x) movies$title[match(x, movies$movieId)])
# recom_result
# 
# 
# recommender_model <- Recommender(ratingmat, method = "UBCF", param=list(method="Cosine",nn=30))
# #ubcf的參數為什麼要用cosine，nn是餘旋函數?為什麼設30?
# recom <- predict(recommender_model, ratingmat[3], n=10) #Obtain top 10 recommendations for 1st user in dataset
# #這邊的predict是預測模型嗎?
# recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain Top-10 recommendations
#這邊的推薦是怎麼形成的?
recom_result <- matrix(0,706,10)
for (j in c(1:706)){
  for (i in c(1:10)){
    recom_result[j,i] <- as.integer(recom_list[[j]][i])
  }
}
recom_result <- as.data.frame(recom_result)
recom_result[] <-  lapply(recom_result, function(x) movies$title[match(x, movies$movieId)])
recom_result


































recom_result<-as.data.frame(movies[recom_result,2])
colnames(recom_result)<-list("Top-10 Movies")
recom_result
recom_result <- as.data.frame(recom_result)
recom_result[] <-  lapply(recom_result, function(x) movies$title[match(x, movies$movieId)])
recom_result
##################
#Evaluate Model###
##################

#k=5 meaning a 5-fold cross validation. given=3 meaning 3 items withheld for evaluation
evaluation_scheme <- evaluationScheme(ratingmat, method="cross-validation", k=5, given=3, 
                                      goodRating=5) 
algorithms<-list(
  "random items"=list(name="RANDOM",param=NULL),
  "popular items"=list(name="POPULAR",param=NULL),
  "user-based CF"=list(name="UBCF",param=list(method="Cosine",nn=30))
)

evaluation_results<-evaluate(evaluation_scheme,algorithms,n=c(1,3,5,10,15,20)) #n=c denote top-N
plot(evaluation_results,legend="bottomright") #plot the avged ROC
plot(evaluation_results,"prec/rec") #plot the avged prec/rec

#get results for all runs of 'random items'
eval_results <- getConfusionMatrix(evaluation_results[[1]]) 
#alternatively, get avged result for 'random items'
avg(evaluation_results[[1]])