#Collobarative Filtering Movie Recommendation 
#MovieLens Data


#library importing 
library(recommenderlab) #for recommendation 
library(reshape2)
library(data.table)
library(ggplot2) #visualisation 


#Retreiving the data
movie_data  <- read.csv("/Users/manib/documents/IMDB-Dataset/movies.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("/Users/manib/documents/IMDB-dataset/ratings.csv")

#structure 
str(movie_data)
str(rating_data)


data.table(rating_data)
if (!require("DT")) devtools::install_github("rstudio/DT")

datatable(iris)
#tabular viewer 
datatable(movie_data) #id, title, genres 
datatable(rating_data)


#Summary statsitics
summary(movie_data)
summary(rating_data)

#The genre needs to be implemented in a more useful manner 
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
movie_genre2 <-  as.data.frame(tstrsplit(movie_genre[,1], "[|]", type.convert = TRUE),stringsAsFactors = FALSE)


colnames(movie_genre2) <-c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children",
                "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery", "Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_matl <- matrix(0,10330,18)
genre_matl[1,] <- list_genre
colnames(genre_matl) <- list_genre

for (index in 1:nrow(movie_genre2)){
  for(col in 1:ncol(movie_genre2)){
    gen_col = which(genre_matl[1,] == movie_genre2[index,col])
    genre_matl[index+1,gen_col] <- 1
  } 
}


genre_mat2 <- as.data.frame(genre_matl[-1,], stringsAsFactors =FALSE)

for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}

str(genre_mat2)  


#head(movie_data)
#create a search maxtrix that will generate films based on genres 

SearchMovie <- cbind(movie_data[,1:2],genre_mat2[])

head(SearchMovie)


#A alot of the films have several diverse generes 
#Need to create a sparse matrix for recommendation 

ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE) #The sparse matrix
ratingMatrix <- as.matrix(ratingMatrix) #don't need userId as it's in RatingMatrix already
#rating matrix needs to be converted into a recommenderlab sparse matrix 
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")

install.packages("reshape2") # may already be installed
library(reshape2)

RatingMatrix
library(recommenderlab)

#building a recommendation model 
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)



lapply(recommendation_model, "[[", "description")
#For this project item-based colloborative filtering will be used 

recommendation_model$IBCF_realRatingMatrix$parameters
#checking similarity of films
movie_similarity <- similarity(ratingMatrix)[ ,1:4],method = "cosine",which="items")

as.matrix(similarity_mat)

image(as.matrix(movie_similarity), main = "movie similarity")


#rating values
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values)

#The measure of rating as count of numbers 

Table_rating <- table(rating_values)
Table_rating


#most viewed films visualisation 

movie_views <- colCounts(ratingMatrix) #count views for each movie
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) #create dataframe of views 
table_views <- table_views[order(table_views$views, 
                                 decreasing = TRUE), ] #sort by number of views 
table_views$title <- NA 
for (index in 1:30525){
  table_views[index,3] <- as.character(subset(movie_data,
                                              movie_data$movieId == table_views[index,1])$title
                             
}
table_views[1:6,]













#There is a lot of sparse data

#These steps need to be taken
#1.select useful data 
#2.Normalise it
#3.Binarize it

#Determine how many user's need to rate a movie in order for it to be useful
#Assume 50 

movie_ratings <- ratingMatrix [rowCounts(ratingMatrix) > 50, colCounts(ratingMatrix)

movie_ratings)








#Distribution of the average rating of users
average_ratings  <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("black"), col=I("blue")) +
  ggtitle("Distribution of the average rating per user")


# Load training data
data <- list(list(0, 0, 4.0), list(0, 1, 2.0), list(1, 1, 3.0),
             list(1, 2, 4.0), list(2, 1, 1.0), list(2, 2, 5.0))
df <- createDataFrame(data, c("userId", "movieId", "rating"))
training <- df
test <- df

# Fit a recommendation model using ALS with spark.als
model <- spark.als(training, maxIter = 5, regParam = 0.01, userCol = "userId",
                   itemCol = "movieId", ratingCol = "rating")

# Model summary
summary(model)

# Prediction
predictions <- predict(model, test)
head(predictions)
















  










































#Implement the sum of rows and columns with the similiary of the objects above 0

sum_rows <- rowSums(info$sim > 0)
table(sum_rows)
sum_cols <- colSums(info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I ("red"))+ ggtitle("Distribution of the column count")



#Recommending movies
top_recommendations <- 10 # the number of items to recommend for each user 
predicted_recommendations <- predict(object = recommendation_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations



IB <- Recommender(getData(evalu, "train"), "IBCF", 
                  param=list(normalize = "Z-score",method="Cosine"))

p1 <- predict(IB, getData(evalu, "known"), type="ratings")

p1@data@x[p1@data@x[] < 1] <- 1
p1@data@x[p1@data@x[] > 5] <- 5

IB_acc <- calcPredictionAccuracy(p1, getData(evalu, "unknown"))


# print out errors in table
error <- rbind(UB_acc,IB_acc)
error

##            RMSE      MSE       MAE
## UB_acc 1.336280 1.772324 0.9614739
## IB_acc 1.472632 2.162741 1.1342260







