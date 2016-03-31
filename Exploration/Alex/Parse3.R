#rm(list=ls())

library("recommenderlab")
library(ggplot2)
library(gridExtra)
library(dplyr)


################R script to calulate affinity matrix of users ################
# Four variables for each user: age, sex, salary, zipcode
# We calculate similarily for each and take the weighted average of them

# A function to transform the Ordinal variable to quantitative variable
trans_ordinal <- function(ordinal_col) { 
  M <- max(ordinal_col)
  col_trans <- (ordinal_col - 1/2) / M
  return(col_trans)
}


user <- read.table("~/ml-100k/u.user", sep = "|", 
                   colClasses = c("character", "integer", "factor", "character", "character"), 
                   col.names = c("user_id", "age", "gender", "occupation", "zipcode"))

salaries = c()
salaries["administrator"] = 100
salaries["artist"] = 50
salaries["doctor"] = 200
salaries["educator"] = 50
salaries["engineer"] = 100
salaries["entertainment"] = 50
salaries["executive"] = 120
salaries["healthcare"] = 80
salaries["homemaker"] = 20
salaries["lawyer"] = 130
salaries["librarian"] = 60
salaries["marketing"] = 70
salaries["none"] = 20
salaries["other"] = 50
salaries["programmer"] = 100
salaries["retired"] = 20
salaries["salesman"] = 70
salaries["scientist"] = 70
salaries["student"] = 20
salaries["technician"] = 60
salaries["writer"] = 70

# remove id
user$user_id <- NULL
# transform age
user$age <- trans_ordinal(user$age)
# transform gender
user$gender <- ifelse(user$gender == 'M', 1, 0)

library(plyr)
suppressPackageStartupMessages(library(dplyr))
# Reorder the occupation
#user$occupation <- as.numeric(mapvalues(user$occupation, from = unique(user$occupation), to = 1:21))

user$salaries <- as.numeric(salaries[user$occupation])/150

user$occupation <-NULL

# since occupation is now a ordinal column, we can use the same transformation as age
#user$occupation <- trans_ordinal(user$occupation)

# remove zipcode for now
user$zipcode <- NULL

# A function to calculate the affinity matrix
affinity_mat <- function(user_df) { # user_df is the data frame of user info.
  n <- dim(user_df)[1] # number of users
  aff_mat <- matrix(numeric(n*n), nrow = n)
  # set the diagonal element to 1
  diag(aff_mat) <- 1
  
  # vectors to store the variance and mean of each user
  var_user <- sapply(1:n, FUN = function(x) sum((user[x, ] - mean(as.numeric(user[x, ])))^2))
  
  mean_user = sapply(1:n, FUN = function(x) mean(as.numeric(user[x, ])))
  
  
  for (i in seq_len(n)) {
    if (i == n) break
    #if (i %% 5 == 2) print(i)
    for (j in seq(i+1, n)) {
      nume <- sum((user[i, ] - mean_user[i]) * (user[j, ] - mean_user[j]))
      denum <- sqrt(var_user[i] * var_user[j])
      aff_mat[i, j] <- nume/denum
    }
  }
  return(aff_mat)
}


system.time(affinity_matrix <- affinity_mat(user[1:843, ]))




data = read.table("~/ml-100k/u.data")
df.ratings = data.frame(userid=data$V1, movieid=data$V2, rating=data$V3, time=data$V4)
rm(data)

data = read.csv("~/ml-100k/u.user", sep="|", header=FALSE)
df.users = data.frame(userid=data$V1, age=data$V2, gender=data$V3, occupation = data$V4, zip=data$V5)
rm(data)

data = read.csv("~/ml-100k/u.genre", sep="|", header=FALSE)
df.genre = data.frame(genre=data$V1, genreid=data$V2)
rm(data)

data = read.csv("~/ml-100k/u.item", sep="|", allowEscapes=FALSE, header=FALSE)
colnames = c("movieid", "title", "date_released", "video_released", "IMDB_URL", as.character(df.genre$genre))
df.movies = data.frame(data)
colnames(df.movies) = colnames
rm(colnames)
rm(data)






id_count<- function(data, id) {
  
  if(length(id) == 1) {
    return(  length(data[as.numeric(data)==id])  )
  }
  else
  {
    print(id)
    return(
      sum(
        mapply(
        FUN=id_count,
        id=id,
        MoreArgs=list(data=data)    
           )
      )
    )
  }
  
}

id_list<- function(data, id) {
  if(length(id) == 1) {
    return(  which(as.numeric(data)==id)  )
  }
  else
  {
    return(
      c(
        unlist(
          mapply(
            FUN=id_list,
            id=id,
            MoreArgs=list(data=data)    
          )
        )
      )
    )
  }
}

genre_count <-mapply(
  FUN=id_count, 
  data=df.movies[,6:24], 
  MoreArgs=list(id=1))

df.genre$numFilms = unname(genre_count)

genre_list <-mapply(
  FUN=id_list, 
  data=df.movies[,6:24], 
  MoreArgs=list(id=1))


ratingListByGenre <-mapply(
  FUN=id_list, 
  id=genre_list, 
  MoreArgs=list(data=df.ratings[,2]))

mean_unlist <- function(data, indexList, column) {
  indexVector = unlist(indexList)
  return(mean(as.numeric(data[indexVector,column])))
}

dist_unlist <- function(data, indexList, column) {
  indexVector = unlist(indexList)
  return(tabulate(data[indexVector,column], nbins=5 ))
}

#Input - 5 Columns, output - star rating
mean_rating_star <- function(data)
{
  return(apply(data*(5:1), MARGIN=1, FUN=sum)/apply(data, MARGIN=1, FUN=sum))
}


df.genre$AvgRating <-
  mapply(
    FUN=mean_unlist,
    indexList=ratingListByGenre,
    MoreArgs=list(data=df.ratings, column=3)  
  )

ratingsByGenre = mapply(
  FUN=dist_unlist,
  indexList=ratingListByGenre,
  MoreArgs=list(data=df.ratings, column=3)  
)


df.genre[,c("1","2","3","4","5")] = t(ratingsByGenre)
  
ratingListByMovie = mapply(
  FUN=id_list, 
  id=df.movies$movieid, 
  MoreArgs=list(data=df.ratings[,2]))



df.movies$AvgRating = mapply(
  FUN=mean_unlist,
  indexList=ratingListByMovie,
  MoreArgs=list(data=df.ratings, column=3)  
)

ratingsByMovie = mapply(
  FUN=dist_unlist,
  indexList=ratingListByMovie,
  MoreArgs=list(data=df.ratings, column=3)  
)

df.movies[,c("1","2","3","4","5")] = t(ratingsByMovie)

design.matrix = df.movies[,-c(1:5, 26:30)]
for(i in 1:dim(design.matrix)[1])
{
  design.matrix[i,1:19] = design.matrix[i,1:19]/sum(design.matrix[i,1:19])
}
out = lm(AvgRating ~ ., data = design.matrix)

#out = lm(AvgRating ~ ., data = df.movies[,-c(1:5, 26:30)])

#df.genre$fittedGenres = unname(out$coefficients)[2:20] + unname(out$coefficients[1])


df.movies$expectedGenreRating = fitted(out)
  
numRatings = apply(df.movies[,26:30], MARGIN=1, FUN=sum)

PriorRatings = (numRatings*df.movies$AvgRating + 20*df.movies$expectedGenreRating)/
                (numRatings + 20)

df.movies$adjustedRating = 
  PriorRatings - df.movies$expectedGenreRating

df.users$numFilms = as.numeric(table(sort(df.ratings[,1])))

temp = sort.int(df.users$numFilms, index.return=TRUE)

df.sortedUsers = data.frame(userid=temp$ix, numFilms=temp$x)
df.sortedUsers$cumulative = cumsum(df.sortedUsers$numFilms)
df.sortedUsers$sortedOrder = 1:length(df.sortedUsers$userid)

df.ratingsTraining = df.ratings[which(df.ratings$userid %in% 1:743), ]
df.ratingsTest = df.ratings[which(df.ratings$userid %in% 744:843), ]

ratingListByMovie.train = mapply(
  FUN=id_list, 
  id=df.movies$movieid, 
  MoreArgs=list(data=df.ratingsTraining[,2]))

ratingListByMovie.test = mapply(
  FUN=id_list, 
  id=df.movies$movieid, 
  MoreArgs=list(data=df.ratingsTest[,2]))

moviesByAdjRating = df.movies[sort(df.movies$adjustedRating, index.return=TRUE, decreasing=TRUE)$ix,c(2,3,25,31,32)]

recommended = matrix(numeric(1682*100), nrow = 1682)
actual = matrix(numeric(1682*100), nrow = 1682)

for(i in 1:1682)
{
  trainRatings = df.ratingsTraining[ratingListByMovie.train[[i]],]
  testRatings = df.ratingsTest[ratingListByMovie.test[[i]],]
  
  if(dim(testRatings)[1] == 0 || dim(testRatings)[1] == 0)
  {
    actual[i,] = 0
    recommended[i,] = 0
    next
  }
  for(j in 1:100)
  {
    if(testRatings[testRatings$userid==j+743,2] == i)
    {
      actual[i,j] = testRatings[testRatings$userid==j+743,2]
    }
    else
    {
      actual[i,j] = 0
    }
    j+743
    recommended[i,j]
    
  }

}

# 
# p1 = ggplot(df.sortedUsers) +
#       geom_smooth(
#       aes(x=sortedOrder, 
#           y=cumulative)
#       )  +
#   theme(panel.grid.major = element_line(color = "blue", linetype="dotted"))+
#   labs(title="CDF of reviews by users", x="Users (from least to most prolific)", y="Cumulative Total Reviews")
# 
# p2 = ggplot(df.sortedUsers,aes(x=numFilms)) +
#   geom_histogram(binwidth=1) + 
#   geom_step(aes(stat="ecdf", y=(sortedOrder*100/max(sortedOrder)))) +
#   geom_step(aes(stat="ecdf", y=(cumulative*100/max(cumulative)))) + 
#   theme(panel.grid.major = element_line(color = "blue", linetype="dotted"))+
#   labs(title="CDF of reviews, users + Histogram of films reviewed", x="Films Reviewed", y="Count (Histogram)/% of Users/Reviews")
# 
# grid.arrange(p1,p2)
# 
# 
# # p3 = ggplot(df.genre, aes(x=genre, y=mean_rating_star(df.genre[5:9]), size=numFilms)) +
# #   geom_point() + labs(title="Distribution of Films/Ratings by Genre", y = "Rating")
# 
#  p3 = ggplot(df.genre, aes(x=genre, y=AvgRating, size=numFilms)) +
#    geom_point() + labs(title="Distribution of Films/Ratings by Genre", y = "Rating")
# 
# 
# p4 = ggplot(df.movies, aes(x=apply(df.movies[6:24], MARGIN=1, FUN=sum), y=AvgRating)) +
#   geom_point() + labs(title="Number of genres Per Movie", x="# of Genres", y="Film Rating")
# 
# 
# 
# grid.arrange(p3,p4)