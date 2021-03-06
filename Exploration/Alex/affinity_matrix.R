################R script to calulate affinity matrix of users ################
# Four variables for each user: age, sex, salary, zipcode
# We calculate similarily for each and take the weighted average of them

# A function to transform the Ordinal variable to quantitative variable
rm(list=ls())

trans_ordinal <- function(ordinal_col) { 
  M <- max(ordinal_col)
  col_trans <- (ordinal_col - 1/2) / M
  return(col_trans)
}




user <- read.table("~/ml-100k/u.user", sep = "|", 
                   colClasses = c("character", "integer", "factor", "character", "character"), 
                   col.names = c("user_id", "age", "gender", "occupation", "zipcode"))

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

user$salaries <- as.numeric(salaries[user$occupation])/150

df.ratings100 = df.ratings[which(df.ratings$movie %in% 1:100), ]

user$avgRating = sapply(1:n, function(n) {mean(df.ratings100[which(df.ratings100$userid == n), 3]) })

join_df = join(df.ratings100, df.ratings100, by=c("movieid"), type="inner")


# remove id
user$user_id <- NULL
# transform age
user$age <- trans_ordinal(user$age)
# transform gender
user$gender <- ifelse(user$gender == 'M', 1, 0)

library(plyr)
suppressPackageStartupMessages(library(dplyr))
# Reorder the occupation
user$occupation <- as.numeric(mapvalues(user$occupation, from = unique(user$occupation), to = 1:21))
# since occupation is now a ordinal column, we can use the same transformation as age
user$occupation <- trans_ordinal(user$occupation)

# remove zipcode for now
user$zipcode <- NULL



# A function to calculate the affinity matrix
# the output will be a list of 3 affinity matrix for each column
 # user_df is the data frame of user info.
  # normalize the data frame
user_df <- scale(user)
  
n <- dim(user)[1] # number of users
  
aff_age <- matrix(numeric(n*n), nrow = n)
aff_gender <- matrix(numeric(n*n), nrow = n)
aff_occupation <- matrix(numeric(n*n), nrow = n)
aff_count <- matrix(numeric(n*n), nrow = n)
aff_ratings_diff <- matrix(numeric(n*n), nrow = n)
# set the diagonal element to 1
diag(aff_age) <- 1
diag(aff_gender) <- 1
diag(aff_occupation) <- 1

# a vector to store the variance of each user
var_user <- sapply(1:n, FUN = function(x) sum((user[x, ] - mean(as.numeric(user[x, ])))^2))
# a vector to store the mean of each user
mean_user <- rowMeans(user)
#join_df = join(attr_df, attr_df, by=c(attr))

index = 0
indexindex=0
indexes = rep(0, times=n*(n+1)/2)
  
for (i in seq_len(n)) {
  if (i == n) break
  join_df_temp = join_df[which(join_df[,1] == i),]
  aff_count[i,i] <- 100
  index = (i-1)*n + i
  indexindex = indexindex + 1
  indexes[indexindex] = index
  for (j in seq(i+1, n)) {
    index = index + 1
    age <- (user[i, 'age']-mean_user[i]) * (user[j, 'age'] - mean_user[j])
    gender <- (user[i, 'gender']-mean_user[i]) * (user[j, 'gender'] - mean_user[j])
    occu <- (user[i, 'salaries']-mean_user[i]) * (user[j, 'salaries'] - mean_user[j])
    aff_age[i, j] <- age/sqrt(var_user[i]*var_user[j])
    aff_gender[i, j] <- gender/sqrt(var_user[i]*var_user[j])
    aff_occupation[i, j] <- occu/sqrt(var_user[i]*var_user[j])
    matches = join_df_temp[which(join_df_temp[,5] == j),]
    aff_count[i, j] <- length(matches[,2])
    if(length(matches[,2]) > 0) {
#         print(paste("i: ",i,"\nj: ", j, "\n"))
#         print(matches[,3])
      indexindex = indexindex + 1
      indexes[indexindex] = index
      aff_ratings_diff[i, j] <- mean(matches[,3]) - mean(matches[,6])  
      
    } else {
    aff_ratings_diff[i, j] <- NA
    }
  }
}



indexes = indexes[1:indexindex]
arr_age = unlist(t(aff_age))[indexes]
arr_gender = unlist(t(aff_gender))[indexes]
arr_occupation = unlist(t(aff_occupation))[indexes]
arr_count = unlist(t(aff_count))[indexes]
arr_ratings_diff = unlist(t(aff_ratings_diff))[indexes]

regression.df = data.frame(arr_ratings_diff, arr_age, arr_gender, arr_occupation)#, arr_count)



outUserDiffs = lm(arr_ratings_diff ~ . + 0, regression.df)
outUsers = lm(user$avgRating ~ user$age + user$gender + user$salaries + 0)

# affinity_matrix is a list of affinity matrix for each column (age, sex, occupation)
#system.time(affinity_matrix <- affinity_mat(user))

# 
# 
# id_count<- function(data, id) {
#   
#   if(length(id) == 1) {
#     return(  length(data[as.numeric(data)==id])  )
#   }
#   else
#   {
#     print(id)
#     return(
#       sum(
#         mapply(
#           FUN=id_count,
#           id=id,
#           MoreArgs=list(data=data)    
#         )
#       )
#     )
#   }
#   
# }
# 
# id_list<- function(data, id) {
#   if(length(id) == 1) {
#     return(  which(as.numeric(data)==id)  )
#   }
#   else
#   {
#     return(
#       c(
#         unlist(
#           mapply(
#             FUN=id_list,
#             id=id,
#             MoreArgs=list(data=data)    
#           )
#         )
#       )
#     )
#   }
# }
# 
# genre_count <-mapply(
#   FUN=id_count, 
#   data=df.movies[,6:24], 
#   MoreArgs=list(id=1))
# 
# df.genre$numFilms = unname(genre_count)
# 
# genre_list <-mapply(
#   FUN=id_list, 
#   data=df.movies[,6:24], 
#   MoreArgs=list(id=1))
# 
# 
# ratingListByGenre <-mapply(
#   FUN=id_list, 
#   id=genre_list, 
#   MoreArgs=list(data=df.ratings[,2]))
# 
# mean_unlist <- function(data, indexList, column) {
#   indexVector = unlist(indexList)
#   return(mean(as.numeric(data[indexVector,column])))
# }
# 
# dist_unlist <- function(data, indexList, column) {
#   indexVector = unlist(indexList)
#   return(tabulate(data[indexVector,column], nbins=5 ))
# }
# 
# #Input - 5 Columns, output - star rating
# mean_rating_star <- function(data)
# {
#   return(apply(data*(5:1), MARGIN=1, FUN=sum)/apply(data, MARGIN=1, FUN=sum))
# }
# 
# 
# 
# 
# df.genre$AvgRating <-
#   mapply(
#     FUN=mean_unlist,
#     indexList=ratingListByGenre,
#     MoreArgs=list(data=df.ratings, column=3)  
#   )
# 
# ratingsByGenre = mapply(
#   FUN=dist_unlist,
#   indexList=ratingListByGenre,
#   MoreArgs=list(data=df.ratings, column=3)  
# )
# 
# 
# df.genre[,c("1","2","3","4","5")] = t(ratingsByGenre)
# 
# ratingListByMovie = mapply(
#   FUN=id_list, 
#   id=df.movies$movieid, 
#   MoreArgs=list(data=df.ratings[,2]))
# 
# 
# 
# df.movies$AvgRating = mapply(
#   FUN=mean_unlist,
#   indexList=ratingListByMovie,
#   MoreArgs=list(data=df.ratings, column=3)  
# )
# 
# ratingsByMovie = mapply(
#   FUN=dist_unlist,
#   indexList=ratingListByMovie,
#   MoreArgs=list(data=df.ratings, column=3)  
# )
# 
# df.movies[,c("1","2","3","4","5")] = t(ratingsByMovie)
# 
# design.matrix = df.movies[,-c(1:5, 26:30)]
# for(i in 1:dim(design.matrix)[1])
# {
#   design.matrix[i,1:19] = design.matrix[i,1:19]/sum(design.matrix[i,1:19])
# }
# out = lm(AvgRating ~ ., data = design.matrix)
# 
# #out = lm(AvgRating ~ ., data = df.movies[,-c(1:5, 26:30)])
# 
# df.genre$fittedGenres = c(unname(out$coefficients)[2:19] + unname(out$coefficients[1]),  unname(out$coefficients[1]))
# 
# 
# df.movies$expectedGenreRating = fitted(out)
# 
# numRatings = apply(df.movies[,26:30], MARGIN=1, FUN=sum)
# 
# PriorRatings = (numRatings*df.movies$AvgRating + 20*df.movies$expectedGenreRating)/
#   (numRatings + 20)
# 
# df.movies$adjustedRating = 
#   PriorRatings - df.movies$expectedGenreRating
# 
# df.users$numFilms = as.numeric(table(sort(df.ratings[,1])))
# 
# temp = sort.int(df.users$numFilms, index.return=TRUE)
# 
# df.sortedUsers = data.frame(userid=temp$ix, numFilms=temp$x)
# df.sortedUsers$cumulative = cumsum(df.sortedUsers$numFilms)
# df.sortedUsers$sortedOrder = 1:length(df.sortedUsers$userid)
# 
# df.ratingsTraining = df.ratings[which(df.ratings$userid %in% 1:743), ]
# df.ratingsTest = df.ratings[which(df.ratings$userid %in% 744:843), ]
# 
# ratingListByMovie.train = mapply(
#   FUN=id_list, 
#   id=df.movies$movieid, 
#   MoreArgs=list(data=df.ratingsTraining[,2]))
# 
# ratingListByMovie.test = mapply(
#   FUN=id_list, 
#   id=df.movies$movieid, 
#   MoreArgs=list(data=df.ratingsTest[,2]))
# 
# moviesByAdjRating = df.movies[sort(df.movies$adjustedRating, index.return=TRUE, decreasing=TRUE)$ix,c(2,3,25,31,32)]
# 
# 
