rm(list=ls())

library("recommenderlab")
library(ggplot2)
library(gridExtra)
library(dplyr)

data = read.table("../../Data/ml-100k/u.data")
df.ratings = data.frame(userid=data$V1, movieid=data$V2, rating=data$V3, time=data$V4)
rm(data)

data = read.csv("../../Data/ml-100k/u.user", sep="|", header=FALSE)
df.users = data.frame(userid=data$V1, age=data$V2, gender=data$V3, occupation = data$V4, zip=data$V5)
rm(data)

data = read.csv("../../Data/ml-100k/u.genre", sep="|", header=FALSE)
df.genre = data.frame(genre=data$V1, genreid=data$V2)
rm(data)

data = read.csv("../../Data/ml-100k/u.item", sep="|", allowEscapes=FALSE, header=FALSE)
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


out = lm(AvgRating ~ ., data = df.movies[,-c(1:5, 26:30)])

df.genre$fittedGenres = unname(out$coefficients)[2:20] + unname(out$coefficients[1])

df.users$numFilms = as.numeric(table(sort(df.ratings[,1])))

temp = sort.int(df.users$numFilms, index.return=TRUE)

df.sortedUsers = data.frame(userid=temp$ix, numFilms=temp$x)
df.sortedUsers$cumulative = cumsum(df.sortedUsers$numFilms)
df.sortedUsers$sortedOrder = 1:length(df.sortedUsers$userid)

p1 = ggplot(df.sortedUsers) +
      geom_smooth(
      aes(x=sortedOrder, 
          y=cumulative)
      )  +
  theme(panel.grid.major = element_line(color = "blue", linetype="dotted"))+
  labs(title="CDF of reviews by users", x="Users (from least to most prolific)", y="Cumulative Total Reviews")

p2 = ggplot(df.sortedUsers,aes(x=numFilms)) +
  geom_histogram(binwidth=1) + 
  geom_step(aes(stat="ecdf", y=(sortedOrder*100/max(sortedOrder)))) +
  geom_step(aes(stat="ecdf", y=(cumulative*100/max(cumulative)))) + 
  theme(panel.grid.major = element_line(color = "blue", linetype="dotted"))+
  labs(title="CDF of reviews, users + Histogram of films reviewed", x="Films Reviewed", y="Count (Histogram)/% of Users/Reviews")

grid.arrange(p1,p2)


# p3 = ggplot(df.genre, aes(x=genre, y=mean_rating_star(df.genre[5:9]), size=numFilms)) +
#   geom_point() + labs(title="Distribution of Films/Ratings by Genre", y = "Rating")

 p3 = ggplot(df.genre, aes(x=genre, y=AvgRating, size=numFilms)) +
   geom_point() + labs(title="Distribution of Films/Ratings by Genre", y = "Rating")


p4 = ggplot(df.movies, aes(x=apply(df.movies[6:24], MARGIN=1, FUN=sum), y=AvgRating)) +
  geom_point() + labs(title="Number of genres Per Movie", x="# of Genres", y="Film Rating")



grid.arrange(p3,p4)