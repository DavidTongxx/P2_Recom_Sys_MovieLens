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


df.users$numFilms = as.numeric(table(sort(df.ratings[,1])))

p1 = ggplot(df.users,aes(x=numFilms)) +geom_histogram(binwidth=1)

p2 = ggplot(df.users,aes(x=numFilms)) +stat_ecdf()


df.genre$numFilms = rep(0, times=19)
for(i in 1:19) {
  #df.genre$numFilms[i] = count(df.ratings[df.ratings[,6 + i] == 1, ])
  print(i)
  temp = as.numeric(df.movies[,as.character(df.genre[i, 1])])
  x = as.numeric(table(temp))
}


grid.arrange(p1,p2)
