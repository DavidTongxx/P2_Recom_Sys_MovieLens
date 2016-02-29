library("recommenderlab")
data(MovieLense)
ML_df <- as(MovieLense, "data.frame")
ML_mat <- as(MovieLense, "matrix")
#head(ML_df)

user <- read.table("./Data/ml-100k/u.user", sep = "|", 
                   colClasses = c("character", "integer", "factor", "character", "character"),
                   col.names = c("user_id", "age", "gender", "occupation", "zipcode"))

