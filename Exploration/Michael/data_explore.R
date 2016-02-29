library("recommenderlab")
data(MovieLense)
ML_df <- as(MovieLense, "data.frame")
ML_mat <- as(MovieLense, "matrix")
#head(ML_df)

user <- read.table("./Data/ml-100k/u.user", sep = "|")

