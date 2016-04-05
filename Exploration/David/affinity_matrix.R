################R script to calulate affinity matrix of users ################
# Four variables for each user: age, sex, salary, zipcode
# We calculate similarily for each and take the weighted average of them

# A function to transform the Ordinal variable to quantitative variable
trans_ordinal <- function(ordinal_col) { 
  M <- max(ordinal_col)
  col_trans <- (ordinal_col - 1/2) / M
  return(col_trans)
}



user <-read.table("http://files.grouplens.org/datasets/movielens/ml-100k/u.user",sep = "|", 
                  colClasses = c("character", "integer", "factor", "character", "character"), 
                  col.names = c("user_id", "age", "gender", "occupation", "zipcode"))



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
affinity_mat <- function(user_df) { # user_df is the data frame of user info.
  # normalize the data frame
  user_df <- scale(user_df)
  
  n <- dim(user_df)[1] # number of users
  
  aff_age <- matrix(numeric(n*n), nrow = n)
  aff_gender <- matrix(numeric(n*n), nrow = n)
  aff_occupation <- matrix(numeric(n*n), nrow = n)
  # set the diagonal element to 1
  diag(aff_age) <- 1
  diag(aff_gender) <- 1
  diag(aff_occupation) <- 1
  
  # a vector to store the variance of each user
  var_user <- sapply(1:n, FUN = function(x) sum((user_df[x, ] - mean(as.numeric(user_df[x, ])))^2))
  # a vector to store the mean of each user
  mean_user <- rowMeans(user_df)
  
  for (i in seq_len(n)) {
    if (i == n) break
    for (j in seq(i+1, n)) {
      age <- (user_df[i, 'age']-mean_user[i]) * (user_df[j, 'age'] - mean_user[j])
      gender <- (user_df[i, 'gender']-mean_user[i]) * (user_df[j, 'gender'] - mean_user[j])
      occu <- (user_df[i, 'occupation']-mean_user[i]) * (user_df[j, 'occupation'] - mean_user[j])
      aff_age[i, j] <- age/sqrt(var_user[i]*var_user[j])
      aff_age[i, j] <- gender/sqrt(var_user[i]*var_user[j])
      aff_occupation[i, j] <- occu/sqrt(var_user[i]*var_user[j])
    }
  }
  output <- list(aff_age, aff_gender, aff_occupation)
  return(output)
}

# affinity_matrix is a list of affinity matrix for each column (age, sex, occupation)
system.time(affinity_matrix <- affinity_mat(user))
