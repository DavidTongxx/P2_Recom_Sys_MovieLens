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
  
  # a vector to store the variance of each user
  var_user <- sapply(1:n, FUN = function(x) sum((user[x, ] - mean(as.numeric(user[x, ])))^2))
  
  for (i in seq_len(n)) {
    if (i == n) break
    for (j in seq(i+1, n)) {
      nume <- 0
      denum <- 0
      nume <- sum((user[i, ] - mean(as.numeric(user[i, ]))) * (user[j, ] - mean(as.numeric(user[j, ]))))
      denum <- sqrt(var_user[i] * var_user[j])
      aff_mat[i, j] <- nume/denum
    }
  }
  return(aff_mat)
}


system.time(affinity_matrix <- affinity_mat(user[1:100, ]))

