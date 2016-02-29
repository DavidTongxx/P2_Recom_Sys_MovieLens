library("recommenderlab")
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
# 943 users x 1682 movies rating
data(MovieLense)
# convert it to data.frame and matrix
ML_df <- as(MovieLense, "data.frame") # 10k out of 1M valid rating
ML_mat <- as(MovieLense, "matrix")
head(ML_df)

# read in the user info data
user <- read.table("./Data/ml-100k/u.user", sep = "|", 
                   colClasses = c("character", "integer", "factor", "character", "character"),
                   col.names = c("user_id", "age", "gender", "occupation", "zipcode"))
# See the proportion of each occupation
user %>% group_by(occupation) %>% summarise(n = n()) %>% mutate(percentage = n/sum(n)) %>% 
  ggplot(aes(y = percentage, x = reorder(occupation, percentage))) + geom_bar(stat = 'identity') +
  coord_flip() + xlab("Occupation") + ggtitle("Occupation Proportion")

# merge the rating data.frame and user info.
ML_df <- left_join(ML_df, user, by = c("user" = "user_id")) # "user" in ML_df is Factor
head(ML_df)

# sort average rating by each occupation
ML_df %>% group_by(occupation) %>% summarise(mean = mean(rating), sd = sd(rating)) %>% 
  ggplot(aes(x = reorder(occupation, mean), y = mean)) + geom_point() + coord_flip()

# 5 highest rating interval
ML_df %>% group_by(occupation) %>% summarise(mean = mean(rating), sd = sd(rating)) %>% 
  mutate(lower_bound = ifelse(mean - 2*sd > 0, mean - 2*sd, 0),
         upper_bound = ifelse(mean + 2*sd < 5, mean + 2*sd, 5)) %>% # An approximate estimate of interval
  arrange(desc(lower_bound)) %>% head(5) # display the top 5
