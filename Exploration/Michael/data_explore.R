library("recommenderlab")
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
# 943 users x 1682 movies rating
data(MovieLense)
# convert it to data.frame and matrix
ML_df <- as(MovieLense, "data.frame") # 100k out of 1M valid rating
ML_mat <- as(MovieLense, "matrix")
head(ML_df)

# read in the user info data
user <- read.table("./Data/ml-100k/u.user", sep = "|", 
                   colClasses = c("character", "integer", "factor", "character", "character"),
                   col.names = c("user_id", "age", "gender", "occupation", "zipcode"))
# A summarizing plot of each occupation
user %>% ggplot(aes(x = reorder(occupation, table(occupation)[occupation]), fill = gender)) + geom_bar(aes(y = (..count..)/sum(..count..))) + #stat = 'identity') +
  coord_flip() + xlab("Occupation") + ylab("Proportion") + ggtitle("Occupation Proportion") #+ scale_y_continuous(labels = percent)

# merge the rating data.frame and user info.
ML_df <- left_join(ML_df, user, by = c("user" = "user_id")) # "user" in ML_df is Factor
head(ML_df)

# sort average rating by each occupation
ML_df %>% group_by(occupation) %>% summarise(mean = mean(rating), sd = sd(rating)) %>% 
  ggplot(aes(x = reorder(occupation, mean), y = mean)) + geom_point(size = 3) + coord_flip() +
  xlab("Occupation") + ylab("Rating Average") + ggtitle("Average Rating for Each Occupation")

# 5 highest rating interval
ML_df %>% group_by(occupation) %>% summarise(mean = mean(rating), sd = sd(rating)) %>% 
  mutate(lower_bound = ifelse(mean - 2*sd > 0, mean - 2*sd, 0),
         upper_bound = ifelse(mean + 2*sd < 5, mean + 2*sd, 5)) %>% # An approximate estimate of interval
  arrange(desc(lower_bound)) %>% head(5) # display the top 5

# Use group division criterion from David and plot for each sub-group
# u_id_1: user id for those who rated less than 66
# u_id_2: user id for those who rated more or equal to 66

# Occupation plot for each sub-group
user %>% mutate(group = ifelse(user_id %in% u_id_1, "group1 (# of rating < 66)", "group2 (# of rating >= 66)")) %>% 
  ggplot(aes(x = reorder(occupation, table(occupation)[occupation]), fill = gender)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_grid(.~group) +
  coord_flip() + xlab("Occupation") + ylab("Proportion") + ggtitle("Occupation Proportion") 

# Average rating plot for each sub-group in the same order as in the overall plot
ML_df %>% mutate(group = ifelse(user %in% u_id_1, "group1 (# of rating < 66)", "group2 (# of rating >= 66)")) %>% 
  group_by(occupation, group) %>% summarise(mean = mean(rating)) %>%
  ggplot(aes(x = factor(occupation, levels = rev(ord$occupation)), y = mean)) + geom_point(size = 3) +
  facet_grid(.~group) +
  coord_flip() + xlab("Occupation") + ylab("Rating Average") + ggtitle("Average Rating for Each Occupation")





