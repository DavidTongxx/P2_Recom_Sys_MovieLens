#Week 6-Week7 
Feb. 26th -March 3rd
Basic Summary
Topic : P2 Initial Exploration
DataSource MovieLens 100 K 
(Link http://grouplens.org/datasets/movielens/)
Related Reference: R/recommenderlab package...
(Slides by Michael Hahsler P33-36 http://michael.hahsler.net/research/Recommender_SMU2011/slides/Recomm_2011.pdf)

#Week 9
1. Reading Materials
Recommender Systems 101 – a step by step practical example in R R-bloggers Link
http://www.r-bloggers.com/recommender-systems-101-a-step-by-step-practical-example-in-r/

1)Content based recommender systems [genre of movies; actors; actress; director; year of releasing,] 

v.s. 

2)Collaborative filtering recommender systems [rates of movies; users'occupation; gender; rating scores]

Here we will only focus on the second part 2) Collaborative filtering recommender systems. This will be done in the following steps:

[1]create affinity matrix based on the demographic information of users [ say ---> first 500 users]

This week and next week -> combine age,occupation,gender,....,

We also need to use those user's rating scores to test our affinity matrix. However, no rating information should be include in building this affinity matrix. This part is littlbe bit tricky but this matrix is crucial in the future.


[2]combine user's rating information prediction movie for "active" user. [Application of CHTC]

Xingxing March 15th

