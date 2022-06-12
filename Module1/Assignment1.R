# Question 1 ----
# Create a string object that is equal to 'Data Science'. Then write a for loop that prints out each individual letter in the string. 
# For example, the first letter printed should be 'D'. The second should be 'a'. The third should be 't', and so on.
# define the string

ds <- "Data Science"

# use strsplit funciton to iterate through characters
ds_split = strsplit(ds, "")[[1]]

# loop through and print
for (character in ds_split){
  print(character)
}


# Question 2 ----
# Write a for loop to create 5 randomly generated dataframes, and print each of the dataframes.

library(glue)
# use glue package for formatting print statement
range <- 1:5
for (iteration in range){
  print(glue("Dataframe for iteration # {iteration}"))
  print(data.frame(replicate(10,sample(0:1,10,rep=TRUE))))
}

# Question 3 ----
# Write a for loop to create 5 randomly generated dataframes of length 100,200,500,800, and 1000, and print each of the dataframes.

df_list = list()
df_size <- c(100,200, 500, 800, 1000)
for (iteration in range){
  current_size <- df_size[iteration]
  print(glue("Dataframe of length {current_size} for iteration # {iteration}"))
  df = data.frame(replicate(10, sample(0:1,current_size, rep = TRUE)))
  print(df)
  df_list[[iteration]] <- df
}

# Question 4 ----
# Using the dataframes generated from the previous question, run a linear regression (specified in whatever way you want) on each of the dataframes. 
# Print out the linear regression results for each dataframe using a for loop.

# create a y_variable for each df
library(dplyr)

for (iteration in range){
  df <- df_list[[iteration]]
  df <- df %>% mutate(y = X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)
  df_list[[iteration]] <- df
}

model_list <- list()
train_df_list <- list()
library(modelr)
library(broom)

set.seed(123)

for (iteration in range){
  df <- df_list[[iteration]]
  sample <- sample(c(TRUE, FALSE), nrow(df), replace = T, prob = c(0.6,0.4))
  train <- df[sample, ]
  test <- df[!sample, ]
  model <- lm(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = train)
  print("===============================================")
  print(glue("For model #{iteration}, the results are :"))
  print(tidy(model))
  print("===============================================")
  model_list[[iteration]] <- model
  train_df_list[[iteration]] <- train
}


# Question 5 ----
# Building on the code from Question 4, identify which linear regression has the highest R-Squared

r_scores <- list()
for (iteration in range){
  r_scores[[iteration]] <- rsquare(model_list[[iteration]], train_df_list[[iteration]])
}

which.max(r_scores)
print(r_scores)

# looks like all models have the same R-squared value. 