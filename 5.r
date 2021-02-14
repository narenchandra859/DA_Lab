# 5. Write a R script for Multiple linear regression analysis over advertisement dataset to predict sales 
#   based on the past history of advertisement data with different medias, budget-spent-for-advertisement and sales units (media,budget,sales).
#   Do the predictions :
#   a. Using predefined functions.
#   b. Without using predefined functions

# Predicting sales(y) wrt budget given for tv, radio(x1, x2)

df = data.frame(
  sales = c(2,5,7,10,12,15,20),
  budget_tv = c(5,15,25,30,35,50,100),
  budget_radio = c(7,12,17,25,30,35,70)
)

df

# a

model = lm(df$sales ~ df$budget_tv + df$budget_radio)
df$pred_builtin = predict(model, data = df)

# b. Using normal equation method 

x <- df[,2:3]
x$intercept <- 1 
x <- as.matrix(x)
x_transpose = t(x)

# inverse( X_transpose * X) * X_transpose  * y 
coef <- solve( x_transpose %*% x ) %*% x_transpose %*% df$sales  
df$pred <- coef[1]* df$budget_tv +  coef[2]*df$budget_radio + coef[3]

df