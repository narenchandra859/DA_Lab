# 4. Write a R script for performing simple Linear Regression over advertisement dataset to predict sales based on the past history 
# of sales and budget spent for advertisement. Advertisement data has to be stored in csv file and accessed with fields (Budget, sales).
#   Do the predictions :
#   a. Using predefined functions.
#   b. Without using predefined functions

df = data.frame(
  sales = c(1,3,5,5,10,14,18),
  budget = c(1,3,5,12,34,45,60)
)

# a

model = lm(sales ~ budget, data = df)
df$pred_builtin = predict(model, data = df)

# b
xm = mean(df$budget)
ym = mean(df$sales)
num = sum((df$budget - xm) * (df$sales - ym))
den = sum((df$budget - xm) ^ 2)

b1 = num / den
b0 = ym - b1 * xm

df$pred = b0 + b1 * df$budget
df

print('Built-in')
print(model$coefficients)
print('Scratch ')
print(c(intercept=b0,budget=b1))

# compare

plot(df$budget, df$sales)
lines(df$budget, df$pred_builtin, col = "blue")
plot(df$budget, df$sales)
lines(df$budget, df$pred, col = "red")

