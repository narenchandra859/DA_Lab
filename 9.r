# 9. Write a R script to Demonstrate Linear Discriminate Analysis & QDA over Bank data set with 
#   (qualification, credit card, bank balance, default details), using predefined functions.
#   Do the predictions of defaulter:
#   a. Using predefined functions.

#LDA 

library(ISLR) #credit card dataset 
library(MASS)

df = Default
df

index = sample(x = 1:nrow(df), size = round(nrow(df) * 0.7))
train = df[index, ]
test = df[-index, ]

model = lda(default ~ student + balance + income, data = train)
summary(model)

test$pred = predict(model, test)$class
table(test$pred, test$default)

#QDA

df = Default

df$student = as.factor(as.numeric(df$student))

index = sample(x = 1:nrow(df), size = round(nrow(df) * 0.9))
train = df[index, ]
test = df[-index, ]

model = qda(default ~ student + balance + income, train)
summary(model)

test$pred = predict(model, test)$class
table(test$pred, test$default)
