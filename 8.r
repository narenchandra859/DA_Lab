# 8. Write a R script to perform logistic regression over Bank data set with the fields 
#   (qualification, credit card, bank balance, default details).
#   Do the predictions of defaulter:
#     a. Using predefined functions.
#     b. Without using predefined functions
# refer: https://web.stanford.edu/class/archive/cs/cs109/cs109.1178/lectureHandouts/220-logistic-regression.pdf
# refer: https://github.com/pranavh4/Data-Analytics-Lab/blob/master/Q8%20-%20Logistic%20Regression.Rmd

library(ISLR)
df <- Default
# label encoding
df['student'] = apply(df['student'], 1, function(x) {if(x=="No") return(0) else return(1)})
df['default'] = apply(df['default'], 1, function(x) {if(x=="No") return(0) else return(1)})

# split test, train
train_ind = sample.int(nrow(df),size = 0.75*nrow(df))
train <- df[train_ind,]
test <- df[-train_ind,]

# min max normalize
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}
train['balance'] <- sapply(train['balance'], normalize)
train['income'] <- sapply(train['income'], normalize)

#split x, y
x_train <- train[,-1]
y_train <- train[,1]
x_test <- train[,-1]
y_test <- train[,1]

# logreg
logisticReg <- function(X,Y,lr=1){
  params <- rep(0,ncol(x_train)+1)
  for(i in 1:10) {
    for(row in 1:nrow(X)){
      x<-as.numeric(c(list(1),X[row,]))
      pred <- as.double(1/as.double(1+exp(-(x%*%params))))
      loss <- Y[row] - pred
      for(p in 1:length(params))
        params[p] <- params[p] + lr*x[p]*loss
    }
  }
  return(params)
}

# helper functions
getPred<-function(x){
  x<-as.numeric(c(list(1),x))
  return(1/(1+exp(-(x%*%params))))
}
getClass <- function(x){
  if(x>=0.5)
    return(1)
  return(0)
}

# inbuilt
model <- glm(default ~ student + balance + income, data = train, family = binomial)

# get parameters
params <- logisticReg(x_train,y_train)

pred <- sapply(apply(x_train,1,getPred),getClass)
predLib <- sapply(predict(model,x_train,type='response'),getClass)

# compare
print("Confusion Matrix for Custom LR")
table(pred,y_train)
print("Confusion Matrix for LR using Library Function")
table(predLib,y_train)

params
model$coefficients
