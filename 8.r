# 8. Write a R script to perform logistic regression over Bank data set with the fields 
#   (qualification, credit card, bank balance, default details).
#   Do the predictions of defaulter:
#     a. Using predefined functions.
#     b. Without using predefined functions

library(ISLR)
df <- Default
# label encoding
df['student'] = apply(df['student'], 1, function(x) {if(x=="No") return(0) else return(1)})
df['default'] = apply(df['default'], 1, function(x) {if(x=="No") return(0) else return(1)})

# split test, train
train_ind = sample.int(nrow(df),size = 0.75*nrow(df))
train <- df[train_ind,]
test <- df[-train_ind,]

# min max scale BALANCE and INCOME
max_bal <- max(train['balance'])
min_bal <- min(train['balance'])
max_inc <- max(train['income'])
min_inc <- min(train['income'])

train['balance'] <- apply(train['balance'],1,function(x) return((x-min_bal)/(max_bal-min_bal)))
train['income'] <- apply(train['income'],1,function(x) return((x-min_inc)/(max_inc-min_inc)))

#split x, y
x_train <- train[,-1]
y_train <- train[,1]
x_test <- train[,-1]
y_test <- train[,1]

# logreg
logisticReg <- function(X,Y,lr=1,threshold=0.1){
  params <- rep(0,ncol(x_train)+1)
  prev_loss <- 0.0
  diff <- Inf
  
  while(diff>threshold){
    curr_loss <- 0.0
    gradient <- rep(0,length(params))
    diff <- 0
    
    for(row in 1:nrow(X)){
      x<-as.numeric(c(list(1),X[row,]))
      pred <- as.double(1/as.double(1+exp(-(x%*%params))))
      loss <- Y[row] - pred
      curr_loss <- curr_loss + (loss^2)
      for(p in 1:length(params))
        params[p] <- params[p] + lr*x[p]*loss
    }
    
    curr_loss <- sqrt(curr_loss)
    diff <- abs(curr_loss-prev_loss)
    prev_loss <- curr_loss
    print(curr_loss)
    
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

# calculate
pred <- sapply(apply(x_train,1,getPred),getClass)
predLib <- sapply(predict(model,x_train,type='response'),getClass)

# compare
print("Confusion Matrix for Custom LR")
table(pred,y_train)
print("Confusion Matrix for LR using Library Function")
table(predLib,y_train)

