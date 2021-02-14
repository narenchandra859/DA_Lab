# Write a R script for KNN algorithm implementation for loan defaulters classification without using predefined functions. 
# Compare your results with predefined functions. Consider K=1 and do the classification. 
# Then you have to classify for k=2, K=3. Loan-defaulters.csv file has the following fields : age,loan,defaulter-status.

# testing on same data, if required just create another Age,Loans set to test on

library(class)

df = data.frame(
  age = c(25,35,30,20,27,30,32,22,26,500),
  loan = c(10000,20000,10000,15000,12000,17000,35000,28000,19000,3000000),
  default = c("Y", "N", "Y", "Y", "N", "Y", "N", "N", "Y","Y")
)
df

myKnn <- function(x, k) {
  distance = vector()
  age <- as.numeric(x[1])
  loan <- as.numeric(x[2])
  length(distance) = nrow(df)
  for(i in 1:nrow(df)) {
    distance[i] = sqrt(((age - df[i, 1])^2) + ((loan - df[i, 2])^2))
  } 
  tmp <- df
  tmp$dist = distance
  tmp <- tmp[order(tmp$dist),]
  cnt <- table(tmp[1:k, 3])
  classes <- names(cnt)
  pred <- classes[which.max(cnt)]
  return(pred)
}

df$predk1 <- apply(df, 1, function(x) myKnn(x, 1))
df$predk2 <- apply(df, 1, function(x) myKnn(x, 2))
df$predk3 <- apply(df, 1, function(x) myKnn(x, 3))

y <- df[,3]
x <- df[,1:2]

df$predk1_builtin <- knn(train = x, cl = y, test = df[,1:2], k = 1)
df$predk2_builtin <- knn(train = x, cl = y, test = df[,1:2], k = 2)
df$predk3_builtin <- knn(train = x, cl = y, test = df[,1:2], k = 3)

df