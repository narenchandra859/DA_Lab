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