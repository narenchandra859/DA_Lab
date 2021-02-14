# 12. Write a R script for visualizing the following.
# a. Consider students marks scored in (Physics,Chemistry,Maths,FOC,CAD). Draw the box plot for the same.
# b. Consider different party candidates’ votes received from public in election. Show the votes received in percentage in box plot.
# c. Consider the students’ performance in different subjects. (Like student roll.no, their marks, gender). 
#   Show the box plot for girls and boys students’ performance.

df <- data.frame(
  name = c("Ramesh", "Suresh", "Hitesh"),
  physics = c(100,90,90),
  chemistry = c(90,100,80),
  maths = c(75, 85, 100),
  foc = c(65, 70, 70),
  cad = c(70, 75, 75)
)

par(mfrow=c(1,1))
boxplot(df[1:nrow(df),2:6], names = colnames(df[1:nrow(df),2:6]), main = paste("Overall Performance in all subjects"), ylab = "Marks")


ele = data.frame(
  votes = c(54,42,10, 100, 20, 80),
  labels = c("Party A","party B", "Party C","Party A","party B", "Party C")
)

boxplot(ele$votes~ele$labels, main = "Party Votes Distribution (Boxplot)", name = ele$labels)


df <- data.frame(
  rollno = c(100, 101, 102, 103, 104),
  marks1 = c(90,100,85,90,70),
  marks2 = c(85,90,100,95,75),
  gender = c('M','M','F','F','M')
)

boxplot(df$marks1~df$gender, labels = c("Male", "Female"), main = "Performance of Boys vs Girls in Subject 1")
boxplot(df$marks2~df$gender, labels = c("Male", "Female"), main = "Performance of Boys vs Girls in Subject 2", add = TRUE, border='red')

