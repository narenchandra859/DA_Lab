# 11. Write a R script for visualizing the following.
# a. Consider students marks scored in Physics,Chemistry,Maths,FOC,CAD. Draw the bar chart for the same.
# b. Consider different party candidates’ votes received from public in election. Show the votes received in percentage in bar chart.
# c. Consider the students’ performance in different subjects. (Like student roll.no, their marks, gender). 
#   Show the bar chart for girls and boys students’ performance.

df <- data.frame(
  name = c("Ramesh", "Suresh", "Hitesh"),
  physics = c(100,90,90),
  chemistry = c(90,100,80),
  maths = c(75, 85, 100),
  foc = c(65, 70, 70),
  cad = c(70, 75, 75)
)

for(row in 1:nrow(df)) {
  x <- df[row, 2:6]
  barplot(as.numeric(x), names.arg = x, main = paste('Student',df[row,'name'],'marks distribution'), col = rainbow(length(x)))
  legend("topright", legend = labels, fill  = rainbow(length(x)), cex = 1)
}


df = data.frame(
  votes = c(55,65,74,23,21,29,39,10),
  party = c("A", "B", "C", "A", "B", "C", "A", "B")
)
labels <- c("Party A", "Party B", "Party C")
v_a <- sum(df$votes[df$party == "A"])/sum(df$votes)*100
v_b <- sum(df$votes[df$party == "B"])/sum(df$votes)*100
v_c <- sum(df$votes[df$party == "C"])/sum(df$votes)*100
v <- c(v_a, v_b, v_c)
barplot(v, names.arg = paste(round(v, 1)), main = paste("Vote share (%) per party"), col = rainbow(length(x)))
legend("topright", legend = labels, fill = rainbow(length(x)), cex = 1)

df <- data.frame(
  rollno = c(100, 101, 102, 103, 104),
  marks1 = c(90,100,85,90,70),
  marks2 = c(85,90,100,95,75),
  gender = c('M','M','F','F','M')
)

x =  c(mean(df[df$gender == 'M', ]$marks1), mean(df[df$gender == 'F', ]$marks1))
barplot(x, names.arg = paste(round(x, 1),"/100"), main = "Average Performance of Girls vs Boys", col = rainbow(length((x))))
legend("topright", legend = c("Boys", "Girls"), fill = rainbow(length((x))), cex = 1)
