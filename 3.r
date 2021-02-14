# Write a R script to handle the following errors and apply the following data preprocessing over
# dept.csv (Fields in dept.csv file : dept-name, dept-no, no-of-staff-in-dept,avg-no-of-publications)
# a. Remove numbers in dept-name col 
# b. Remove special char in names col 
# c. In no_of_staff_in_dept , if NA is there fill it(avg) 
# d. Handle outliers in avg_no_of_publications column 
# e. Handle impossible values in no_of_staff_in_dept , in dept data set(handle character values in no_of_staff_in_Dept column) 
# f. Redundant Whitespace deletion in dept name 
# g. Fixing capital letter mismatches in dept name

library(tools)

df = data.frame(
  deptName = c("CSE", "ISE", "MeCH", "$$ML", "  2BT", "CHEM123", "IEM$"),
  deptNo = c(101, 102, 103, 104, 105, 106, 107),
  numStaff = c("c50",NA,50,50,50,50,50),
  avgPubl = c(25,30,20,15,35,30,10000)
)
df

# a, b, f, g

df$deptName <- gsub('[[:punct:]]|[[:digit:]]|\\s+', '', df$deptName)
df$deptName <- toTitleCase(tolower(df$deptName))

# d 
df <- df[!(df$avgPubl %in% boxplot(df$avgPubl)$out),]

# e 
df$numStaff <- as.character(df$numStaff)
df$numStaff <- ifelse(grepl('[[:alpha:]]', df$numStaff), as.numeric(gsub('[[:alpha:]]', '', df$numStaff)), as.numeric(df$numStaff))

# c
df$numStaff[is.na(df$numStaff)] <- mean(df$numStaff, na.rm = TRUE)

df


