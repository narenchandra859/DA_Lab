# Write a R script to handle the following errors and apply the following data preprocessing over
# student.csv (Fields in student.csv file : stud-name,usn,address,dept,cgpa)
# a. Remove numbers in names col
# b. Remove special char in names col
# c. In cgpa , if NA is there fill it(avg)
# d. Handle outliers in cgpa
# e. Handle impossible values in CGPA for stud data set(handle percentage in cgpa)
# f. Redundant Whitespace deletion in stud name
# g. Fixing capital letter mismatches in stud name

library(tools)

df <- data.frame(
  name = c("R@aMe$sh", "          Sures$h ", "123H1itesh", "Umesh", "gAnesh", "Vignesh", "Ritesh"),
  usn = c(101, 102, 103, 104, 105, 106, 107),
  address = c("Blr", "Blr", "Mys", "Blr", "Blr", "Blr", "Mys"),
  dept = c("CSE","CSE","CSE","CSE","CSE","CSE","CSE"),
  cgpa = c(9.04, 9.56, NA, 100.10, -2.0, "90%", 9.25)
)
df

# a, b, f, g
df$name <- gsub('[[:digit:]]', '', df$name)
df$name <- gsub('[[:punct:]]', '', df$name)
df$name <- gsub('\\s+', '', df$name)
df$name <- toTitleCase(tolower(df$name))

# e
df$cgpa <- as.character(df$cgpa)
df$cgpa <- ifelse(grepl('%', df$cgpa), as.numeric(gsub('%', '', df$cgpa))/10, as.numeric(df$cgpa))

# d
df <- df[!(df$cgpa %in% boxplot(df$cgpa)$out),]
df

# c
df$cgpa[is.na(df$cgpa)] <- mean(df$cgpa, na.rm = TRUE)

df
