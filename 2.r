# Write a R script to handle the following errors and apply the following data preprocessing over
# Faculty.csv (Fields in Faculty.csv file : faculty-name, faculty-no, address, dept, designation, salary)
# a. Remove numbers in names col
# b. Remove special char in names col
# c. In salary , if NA is there fill it(avg)
# d. Handle outliers in salary 
# e. Handle impossible values in salary for faculty data set(handle character values in slary)
# f. Redundant Whitespace deletion in faculty name
# g. Fixing capital letter mismatches in faculty name

library(tools)

df = data.frame(
  name = c("R@aMe$sh", "          Sures$h ", "123H1itesh", "Umesh", "gAnesh", "Vignesh", "Ritesh"),
  num = c(101, 102, 103, 104, 105, 106, 107),
  address = c("Blr", "Blr", "Mys", "Blr", "Blr", "Blr", "Mys"),
  dept = c("CSE","CSE","CSE","CSE","CSE","CSE","CSE"),
  desgn = c("Prof","Prof","Prof","Prof","Prof","Prof","Prof"),
  salary = c(55000, 60000, 70000, "1fsfs0", 50000, NA, 65000)
)
df

# a, b, f, g
df$name <- gsub('[[:punct:]]|[[:digit:]]|\\s+','',df$name)
df$name <- toTitleCase(tolower(df$name))

# e
df$salary <- as.character(df$salary)
df$salary <- ifelse(grepl('[[:alpha:]]', df$salary), as.numeric(gsub('[[:alpha:]]', '', df$salary)), as.numeric(df$salary))

# d
df <- df[!(df$salary %in% boxplot(df$salary)$out),]

# c
df$salary[is.na(df$salary)] <- mean(df$salary, na.rm = TRUE)

df

