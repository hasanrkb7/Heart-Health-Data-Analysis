# TILP2400 Exercises 1
# Rakibul Hasan Remon

# Task 1
# R and RStudio installed

# Task 2
# RScript created and saved

# Task 3
hdata <- read.table("0.3 Datasets.txt", header = TRUE)
getwd()

# Task 4
names(hdata)
# names() prints the names of all 10 variables in the dataset
head(hdata)
# head() prints the first 6 rows of the dataset

# Task 5
nrow(hdata)
# 876  observations
ncol(hdata)
# 10 variables
dim(hdata)
# dim() reveals both rows and columns together: 876 rows and 10 columns

# Task 6
str(hdata)
# chol is numeric (num), smoking is character (chr)
hdata$chol
# prints all 876 cholesterol values
hdata$syst
# First three values of blood pressure (syst): 168, 126, 106

# Task 7
hdata$smoking <- factor(hdata$smoking)
levels(hdata$smoking)
# smoking has 3 levels: no, sometimes, yes
hdata$age <- factor(hdata$age)
levels(hdata$age)
# There are 4 different age groups: 25-34, 35-44, 45-54, 55-64

# Task 8
hdata$logsyst <- log(hdata$syst)
hdata$sqrtbmi <- sqrt(hdata$bmi)
hdata$mwaist <- hdata$waist / 100

# Task 9
hdata[5, 2]
hdata[1:10, ]
hdata[ , 1:4]
hdata[1:5, c(1, 2, 10)]
hdata[hdata$gender == "male", ]
hdata[ , c("chol", "syst")]
hdata[hdata$gender == "male", c("chol", "syst")]
# Select observations for women for smoking, age, and syst:
hdata[hdata$gender == "female", c("smoking", "age", "syst")]
# First woman (row 2844): smoking = no, age group = 45-54, blood pressure = 126

# Task 10
any(is.na(hdata))
# TRUE - there is missing data in the dataset
any(is.na(hdata$age))
# FALSE - no missing values in age variable
any(is.na(hdata$marit))
# TRUE - there are missing observations in the marit variable
hdataN <- na.omit(hdata)
nrow(hdataN)
# After removing missing data, hdataN has 874 observations (2 rows removed)