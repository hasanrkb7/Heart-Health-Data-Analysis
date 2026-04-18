# TILP2400-26-2
# Rakibul Hasan Remon
# Exercises 2

# loading the data first
hdata <- read.table("0.3 Datasets.txt", header = TRUE)

# convert age, gender and smoking to factor variables
hdata$age     <- factor(hdata$age)
hdata$gender  <- factor(hdata$gender)
hdata$smoking <- factor(hdata$smoking)

# Task 1

# Task 2
table(hdata$smoking)
# most participants are in the "no" category with 623 people

# Task 3
prop.table(table(hdata$gender)) * 100
# females: 52.97%, males: 47.03%

# Task 4
f <- table(hdata$age)
f
sumf <- cumsum(f)
sumf
# participants under 45 years old: 427 (203 from 25-34 + 224 from 35-44)

# Task 5
relf    <- prop.table(table(hdata$age)) * 100
relsumf <- cumsum(relf)
relsumf
# 73.7% of participants are under 55 years old

# Task 6
round(cbind(f, relf, sumf, relsumf), 1)
# 45-54 age group makes up 25.0% of participants
# participants aged 25-44: 427

# Task 7
table(hdata$age, hdata$smoking)
# 55-64 year olds who smoke regularly: 34
# 25-34 year olds who smoke regularly: 55

# Task 8
round(prop.table(table(hdata$age, hdata$smoking), margin = 1) * 100, 1)
# most skewed age group: 55-64 (82.2% non-smokers, only 14.8% regular smokers)
# most uniform age group: 25-34 and 35-44 are quite similar to each other

# Task 9
round(prop.table(table(hdata$age, hdata$smoking), margin = 2) * 100, 1)
# note: this table uses age vs smoking, not gender
# so the column percentages show age groups within each smoking category
# 55-64 makes up 30.3% of non-smokers, 13.7% of occasional smokers, 16.8% of regular smokers
# 25-34 makes up 21.3% of non-smokers, 29.4% of occasional, 27.2% of regular smokers

# Task 10
hdataord <- hdata[order(hdata$syst, decreasing = TRUE), ]
head(hdataord$syst, 3)
# three highest blood pressure values: 240, 222, 214