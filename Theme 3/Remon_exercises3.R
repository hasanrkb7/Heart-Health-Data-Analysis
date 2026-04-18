# TILP2400
# Rakibul Hasan Remon
# Exercises 3

# Load data and convert variables to factors
hdata <- read.table("0.3 Datasets.txt", header = TRUE)
hdata$age     <- factor(hdata$age)
hdata$gender  <- factor(hdata$gender)
hdata$smoking <- factor(hdata$smoking)

# Task 1

# Task 2
table1 <- table(hdata$age)
barplot(table1, horiz = FALSE)

# Task 3
# bars are blue, y-axis goes 0-250, title added
barplot(table1, horiz = FALSE, ylab = "count", ylim = c(0, 250),
        space = 0.3, col = "blue", main = "Participants by age group")

# Task 4
# females: 52.97%, males: 47.03%
table1 <- sort(prop.table(table(hdata$gender)) * 100, decreasing = FALSE)
barplot(table1, horiz = TRUE)

# Task 5
barplot(table1, horiz = TRUE, xlab = "%", xlim = c(0, 60),
        main = "Participants by gender", col = "steelblue")

# Task 6
table1 <- table(hdata$gender, hdata$age)
barplot(table1, beside = TRUE, horiz = FALSE)

# Task 7
barplot(table1, beside = TRUE, horiz = FALSE, ylim = c(0, 150),
        ylab = "count", col = c("pink", "purple"),
        main = "Participants by gender and age group")

# Task 8
barplot(table1, beside = TRUE, horiz = FALSE, ylim = c(0, 150),
        ylab = "count", col = c("pink", "purple"),
        main = "Participants by gender and age group")
legend("topleft", legend = c("female", "male"), fill = c("pink", "purple"), bty = "n")
box()

# Task 9
png("participants_by_gender_age.png", width = 800, height = 600)
barplot(table1, beside = TRUE, horiz = FALSE, ylim = c(0, 150),
        ylab = "count", col = c("pink", "purple"),
        main = "Participants by gender and age group")
legend("topleft", legend = c("female", "male"), fill = c("pink", "purple"), bty = "n")
box()
dev.off()

# Task 10
# blue = no, purple = yes, red = sometimes
# both genders mostly non-smokers (~65-75%), "sometimes" is the smallest category
table1 <- prop.table(table(hdata$smoking, hdata$gender), margin = 2) * 100
par(mar = c(5, 7, 4, 2))
barplot(table1, beside = TRUE, horiz = TRUE, xlim = c(0, 100),
        xlab = "%", main = "Smoking by gender",
        col = c("blue", "purple", "red"), las = 1)
legend("topright", legend = c("no", "yes", "sometimes"),
       fill = c("blue", "purple", "red"), bty = "n")
box()