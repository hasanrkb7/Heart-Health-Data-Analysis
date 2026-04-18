# TILP2400
# Rakibul Hasan Remon
# Exercises 4

# Load data and convert gender to factor
hdata <- read.table("0.3 Datasets.txt", header = TRUE)
hdata$gender <- factor(hdata$gender)

# Task 1
# blood pressure histogram -- roughly symmetric, centered around 130-140, a few outliers above 200
hist(hdata$syst, main = "Blood pressure distribution", xlab = "Blood pressure (mmHg)", col = "grey")
# cholesterol histogram -- slightly right-skewed, most values between 5-7, a few large outliers
hist(hdata$chol, main = "Cholesterol distribution", xlab = "Cholesterol (mmol/l)", col = "grey")

# Task 2
hdataF <- subset(hdata, gender == "female")
hdataM <- subset(hdata, gender == "male")
nrow(hdataF)  # 464 women
nrow(hdataM)  # 412 men

# Task 3
mean(hdataF$syst, na.rm = TRUE)  # women: 131.96 mmHg
mean(hdataM$syst, na.rm = TRUE)  # men: 138.23 mmHg
# men's blood pressure is on average ~6.3 mmHg higher than women's

sd(hdataF$syst, na.rm = TRUE)  # women: 21.29 -- MORE variation
sd(hdataM$syst, na.rm = TRUE)  # men: 18.83
# women's blood pressure has more variation (larger sd)

# Task 4
median(hdataF$syst, na.rm = TRUE)  # 128 -- half of women have BP below 128, half above
median(hdataM$syst, na.rm = TRUE)  # 136 -- half of men have BP below 136, half above

quantile(hdataF$syst, na.rm = TRUE)  # IQR: 116 to 144 -- middle 50% of women's BP is in this range
quantile(hdataM$syst, na.rm = TRUE)  # IQR: 124 to 148 -- middle 50% of men's BP is in this range
# medians differ from means because a few high outliers pull the mean up -- median is not affected by them

# Task 5
boxplot(syst ~ gender, data = hdata, range = 0,
        main = "Blood pressure by gender", xlab = "gender", ylab = "blood pressure (mmHg)")
# women's box is taller --> women have more variability in BP (wider IQR: 116-144 vs 124-148)
# women also have a longer whisker upward --> women have a greater overall range (96-240 vs 94-222)

# Task 6
table(hdata$marit)    # mode = 2 (married/cohabiting, 644 people) -- most typical marital status
table(hdata$edlevel)  # mode = 4 (basic education, 393 people) -- most typical education level

# Task 7
x_bar <- mean(hdata$syst, na.rm = TRUE)  # 134.91
s     <- sd(hdata$syst, na.rm = TRUE)    # 20.40
n     <- sum(!is.na(hdata$syst))         # 876
lower <- x_bar - 2 * (s / sqrt(n))      # 133.53
upper <- x_bar + 2 * (s / sqrt(n))      # 136.28
lower
upper
# 95% CI: [133.53, 136.28]
# we are 95% confident the true mean blood pressure of ALL Finns aged 25-64 is in this interval

# Task 8 -- two-sided t-test: H0: mean BP = 130
t.test(hdata$syst, mu = 130)
# p-value = 2.281e-12, which is far below 0.05 --> reject H0
# conclusion: the population mean blood pressure is significantly different from 130

# Task 9 -- one-sided t-test: H0: mean = 135, H1: mean > 135
t.test(hdata$syst, mu = 135, alternative = "greater")
# p-value = 0.554, which is above 0.05 --> cannot reject H0
# conclusion: we cannot say the population mean is significantly greater than 135

# Task 10 -- two-sample t-test: H0: no difference between women and men
t.test(hdataF$syst, hdataM$syst)
# p-value = 4.285e-06, far below 0.05 --> reject H0
# conclusion: there IS a statistically significant difference in blood pressure between women and men