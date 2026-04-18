# TILP2400-26-2
# Rakibul Hasan Remon
# Exercises 5: Scatter plot, correlation and dependence

# load data and convert to factors
hdata <- read.table("0.3 Datasets.txt", header = TRUE)
hdata$age     <- factor(hdata$age)
hdata$gender  <- factor(hdata$gender)
hdata$smoking <- factor(hdata$smoking)

# Task 1 - scatter plot: cholesterol (x) vs blood pressure (y)
plot(hdata$chol, hdata$syst,
     xlab = "total cholesterol (mmol/l)",
     ylab = "systolic blood pressure (mmHg)")
# The points are widely scattered with no clear linear pattern.
# There is no strong linear relationship between cholesterol and blood pressure.
# The plot reveals some outliers, especially at high blood pressure values.

# Task 2 - same plot, women in purple, men in black
plot(hdata$chol, hdata$syst,
     xlab = "total cholesterol (mmol/l)",
     ylab = "systolic blood pressure (mmHg)",
     col = ifelse(hdata$gender == "female", "purple", "black"))
# The plot does not show a clear difference in blood pressure between women and men.
# The purple and black points are mixed together throughout the plot.

# Task 3 - correlation coefficient between syst and chol
cor(hdata$syst, hdata$chol)
# r = 0.275 — this is a weak positive linear relationship.
# This supports the conclusion from Task 1: no strong linear relationship.

# Task 4 - correlation matrix for syst, chol, weight, waist, bmi
subdata <- subset(hdata, select = c("syst", "chol", "weight", "waist", "bmi"))
cor(subdata)
# syst correlates most strongly with waist (r = 0.396)
# syst correlates most weakly with chol (r = 0.275)

# Task 5 - Spearman rank correlation matrix
cor(subdata, method = "spearman")
# The values are slightly higher than Pearson correlations in Task 4.
# This suggests there are some outliers in the data that pull Pearson values down,
# since Spearman is based on ranks and is not sensitive to outliers.

# Task 6 - scatter plot and correlation for women and men separately
hdataF <- subset(hdata, gender == "female")
plot(hdataF$chol, hdataF$syst,
     xlab = "total cholesterol (mmol/l)",
     ylab = "systolic blood pressure (mmHg)",
     main = "Women")
cor(hdataF$syst, hdataF$chol)
# Women: r = 0.295

hdataM <- subset(hdata, gender == "male")
plot(hdataM$chol, hdataM$syst,
     xlab = "total cholesterol (mmol/l)",
     ylab = "systolic blood pressure (mmHg)",
     main = "Men")
cor(hdataM$syst, hdataM$chol)
# Men: r = 0.225
# Women show a slightly stronger relationship between blood pressure and cholesterol.

# Task 7 - boxplots of blood pressure by age group and smoking status
boxplot(hdata$syst ~ hdata$age,
        xlab = "age group",
        ylab = "systolic blood pressure (mmHg)")
# Blood pressure tends to increase with age — older age groups have higher medians.
# So yes, blood pressure appears to be associated with age.

boxplot(hdata$syst ~ hdata$smoking,
        xlab = "smoking status",
        ylab = "systolic blood pressure (mmHg)")
# The boxplots for all three smoking groups look very similar.
# Blood pressure does not appear to be strongly associated with smoking status.

# Task 8 - contingency table: smoking by age
table(hdata$smoking, hdata$age)
# Looking at the table, the proportion of regular smokers ("yes") decreases with age
# while non-smokers ("no") increase. This suggests the variables may be dependent,
# but the table alone is not enough to confirm it statistically.

# Task 9 - chi-squared test: smoking and age
# H0: smoking and age are independent
# H1: smoking and age are dependent
chisq.test(table(hdata$smoking, hdata$age))
# X-squared = 20.889, df = 6, p-value = 0.00192
# p-value < 0.05, so we reject H0.
# Conclusion: smoking and age are statistically dependent.

# Task 10 - contingency table and chi-squared test: smoking and gender
table(hdata$smoking, hdata$gender)
# Men smoke more regularly than women, women are more often non-smokers.
# This hints at dependence between smoking and gender.
chisq.test(table(hdata$smoking, hdata$gender))
# X-squared = 12.934, df = 2, p-value = 0.00155
# p-value < 0.05, so we reject H0.
# Conclusion: smoking and gender are statistically dependent.