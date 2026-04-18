# TILP2400-26-2
# Rakibul Hasan Remon
# Exercises 6: Statistical modeling

# load data and convert gender to factor
hdata <- read.table("0.3 Datasets.txt", header = TRUE)
hdata$gender <- factor(hdata$gender)

#Task 1
# scatter plot: chol on x-axis, syst on y-axis
plot(hdata$chol, hdata$syst,
     xlab = "total cholesterol (mmol/l)",
     ylab = "systolic blood pressure (mmHg)")

#Task 2
model1 <- lm(syst ~ chol, data = hdata)
model1
# a = 106.506, b = 5.015
# regression equation: syst_hat = 106.506 + 5.015 * chol

coefficients <- coef(model1)
abline(a = coefficients[1], b = coefficients[2], col = "red")

# Intercept a = 106.506: average blood pressure when cholesterol = 0.
# Not realistic in practice, just the starting point of the line.
# Slope b = 5.015: for each 1 unit increase in cholesterol, average blood pressure increases by 5.015 mmHg.

#Task 3
confint(model1)
# CI for intercept: [99.80, 113.22]
# CI for slope (chol): [3.85, 6.18]
# The CI for chol does not include zero, so the effect of cholesterol on blood pressure is statistically significant at the 0.05 level.
# At population level, a 1 unit increase in cholesterol is associated with a 3.85 to 6.18 mmHg increase in average blood pressure, 95% surely.

#Task 4
# 4.1: from the plot, predicted syst when chol = 6 is roughly ~137 mmHg
# 4.2: manual calculation: 106.506 + 5.015 * 6 = 136.596 mmHg
dataNEW <- data.frame(chol = 6)
predict(model1, dataNEW, interval = "predict")
# fit = 136.60, lwr = 98.06, upr = 175.13
# predicted blood pressure = 136.60 mmHg
# 95% prediction interval: [98.06, 175.13] mmHg

#Task 5
# two-covariate model: syst ~ chol + gender
# female = reference (x2 = 0), male = x2 = 1
model2 <- lm(syst ~ chol + gender, data = hdata)
model2
# a = 105.228, b1 = 4.801, b2 = 5.295
# equation: syst_hat = 105.228 + 4.801*chol + 5.295*gender(male)

#Task 6
# Intercept a = 105.228: average blood pressure of a woman with cholesterol = 0.
# b1 = 4.801: for each 1 unit increase in cholesterol, average blood pressure increases by 4.801 mmHg, keeping gender constant.
# b2 = 5.295: men have on average 5.295 mmHg higher blood pressure than women, when cholesterol level is the same.

#Task 7
confint(model2)
# CI for chol (b1):       [3.64, 5.96] -- does not include 0, significant
# CI for gendermale (b2): [2.70, 7.89] -- does not include 0, significant
# Both regression coefficients are statistically significant at the 0.05 level.
# At population level, cholesterol has a positive effect on blood pressure, and men have higher blood pressure than women on average.

#Task 8
# equations by gender (same slope b1, different intercepts):
# women (x2=0): syst_hat = 105.228 + 4.801*chol
# men   (x2=1): syst_hat = (105.228 + 5.295) + 4.801*chol = 110.523 + 4.801*chol

plot(hdata$chol, hdata$syst,
     xlab = "total cholesterol (mmol/l)",
     ylab = "systolic blood pressure (mmHg)")

coefficients <- coef(model2)
abline(a = coefficients[1], b = coefficients[2], col = "red")               # women
abline(a = coefficients[1] + coefficients[3], b = coefficients[2], col = "blue")  # men

legend("topleft", legend = c("female", "male"),
       col = c("red", "blue"), lty = 1, bty = "n")

#Task 9
# women: 105.228 + 4.801*6 = 134.034 mmHg
# men:   110.523 + 4.801*6 = 139.329 mmHg
# difference = 5.295 mmHg (= b2, the gender coefficient)
# men have 5.295 mmHg higher predicted blood pressure than women.

#Task 10
dataNEW <- data.frame(chol = c(6, 6), gender = c("male", "female"))
predict(model2, dataNEW, interval = "predict")
# male:   fit = 139.33, lwr = 101.09, upr = 177.56
# female: fit = 134.03, lwr = 95.80,  upr = 172.27
# predictions match the manual calculations in Task 9
# both intervals have the same width -- uncertainty is equal for both genders

#Task 11
fittedv    <- fitted(model2)
sresiduals <- rstandard(model2)

# residuals vs fitted values
plot(fittedv, sresiduals,
     xlab = "fitted values",
     ylab = "standardized residuals")
abline(0, 0)
# no clear systematic pattern -- independence and homoscedasticity seem ok

# QQ plot for normality
qqnorm(sresiduals)
abline(0, 1)
# most points follow the line in the middle, but there are some clear deviations at the upper tail (a few high-BP outliers)
# normality is roughly ok but not perfect due to outliers