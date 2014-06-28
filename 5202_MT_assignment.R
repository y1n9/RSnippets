# Read data file
admissions = read.table("~/Downloads/CH01PR19.txt", quote="\"")

# Fit linear model with V1 (GPA) as the response and V2 (ACT) as the predictor variable
# Linear model: Y = b0 + b1*X + e, where e ~ NID(0, sigma^2)
reg = with(admissions, lm(V1 ~ V2))

# Find estimated coefficients
summary(reg)

# The Least Square Estimates are b0^ = 2.11405 and b1^ = 0.03883

anova(reg)

# F value = 9.2402, Pr(>F) = 0.002917
# reject null hypothesis that b1 = 0, there is a relationship between GPA and ACT

# 95% Prediction interval of GPA score for freshman with ACT = 25
predict(reg, data.frame(V2=25), interval="predict", level=0.95)

# 95% Prediction interval 3.472999 +- 1.266051   (2.206948, 4.73905)

# Scatter plot with fitted regression line
with(admissions, plot(V2, V1, xlab="ACT", ylab="GPA", main="Scatter plot of GPA vs ACT")); abline(coefficients(reg), col="blue")

# Plot residuals against X
with(admissions, plot(V2, abs(residuals(reg)), xlab="ACT", ylab="abs(Residuals)", main="abs(Residuals) against X"))

# QQ plot to check normality assumption
qqnorm(residuals(reg), ylab="Residuals"); qqline(residuals(reg), col="blue")

# Shapiro test for Normality
shapiro.test(residuals(reg))

# p-value = 0.0003304. Reject null hypothesis, data not Normal

##############################################################################
# Remedies
##############################################################################
# Removing outliers 
library(car)
outlierTest(reg)

newdata = admissions[-c(9,115), ]


# Refit model for data aftr removing outliers
refit2 = with(newdata, lm(V1 ~ V2))
summary(refit2)

# Check assumptions
with(newdata, plot(V2, V1, xlab="ACT", ylab="GPA", main="(Refit) Scatter plot of GPA vs ACT")); abline(coefficients(refit), col="blue")
with(newdata, plot(V2, residuals(refit2), xlab="ACT", ylab="Residuals", main="Residuals")); abline(0,0)
with(newdata, plot(V2, abs(residuals(refit2)), xlab="ACT", ylab="abs(Residuals)", main="abs(Residuals)"))
qqnorm(residuals(refit2), ylab="Residuals"); qqline(residuals(refit2), col="blue")
shapiro.test(residuals(refit2))

predict(refit2, data.frame(V2=25), interval="predict", level=0.95)

