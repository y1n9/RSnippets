# Read data file
life <- read.table("~/Downloads/life.txt", header=T, quote="\"")

# Q2(a) Plot scatter plot matrix using 'pairs' function
pairs(~ P_TV + P_Dr, data=life)
# The linear model does not really fit the data

# Q2(b) Make log transformation on all 3 variables and plot scatter plot of transformed data
life_log = data.frame(
  LifeExp = log(life["LifeExp"]),
  P_TV = log(life["P_TV"]),
  P_Dr = log(life["P_Dr"])
)
pairs(~ P_TV + P_Dr, data=life_log)
# The linear model fits the transformed model well

# Q2(c)(i) Fit model: log(LifeExp) = b0 + b1*log(P_TV) + b2*log(P_Dr) + e
reg = lm(LifeExp ~ P_TV + P_Dr, data=life_log)
summary(reg)
anova(reg)

# Q2(c)(ii) 
reduced_model = lm(LifeExp ~ P_TV, data=life_log)
anova(reg, reduced_model)

# Q2(c)(iii) t-test for each predictor from summary(reg)

# Q2(c)(iv) The 95% prediction interval for overall average life expectancy in a country where 
# P_TV = 50 and P_Dr = 500, is (57.27406, 73.04493)
x = predict(reg, data.frame(P_TV=log(50), P_Dr=log(500)), interval="predict", level=0.95)
exp(x)
