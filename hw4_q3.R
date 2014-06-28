#Q3(a)
# backward elimination
happy <- read.table("~/Downloads/happy.txt", header=T, quote="\"")
attach(happy)

x1 = money
x2 = sex
x3 = work
x4 = love

# Step 1: full model
lm.x1.x2.x3.x4 = lm(happy$happy~x1 + x2 + x3 + x4)
summary(lm.x1.x2.x3.x4)

# Step 2: remove x2 w p-value = 0.197012
lm.x1.x3.x4 = lm(happy$happy~x1 + x3 + x4)
summary(lm.x1.x3.x4)

#Stop process as all predictors in the model are statistically significant.

#========================================
# Forward selection
# Step 1
m0 = lm(happy$happy~1)
m1 = lm(happy$happy~x1)
m2 = lm(happy$happy~x2)
m3 = lm(happy$happy~x3)
m4 = lm(happy$happy~x4)

anova(m0,m1)
anova(m0,m2)
anova(m0,m3)
anova(m0,m4)

# Step 2: m4 produced the minimum p-value: 3.506e-09. Therefore x4 enters our model.
m41 = lm(happy$happy~x4+x1)
m42 = lm(happy$happy~x4+x2)
m43 = lm(happy$happy~x4+x3)

anova(m41,m4)
anova(m42,m4)
anova(m43,m4)

# Step 3: m43 produced the minimum p-value: 0.009873. Therefore x3 enters our model.
m431 = lm(happy$happy~x4+x3+x1)
m432 = lm(happy$happy~x4+x3+x2)

anova(m431,m43)
anova(m432,m43)

# Step 4: m431 produced the minimum p-value: 0.07333. Therefore x1 enters our model.
m4312 = lm(happy$happy~x4+x3+x1+x2)

anova(m4312,m431)
# The min p-value is 0.724. Therefore, no variable should be further added to the model.

#========================================
#Q3(b) R adj criterion
X= cbind(x1,x2,x3,x4)
library(leaps)
a = leaps(X, happy$happy, method="adjr2")
a$which[a$adjr2 == max(a$adjr2)]  #TRUE FALSE  TRUE  TRUE
max(a$adjr2)  #0.6842126
#========================================
#Q3(c) SBC criterion
b = leaps(X, happy$happy, method="Cp")
b$which[b$Cp == min(b$Cp)]  #TRUE FALSE  TRUE  TRUE
min(b$Cp)  #3.126758

# AIC criteria
step(lm(happy$happy ~ x1+x2+x3+x4)) #happy$happy ~ x1 + x3 + x4 w AIC = 7.221

