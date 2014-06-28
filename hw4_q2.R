#Q2(a)
flower <- read.table("~/Downloads/flower.txt", header=T, quote="\"")
x2 = as.numeric(flower$x2 == "Late")
x3 = as.numeric(flower$x2 == "Early")
x1 = flower$x1

lm(flower$y~x1+x2+x3)
lm(flower$y~x1+x3+x2)

#Q2(b)
reg = lm(flower$y ~ x1 + x2)
summary(reg)

#Q2(c)(i)
reg = lm(flower$y ~ x1*x2)
summary(reg)

#Q2(c)(ii) partition data into 2 groups, late and early and find the resp model
late = subset(flower, x2 == "Late")
early = subset(flower, x2 == "Early")
reg.late = with(late, lm(y~x1))
summary(reg.late)

reg.early = with(early, lm(y~x1))
summary(reg.early)

# Late model:   Y = 71.623 - 0.0411*X1 
# Early model: 	Y = 83.147 - 0.0399*X1

#Q2(c)(iii) Test if the slopes of the 2 regression lines in (ii) are the same
# H0: b3 = 0 vs Ha: b3 not zero
# SSR(X1*X2 | X1,X2) = SSR(x1,x2,x1x2) - SSR(x1,x2) = 0.5760357
lm.x1.x2.x1x2 = lm(flower$y ~ x1 * x2)
lm.x1.x2 = lm(flower$y ~ x1+x2)
ssr.x1.x2.x1x2 = sum(anova(lm.x1.x2.x1x2)$"Sum Sq"[1:3])  #3467.276
ssr.x1.x2 = sum(anova(lm.x1.x2)$"Sum Sq"[1:2])  #3466.7
sse.x1.x2.x1x2 = anova(lm.x1.x2.x1x2)$"Sum Sq"[4]  #870.6598

# f* = [SSR(X1*X2 | X1,X2)/1] / (sse.x1.x2.x1x2/(20-4))
(0.5760357/1) / (sse.x1.x2.x1x2/(20-4))  # 0.01058573 < F(.95,1,16) = 4.493998
# Do not reject H0 that the slopes for the two regression lines are the same.

#Q2(c)(iv) Test if the 2 regression lines in (ii) are the same
# H0: b2 = b3 = 0 vs Ha: not both b2= 0 and b3 = 0
# SSR(X2,X1*X2 | X1) = SSR(x1,x2,x1x2) - SSR(x1) = 887.5265
lm.x1 = lm(flower$y ~ x1)
ssr.x1 = anova(lm.x1)$"Sum Sq"[1]  #2579.75

# f* = [SSR(X2,X1*X2 | X1)/2] / (sse.x1.x2.x1x2/(20-4))
(887.5265/2) / (sse.x1.x2.x1x2/(20-4)) # 8.154979 > F(.95,2,16) = 3.633723
# Conclude Ha and regression functions for 2 lines are not identical.

