# Input work crew data
X1 = c(4,4,4,4,6,6,6,6)
X2 = c(2,2,3,3,2,2,3,3)
Y = c(42,39,48,51,49,53,61,60)

lm.x1 = lm(Y ~ X1)
lm.x2 = lm(Y ~ X2)
lm.x1.x2 = lm(Y ~ X1 + X2)

# SSR(x2|x1) = SSR(x1,x2) - SSR(x1)
ssto = sum(anova(lm.x1)$"Sum Sq")
ssr.x1.x2 = sum(anova(lm.x1.x2)$"Sum Sq"[1:2])
ssr.x1 = anova(lm.x1)$"Sum Sq"[1]
ssr.x2 = anova(lm.x2)$"Sum Sq"[1]
# also
sse.x1 = anova(lm.x1)$"Sum Sq"[2] # eq sum(lm.x1$resid^2)
sse.x2 = anova(lm.x2)$"Sum Sq"[2] # eq sum(lm.x2$resid^2)

# Coefficient of partial determination measures the proportion explained by one additional X
# The partial r2 of x1 while controlling for x2 is 0.9291457
# pr2_1.2 = SSR(x1|x2) / SSE(x2)
(ssr.x1.x2 - ssr.x2) / sse.x2
# The partial r2 of x2 while controlling for x1 is 0.9066225
# pr2_2.1 = SSR(x2|x1) / SSE(x1)
(ssr.x1.x2 - ssr.x1) / sse.x1

# Coefficient of multiple determination r2 = SSR / SSTO = 0.9580232
ssr.x1.x2 / ssto

# Coefficient of determination r2 measures the proportion of variation explained by X
# r2 of x1 is SSR(x1) / SSTO = 0.5504614
ssr.x1 / ssto
# r2 of x2 is SSR(x2) / SSTO = 0.4075618
ssr.x2 / ssto

# Adjusted r2 is 1 - ((n-1)/(n-p))*SSE/SSTO = 0.9412325
1 - ((8-1)/(8-3))*(ssto - ssr.x1.x2)/ssto

# Part B. Because the 2 predictor variables are not correlated, 
# r2 of x1 + r2 of x2 = coeff of multiple determination r2
# 0.5504614 + 0.4075618 = 0.9580232
