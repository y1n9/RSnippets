#############################################
###        Simple Linear Regression       ###
#############################################

# Attach Devore7 package to current R session
rm(list=ls())	# Remove all objects in current session
objects() 		# Show existing objects in current session
library(Devore7)

# Show all data sets in Devore7 package
data(package = 'Devore7')

# Load Example 12.01 data set
data(xmp12.01)
str(xmp12.01)	# Print description of data set structure

# Scatter plot of data set
with(xmp12.01, plot(x~y, xlab="Palprebal fissure width (x, cm)", 
	ylab="Ocular surface area (y, cm^2)"))

# with(data, ..) evaluates an expression in a data environment
# use ?with for more info

# Fit a linear model to xmp12.01 data set using lm(..)
reg= with(xmp12.01, lm(y~x))

# Add line of best fit to current plot
abline(reg)

# Print a summary of the results of various model fitting functions
summary(reg)

# Compute Analysis of Variance (ANOVA) table for the regression fit
anova(reg)

# Get four plots, including normal probability plot, of residuals
par(mfrow=c(2,2));plot(reg)


###############################################
###        Multiple Linear Regression       ###
###############################################

# Enter data 
# the c function combines values into a vector or list
x1 = c(38,41,34,35,31,34,29,32)
x2 = c(47.5,21.3,36.5,18.0,29.5,14.2,21.0,10.0)
y = c(66.0,43.0,36.0,23.0,22.0,14.0,12.0,7.6)

# Fit regression model
regm = lm(y ~ x1+x2)
summary(regm)

anova(regm)

# Get four plots, including normal probability plot, of residuals
par(mfrow=c(2,2));plot(regm)

# plot predicted vs actual y values
pred = predict(regm,interval="prediction")	# predict.lm produces predicted values
round(pred,0)	# round predicted values to 0 place

par(mfrow=c(1,1));plot(pred[,1]~y, ylab="Predicted values, y cap", xlab="Actual values, y")
abline(a=0,b=1)
