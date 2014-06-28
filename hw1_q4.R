# Q4(a)
hubble = read.table("~/Downloads/hubble.txt", header=T, quote="\"")
y = hubble$y
x = hubble$x
plot(y~x)

# Q4(b)
reg = lm(y~x)
confint(reg, level=0.95)

# Q4(c)
predict(reg, data.frame(x=100), interval="predict")
