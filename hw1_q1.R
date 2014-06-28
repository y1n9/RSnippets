# Q3(a)
admissions = read.table("~/Downloads/CH01PR19.txt", quote="\"")
y = admissions$V1
x = admissions$V2
reg = lm(y~x)
summary(reg)

# Q3(b)
plot(y~x)
lines(reg$fitted.values~x)

# Q3(c)
2.11405 + 0.03883*30
