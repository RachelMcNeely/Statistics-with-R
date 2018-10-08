
library(readr)
mydata <- read_csv("~/Math 440/mydata.csv")
#View(mydata)
plot(y~x,pch=19, col="red", data=mydata)
my_lm <- lm(y~x, data = mydata)
summary(my_lm)
abline(my_lm)
