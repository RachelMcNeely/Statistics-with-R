library(readr)
BodyFatLM <- read_csv("~/Math 440/BodyFatLM.csv")
View(BodyFatLM)

bodyFat.lm<-lm(bodyfat~neck+chest+abdomen+hip+thigh+knee+ankle+biceps+forearm+wrist,data=BodyFatLM)
summary(bodyFat.lm)

X<-model.matrix(bodyfat~neck+chest+abdomen+hip+thigh+knee+ankle+biceps+forearm+wrist,data=BodyFatLM)
X


correlation <- cor(BodyFatLM)
round(correlation, 2)

xTrans = t(X)

19.34*solve(xTrans%*%X)

bodyFatReduced.lm<-lm(bodyfat~neck+abdomen+hip+wrist,data=BodyFatLM)
anova(bodyFat.lm, bodyFatReduced.lm)

x <- as.matrix(BodyFatLM[,-1])
y <- as.double(as.matrix(BodyFatLM[,1]))
install.packages(glmnet)
library(glmnet)
grid<-10^seq(10,-2,length=100)
ridge.mod<-glmnet(x,y,alpha=0,lambda=grid)
plot(ridge.mod,lwd=2)

