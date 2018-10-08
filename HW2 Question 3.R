library(readr)
BodyFatLM <- read_csv("~/Math 440/BodyFatLM.csv")
#View(BodyFatLM)

bodyFat.lm<-lm(bodyfat~neck+chest+abdomen+hip+thigh+knee+ankle+biceps+forearm+wrist,data=BodyFatLM)
summary(bodyFat.lm)


initial <- lm(bodyfat~1, data = BodyFatLM)

stepForward <- step(initial, direction="forward", scope = list(lower = initial, upper = bodyFat.lm))
stepForward$anova # display results

stepBackward <- step(bodyFat.lm, direction="backward")
stepBackward$anova # display results

install.packages("leaps")
library(leaps)
bestSubset <- regsubsets(bodyfat~neck+chest+abdomen+hip+thigh+knee+ankle+biceps+forearm+wrist,data=BodyFatLM)
summary(bestSubset)
