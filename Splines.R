library(readr)
dataset <- read_csv("~/Math 440/cars04.csv")
View(dataset)


library(splines)
dataset$Hybrid<-ifelse(dataset$Hybrid==1, 1,2)
pairs(dataset[,3:13], data=dataset,col = dataset$Hybrid)
data_without_carname <- dataset[,2:13]
lm <- lm(SuggestedRetailPrice~., data = data_without_carname)
summary(lm)

install.packages("gam")
library(gam)
gam.lm <- gam(SuggestedRetailPrice~s(CityMPG,6)+EngineSize+DealerCost+s(Weight,6)+lo(Width,3)+s(Horsepower,6), data=dataset)
summary(gam.lm)
