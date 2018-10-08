#Problem 1
trees_data = trees[,0:1]
tree.lm <- lm(Volume ~ trees$Girth+trees$Height+(trees$Girth*trees$Height), data = trees) 
summary(tree.lm)

library(boot)
tree.glm<-glm(Volume ~ trees$Girth+trees$Height+(trees$Girth*trees$Height), data = trees)
summary(tree.glm)
boot.fn<-function(data,index){
  return(coef(lm(Volume ~ trees$Girth+trees$Height+(trees$Girth*trees$Height),data=data,subset=index)))}
boot.fn(trees,sample(96,96,replace=T))
boot(trees,boot.fn,1000)
summary(trees)$coef

#Problem 2
library(MASS)
head(Pima.tr)
data2 = Pima.tr
log.reg.mod = glm(type~., family = binomial(link='logit'), data = data2)
summary(log.reg.mod)
log.reg.pred = predict(log.reg.mod, data2)
print(log.reg.pred)
table(Pima.tr$type, log.reg.pred>0.5)

#LDA
lda1 = lda(type~., data = data2)
plot(lda1)

#QDO
qda1 = qda(type~., data = data2)
plot(qda1)
