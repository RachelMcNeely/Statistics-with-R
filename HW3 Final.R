install.packages(ISLR)
library(ISLR)
rm(College)
head(College)

#nrow(College)   #777 rows (universities)
#sum(is.na(College$Apps))

#changing Private to binary 
Private <- ifelse(College$Private == "Yes", 1, 0)
College$Private <- Private
head(College)

set.seed(1)
train_index <- sample(seq_len(nrow(College)),nrow(College)/2)
train <- College[train_index, ]
head(train)
test<- College[-train_index, ]


college.ols <- lm(Apps ~ ., data = train)
summary(college.ols)
plot(train$Apps~predict(college.ols),pch=19,col=12)
#Test Error
mean((train$Apps-predict(college.ols))^2)

#Ridge with cross-validation
x <- model.matrix(Apps ~., data = College)
y <- College$Apps
train2 <-sample(1:nrow(x),nrow(x)/2)
test2 <-(-train2)
y.test <- y[test2]
library(glmnet)
grid<-10^seq(10,-2,length=100)
ridge.mod<-glmnet(x[train2,-1],y[train2],alpha=0,lambda=grid,thresh=1e-12)

set.seed(1)
cv.out<-cv.glmnet(x[train2,],y[train2],alpha=0)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam
log(bestlam)
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[test2,-1])
#Test error
mean((ridge.pred-y.test)^2)

# Lasso
lasso.mod<-glmnet(x[train2,],y[train2],alpha=1,lambda=grid)
plot(lasso.mod,lwd=2)
set.seed(1)
cv.out<-cv.glmnet(x[train2,],y[train2],alpha=1)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam
log(bestlam)
lasso.pred<-predict(lasso.mod,s=bestlam,newx=x[test2,])
#Test Error
mean((lasso.pred-y.test)^2)
out<-glmnet(x[,-1],y,alpha=1,lambda=grid)
lasso.coef<-predict(out,type="coef",s=bestlam)[1:18,]
lasso.coef
lasso.coef[lasso.coef!=0]

# PCR
#install.packages("pls")
library(pls)
set.seed(1)
pcr.fit<-pcr(Apps~.,data=College,subset=train2,scale=T,validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred<-predict(pcr.fit,x[test2,-1],ncomp=8)
#Test Error
mean((pcr.pred-y.test)^2)
pcr.fit<-pcr(y~x[,-1],scale=T,ncomp=8)
summary(pcr.fit)

# Partial Least Squares
set.seed(1)
pls.fit<-plsr(Apps~.,data=College,subset=train2,scale=T,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred<-predict(pls.fit,x[test2,-1],ncomp=6)
#Test Error
mean((pls.pred-y.test)^2)


