install.packages(ISLR)
library(ISLR)
head(College)
#nrow(College)   #777 rows (universities)

smp_size <- floor(0.75 * nrow(College))
set.seed(123)
train_ind <- sample(seq_len(nrow(College)), size = smp_size)
# Generate a training and testing set of data.
train <- College[train_ind, ]
test <- College[-train_ind, ]


college.lm <- lm(College$Apps~College$Enroll+College$Top10perc+College$Top25perc+College$F.Undergrad+College$P.Undergrad+College$Outstate+College$Room.Board+College$Books+College$Books+College$Personal+College$PhD+College$Terminal+College$S.F.Ratio+College$perc.alumni+College$Expend+College$Grad.Rate, data = train)
summary(college.lm)
#using a small lambda is the same as using ordinary least squares regression    

#CALCULATING TEST ERROR FOR LINEAR MODEL
# Now let's generate some new data and estimate ErrT
Nsims<-100
ErrT<-NULL
for(i in 1:Nsims){
  Y.new<-Y.true+rnorm(1001,0,50)
  new.err<-mean((Y.new-predict(college.lm))^2)
  ErrT<-c(ErrT,new.err)
}
ErrT
ErrT.hat<-mean(ErrT)
ErrT.hat


#RIDGE REGRESSION MODEL W LAMBDA CHOSEN BY CROSS-VALIDATION
# Ridge
# Lab 2 Ridge Regression and Lasso
x<-model.matrix(Apps~.,College)
y<-College$Apps
library(glmnet)
grid<-10^seq(10,-2,length=100)
head(x)

ridge.mod<-glmnet(x,y,alpha=0,lambda=grid)
plot(ridge.mod,lwd=2)
dim(coef(ridge.mod))
ridge.mod$lambda[1]
round(coef(ridge.mod)[,1],3)
ridge.mod$lambda[100]
round(coef(ridge.mod)[,100],3)
ridge.mod$lambda[83]
round(coef(ridge.mod)[,83],3)
bridge.ridge<-glmnet(x,y,alpha=0,lambda=1.15)
plot(College$Apps~predict(bridge.ridge,newx=x),pch=19,col=12)
mean((College$Apps-predict(bridge.ridge,newx=x))^2)

# Let's select lambda using a training set and cross-validation
set.seed(1)
train<-sample(1:nrow(x),nrow(x)/2)
test<-(-train)
y.test<-y[test]
ridge.mod<-glmnet(x[train,-1],y[train],alpha=0,lambda=grid,thresh=1e-12)
# Let's look at lamba=4
ridge.pred<-predict(ridge.mod,s=4,newx=x[test,-1])
mean((ridge.pred-y.test)^2)
# How does this compare with just using the mean to predict y?
mean((mean(y[train])-y.test)^2)
# How about the extremes?
ridge.pred<-predict(ridge.mod,s=1e10,newx=x[test,-1])
mean((ridge.pred-y.test)^2)
ridge.pred<-predict(ridge.mod,s=0,newx=x[test,-1],exact=TRUE)
mean((ridge.pred-y.test)^2)
lm(y~x,subset=train)
predict(ridge.mod,s=0,exact=T,type="coef")[1:20.]

# Cross validation to pick best lambda
set.seed(1)
cv.out<-cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam
log(bestlam)
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[test,-1])
mean((ridge.pred-y.test)^2)
out<-glmnet(x[,-1],y,alpha=0)
round(predict(out,type="coef",s=bestlam)[1:20,],4)

#TEST ERROR OF RIDGE MODEL
Nsims<-100
ErrT<-NULL
for(i in 1:Nsims){
  Y.new<-Y.true+rnorm(1001,0,50)
  new.err<-mean((Y.new-predict(bridge.ridge,newx=x))^2)  ###bridge.ridge,newx=x DOES NOT WORK
  ErrT<-c(ErrT,new.err)
}
ErrT
ErrT.hat<-mean(ErrT)
ErrT.hat






