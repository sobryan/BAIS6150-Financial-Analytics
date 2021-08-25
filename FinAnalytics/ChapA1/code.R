#Chapter A
choose(100,50) * .5^50 * .5^(100-50)

#Distribution p.d.f.s:
library(ggplot2)
par(mfrow=c(2,2))
hist(rpois(200,lambda=5),main="")
hist(rpois(200,lambda=5),main="")
hist(rpois(200,lambda=5),main="")
hist(rpois(200,lambda=5),main="")

#normal cdf and pdf:
ggplot(data.frame(x=c(-3,3)),aes(x=x)) +
  stat_function(fun=dnorm,colour="blue") +
  stat_function(fun=pnorm,colour="red")

#scale of normal:
dnorm1<-function(x) dnorm(x,mean=0,sd=.25)
ggplot(data.frame(x=c(-3,3)),aes(x=x)) +
  stat_function(fun=dnorm, colour="blue") +
  stat_function(fun=dnorm1, colour="blue")
dnorm(x=0,mean=0,sd=1)
dnorm(x=0,mean=0,sd=.25)
dnorm_limit<-function(x) {
  y <- dnorm(x)
  y[x<0|x>2]<-NA
  y
}
ggplot(data.frame(x=c(-3,3)),aes(x=x)) +
  stat_function(fun=dnorm_limit,geom="area",fill="blue",alpha=0.2) +
  stat_function(fun=dnorm)

#uniform dist:
dunif1 <-function(x) dunif(x,max=1)
dunif2 <-function(x) dunif(x,max=2)
dunif3 <-function(x) dunif(x,max=3)
ggplot(data.frame(x=c(-3,5)),aes(x=x)) +
  stat_function(fun=dunif1, colour="blue") +
  stat_function(fun=dunif2, colour="green") +
  stat_function(fun=dunif3, colour="red")

#exp dist:
dexp2<-function(x) dexp(x,2)
dexp3<-function(x) dexp(x,3)
ggplot(data.frame(x=c(0,4)),aes(x=x)) +
  stat_function(fun=dexp, colour="blue") +
  stat_function(fun=dexp2, colour="blue") +
  stat_function(fun=dexp3, colour="blue") +
  ylim(0,4)

#normal dist:
dnorm11<-function(x) dnorm(x,mean=1,sd=1)
dnorm12<-function(x) dnorm(x,mean=1,sd=.5)
ggplot(data.frame(x=c(-2,4)),aes(x=x)) +
  stat_function(fun=dnorm, colour="blue") +
  stat_function(fun=dnorm11, colour="green") +
  stat_function(fun=dnorm12, colour="red") +
  ylim(0,1)

#lognormal dist:
dlognorm <- function(x,sigma) { 1/x*dnorm(log(x),sd=sigma) }
dlognorm1<-function(x) dlognorm(x,sigma=.5)
dlognorm2<-function(x) dlognorm(x,sigma=1)
dlognorm3<-function(x) dlognorm(x,sigma=2)
ggplot(data.frame(x=c(-2,4)),aes(x=x)) +
  stat_function(fun=dlognorm1, colour="blue") +
  stat_function(fun=dlognorm2, colour="green") +
  stat_function(fun=dlognorm3, colour="red") +
  ylim(0,1.5)

#t dist:
t1<-function(x) dt(x,df=1)
t4<-function(x) dt(x,df=4)
t25<-function(x) dt(x,df=25)
ggplot(data.frame(x=c(-4,4)),aes(x=x)) +
  stat_function(fun=t1, colour="blue") +
  stat_function(fun=t4, colour="green") +
  stat_function(fun=t25, colour="red") +
  ylim(0,.5)

#Hypothesis Testing:
flip<-rbinom(50,1,.55)
flip
prop.test(sum(flip), 50, p=0.5, correct=FALSE)

#Regression:
library(datasets)
data(mtcars)
x<-mtcars$mpg
y<-mtcars$wt
Sxy<-sum((x-mean(x))*(y-mean(y)))
Sxx <- sum((x-mean(x))^2)
beta1 <- Sxy/Sxx
beta1
beta0 <- mean(y) - beta1*mean(x)
beta0
yhat <- beta0 + beta1*x
length(x)

m1<-lm(data=mtcars,wt~mpg+1)
summary(m1)
m1$coeff
yhat = m1$coeff[1]+m1$coeff[2]*x
plot(x,y,col=4,xlab="x: weight",ylab="y: weight")
points(x,yhat,col=2)
