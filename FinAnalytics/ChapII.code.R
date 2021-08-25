#Chapter II
setwd(paste(homeuser,"/FinAnalytics/ChapII",sep=""))

x = c(1.3,1.2,1.3,NA,1.4,1.5)
plot(x,ylab="EUR prices")
is.na(x)

#Filter prices:
x[x > 1.3]
#Keeps the NA intact:
y <- diff(log(x))
round(y,3)

#g(x,y) is a power function:
g <-function(x,y=5) { return(x^y) }
g(4)
g(4,6)
g(4,y=7)
g(y=8,x=4)
g

#3 assignment operators
x <- 1
assign("x",2)
x
x = 3
x
f <-function(x)
{
  x = 4
  x
}
f(x)
x
#The fourth type is "<<-"
x = 3
x
f <-function(x)
{
  x <<- 4
  x
}
f(x)
x
typeof(f)
typeof(x)

#Classic if-else:
call_type = 2
if(call_type == 1) {
  str = "f(2)"
} else {
  str = "g(2)"
}
eval(parse(text=str))
#Not so classic if-else function:
call_type = 2
ifelse(call_type == 1,
  eval(parse(text="f(2)")),
  eval(parse(text="g(2)")))

#Functional nature:
set.seed(1)
vec = c(1:3)
sapply(vec,rnorm)

#Create two column matrix:
A = cbind(rep(x,length(y)),y)
A
B = rbind(rep(x,length(y)),y)
B
t(A) == B
sum(t(A) == B)

#Subscripting: positive and negative
B[,4]
B[,-4]
t(A)[,-4] == B[,-4]
sum(t(A)[-2,-4] == B[-2,-4])

#Ranges and looping:
n <- 12
z <- 1:n
z
z <- c(1:n)
z
z <- vector(length=n)
for(i in 1:n)
  z[i] <- i
z

#Matrices and arrays:
mat2by4 <- matrix(1:8, nrow=2, ncol=4)
mat2by4
arr2by4by3 <- array(1:24, dim=c(2,4,3))
arr2by4by3

arr2by4by3[1,,]
arr2by4by3[1,-4,]
arr2by4by3[1,c(-3,-4),]

length(c(-3,-4))
dim(arr2by4by3[1,c(-3,-4),])

A <- arr2by4by3[1,c(-3,-4),]
t(A)
A <- arr2by4by3[1,c(-3,-4),]
A
t(A)
A%*%t(A)
1+9*9+17*17

#Exception handling:
fh <- 0
tryCatch({
  #main block
  fh <<- file("file1.txt", open="r")
}, warning = function(w) {
  #warning-handler-code
  print(w)
  fh <<- NA
}, error = function(e) {
  #error-handler-code
  print(e)
  fh <<- NA
}, finally = {
  #cleanup-code
})
if(!is.na(fh)) readLines(fh)

#Setting precision:
options(digits=10)
pi = 3.1415926535897932384626
pi

if(FALSE) {
  x = -1.5
  abs(x)  #absolute value
  sqrt(-x)	#square root
  ceiling(3.475) #is 4
  floor(3.475) #is 3
  trunc(5.99) #is 5
  round(3.475, digits=2) #is 3.48
  signif(3.475, digits=2) #is 3.5
  x = pi / 4
  x
  v = c(cos(x),sin(x), tan(x))
  v
  acos(v[1])
  log(x)	  #natural logarithm
  log10(x)	#common logarithm
  exp(x)	  #e^x
}

#Random sampling:
plot(density(rbinom(50,50,1/2)))

options(digits=6)
set.seed(99)
sample(10, replace = TRUE)

#String concatenation:
print(paste("PCLN","UNP","IBM","MCD","PFE",sep=","))

#Date and string functions:
date <- as.Date("2014-02-01")
substr(date,9,11)

#String array:
tickers <- c("PCLN","UNP","IBM","MCD","PFE")
match('MCD',tickers)

#Data frame:
L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = TRUE)
d <- data.frame(x = 1, y = 1:10, fac = fac)
d[1:4,]
d$fac

#Input-ouput:
write.csv(d,file="d.txt",row.names=FALSE)
e <- read.csv("d.txt",header=TRUE)
e[1:4,]
names(e)
names(e) <- c(names(e)[1:2],"factor")
e[-c(2:dim(e)[1]),]
typeof(e)

#Lists:
c(1,c(1,2),3,"A",c(4,5))
list(1,c(1,2),3,"A",list(4,5))
l <- list(1,c(1,2),3,"A",list(4,5))
l[2]
l[[2]]

e[[1]]
e[[2]]
e[[3]]

obtainPrices <- function() {
  A <- matrix(c("VRSN","UNP","HPQ","NSC"),nrow=1)
  B <- matrix(c(37.61,125.62,50.48,50.44),nrow=1)
  list(A,B)
}
res <- obtainPrices()
res[[1]]
res[[2]]
