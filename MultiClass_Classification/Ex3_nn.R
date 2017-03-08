# read the trained weights for the middle and the outer layers
theta1<-as.matrix(read.csv("ex3dataTheta1.txt",header=FALSE))
theta2<-as.matrix(read.csv("ex3dataTheta2.txt",header=FALSE))

X<-as.matrix(read.csv('ex3data1X.txt',header=FALSE))
targety<-read.csv('ex3dataY.txt',header=FALSE)

sigmoid<-function(x){
  1/(exp(-x)+1)
}

X<-cbind(v0=rep(1,dim(X)[1]),X)

predict<-function(t1,t2,X){
  results<-sigmoid(cbind(rep(1,dim(X)[1]),sigmoid(X%*%t(t1)))%*%t(t2))
  return(apply(results,1,function(x) which.max(x)))
}

pred<-predict(theta1,theta2,X)

accuracy<- mean(pred==targety)*100

cat("the accuracy by neron net work is ",accuracy,'\n')