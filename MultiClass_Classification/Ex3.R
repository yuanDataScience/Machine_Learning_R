rm(list=ls())
library(ggplot2)
library(optimx)
library(ucminf)
library(Rcgmin)

#read X and y labels
X<-read.csv('ex3data1X.txt',header=FALSE)
targety<-read.csv('ex3dataY.txt',header=FALSE)


number_labels<-10
feature_number<-dim(X)[2]+1
model_X<-as.matrix(cbind(v0=rep(1,times=dim(X)[1]),X))

sigmoid<-function(x) 1/(exp(-x)+1)

fR<-function(theta,X,y,lambda){
  m<-length(y)
  cost<- -1*(1/m)*(t(y)%*%log(sigmoid(X%*%theta))+t(1-y)%*%log(1-sigmoid(X%*%theta)))+
    lambda/(2*m)*t(theta[-1])%*%theta[-1]
  return(cost[1,1])
}

gR<-function(theta,X,y,lambda){
  m<-length(y)
  grad<--(1/m)*(t(y-sigmoid(X%*%theta))%*%X)
  grad[-1]<- grad[-1]+lambda/m*theta[-1]
  return(t(grad))
}

thetas<-matrix(rep(0,times=number_labels*feature_number),nrow=number_labels)
initial_theta<-rep(0,times=feature_number)

for (i in seq(number_labels)){
 #thetas[i,]=optimx(par=initial_theta,fR,gR,method="Rcgmin",X=model_X,y=(targety==i))
 # thetas[i,]=optim(par=initial_theta,fR,gR,method="CG",X=model_X,y=as.integer(targety==i),lambda=0.1)$par
   thetas[i,]<-Rcgmin(par=initial_theta,fR,gR,X=model_X,y=as.integer(targety==i),lambda=0.1)$par
  }

predict<-sigmoid(model_X%*%t(thetas))
results<-apply(predict,1,function(x) which(x==max(x),arr.ind=TRUE))
mean(results==targety)*100