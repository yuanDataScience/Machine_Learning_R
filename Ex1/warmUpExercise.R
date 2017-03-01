rm(list=ls())
#return the 5*5 identity matrix
warmUpExercise<-function() {diag(5)}
warmUpExercise()

#read data from ex1data1.txt
profit1<-read.csv("ex1data1.txt",header=FALSE)
head(profit1)
names(profit1)<-c("population","profit")
head(profit1)
plot(profit1$population,profit1$profit,xlab="Population of City in 10,000s",
     ylab="Profit in $10,000s", pch=4,col="red")

#prepare the design matrix, the initial theta values, learning rate, and iteration times
x=model.matrix(profit1$profit~profit1$population)
y<-profit1$profit
theta=c(0,0)
computeCost<-function(X,Y,theta){
  cost<-1/2*t(X%*%theta-Y)%*%(X%*%theta-Y)/length(Y)
  return(cost)
}
initialCost<-computeCost(x,y,theta)



#use gradient descendent
gradientDescent<-function(X,Y,Theta,Alpha,Iter){
  temp_cost<-rep(0,Iter)
  temp_theta<-Theta
  for (i in seq(Iter)){
    temp_cost[i]<-computeCost(X,Y,temp_theta)
    temp_theta<-temp_theta-(Alpha/length(Y))*t(X)%*%(X%*%temp_theta-Y)
    
  }
  return(list(temp_cost,temp_theta))
}

alpha=0.01
iteration=1500
cost_history<-rep(0,iteration)
results<-gradientDescent(x,y,theta,alpha,iteration)
results[[2]]

computeCost(x,y,c(-3.630291,1.166362))

