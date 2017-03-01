library(rgl)
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
datax=model.matrix(profit1$profit~profit1$population)
targety<-profit1$profit
theta=c(0,0)
computeCost<-function(X,Y,theta){
  cost<-1/2*t(X%*%theta-Y)%*%(X%*%theta-Y)/length(Y)
  return(cost)
}
initialCost<-computeCost(datax,targety,theta)



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
results<-gradientDescent(datax,targety,theta,alpha,iteration)
theta<-results[[2]]

#plot the regression line and the data points
plot(profit1$population,profit1$profit,type="n")
abline(results[[2]][1],results[[2]][2])
points(profit1$population,profit1$profit,col="red")

#predict the values
predict1<-c(1,3.5)%*%theta
cat('For population = 35,000, we predict a profit of ',predict1*10000,'\n')
predict2<-c(1,7)%*%theta
cat('For population = 35,000, we predict a profit of ',predict2*10000,'\n')

#draw 3D surface plot for the cost function vs. the theta values
theta0_vals = seq(-10,10,length.out=100)
theta1_vals = seq(-1, 4, length.out=100)
J_vals=rep(0,length(theta0_vals)*length(theta1_vals))
theta_grids<-expand.grid(theta0_vals,theta1_vals)
names(theta_grids)<-c("theta0","theta1")
theta_grids_calculation<-as.data.frame(t(theta_grids))
J_vals<-sapply(theta_grids_calculation,function(x) computeCost(datax,targety,x))
plot3d(theta_grids$theta0,theta_grids$theta1,J_vals,col="blue")

contour(theta0_vals,theta1_vals,t(J_matrix),nlevels=30)
points(theta[1],theta[2],col="red")


