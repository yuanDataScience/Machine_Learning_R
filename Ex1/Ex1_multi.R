library(rgl)

rm(list=ls())


#read data from ex1data1.txt
house1<-read.csv("ex1data2.txt",header=FALSE)
head(house1)
names(house1)<-c("size","bedrooms","price")
house1_features<-scale(house1[,1:2])
means<-colMeans(house1[,1:2])
sds<-apply(house1[,1:2],2,sd)


#prepare the design matrix, the initial theta values, learning rate, and iteration times
datax=model.matrix(house1$price~house1_features)
targety<-house1$price
theta=rep(0,times=dim(datax)[2])

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
iteration=400
cost_history<-rep(0,iteration)
results<-gradientDescent(datax,targety,theta,alpha,iteration)

plot(1:length(results[[1]]),results[[1]],xlab="number of iteration",ylab="costJ",
     col="blue",type="l",lwd="2")
theta<-results[[2]]
cat("Theta computed from gradient descent:\n",theta)

#plot the regression line and the data points
plot(profit1$population,profit1$profit,type="n")
abline(results[[2]][1],results[[2]][2])
points(profit1$population,profit1$profit,col="red")

#Estimate the price of a 1650 sq-ft, 3 br house
x=c((1650-means[1])/sds[1],(3-means[2])/sds[2])
px=c(1,x)
pred1<-t(px)%*%theta

#normal equation by OLS
house_normal<-read.csv("ex1data2.txt",header=FALSE)
names(house_normal)<-c("size","bedrooms","price")
house_normal_features<-house_normal[,1:2]
design_matrix<-model.matrix(house_normal$price~house_normal$size+house_normal$bedrooms)
theta_ols<-solve(t(design_matrix)%*%design_matrix)%*%t(design_matrix)%*%house_normal$price

#Estimate the price of a 1650 sq-ft, 3 br house by OLS

px=c(1,1650,3)
pred_ols<-t(px)%*%theta_ols
