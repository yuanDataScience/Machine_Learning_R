rm(list=ls())
library(ggplot2)
library(optimx)
library(ucminf)
library(Rcgmin)

# read the trained weights for the middle and the outer layers
theta1<-as.matrix(read.csv("ex4dataTheta1.txt",header=FALSE))
theta2<-as.matrix(read.csv("ex4dataTheta2.txt",header=FALSE))
nnparameter<-c(as.vector(theta1),as.vector(theta2))

data_X<-as.matrix(read.csv('ex4data1X.txt',header=FALSE))
targety<-read.csv('ex4dataY.txt',header=FALSE)

sigmoid<-function(x){
  1/(exp(-x)+1)
}

nn_input_layer_size=400
nn_hidden_layer_size=25
nn_number_labels=10
nn_observation_number=dim(data_X)[1]


#The cost function is defined as adding all the errors and theta square together
nnCostFunction<-function(nnparameter,input_layer_size,hidden_layer_size,number_labels,X,y,lambda){
  t1_elements<-(input_layer_size+1)*hidden_layer_size
  total_size<-length(nnparameter)
  t2_start<-t1_elements+1
  
  t1<-matrix(nnparameter[1:t1_elements],nrow=hidden_layer_size)
  t2<-matrix(nnparameter[t2_start:total_size],nrow=number_labels)
  X<-cbind(v0=rep(1,dim(X)[1]),X)
  
  observation_numbers<-dim(X)[1]
  h<- sigmoid(cbind(rep(1,observation_numbers),sigmoid(X%*%t(t1)))%*%t(t2))
  t<-matrix(0L,nrow=observation_numbers,ncol=number_labels)
  for(i in seq(observation_numbers)){
    t[i,y[i,1]]=1
  }
  m<-observation_numbers
  -1/m*sum(t*log(h)+(1-t)*log(1-h))+lambda/(2*m)*(sum((t1[,-1])*t1[,-1])+sum(t2[,-1]*t2[,-1]))
 
}




nnGradient<-function(nnparameter,input_layer_size,hidden_layer_size,number_labels,X,y,lambda){
  t1_elements<-(input_layer_size+1)*hidden_layer_size
  total_size<-length(nnparameter)
  t2_start<-t1_elements+1
  
  t1<-matrix(nnparameter[1:t1_elements],nrow=hidden_layer_size)
  t2<-matrix(nnparameter[t2_start:total_size],nrow=number_labels)
  X<-cbind(v0=rep(1,dim(X)[1]),X)
  
  #calculate the predited result matrix using forward feeding
  m<-dim(X)[1]
  h<- sigmoid(cbind(rep(1,m),sigmoid(X%*%t(t1)))%*%t(t2))
  
  #establish the output matrix for observed results
  t<-matrix(rep(0L,m*number_labels),nrow=m)
  for(i in seq(m)){
    t[i,y[i,1]]=1
  }
  
  # the outer layer error is the difference between observed and predicted matrices
  sigma_outlayer<-h-t
  
  #calculate error transferred from outer to middle layer
  transferred_error_middle<-sigma_outlayer%*%t2[,-1]
  #calculate the predicted values for the middle layer
  pred_middle<-sigmoid(X%*%t(t1))
  #combine the transferred and the derivative of sigma function of the middle layer to get sigma error
  sigma_middle<-transferred_error_middle*pred_middle*(1-pred_middle)  #5000*25 each line corresponding to one obs
  
  #calculate the devrivative for each first layer weights
  theta1_gradient<- 1/m*t(sigma_middle)%*%X  #X is the first layer a1
  #the derivative for the weights between middle and outerlayer can be easier to calculate
  theta2_gradient<- 1/m*t(sigma_outlayer)%*%cbind(rep(1,m),pred_middle)
  
  #consider the regularization
  theta1_gradient[,-1]<-theta1_gradient[,-1]+lambda/m*t1[,-1]
  theta2_gradient[,-1]<-theta2_gradient[,-1]+lambda/m*t2[,-1]
  c(as.vector(theta1_gradient),as.vector(theta2_gradient))
  
}

randInitializeWeights<-function(L_in, L_out){
  epsilon_init=0.12
  w<-matrix(runif((L_in+1)*L_out)*epsilon_init*2-epsilon_init,nrow=L_out,ncol=L_in+1)
  return(w)
}


debugInitializeWeights<-function(fout,fin){
  total_element<-fout*(fin+1)
  w<-matrix(sin(1:total_element),nrow=fout,ncol=fin+1)/10
  return(w)
  
}

checkNNGradients<-function(lambda=0){
  
  input_layer_size = 3
  hidden_layer_size = 5
  num_labels = 3
  m = 5 #number of observations
  
  # We generate some 'random' test data
  Theta1 = debugInitializeWeights(hidden_layer_size, input_layer_size)
  Theta2 = debugInitializeWeights(num_labels, hidden_layer_size)
  # Reusing debugInitializeWeights to generate X
  feature_X  = debugInitializeWeights(m, input_layer_size-1 )
  target_y  = t(1 + t(1:m %% num_labels))
  
  nnpara<-c(as.vector(Theta1),as.vector(Theta2))
  numgrad<-computeNumericalGradient(nnpara,nnCostFunction,input_layer_size,hidden_layer_size,num_labels,feature_X,target_y,lambda)
  grad<-nnGradient(nnpara,input_layer_size,hidden_layer_size,num_labels,feature_X,target_y,lambda)
  
  diff<-numgrad-grad
  total<-numgrad+grad
  
  norm_diff<- sqrt((t(diff)%*%diff)/(t(total)%*%total))
  cat("the norm of the difference is ",norm_diff,'\n')
  result<-as.data.frame(cbind(numgrad,grad))
  names(result)<-c("numgrad","grad")
  return(result)
}

computeNumericalGradient<-function(theta,FUN = nnCostFunction,...){
  numgrad = rep(0,length(theta))
  perturb = rep(0,length(theta))
  e = 1e-4
  for (p in seq_along(theta)){
 # Set perturbation vector
  perturb[p] = e
  loss1 = FUN(theta - perturb,...)
  loss2 = FUN(theta + perturb,...)
  # Compute Numerical Gradient
  numgrad[p] = (loss2 - loss1) / (2*e);
  perturb[p] = 0;
  }
  
  return(numgrad)

}

check_results<-checkNNGradients()

lambda<-3
check_results<-checkNNGradients(lambda)

nnCostFunction(nnparameter,400,25,10,data_X,targety,3)

initial_theta1<-randInitializeWeights(nn_input_layer_size,nn_hidden_layer_size)
initial_theta2<-randInitializeWeights(nn_hidden_layer_size,nn_number_labels)
initial_thetas<-c(as.vector(initial_theta1),as.vector(initial_theta2))

result_thetas<-Rcgmin(par=initial_thetas,nnCostFunction,nnGradient,input_layer_size=400,hidden_layer_size=25,number_labels=10,X=data_X,y=targety,lambda=1)$par

theta1_length<-(nn_input_layer_size+1)*nn_hidden_layer_size
theta2_start<-theta1_length+1
total_length<-length(result_thetas)
result_theta1<-matrix(result_thetas[1:theta1_length],nrow=nn_hidden_layer_size)
result_theta2<-matrix(result_thetas[theta2_start:total_length],nrow=nn_number_labels)

predict<-function(t1,t2,X){
  X<-cbind(rep(1,dim(X)[1]),X)
  results<-sigmoid(cbind(rep(1,dim(X)[1]),sigmoid(X%*%t(t1)))%*%t(t2))
  return(apply(results,1,function(x) which.max(x)))
}

pred<-predict(result_theta1,result_theta2,data_X)

accuracy<- mean(pred==targety)*100

cat("the accuracy by neron net work is ",accuracy,'\n')
