###We call echoStateFUnction for each iteration since there is a conflict between train(Caret) and train(echoStateFunction)
##This code block performs a 10-fold cross validation
library("Metrics")
library("caret")
source("echoStateFunction.R")
mydata <- read.csv("natal.csv")
folds <- createFolds(mydata$MeanTemperature, k = 10)	
i=1
sum=0
while(i<11)
{
  test <- mydata[folds[[i]],]
  train <- mydata[-folds[[i]],]
  net_u = as.matrix(train[,2])									
  net_Yt = matrix(train[,7])									
									
									
net <- createESN(leaking.rate =0.3,									
								lambda = 0.77,	
								n.neurons =5 ,	
								wash.out = 11,	
								feedback = FALSE,	
								regCoef =0.0006135815,	
								resCon = 1,	
								U = net_u,	
								Y = net_Yt)	
source("echoStateFunction.R")									
trained_net <- train(net)									
									
net_test=matrix(test[,7])									
Ypred <- predict(trained_net,									
								U = as.matrix(net_test),	
								generative = FALSE,	
								genNum = 2000)	
									
error <- rmse(Ypred,net_test)
  sum=sum+error
  i=i+1
}
generalRMSE <- sum/10						
