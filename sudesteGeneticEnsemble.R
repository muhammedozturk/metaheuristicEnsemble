library("rlist")
library("NMOF")
library("Metrics")
library("metaheuristicOpt")
###sudeste
skipV <- sample(1:1000000,1)									
mydata <- read.csv("D:/makaleler/deepLearnTuning/derivativeFreeEnsemble/dataSet/19911_25870_compressed_sudeste.csv/sudeste.csv",nrows=1500,skip=skipV)		
####function to be optimized
Test_Fun <- function(x) {
net_u = as.matrix(mydata[1:1000,26])
net_Yt = matrix(mydata[1:1000,20])


net <- createESN(leaking.rate = 0.24,
								lambda = x,
								n.neurons =50 ,
								wash.out = 10,
								feedback = FALSE,
								regCoef = 1e-8,
								resCon = 1,
								U = net_u,
								Y = net_Yt)

trained_net <- train(net)

net_test=matrix(mydata[1001:1250,20])
Ypred <- predict(trained_net,
								U = as.matrix(net_test),
								generative = FALSE,
								genNum = 2000)

error <- rmse(Ypred,net_test)
error <- error * (-1)
return(error)
}


library("GA")

sphere <- function(y){
###maximizes the objective function
f <- function(x)  Test_Fun(x)
GA <- ga(type = "real-valued", fitness = f,maxiter=3, lower = 1, upper = 2,pmutation=y)
summary(GA)
plot(GA)
return(GA@solution)
}


##########ABC
## Define parameter
numVar <- 2
rangeVar <- matrix(c(0.1,0.4), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultABC <- ABC(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value1 <- sphere(resultABC)


########WOA
## Define parameter
numVar <- 2
rangeVar <- matrix(c(0.1,0.7), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultWOA <- WOA(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value2 <- sphere(resultWOA)


########DA
## Define parameter
numVar <- 2
rangeVar <- matrix(c(0.1,0.7), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultDA <- DA(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value3 <- sphere(resultDA)

########FFA
## Define parameter
numVar <- 2
rangeVar <- matrix(c(0.1,0.7), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultFFA <- FFA(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value4 <- sphere(resultFFA)


#########################################
###########optimal conf application
############################################
optimConf <- (optimum.value1+optimum.value2+optimum.value3+optimum.value4)/4


optimConf <- as.numeric(optimConf)
f <- function(x)  Test_Fun(x)
GA <- ga(type = "real-valued", fitness = f,maxiter=3, lower = 1, upper = 2,pmutation=optimConf) )
summary(GA)
plot(GA)


conf <- as.numeric(GA@solution)


net_u = as.matrix(mydata[1251:1500,26])
net_Yt = matrix(mydata[1251:1500,20])


net <- createESN(leaking.rate = 0.24,
								lambda = conf[1],
								n.neurons =50 ,
								wash.out = 10,
								feedback = FALSE,
								regCoef = 1e-8,
								resCon = 1,
								U = net_u,
								Y = net_Yt)

trained_net <- train(net)

net_test=matrix(mydata[1001:1500,20])
Ypred <- predict(trained_net,
								U = as.matrix(net_test),
								generative = FALSE,
								genNum = 2000)

error <- rmse(Ypred,net_test)


