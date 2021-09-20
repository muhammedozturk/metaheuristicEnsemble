library("rlist")
library("NMOF")
library("Metrics")
library("rBayesianOptimization")
library("metaheuristicOpt")
###sudeste
skipV <- sample(1:1000000,1)									
mydata <- read.csv("D:/makaleler/deepLearnTuning/derivativeFreeEnsemble/dataSet/19911_25870_compressed_sudeste.csv/sudeste.csv",nrows=1500,skip=skipV)		
####function to be optimized
####function to be optimized
Test_Fun <- function(x) {
net_u = as.matrix(mydata[1:1000,26])
net_Yt = matrix(mydata[1:1000,20])


net <- createESN(leaking.rate = x,
								lambda = 1.1,
								n.neurons =5 ,
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
list(Score = error,
Pred = 0)
}

sphere <- function(y){
###maximizes the objective function
OPT_Res <- BayesianOptimization(Test_Fun,
bounds = list(x = c(0.1,0.2,0.4,0.7)),
init_points = 2, n_iter = y,
acq = "ucb", kappa = 2.4, eps = 0.0,
verbose = TRUE)
return(OPT_Res[2]$Best_Value)
}


##########ABC
## Define parameter
numVar <- 3
rangeVar <- matrix(c(1,3), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultABC <- ABC(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value <- sphere(resultABC)


########WOA
## Define parameter
numVar <- 3
rangeVar <- matrix(c(1,3), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultWOA <- WOA(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value <- sphere(resultWOA)


########DA
## Define parameter
numVar <- 3
rangeVar <- matrix(c(1,3), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultDA <- DA(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value <- sphere(resultDA)

########FFA
## Define parameter
numVar <- 3
rangeVar <- matrix(c(1,3), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultFFA <- FFA(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value <- sphere(resultFFA)


#########################################
###########optimal conf application
############################################
optimConf <- (resultABC[3]+resultWOA[3]+resultDA[3]+resultFFA[3])/4

OPT_Res <- BayesianOptimization(Test_Fun,
bounds = list(x = c(0.1,0.2,0.4,0.7)),
init_points = 2, n_iter = optimConf,
acq = "ucb", kappa = 2.4, eps = 0.0,
verbose = TRUE)

net_u = as.matrix(mydata[1251:1500,26])
net_Yt = matrix(mydata[1251:1500,20])


net <- createESN(leaking.rate = OPT_Res$Best_Par,
								lambda = 1.1,
								n.neurons =5 ,
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


