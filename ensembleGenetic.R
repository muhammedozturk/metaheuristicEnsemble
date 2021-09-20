library("rlist")
library("NMOF")
library("Metrics")
library("metaheuristicOpt")
###natal icin uyarlama
mydata <- read.csv("natal.csv")
####function to be optimized
Test_Fun <- function(x) {
mydata=read.csv("natal.csv")
net_u = as.matrix(mydata[1:64,2])
net_Yt = matrix(mydata[1:64,7])


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

net_test=matrix(mydata[64:128,7])
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
GA <- ga(type = "real-valued", fitness = f,maxiter=3, lower = 0.1, upper = 0.4,pmutation=y)
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
GA <- ga(type = "real-valued", fitness = f,maxiter=3, lower = 0.1, upper = 0.8,pmutation=optimConf) )
summary(GA)
plot(GA)


conf <- as.numeric(GA@solution)


net_u = as.matrix(mydata[1:64,2])
net_Yt = matrix(mydata[1:64,7])


net <- createESN(leaking.rate = conf,
								lambda = 1.1,
								n.neurons =5 ,
								wash.out = 10,
								feedback = FALSE,
								regCoef = 1e-8,
								resCon = 1,
								U = net_u,
								Y = net_Yt)

trained_net <- train(net)

net_test=matrix(mydata[64:128,7])
Ypred <- predict(trained_net,
								U = as.matrix(net_test),
								generative = FALSE,
								genNum = 2000)

error <- rmse(Ypred,net_test)


