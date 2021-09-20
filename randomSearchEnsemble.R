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



sphere <- function(y){
###maximizes the objective function
leakingliste <- c()
sonucliste <- c()
count <-1 
j <- 1
while(count<y){
 x1 <- runif(1, 0.1, 0.9)
sonuc <- Test_Fun(x1)
leakingliste[j] <- x1
sonucliste[j] <- sonuc
j <- j+1
count <- count+1
}

#######en iyi leaking bul###########################
j <- 1
indisUltimate <- y-1
dizi <- 1:indisUltimate
sonucIndis <- 1
max <- sonucliste[1]
for(j in dizi)
{
	if(sonucliste[j]>max){
		max <- sonucliste[j]
	sonucIndis <- j
	}
}
return(sonucliste[sonucIndis])
}####sphere sonu


##########ABC
## Define parameter
numVar <- 2
rangeVar <- matrix(c(1,15), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultABC <- ABC(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value1 <- sphere(resultABC)


########WOA
## Define parameter
numVar <- 2
rangeVar <- matrix(c(1,15), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultWOA <- WOA(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value2 <- sphere(resultWOA)


########DA
## Define parameter
numVar <- 2
rangeVar <- matrix(c(1,15), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultDA <- DA(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value3 <- sphere(resultDA)

########FFA
## Define parameter
numVar <- 2
rangeVar <- matrix(c(1,15), nrow=2)
## calculate the optimum solution using artificial bee colony algorithm
resultFFA <- FFA(sphere, optimType="MAX", numVar, numPopulation=3,
maxIter=5, rangeVar)
## calculate the optimum value using sphere function
optimum.value4 <- sphere(resultFFA)


#########################################
###########optimal conf application
############################################
optimConf <- (resultABC[1]+resultWOA[1]+resultDA[1]+resultFFA[1])/4


######################################
leakingliste <- c()
sonucliste <- c()
count <-1 
j <- 1
while(count<optimConf){
 x1 <- runif(1, 0.1, 0.9)
sonuc <- Test_Fun(x1)
leakingliste[j] <- x1
sonucliste[j] <- sonuc
j <- j+1
count <- count+1
}

#######en iyi leaking uygula###########################
j <- 1
indisUltimate <- optimConf-1
dizi <- 1:indisUltimate
sonucIndis <- 1
max <- sonucliste[1]
for(j in dizi)
{
	if(sonucliste[j]>max){
		max <- sonucliste[j]
	sonucIndis <- j
	}
}
leakingliste[sonucIndis]
#########################################





net_u = as.matrix(mydata[1:64,2])
net_Yt = matrix(mydata[1:64,7])


net <- createESN(leaking.rate = leakingliste[sonucIndis],
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


