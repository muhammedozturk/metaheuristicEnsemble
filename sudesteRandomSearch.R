library("rlist")									
library("NMOF")									
library("Metrics")									
library("metaheuristicOpt")									
###natal icin uyarlama									
									
skipV <- sample(1:1000000,1)									
mydata <- read.csv("D:/makaleler/deepLearnTuning/derivativeFreeEnsemble/dataSet/19911_25870_compressed_sudeste.csv/sudeste.csv",nrows=1500,skip=skipV)									
####function to be optimized									
Test_Fun <- function(x) {									
net_u = as.matrix(mydata[1:1000,26])									
net_Yt = matrix(mydata[1:1000,20])									
									
									
net <- createESN(leaking.rate = 0.38,									
								lambda = 1.8,	
								n.neurons =50 ,	
								wash.out = 6,	
								feedback = FALSE,	
								regCoef = x,	
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
									
									
									
sphere <- function(y){									
###maximizes the objective function									
leakingliste <- c()									
sonucListe <- c()									
count <-1 									
j <- 1									
while(count<y){									
 x1 <- runif(1, 1e-100, 1e-2)									
sonuc <- Test_Fun(x1)									
leakingliste[j] <- x1									
sonucListe[j] <- sonuc									
j <- j+1									
count <- count+1									
}									
									
#######en iyi leaking bul###########################									
j <- 1									
indisUltimate <- y-1									
dizi <- 1:indisUltimate									
sonucIndis <- 1									
max <- sonucListe[1]									
for(j in dizi)									
{									
	if(sonucListe[j]>max){								
		max <- sonucListe[j]							
	sonucIndis <- j								
	}								
}									
return(sonucListe[sonucIndis])									
}####sphere sonu									
									
									
##########ABC									
## Define parameter									
numVar <- 2									
rangeVar <- matrix(c(1,15), nrow=2)									
## calculate the optimum solution using artificial bee colony algorithm									
resultABC <- ABC(sphere, optimType="MAX", numVar, numPopulation=3,									
maxIter=5, rangeVar)									
## calculate the optimum value using sphere function									
									
									
									
########WOA									
## Define parameter									
numVar <- 2									
rangeVar <- matrix(c(1,15), nrow=2)									
## calculate the optimum solution using artificial bee colony algorithm									
resultWOA <- WOA(sphere, optimType="MAX", numVar, numPopulation=3,									
maxIter=5, rangeVar)									
## calculate the optimum value using sphere function									
									
									
									
########DA									
## Define parameter									
numVar <- 2									
rangeVar <- matrix(c(1,15), nrow=2)									
## calculate the optimum solution using artificial bee colony algorithm									
resultDA <- DA(sphere, optimType="MAX", numVar, numPopulation=3,									
maxIter=5, rangeVar)									
## calculate the optimum value using sphere function									
									
									
########FFA									
## Define parameter									
numVar <- 2									
rangeVar <- matrix(c(1,15), nrow=2)									
## calculate the optimum solution using artificial bee colony algorithm									
resultFFA <- FFA(sphere, optimType="MAX", numVar, numPopulation=3,									
maxIter=5, rangeVar)									
## calculate the optimum value using sphere function									
									
									
									
#########################################									
###########optimal conf application									
############################################									
optimConf <- (resultABC[1]+resultWOA[1]+resultDA[1]+resultFFA[1])/4									
									
									
##optimConf means iteration here####################################									
leakingliste <- c()									
sonucListe <- c()									
count <-1 									
j <- 1									
while(count<optimConf){									
 x1 <- runif(1, 1e-100, 1e-2)									
sonuc <- Test_Fun(x1)									
leakingliste[j] <- x1									
sonucListe[j] <- sonuc									
j <- j+1									
count <- count+1									
}									
									
#######en iyi leaking uygula###########################									
j <- 1									
indisUltimate <- length(sonucListe)									
dizi <- 1:indisUltimate									
sonucIndis <- 1									
max <- sonucListe[1]									
for(j in dizi)									
{									
	if(sonucListe[j]>max){								
		max <- sonucListe[j]							
	sonucIndis <- j								
	}								
}									
leakingliste[sonucIndis]									
#########################################									
									
									
									
									
									
net_u = as.matrix(mydata[1251:1500,26])									
net_Yt = matrix(mydata[1251:1500,20])									
									
									
net <- createESN(leaking.rate = 0.38,									
								lambda = 1.8,	
								n.neurons =50 ,	
								wash.out = 6,	
								feedback = FALSE,	
								regCoef = leakingliste[sonucIndis],	
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
error									
########################################################									
