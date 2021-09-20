# metaheuristicEnsemble

A metaheuristic ensemble technique is proposed to configure the initialization hyper-parameters of hyper-parameter optimization (HO). The proposed method is devised after an extensive time analysis of metaheuristics and applied to Echo State Network (ESN). The experiment performed with weather forecast data shows that metaheuristic initialization methods are quite compatible with evolutionary algorithms. In the benchmark, the proposed method outperformed two alternatives. Probabilistic methods such as Bayesian optimization are not preferable for metaheuristic initialization methods, according to the results of the experiment. Metaheuristic hyper-parameter initialization methods can be performed by utilizing Random search that provides a moderate performance in which there are hardware-restricted sources. The method can be performed with any R IDE.

# Documentation

ensembleBayes.R, ensembleGenetic.R, and randomSearchEnsemble.R main source codes including metaheuricstic ensemble to initialize the hyper-parameters of those methods 

First install following packages:

library("rlist")
library("NMOF")
library("Metrics")
library("rBayesianOptimization")
library("metaheuristicOpt")

Second, run echoStateFunction.R includes Echo state Function that was utilized to perform weather forecasting.

The data sets Natal, Manaus, Saopaulo, and Sudeste are of Brazilian weather data and can be accessed through
 https://github.com/ASOCDataSets/Weather-DataSet
 https://www.kaggle.com/rtatman/reading-in-sudeste-csv-file
