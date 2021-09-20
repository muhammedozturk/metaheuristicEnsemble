########################################
########################################
###IMPLEMENTATION OF ECHO STATE NETWORKS
###By Matthias Adriaens#################
###github/matthiasadriaens/EchoStateNet#
########################################
########################################


esn_validity <- function(esn) {
  warnings <- character()

  #Check the different validity criteria
  if(length(esn@leaking.rate) != 1){
    warnings <- c(warnings,paste("Leaking rate is length", esn@leaking.rate, ".  Should be 1", sep = " "))
  }

  if(length(warnings) == 0) TRUE else warnings
}

#' @param leaking.rate The rate which the net is learning at
#' @param lambda The lambda of the ESN
#' @param W_in W_in of the network
#' @param W_out W_out of the network
#' @param W The reservoir of the matrix
#' @param U The input matrix for the network
#' @param Y The response signal for the network to be trained with
#' @author Matthias Adriaens
setClass("ESN",representation(leaking.rate = "numeric",
                              lambda = "numeric",
                              n.neurons = "numeric",
                              W_in = "matrix",
                              W_out = "matrix",
                              W = "matrix",
                              W_fb = "matrix",
                              U = "matrix",
                              Y = "matrix",
                              X = "matrix",
                              regCoef = "numeric",
                              wash.out = "numeric",
                              feedback = "logical",
                              resCon = "numeric",
                              x = "matrix"),
         #prototype(leaking.rate = 0.2,
         #           lambda = 0.5),
         validity = esn_validity
)

init_W_in <- function(N,K){
  #N number of reservoir units, K is number of inputs
  W_in <- matrix(runif(N*(K+1), -0.5, 0.5),
                 nrow = N,
                 ncol = (K+1))
  return(W_in)
}

init_W <- function(N,lambda,resCon){
  #N is number of reservoir units
  W <- matrix(runif(N*N, -0.5, 0.5)
              ,nrow = N
              ,ncol = N)*
       matrix(rbinom(N*N,1,resCon),N)
  cat('Computing spectral radius...')
  spectral.radius = abs(eigen(W,only.values=TRUE)$values[1])
  print('done.')
  W = W/spectral.radius*lambda
  return(W)
}

init_W_out <- function(K,N,L){
  #L = number of outputs
  W_out <- matrix()
  return(W_out)
}

init_W_fb <- function(N){
  #L = number of outputs
  W_fb <- matrix(runif(N*1,-0.5,0.5),N)
  return(W_fb)
}


init_reservoir <- function(N,K,L,lambda,resCon){
  init_res <- list()
  init_res[["W_in"]] <- init_W_in(N,K)
  init_res[["W"]] <- init_W(N,lambda,resCon)
  init_res[["W_out"]] <- init_W_out(K,N,L)
  init_res[["W_fb"]] <- init_W_fb(N)
  return(init_res)
}

createESN <- function(leaking.rate =0.5,
                    lambda = 1.25,
                    n.neurons = 1000,
                    wash.out = 0,
                    U,
                    Y,
                    feedback = FALSE,
                    regCoef = 0.0025,
                    resCon = 1){
  N <- n.neurons
  K <- ncol(U)
  L <- ncol(Y)
  init_res <- list()
  init_res <- init_reservoir(N,K,L,lambda,resCon)

  x <- matrix(0,nrow = n.neurons,ncol =1)
  X <- matrix(0,1+ncol(U) + n.neurons,(nrow(Y)-wash.out))

  Y <- as.matrix(Y[1:nrow(Y),])

  esn <- new("ESN",
             leaking.rate = leaking.rate,
             lambda = lambda,
             n.neurons = n.neurons,
             W_in = init_res[["W_in"]],
             W_out = init_res[["W_out"]],
             W = init_res[["W"]],
             W_fb = init_res[["W_fb"]],
             U = U,
             Y =  Y,
             X = X,
             regCoef = regCoef,
             wash.out = wash.out,
             feedback = feedback,
             resCon = resCon,
             x = x)
  return(esn)
}

#######################################
####TRAINING THE ECHO STATE NETWORK####
#######################################

setGeneric("train", function(esn) 0)
#Matrix runs the reservoir and collects the reservoir states for a given initilized echo state network
setMethod("train", signature(esn = "ESN"), function(esn) {
  #x <- matrix(0,nrow = esn@n.neurons,ncol =1)

  for(i in 1:(nrow(esn@Y))){
      #Calculate feedback matrix if needed
      u_out <- ifelse(i == 1,0,esn@Y[i,])
      feedbackMatrix <- ifelse(esn@feedback,1,0)*u_out*esn@W_fb
      #Update equation for the reservoir states
      esn@x <- (1-esn@leaking.rate)*esn@x + esn@leaking.rate*tanh(esn@W_in%*%t(t(c(1,esn@U[i,]))) + esn@W%*%esn@x + feedbackMatrix)
      #Collecting all the reservoir states
      #Wash out the initial set up
    if(i > esn@wash.out){
      esn@X[,i-esn@wash.out] <- c(1,esn@U[i,],as.matrix(esn@x))
    }
  }
  #Train W_out in a linear way using Ridge regression
  esn@Y <- as.matrix(esn@Y[(esn@wash.out+1):nrow(esn@Y),])
  esn@W_out <- t(esn@Y)%*%t(esn@X)%*%solve(esn@X%*%t(esn@X) + esn@regCoef*diag(nrow(esn@X)))
  esn
})

#######################################
####PREDICTING WITH ECHO STATE NET#####
#######################################
setGeneric("predict", function(esn, U,generative,genNum) 0)
#Method predicts an an output matrix for a given input matrix and a trained ESN
setMethod("predict", signature(esn = "ESN", U = "matrix",generative = "logical",genNum = "numeric"),
  function(esn,U,generative,genNum) {
    #Init single reservoir state
    x <- matrix(0,nrow = esn@n.neurons,ncol =1)
    #Run in Generative mode
    if(isTRUE(generative)){
      Yp <- matrix(0, nrow = (genNum +1), ncol = ncol(esn@Y))
      u_in <- U[1,]
      for(i in 1:genNum){
        print(dim(t(t(c(1,u_in)))))
        esn@x <- (1-esn@leaking.rate)*esn@x + esn@leaking.rate*tanh(esn@W_in%*%t(t(c(1,u_in)))+ esn@W%*%esn@x)
        y <- esn@W_out %*% c(1,u_in,as.matrix(esn@x))
        Yp[i+1,] <- y
        #Genrative mode, so predicted output gets next input
        u_in <- y
      }
    }
    #Run in prediction mode
    else{
      Yp <- matrix(0, nrow = nrow(U) , ncol = ncol(esn@Y))
      u_out <- Yp[1,]
      for (i in 1:(nrow(U) - 1)) {
        #Calculate reservoir state with given inputs
        x <- (1-esn@leaking.rate)*esn@x + esn@leaking.rate*tanh(esn@W_in%*%t(t(c(1,U[i,])))+ esn@W%*%esn@x)
        #Predict output with trained w_out layer
        y <- esn@W_out %*% c(1,U[i,],as.matrix(esn@x))
        Yp[i+1,] <- y
        u_out <- y
      }
    }
    #Return the output
    Yp
})









