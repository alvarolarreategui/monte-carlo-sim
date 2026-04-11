arrivals <-  function(lambda, NDAYS){
    ### Generate Daily Arrivals
    # In: lambda, rate of Erlang distribution
    #     NDAYS, number of days to simulate 
    # Out: A, (NDAYS) vector of # of Erlang daily arrivals
    
    # generate 4 exponential RVs as row vectors
    u <- matrix(data = runif(NDAYS*4), nrow = 4)   
    x <- -(1/lambda) * log(u, base = exp(1))
    
    # generate daily arrivals (customers) as sum of the 4 expos
    A <- 22 + ceiling(1 + colSums(x))
    return(A)
  }

get_item_prob <-function(){
  # helper function to return the empirical distribution of the demand of each product
  P <- matrix(data = c( 0.333773913, 0.887931549, 0.896517799, 0.807781367, 0.850962972,  
                        0.359006211, 0.034394689, 0.035120148, 0.162315528, 0.098244986, 
                        0.199821118, 0.036262994, 0.034812076, 0.022906832, 0.035418282, 
                        0.062260870, 0.015731521, 0.013793652, 0.004780124, 0.008626001, 
                        0.027627329, 0.012650806, 0.009878163, 0.001361491, 0.003945302, 
                        0.009977640, 0.005565162, 0.004799952, 0.000427329, 0.001659611, 
                        0.004253416, 0.004223561, 0.002872021, 0.000119255, 0.000596267, 
                        0.001768944, 0.001122970, 0.000884463, 0.000049689, 0.000129191, 
                        0.001132919, 0.001659611, 0.000914277, 0.000089441, 0.000308072, 
                        0.000377640, 0.000457137, 0.000407449, 0.000168943, 0.000109316 ),
               byrow = TRUE, 
               ncol = 5)
  colnames(P) <- c('Baguette', 'Croissant', 'PainChoco', 'Coupe', 'Banette') 
  rownames(P) <- c('P(X=0)', 'P(X=1)', 'P(X=2)', 'P(X=3)', 'P(X=4)', 'P(X=5)', 'P(X=6)', 'P(X=7)','P(X=8)','P(X=9)')
  return(P)
}

item_demand <- function(A){
  ### Generate daily demand as an aggregate of items demand across the customers
  # In: A, (NDAYS) vector of # of Erlang daily arrivals
  # Out: DD (NDAYS, nprod) matrix of aggregated demand per day, per product
  PP <- get_item_prob()
  DD <- matrix(data = NA, nrow = length(A), ncol = dim(PP)[2])
  
  for (i in 1:length(A)) {
    # for each product generate a vector of daily aggregate demand 
    for (j in 1:dim(PP)[2]){  
      # for a single day generate a sample of product demand for each customer and aggregate
      temp_x <- sample(x = 0:9, size= A[i], prob = PP[,j], replace = TRUE)
      DD[i,j] <- sum(temp_x)
    }
  }
  colnames(DD) <- c('Baguette', 'Croissant', 'PainChoco', 'Coupe', 'Banette') 
  return(DD)
}

ec_params <- function(){
  # helper function to retrieve the economic params
  EC <- data.frame(mrg = c(0.4, 0.6, 0.6,  0.1,  0.3),
                   cst = c(0.9, 0.5, 0.7, 0.05, 0.85))
  return(EC)
}

row2matrix <- function(x, n){
  # helper fct to create a matrix with vector x replicated n rows
  M <- matrix(x, nrow = 1)
  M <- M[rep(1,n),]
  return(M)
}
excess_inventory <- function(DD, Q){
  # returns the excess inventory if there is, 0 otherwise
  Qm <- row2matrix(Q, nrow(DD))
  EI <- pmax(Qm - DD, 0) 
  return(EI)
}
excess_demand <- function(DD, Q){
  # returns the excess demand if there is, 0 otherwise
  Qm <- row2matrix(Q, nrow(DD))
  ED <- -pmin(Qm - DD, 0)
  return(ED)
}

profit_mat <- function(DD, Q, pct_pen){
  # Compute a matrix of profits by day and by product
  # In: DD, matrix of aggregated demand per day, per product
  #     Q, vector of production policy 
  #     pct_pen, shortage penalty as % of margin
  # Out: TP: (nrpod) vector of total profits for one simulation
  EI <- excess_inventory(DD, Q)
  ED <- excess_demand(DD, Q)
  EC <- ec_params()
  MRG <- row2matrix(EC$mrg, nrow(DD))
  CST <- row2matrix(EC$cst, nrow(DD))
  PEN <- row2matrix(pct_pen*EC$mrg/100, nrow(DD))
  PM <- MRG * pmin(Q,DD) - CST * EI - PEN * ED
  return(PM)
}

plot_optimization <- function(ARR, PCT_PEN, IMAX){
  DD <- item_demand(ARR)
  profit <- matrix(data = 0, nrow = IMAX, ncol = 5)  
  for(i in 1:IMAX ){
    profit[i,1] <- sum(profit_mat(DD, c(i, 0, 0, 0, 0), PCT_PEN/100))
    profit[i,2] <- sum(profit_mat(DD, c(0, i, 0, 0, 0), PCT_PEN/100))
    profit[i,3] <- sum(profit_mat(DD, c(0, 0, i, 0, 0), PCT_PEN/100))
    profit[i,4] <- sum(profit_mat(DD, c(0, 0, 0, i, 0), PCT_PEN/100))
    profit[i,5] <- sum(profit_mat(DD, c(0, 0, 0, 0, i), PCT_PEN/100))
  }
  
  Tprofit <- cbind(profit, rowSums(profit) )
  matplot(Tprofit, type = c("p"), pch = 20, col = 1:6, xlab = '# of items produced', ylab = 'Profit (euros)')
  legend("bottomleft", legend = c('Baguette', 'Croissant', 'PainChoco', 'Coupe', 'Banette', 'TOTAL PROFIT') , col=1:6, pch=20) 
}
optimize_profit <- function(DD, pct_pen, toplot=FALSE){
  ### Find quantities of each product that maximize Total Profit
  # In: DD,  matrix of aggregated demand per day, per product 
  #     ECON: data frame of economic parameters
  # Out: Qx, (5) vector of optimal quantities
  Qmin  <- apply(DD, MARGIN = 2, FUN = min)
  TPo <- colSums(profit_mat(DD, Qmin, pct_pen))
  Qx <- c(0, 0, 0, 0, 0)
  TP <- TPo
  Q <- Qmin
  TPlot <- c()
  for (j in 1:5){
    TP <- colSums(profit_mat(DD, Q, pct_pen))
    TPlot <- append(TPlot, sum(TP))
    Qx[j] <- Q[j]
    while (TP[j] - TPo[j] >= 0){
      Qx[j] <- Q[j]
      Q[j] <- Q[j] + 1
      TPo <- TP
      TP <- colSums(profit_mat(DD, Q, pct_pen))
      TPlot <- append(TPlot, sum(TP))
    }
  }
  if(toplot){
    return(TPlot)
  } else {
    return(Qx)
  }
}

run_sims <- function(lambda, NREP, NDAYS, PEN, SEED){
  ### Run NREP replications of the simulation for NDAYS
  # In: lambda, rate of arrivals (Erlang dist.)
  #     NDAYS, (int) # of days for each simulation
  #     NREP, (int) # of replications of the same simulation
  #     PENAL, (int) shortage penalty in cents
  #     SEED
  # Out: BEST_Q_DF, (NREP) vector of optimal quantities
  set.seed(SEED)
  BEST_Q <- matrix(data = NA, nrow = NREP, ncol = 5)
  BEST_TP <- matrix(data = NA, nrow = NREP, ncol = 1)
  EC <- ec_params()
  for (i in (1:NREP)){
    A <- arrivals(lambda, NDAYS)
    DD <- item_demand(A)
    BEST_Q[i,] <- optimize_profit(DD, PEN)
    BEST_TP[i] <- mean(profit_mat(DD, BEST_Q[i,], PEN))
  }
  BEST_Q <- cbind(BEST_Q, BEST_TP)
  colnames(BEST_Q) <- c('Baguette', 'Croissant', 'PainChoco', 'Coupe', 'Banette', 'AVG_PROFIT')
  BEST_Q <- as_tibble(BEST_Q)
  return(BEST_Q)
}
