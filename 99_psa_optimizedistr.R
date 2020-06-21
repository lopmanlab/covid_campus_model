## Find alpha and beta parameter for beta or gamma distribution based on mean and 95% credible intervals

############################################
###Get distribution param for gammas #######
#############################################
getdistr_parms <- function(int.quantiles, int.mean,starting.params,distrib){

    objective.function <- function(params) {
          alpha <- params[1]
          beta <- params[2]
          if (distrib == "beta"){
            calculated.quantiles <- qbeta(p=c(0.025, 0.975), shape1=alpha, shape2=beta)
          }  else if (distrib == "gamma"){
            calculated.quantiles <- qgamma(p=c(0.025, 0.975), shape=alpha, rate=beta)
          }
          calculate.mean <- function(alpha, beta) {
            return(qbeta(p=0.5, shape1=alpha, shape2=beta))
          }
          squared.error.quantiles <- sum((int.quantiles - calculated.quantiles)^2)
          calculated.mode <- calculate.mean(alpha, beta)
          squared.error.mode <- (int.mean - calculated.mode)^2
         return(squared.error.quantiles + squared.error.mode)
}

      nlm.result <- nlm(f = objective.function, p = starting.params)
      nlm.result$estimate

}

