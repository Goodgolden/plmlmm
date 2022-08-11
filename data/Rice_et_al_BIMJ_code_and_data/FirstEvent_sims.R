rm(list=ls())
source('smcp_functions.R')

n <- 1000 #number of subjects
trials <- 1000 # number of data sets

########################################################################################
# Simulation U1 ("sharp" change-point, where \sigma = 0)
########################################################################################

# set the parameters
lambda0 <- .2
lambda1 <- .7
omega <- 2

est <- array(dim=c(trials,4,2)) 
dimnames(est) <- list(simnum=1:trials,
                      parameter=c('log.lambda0','log.lambda1','log.omega', 'log.sigma'),
                      estimate=c('parameter','standard error'))

  for(tr in 1:trials) {
    set.seed(tr + 1000)
    u <- runif(n)
    y <- numeric()
    for(i in 1:n) {
      y[i] <- uniroot(function(x) ppwexp(x,lambda0=lambda0,lambda1=lambda1,omega=omega) - u[i], c(0,1000))$root
    }
    mod1 <- optim(rep(0,4), logisexp.llik, hessian=F, y=y,control=list(trace=0,maxit=1e4))
    
    if(mod1$conv==0) est[tr,,1] <- mod1$par
  }

#Summaries of simulation
(means_log <- apply(est[,,1],2,mean)) # mean of parameter estimates (on log scale)
(medians_log <- apply(est[,,1],2,median)) # median of parameter estimates (on log scale)
(trueparam_log <- log(c(lambda0,lambda1,omega, 0))) # true parameters (on log scale)

(trueparam_ <- c(lambda0, lambda1, omega, 0)) #true parameters (exponentiated from log scale)
(means <- apply(exp(est[,,1]),2,mean)) # mean of parameter estimates (exponentiated)
(medians <- apply(exp(est[,,1]), 2, median)) #median of parameter estimates (exponetiated)
(bias <- means - trueparam_) #bias = mean - true value

(empse <- apply(exp(est[,,1]), 2, sd, na.rm =T))

res_u1 <- cbind.data.frame(trueparam_, means, medians, empse, bias)
rownames(res_u1) <- c("lambda0", "lambda1", "omega", "sigma")
colnames(res_u1) <- c("Parameter", "Mean", "Median", "Empirical SE", "Bias")

est.u1 <- est

########################################################################################
# Simulation U2, U3, U4 (smoothed change-point with \sigma = 0.2, 0.35, 0.5)
########################################################################################

# set the parameters
lambda0 <- 0.2
lambda1 <- 0.7
omega <- 2
sigma.vec <- c(0.2,.35,.5) #this is the parameter that changes for U2, U3, U4

est <- array(dim=c(trials,4,2,3)) 
dimnames(est) <- list(simnum=1:trials,parameter=c('log.lambda0','log.lambda1','log.omega', 'log.sigma'),
                      estimate=c('parameter','standard error'), sigma.true=c('sig.20', 'sig.35', 'sig.50'))

for(i.s in seq_along(sigma.vec)) {
    for(tr in 1:trials) {
        set.seed(tr)
        u <- runif(n)
        y <- numeric()
        for(i in 1:n) {
            y[i] <- uniroot(function(x) 
                cdf.logisexp(x,lambda0=lambda0,lambda1=lambda1,omega=omega,sigma=sigma.vec[i.s]) - u[i],
                lower = 0, upper = 1, extendInt = "yes")$root
        }
        
        mod1 <- try(optim(rep(0.25,4), logisexp.llik, hessian=T, y=y,control=list(trace=0,maxit=1e4)),
                    silent = T) 
        if((ifelse(length(mod1) > 1, mod1$conv==0 & (sum(diag(solve(mod1$hess))<=0) ==0), print(FALSE) )) ){
          est[tr,,1, i.s] <- mod1$par
          est[tr,,2, i.s] <- sqrt(diag(solve(mod1$hess))) }        
    }
}

## sigma=0.2
est.u2 <- est[,,,1]
res_u2 <- sum.sim(est[,,,1], sigma=sigma.vec[1])

## sigma=0.35
est.u3 <- est[,,,2]
res_u3 <- sum.sim(est[,,,2], sigma=sigma.vec[2])

## sigma=0.5
est.u4 <- est[,,,3]
res_u4 <- sum.sim(est[,,,3], sigma=sigma.vec[3])


########################################################################################
# Simulation U5 (no change-point [exponential distribution])
########################################################################################

# set the parameters
lambda1 <- .7

est <- array(dim=c(trials,4,2)) 
dimnames(est) <- list(simnum=1:trials,
                      parameter=c('log.lambda0','log.lambda1','log.omega', 'log.sigma'),
                      estimate=c('parameter','standard error'))
  for(tr in 1:trials) {
      set.seed(tr)
      u <- runif(n)
      y <- numeric()
      
      for(i in 1:n) {
          y[i] <- uniroot(function(x) cdf.nocp(x,lambda1=lambda1) - u[i],
                          lower = 0, upper = 1, extendInt = "yes")$root
      }
      
      mod1 <- optim(rep(0, 4), logisexp.llik, hessian=F, y=y,control=list(trace=0,maxit=1e4))
      
      if(mod1$conv==0) est[tr,,1] <- mod1$par
  }

#summaries of simulation
(means_log <- c(mean(as.numeric(est[,1,1]), na.rm = T),
                mean(as.numeric(est[,2,1]), na.rm = T),
                mean(as.numeric(est[,3,1]), na.rm = T),
                mean(as.numeric(est[,4,1]), na.rm = T))) # mean of parameter estimates (on log scale)
(medians_log <- c(median(as.numeric(est[,1,1]), na.rm = T),
                  median(as.numeric(est[,2,1]), na.rm = T),
                  median(as.numeric(est[,3,1]), na.rm = T),
                  median(as.numeric(est[,4,1]), na.rm = T))) # median of parameter estimates (on log scale)
(trueparam_log <- c('', log(lambda1), '', '')) # true parameters (on log scale)

(trueparam_ <- c(NA, lambda1, NA, NA)) #true parameters (exponentiated from log scale)
(means <- c(mean(exp(as.numeric(est[,1,1])), na.rm = T),
            mean(exp(as.numeric(est[,2,1])), na.rm = T),
            mean(exp(as.numeric(est[,3,1])), na.rm = T),
            mean(exp(as.numeric(est[,4,1])), na.rm = T))) # mean of parameter estimates (exp)
(medians <- c(median(exp(as.numeric(est[,1,1])), na.rm = T),
              median(exp(as.numeric(est[,2,1])), na.rm = T),
              median(exp(as.numeric(est[,3,1])), na.rm = T),
              median(exp(as.numeric(est[,4,1])), na.rm = T))) # median of parameter estimates (exp)
bias <- NA
for(i in 1:length(trueparam_)){
  bias[i] <- ifelse(is.na(trueparam_[i])==F, means[i] - as.numeric(trueparam_[i]), NA)
}
(empse <- c(sd(exp(as.numeric(est[,1,1])), na.rm = T),
            sd(exp(as.numeric(est[,2,1])), na.rm = T),
            sd(exp(as.numeric(est[,3,1])), na.rm = T),
            sd(exp(as.numeric(est[,4,1])), na.rm = T))) # sd of parameter estimates (exp)
summary(as.numeric(est[,1,2])) #all NA--can't be calculated
summary(as.numeric(est[,2,2])) #all NA--can't be calculated
summary(as.numeric(est[,3,2])) #all NA--can't be calculated
summary(as.numeric(est[,4,2])) #all NA--can't be calculated
se2 <- rep(NA, 4) #can't be calculated

est.u5 <- est
res_u5 <- cbind.data.frame(trueparam_, means, medians, empse, se2, bias)
rownames(res_u5) <- c("lambda0", "lambda1", "omega", "sigma")
colnames(res_u5) <- c("Parameter", "Mean", "Median", "SE1", "SE2", "Bias")
res_u5

########################################################################################
# Simulation C1, C2, C3 
########################################################################################

# set the parameters [censoring rate set below]
lambda0 <- .2
lambda1 <- .7
omega <- 2
sigma <- 0.5

cens.rate <- c(0.05,0.2,0.35)

est <- array(dim=c(trials,4,2,3)) 
dimnames(est) <- list(simnum=1:trials,parameter=c('log.lambda0','log.lambda1','log.omega', 'log.sigma'),
                      estimate=c('parameter','standard error'), cens.rate=c('sig.05', 'sig.20', 'sig.35'))

for(i.c in seq_along(cens.rate)) {
    for(tr in 1:trials) {
        set.seed(tr + 1500)
        u <- runif(n)
        y_ev <- numeric()
        
        for(i in 1:n) {
            y_ev[i] <- uniroot(function(x) cdf.logisexp(x,lambda0=lambda0,lambda1=lambda1,omega=omega,sigma=sigma) - u[i],
                               lower = 0, upper = 1, extendInt = "yes")$root
        }
        y_cens <- rexp(n, rate = cens.rate[i.c]) #rate = 0.05 for 10% cens (sigma = 0.5), 0.20 for 25% cens, 0.35 for 40% cens
        y_obs <- ifelse(y_ev < y_cens, y_ev, y_cens)
        ev <- ifelse(y_ev < y_cens, 1, 0)
        
        y <- cbind.data.frame(y_obs, ev)
        
        mod1 <- try(optim(rep(0,4), logisexp.llik_cens, hessian=T, y=y,control=list(trace=0,maxit=1e4)),
                    silent = T)
    
        se.t <- try(solve(mod1$hess), silent=TRUE)  
        
        if((ifelse(is.numeric(se.t) & (length(mod1) > 1), mod1$conv==0 & (sum(diag(solve(mod1$hess))<=0) ==0),
                   print(FALSE) )) ){    
            est[tr,,1, i.c] <- mod1$par
            est[tr,,2, i.c] <- sqrt(diag(solve(mod1$hess)))}
    }
}
## 10% censoring
est.c1 <- est[,,,1]
res_c1 <- sum.sim(est[,,,1], sigma=sigma)
## 25% censoring
est.c2 <- est[,,,2]
res_c2 <- sum.sim(est[,,,2], sigma=sigma)
## 40% censoring
est.c3 <- est[,,,3]
res_c3 <- sum.sim(est[,,,3], sigma=sigma)

##############################################################################
## calculating non-convergence rates for each scenario
sum(is.na(est.u1[,2,1]))/length(est.u1[,1,1])*100
sum(is.na(est.u2[,2,1]))/length(est.u2[,1,1])*100
sum(is.na(est.u3[,2,1]))/length(est.u3[,1,1])*100
sum(is.na(est.u4[,2,1]))/length(est.u4[,1,1])*100
sum(is.na(est.u5[,2,1]))/length(est.u5[,1,1])*100
sum(is.na(est.c1[,2,1]))/length(est.c1[,1,1])*100
sum(is.na(est.c2[,2,1]))/length(est.c2[,1,1])*100
sum(is.na(est.c3[,2,1]))/length(est.c3[,1,1])*100


#######################################################################################
# Manuscript Table 2: Combine simulation results to report together
#######################################################################################

tab2 <- as.data.frame(matrix(data = '', nrow = 8 * 4, ncol = 8), stringsAsFactors = F)
colnames(tab2) <- c("Simulation", "Parameter.symbol", "Parameter.value", 
                    "Mean", "Median", "Empirical.SE", "Avg..Estimated.SE.", "Bias")
tab2[, 1] <- c("U1", rep('', 3), "U2", rep('', 3), "U3", rep('', 3), 
               "U4", rep('', 3), "U5", rep('', 3), "C1", rep('', 3), 
               "C2", rep('', 3), "C3", rep('', 3))

tab2[, 1] <- c("\\textbf{U1}", rep('', 3), "\\textbf{U2}", rep('', 3), "\\textbf{U3}", rep('', 3), 
               "\\textbf{U4}", rep('', 3), "\\textbf{U5}", rep('', 3), "\\textbf{C1}", rep('', 3), 
               "\\textbf{C2}", rep('', 3), "\\textbf{C3}", rep('', 3))

  
tab2[, 2] <- rep(c("lambda_0", "lambda_1", "omega", "sigma"), 8)

tab2[1:4, 3:8] <- cbind(res_u1[, -5], c(NA,NA,NA,NA), res_u1[, 5])
tab2[5:8, 3:8] <- res_u2
tab2[9:12, 3:8] <- res_u3
tab2[13:16, 3:8] <- res_u4
tab2[17:20, 3:8] <- res_u5
tab2[21:24, 3:8] <- res_c1
tab2[25:28, 3:8] <- res_c2
tab2[29:32, 3:8] <- res_c3

for(i in 4:8){
  tab2[, i] <- round(as.numeric(tab2[, i]), 3)
  tab2[, i] <- ifelse(is.na(tab2[, i]) == T, '*', tab2[, i])
}
tab2

## saving results for figure 1 in supplement
write.csv(tab2, "./Table2_SimulationResults.csv", row.names = F)

## formatting for latex output
tab2[, 2] <- rep(c("$\\lambda_0$", "$\\lambda_1$", "$\\omega$", "$\\sigma$"), 8)

colnames(tab2) <- paste('\\textbf{',c('Simulation',
                                      'Parameter.symbol',
                                      'Parameter.value',
                                      'Mean',
                                      'Median',
                                      'Empirical SE',
                                      'Avg. Estimated SE',
                                      'Bias'),'}',sep='')

### exporting the table in tex
print(xtable(tab2,digits=3),
      sanitize.text.function=identity,
      sanitize.colnames.function=identity,
      include.rownames=FALSE,
      file = "Table2_SimulationResults.tex")


##############################################################################
res <- read.csv('./Table2_SimulationResults.csv')

##--------------------------------------------------------------------------------------
# Figure (supplementary fig. 1) for simulations U1-U5 and C1-C3
##--------------------------------------------------------------------------------------
png('./Fig1_Supp.png', width = 500, height = 1000)
m <- matrix(c(1:8, 9, 9),nrow = 5, ncol = 2, byrow = TRUE)
layout(mat = m, heights = c(0.23, 0.23, 0.23, 0.23, 0.08))

plot_sim(res=res, sim=1, sim.name="U1")
plot_sim(res=res, sim=2, sim.name= "U2")
plot_sim(res=res, sim=3, sim.name= "U3")
plot_sim(res=res, sim=4, sim.name= "U4")
plot_sim(res=res, sim=5, sim.name= "U5")
plot_sim(res=res, sim=6, sim.name= "C1")
plot_sim(res=res, sim=7, sim.name= "C2")
plot_sim(res=res, sim=8, sim.name= "C3")

#add legend
par(mar = c(1, 1, 1, 1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top", inset = 0, legend = c("Data", "Model fit"), 
       lty = 1:2, col = "gray30", horiz = T, bty = 'n', cex = 1.2)
dev.off()


