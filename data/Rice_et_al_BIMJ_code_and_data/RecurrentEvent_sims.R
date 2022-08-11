rm(list=ls())
source('smcp_functions.R')
trials <- 1000 # number of simulated data sets
Nvec <- c(200,1000) # sample size
fuvec <- c(4.3,7.6,9.7) # length of follow-up
frailvarvec <- c(0,.1,.45) # frailty variance
run.sims <- TRUE # reproduce results using new simulations? (set to FALSE to save time)


for(i.s in seq_along(Nvec)) {
  naivese <- robustse <- est <- array(dim=c(trials,length(fuvec),length(frailvarvec),4))
  N <- Nvec[i.s]
  
  if(run.sims) {
    for(i.f in seq_along(fuvec)) {
      
      total.followup <- rep(fuvec[i.f],N)
      for(i.v in seq_along(frailvarvec)) {
        for(tr in 1:trials) {
          set.seed(tr)
          sim.data <- array(dim=c(N,100,2))
          sim.df <- data.frame()
          
          # simulating event histories for each individual 
          if(frailvarvec[i.v]==0) frail <- rep(1,N) else frail <- rgamma(N,shape=1/frailvarvec[i.v],rate=1/frailvarvec[i.v])
          for(i in 1:N) {
            x <- 0
            j <- 1
            while(sum(x) < total.followup[i] ) {
              u <- runif(1)
              gt <- try(uniroot(function(x) u-(1-exp(-frail[i]*cumhaz.smcp(x,lambda0=.2,
                                                                           lambda1=.7,
                                                                           omega=2.0, 
                                                                           sigma=.5))),
                                c(0,5e3)),silent=TRUE)
              if(class(gt)!='try-error') {
                x[j] <- gt$root
              } else x[j] <- 1e8
              
              if(sum(x)<total.followup[i]) {
                sim.data[i,j,1] <- x[j]
                sim.data[i,j,2] <- 2
              } else {
                sim.data[i,j,1] <- total.followup[i]-sum(head(x,-1))
                sim.data[i,j,2] <- 3
              }
              j <- j+1
            }
            gaptimes.data <- na.omit(sim.data[i,,])
            sim.df <- rbind(sim.df,data.frame(id=i,
                                              evnum=1:nrow(gaptimes.data),
                                              gaptimes=gaptimes.data[,1],
                                              event.ind=(gaptimes.data[,2]==2)))   
            
          }
          
          theta0 <- rep(0,4)
          names(theta0) <- paste('log.',c('lambda0','lambda1','omega','sigma'),sep='')
          # suppressing warnings due to occasional negative arguments to log() etc. during optimization
          fit0 <- suppressWarnings(try(optim(theta0,smcp.llik,
                                             hessian=TRUE,
                                             timebt=sim.df$gaptimes,event=sim.df$event.ind,
                                             control=list(fnscale=-1,trace=0,REPORT=1),
                                             method='BFGS'),silent=TRUE))
          
          if(class(fit0)!='try-error') {
            
            # suppressing warnings due to nonconvergence issues for some simulation runs
            tab0 <- suppressWarnings(try(get.coeftab(fit0,sim.df,idvar='id',timevar='gaptimes',statusvar='event.ind'),silent=TRUE))
            if(class(tab0)!='try-error') {
              
              if(all(!is.na(tab0$table[,3:4]))) {
                est[tr,i.f,i.v,] <- exp(fit0$par)
                naivese[tr,i.f,i.v,] <- exp(fit0$par)*tab0$table[,3]
                robustse[tr,i.f,i.v,] <- exp(fit0$par)*tab0$table[,4]    
              }
            }
            
          }
          
        }  
      }
      
      
    }
    
    dimnames(robustse) <- dimnames(naivese) <- dimnames(est) <- list(simnum=1:trials,
                                                                     followup.time=fuvec,
                                                                     frailty.variance=frailvarvec,
                                                                     param=paste('log.',c('lambda0','lambda1','omega','sigma'),sep=''))
    
    
    
  }
  
  # summarize results ----
  
  # if not wanting to run full set of simualations, use results files
  if(!run.sims) load(paste('frailty_recurrent_results_N',N,'.RData',sep=''))
  
  avg.est <- apply(est, 2:4, mean, na.rm=TRUE)
  med.est <- apply(est, 2:4, median, na.rm=TRUE)
  esd <- apply(est, 2:4, sd, na.rm=TRUE)
  mbse <- apply(naivese,2:4,mean,na.rm=TRUE)
  robustse <- apply(robustse,2:4,mean,na.rm=TRUE)
  num.fails <- apply(est,2:4,function(x) sum(is.na(x)))[,,1]
  
  paramnames <- c('$\\lambda_0$','$\\lambda_1$','$\\omega$','$\\sigma$')
  
  recur.simtab <- data.frame(fu.time = rep(dimnames(avg.est)[[1]], each=prod(unlist(lapply(dimnames(avg.est),length)[c(1,3)]))),
                             frailty = rep(dimnames(avg.est)[[2]], each=unlist(lapply(dimnames(avg.est),length)[c(1,3)])[2],
                                           times=unlist(lapply(dimnames(avg.est),length)[c(1,3)])[1]),
                             param=rep(paramnames,prod(unlist(lapply(dimnames(avg.est),length)[c(1,2)]))),
                             avg=rbind(matrix(t(avg.est[dimnames(avg.est)[[1]][1],,]),ncol=1),
                                       matrix(t(avg.est[dimnames(avg.est)[[1]][2],,]),ncol=1),
                                       matrix(t(avg.est[dimnames(avg.est)[[1]][3],,]),ncol=1)),
                             med=rbind(matrix(t(med.est[dimnames(avg.est)[[1]][1],,]),ncol=1),
                                       matrix(t(med.est[dimnames(avg.est)[[1]][2],,]),ncol=1),
                                       matrix(t(med.est[dimnames(avg.est)[[1]][3],,]),ncol=1)),
                             esd=rbind(matrix(t(esd[dimnames(avg.est)[[1]][1],,]),ncol=1),
                                       matrix(t(esd[dimnames(avg.est)[[1]][2],,]),ncol=1),
                                       matrix(t(esd[dimnames(avg.est)[[1]][3],,]),ncol=1)),
                             mbse=rbind(matrix(t(mbse[dimnames(avg.est)[[1]][1],,]),ncol=1),
                                        matrix(t(mbse[dimnames(avg.est)[[1]][2],,]),ncol=1),
                                        matrix(t(mbse[dimnames(avg.est)[[1]][3],,]),ncol=1)),
                             robustse=rbind(matrix(t(robustse[dimnames(avg.est)[[1]][1],,]),ncol=1),
                                            matrix(t(robustse[dimnames(avg.est)[[1]][2],,]),ncol=1),
                                            matrix(t(robustse[dimnames(avg.est)[[1]][3],,]),ncol=1)),
                             num.fails=rep(t(num.fails)/1000*100, each=4)
  )
  
  
  recur.simtab[setdiff(1:nrow(recur.simtab),seq(1,nrow(recur.simtab),by=4)),c(1:2,9)] <- ''
  
  colnames(recur.simtab) <- paste('\\textbf{',c('Follow-up time',
                                                'Frailty variance',
                                                'Parameter',
                                                'Mean',
                                                'Median',
                                                'ESD',
                                                'ANSE',
                                                'ARSE',
                                                'Fails'),'}',sep='')
  
  print(xtable(recur.simtab,digits=3),
        sanitize.text.function=identity,
        sanitize.colnames.function=identity,
        include.rownames=FALSE,
        file=paste('frailty_recurrent_simulation_results_N',N,'.tex',sep=''))
  
}

