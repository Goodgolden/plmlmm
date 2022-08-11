rm(list=ls())
source('smcp_functions.R')

# read in data ----
epic.data <- read.csv('EPIC_Obs_Study_Data.csv')
epic.data$event.ind <- epic.data$status==2
epic.data$sex <- ifelse(epic.data$genderF==1,'female','male')
epic.data$num.f508del <- with(epic.data,ifelse(genotype1==0 & genotype2==0,0,
                                               ifelse(genotype1==1 & genotype2==0,1,
                                                      ifelse(genotype1==0 & genotype2==1,2,NA))))
epic.data$total.pex <- with(epic.data,cut(Freq,breaks=c(0:4,Inf),labels=c(1:4,'5+')))
epic.data$evnum.max5 <- with(epic.data,cut(evnum,breaks=c(0:4,Inf),labels=c(1:4,'5+')))

# remove 0 gap times
epic.data.nozero <- subset(epic.data,gaptimes>0)
unique.pts <- subset(epic.data.nozero,!duplicated(id))

# table 1:demographics ----
render.mediqr <- function(x) {
  with(stats.default(x), 
       sprintf("%0.2f (%0.1f)", MEDIAN, IQR))
}

unique.pts$num.f508del <- as.factor(unique.pts$num.f508del)
table1::label(unique.pts$sex) <- 'Sex'
table1::label(unique.pts$num.f508del) <- 'Number of copies of F508del'
table1::label(unique.pts$total.pex) <- 'PEx recorded'
table1::label(unique.pts$VisitAge) <- 'Age at first PEx'
table1::label(unique.pts$max_age) <- 'Age at final visit'
table1::label(unique.pts$followup) <- 'Follow-up time'
table1::label(unique.pts$Freq) <- 'Total number of PEx'

table1::units(unique.pts$VisitAge) <- table1::units(unique.pts$max_age) <- table1::units(unique.pts$followup) <- 'years'

gaptime.lengthtab <- aggregate(gaptimes ~ evnum.max5, FUN=render.mediqr,data=epic.data.nozero)
gaptime.lengthtab[,1] <- paste('after ',gaptime.lengthtab[,1],c('st','nd','rd','th','th'),' PEx',sep='')

# left side of table 1
capture.output(kable(table1(~sex+num.f508del+total.pex+VisitAge+max_age+followup+Freq,
                            render.continuous=render.mediqr,
                            data=unique.pts),format='latex',booktabs=TRUE),
               file='table1.tex')
# right side of table 1
capture.output(kable(gaptime.lengthtab,
                     format='latex',booktabs=TRUE),
               file='table1.tex',append=TRUE)


# figure 1: KM curves by gap time ----
pdf('fig1repro.pdf',height=7, width=7)
par(mfrow=c(1,1))
plot(survfit(Surv(gaptimes,Freq!=evnum)~evnum,
             data=epic.data.nozero,
             subset=(evnum<=3)),
     lty=1:3,lwd=2,
     xlab='Years since previous exacerbation',ylab='Survival function')
legend(x='topright',inset=.025,legend=paste('Gap time',1:3),lwd=2,lty=1:3)
dev.off()


# model fitting ----

Jvec <- 4:15
trend.tabs <- list()
system.time(
  for(i.j in seq_along(Jvec)) trend.tabs[[i.j]] <- get.trendmod(J=Jvec[i.j],dat=epic.data.nozero)
)

# figure 2: QIC plot ----
pdf('fig2repro.pdf',height=5,width=6)
par(mfrow=c(1,1))
plot(Jvec-1,unlist(lapply(trend.tabs,function(tab) tab$qic)),
     xlab='number of trend parameters',ylab='QIC')
dev.off()

optJ <- 11
best.mod <- trend.tabs[[which(Jvec==optJ)]]$table

smcp.trend.est <- do.call(rbind,lapply(trend.tabs,function(fit) head(fit$table[,1],4)))
smcp.trend.se <- do.call(rbind,lapply(trend.tabs,function(fit) head(fit$table[,4],4)))
smcp.parnames <- colnames(smcp.trend.est)


# supplemental table 2 ----
supptab1 <- lapply(trend.tabs,function(tab) apply(tab$table[,c('exp.est','robust.lcl','robust.ucl')],1,
                                                  function(x) c(sprintf('%1$1.3f',x[1]), sprintf('(%1$1.3f, %2$1.3f)',x[2],x[3]))))


out.supptab2 <- t(rbind.fill.matrix(supptab1))
rownames(out.supptab2) <- c('$\\lambda_0$','$\\lambda_1$','$\\omega$','$\\sigma$',
                            paste('$\\exp(\\gamma_{',1:14,'})$',sep=''))
print(xtable(out.supptab2[1:(optJ-1+4),c(1:2,5:6,9:10,15:16)]),
      sanitize.rownames.function=identity,file='Supplement_table2.tex') 

# final model ----
epic.data.nozero[,paste('trend',optJ,sep='')] <- get.trendmat(J=optJ,dat=epic.data.nozero)
# smoothed CP, sex and genotype covariates and 10 trend parameters
theta0 <- rep(0,7+optJ-1)
names(theta0) <- c(paste('log.',c('lambda0','lambda1','omega','sigma'),sep=''),
                   paste('gamma',1:(optJ-1),sep=''),
                   c(paste('genotype',1:2,sep=''),'genderF'))
system.time(
  finalmod <- optim(theta0,smcp.llik,
                    xmat.trend=get.trendmat(J=optJ,dat=epic.data.nozero),
                    xmat=epic.data.nozero[,c(paste('genotype',1:2,sep=''),'genderF')],
                    timebt=epic.data.nozero$gaptimes,event=epic.data.nozero$event.ind,
                    hessian=TRUE,
                    control=list(fnscale=-1,trace=0,REPORT=1),method='BFGS')
)

system.time(
  finalmod.tab <- get.coeftab(mod=finalmod,dat=epic.data.nozero,idvar='id',timevar='gaptimes',statusvar='event.ind',
                              trend.covars=paste('trend',optJ,sep=''),rate.covars=c(paste('genotype',1:2,sep=''),'genderF'))
)

finalmod.esttable <- finalmod.tab$table[,c(2,6,9:10)]
rownames(finalmod.esttable) <- c('$\\lambda_0$','$\\lambda_1$','$\\omega$','$\\sigma$',
                                 paste('$\\exp(\\gamma_',1:10,')$',sep=''),
                                 paste('$\\exp(\\beta_{',c('G1','G2','F'),'})$',sep=''))

print(xtable(finalmod.esttable,digits=3),
      sanitize.rownames.function=identity,file='Table4.tex')


# figure 3: parametric bootstrap ----

vcv <- finalmod.tab$robust.vcov

smch.par <- exp(finalmod$par)
expgamma <- c(1,smch.par[grepl('gamma',names(smch.par))])

futimes <- aggregate(gaptimes ~ id, FUN=function(x) sum(x), data=epic.data.nozero)
colnames(futimes)[2] <- 'total.followup'
fixed.patients <- merge(futimes,unique(epic.data.nozero[,c('id',paste('genotype',1:2,sep=''),'genderF')]),by='id')
m <- nrow(fixed.patients)


B <- 200
set.seed(B)
bootpar <- rmvnorm(B, mean=finalmod$par, sigma=vcv)
epic.data.nozero.boot <- array(dim=c(B,m,6,2))

system.time(
  for(b in 1:B) {
    
    lambda0 <- exp(bootpar[b,'log.lambda0'])
    lambda1 <- exp(bootpar[b,'log.lambda1'])
    patient.hazmult <- exp(as.matrix(fixed.patients[,c(paste('genotype',1:2,sep=''),'genderF')]) %*% 
                             bootpar[b,c(paste('genotype',1:2,sep=''),'genderF')])
    trend.hazmult <- c(1,exp(diag(1,optJ-1) %*%
                               bootpar[b,paste('gamma',1:(optJ-1),sep='')]))
    
    for(i in 1:m) {
      x <- 0
      j <- 1
      while(sum(x) < fixed.patients$total.followup[i] & j<7) {
        u <- runif(1)
        x[j] <- uniroot(function(x) u-(1-surv.smcp(x,lambda0=lambda0*patient.hazmult[i],
                                                   lambda1=lambda1*patient.hazmult[i]*trend.hazmult[j],
                                                   omega=exp(bootpar[b,'log.omega']), 
                                                   sigma=exp(bootpar[b,'log.sigma']))),
                        c(0,2e2))$root
        if(sum(x)<fixed.patients$total.followup[i]) {
          epic.data.nozero.boot[b,i,j,1] <- x[j]
          epic.data.nozero.boot[b,i,j,2] <- 2
        } else {
          epic.data.nozero.boot[b,i,j,1] <- fixed.patients$total.followup[i]-sum(head(x,-1))
          epic.data.nozero.boot[b,i,j,2] <- 3
        }
        j <- j+1
      }
      
    }
    
    
  }
)

pdf('fig3repro.pdf', width = 7, height = 4.5)
op <- par(mfrow = c(2, 3),
          oma = c(2,3,0,0),
          mar = c(2,2,2,2))

km.fits <- list()
system.time(
  for(gt in 1:6) {
    bootgt <- cbind(t(epic.data.nozero.boot[,,gt,1]),t(epic.data.nozero.boot[,,gt,2]))
    bootgt <- as.data.frame(bootgt)
    colnames(bootgt) <- c(paste('gaptime',1:B,sep=''),paste('event',1:B,sep=''))
    bootgt$patient.id <- fixed.patients$id
    bootgt <- data.frame(bootgt,fixed.patients[,c(paste('genotype',1:2,sep=''),'genderF')])
    
    bootgt.long <- reshape(bootgt,direction='long',
                           idvar='patient.id',varying=list(paste('gaptime',1:B,sep=''),paste('event',1:B,sep='')),
                           timevar='bootsim',
                           v.names=c('gaptime','event'))
    plot(survfit(Surv(gaptimes,status==2)~1, 
                 data=epic.data.nozero,
                 subset=(evnum==gt & genderF==1 & genotype1==1)),
         conf.int=FALSE,lwd=2,
         main=paste('Gap time',gt),xlab='Time (years)', col = "black")
    
    
    km.fits[[gt]] <- survfit(Surv(gaptime,event==2)~as.factor(bootsim),
                             data=bootgt.long,
                             subset=(genderF==1 & genotype1==1))
    
    lines(km.fits[[gt]],col='gray',lwd=2)
    
    lines(survfit(Surv(gaptimes,status==2)~1, 
                  data=epic.data.nozero,
                  subset=(evnum==gt & genderF==1 & genotype1==1)),conf.int=FALSE,lwd=2)
    curve(surv.smcp(x,lambda0=smch.par['log.lambda0']*prod(smch.par[c('genotype1','genderF')]),
                    lambda1=smch.par['log.lambda1']*prod(smch.par[c('genotype1','genderF')])*expgamma[gt],
                    omega=smch.par['log.omega'],
                    sigma=smch.par['log.sigma']),add=TRUE,
          lty=2,col="black",lwd=2)
  }
)

title(ylab = "Survival function",
      xlab = "Time (years)",
      outer = TRUE, line = 1)
dev.off()


# supplemental figure 2: hazard functions ----

pdf('hazard_rates.pdf',height=7,width=7)
trend.hazfuns <- lapply(trend.tabs,
                        function(mod) {
                          par.est <- mod$table[,2];
                          lambda0 <- par.est[1];
                          lambda1 <- par.est[2];
                          omega <- par.est[3];
                          sigma <- par.est[4];
                          gamma.est <- c(1,tail(par.est,-4));
                          
                          
                          sapply(gamma.est,function(g) return(function(x) haz.smcp(x,lambda0=lambda0,lambda1=lambda1*g,omega=omega,sigma=sigma)))
                          
                        }
)


timegrid <- seq(0,5,length.out=1001)
trendcol <- rainbow(11,start=3/6,end=4/6)

op <- par(mfrow = c(3, 3),
          oma = c(2,3,0,0),
          mar = c(2,2,2,2))

for(m in 1:(length(trendcol)-3)) {
  plot(timegrid,trend.hazfuns[[m]][[1]](timegrid),type='l',
       ylim=c(0,3),
       lwd=2,col=trendcol[1])
  for(j in 2:(m+3)) lines(timegrid,trend.hazfuns[[m]][[j]](timegrid),type='l',lwd=2,col=trendcol[j])
  
}

plot(0,axes=FALSE,xlab='',ylab='',type='n')
legend(x='center',lwd=2,lty=1,col=trendcol,legend=seq_along(trendcol),title='Gap time')

title(ylab = "Hazard rate function",
      xlab = "Time (years)",
      outer = TRUE, line = 1)
dev.off()

# supplemental figures 3-7 for covariate effects ----
source('supplement_covariate_effects.R')
