ordered.covprof <- expand.grid(genotype=0:2,gender=c('M','F'))[-5,]
covprof <- model.matrix(~as.factor(genotype)+gender,data=ordered.covprof)[,-1] 
covprof.filnames <- paste('SuppFig',3:7)

cov.hazmult <- exp(covprof %*% finalmod$par[c(paste('genotype',1:2,sep=''),'genderF')])

# fig 3: males with 0 copies
# fig 4: males with 1 copy
# fig 5: males with 2 copies
# fig 6: females with 0 copies
# fig 7: females with 2 copies

for(i.c in 1:nrow(cov.hazmult)) {
  pdf(paste(covprof.filnames[i.c],'.pdf',sep=''),
      width = 7, height = 4.5)
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
                   subset=(evnum==gt & genotype1==covprof[i.c,1] & genotype2==covprof[i.c,2] & genderF==covprof[i.c,3])),
           conf.int=FALSE,lwd=2,
           main=paste('Gap time',gt),xlab='Time (years)', col = "black")
      
      
      km.fits[[gt]] <- survfit(Surv(gaptime,event==2)~as.factor(bootsim),
                               data=bootgt.long,
                               subset=(genotype1==covprof[i.c,1] & genotype2==covprof[i.c,2] & genderF==covprof[i.c,3]))
      
      lines(km.fits[[gt]],col='gray',lwd=2)
      
      lines(survfit(Surv(gaptimes,status==2)~1, 
                    data=epic.data.nozero,
                    subset=(evnum==gt & genotype1==covprof[i.c,1] & genotype2==covprof[i.c,2] & genderF==covprof[i.c,3])),conf.int=FALSE,lwd=2)
      curve(surv.smcp(x,lambda0=smch.par['log.lambda0']*cov.hazmult[i.c],
                      lambda1=smch.par['log.lambda1']*cov.hazmult[i.c]*expgamma[gt],
                      omega=smch.par['log.omega'],
                      sigma=smch.par['log.sigma']),add=TRUE,
            lty=2,col="black",lwd=2)
    }
  )
  
  title(ylab = "Survival function",
        xlab = "Time (years)",
        outer = TRUE, line = 1)
  dev.off()
}
