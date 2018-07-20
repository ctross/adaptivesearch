###################################################################################################################################  Risk Sensitive Analysis
################################################### Stephens and Charnov style
                      Rmin <-seq(0.01,0.5,by=0.01)
                       m.Zdif.D<-c()
                       l.Zdif.D<-c()
                       h.Zdif.D<-c()
                       
                       bZ.D<-lZ.D<-aZ.D <-matrix(NA,ncol=steps,nrow=length(Rmin)) 
                       
                    for(i in 1:length(Rmin)){
                       LevyZ <- ((Res.D.h[,3]-Rmin[i])/Res.D.h[,6])
                       if(sum(is.infinite(LevyZ))>0)
                       LevyZ<-LevyZ[-which(is.infinite(LevyZ))]
                       AIBHZ <- ((Res.D.H[,3]-Rmin[i])/Res.D.H[,6])
                       if(sum(is.infinite(AIBHZ))>0)
                       AIBHZ<-AIBHZ[-which(is.infinite(AIBHZ))]
                       
                       lZ.D[i,1:length(LevyZ)] <- pnorm(LevyZ)
                       aZ.D[i,1:length(AIBHZ)] <- pnorm(AIBHZ)


                       m.Zdif.D[i]<- (t.test(pnorm(AIBHZ),pnorm(LevyZ)))$estimate[1]-(t.test(pnorm(AIBHZ),pnorm(LevyZ)))$estimate[2]
                       l.Zdif.D[i]<- (t.test(pnorm(AIBHZ),pnorm(LevyZ)))$conf.int[1] 
                       h.Zdif.D[i]<- (t.test(pnorm(AIBHZ),pnorm(LevyZ)))$conf.int[2] 

                            }
                                                
                       m.Zdif.d<-c()
                       l.Zdif.d<-c()
                       h.Zdif.d<-c()
                       
                       bZ.d<-lZ.d<-aZ.d <-matrix(NA,ncol=steps,nrow=length(Rmin))
                         
                     for(i in 1:length(Rmin)){
                       LevyZ <- ((Res.d.h[,3]-Rmin[i])/Res.d.h[,6])
                       if(sum(is.infinite(LevyZ))>0)
                       LevyZ<-LevyZ[-which(is.infinite(LevyZ))]
                       AIBHZ <- ((Res.d.H[,3]-Rmin[i])/Res.d.H[,6])
                       if(sum(is.infinite(AIBHZ))>0)
                       AIBHZ<-AIBHZ[-which(is.infinite(AIBHZ))]
                       
                       lZ.d[i,1:length(LevyZ)] <- pnorm(LevyZ)
                       aZ.d[i,1:length(AIBHZ)] <- pnorm(AIBHZ)

                       m.Zdif.d[i]<- (t.test(pnorm(AIBHZ),pnorm(LevyZ)))$estimate[1]-(t.test(pnorm(AIBHZ),pnorm(LevyZ)))$estimate[2]
                       l.Zdif.d[i]<- (t.test(pnorm(AIBHZ),pnorm(LevyZ)))$conf.int[1] 
                       h.Zdif.d[i]<- (t.test(pnorm(AIBHZ),pnorm(LevyZ)))$conf.int[2] 
                       
                            }
 
 ############################################### Integral of density under Rmin                
                       Risk.dH <-matrix(NA,ncol=steps,nrow=length(Rmin))  
                       for(j in 1:steps){
                       fx<-density(sample(Res.d.H[,3],replace = TRUE),from=0, to=1.05,bw=0.05)
                     for(i in 1:length(Rmin)){
                     Risk.dH[i,j] <- integrate.xy(fx$x,fx$y,a=0,b=Rmin[i])
                          }}          
                            
                       Risk.dh <-matrix(NA,ncol=steps,nrow=length(Rmin))  
                       for(j in 1:steps){
                       fx<-density(sample(Res.d.h[,3],replace = TRUE),from=0, to=1.05,bw=0.05)
                     for(i in 1:length(Rmin)){
                     Risk.dh[i,j] <- integrate.xy(fx$x,fx$y,a=0,b=Rmin[i])
                          }}              
                 
                       Risk.DH <-matrix(NA,ncol=steps,nrow=length(Rmin))  
                       for(j in 1:steps){
                       fx<-density(sample(Res.D.H[,3],replace = TRUE),from=0, to=1.0,bw=0.05)
                     for(i in 1:length(Rmin)){
                     Risk.DH[i,j] <- integrate.xy(fx$x,fx$y,a=0,b=Rmin[i])
                          }}          
                            
                       Risk.Dh <-matrix(NA,ncol=steps,nrow=length(Rmin))  
                       for(j in 1:steps){
                       fx<-density(sample(Res.D.h[,3],replace = TRUE),from=0, to=1.0,bw=0.05)
                     for(i in 1:length(Rmin)){
                     Risk.Dh[i,j] <- integrate.xy(fx$x,fx$y,a=0,b=Rmin[i])
                          }}  
                          
                          
###############################
zbm<-zbl<-zbh<-zlm<-zll<-zlh<-zam<-zal<-zah<-c()       

   for(i in 1:length(Rmin)){
     zlm[i]<- mean(lZ.d[i,],na.rm=TRUE)
     zll[i]<- HPDI(lZ.d[i,])[1]
     zlh[i]<- HPDI(lZ.d[i,])[2]
     
     zam[i]<- mean(aZ.d[i,],na.rm=TRUE)
     zal[i]<- HPDI(aZ.d[i,])[1]
     zah[i]<- HPDI(aZ.d[i,])[2]
   
   }

predframe <- data.frame(Survival_Probability=c(zam,zlm),Low=c(zal,zll),High=c(zah,zlh),
                        Rmin=Rmin,Type=c(rep("Adaptive",length(Rmin)),rep("Lévy",length(Rmin)))) 
a1<- ggplot(predframe, aes(Rmin, Survival_Probability, colour=Type))+
            geom_path(size=2)+ ggtitle("Non-Destructive")+    ylim(0,1)+
            geom_ribbon(data=predframe,aes(ymin=Low,ymax=High, colour=NULL, fill=Type),alpha=0.3)         
                           
###############################
zbm<-zbl<-zbh<-zlm<-zll<-zlh<-zam<-zal<-zah<-c()    

   for(i in 1:length(Rmin)){
     zlm[i]<- mean(lZ.D[i,],na.rm=TRUE)
     zll[i]<- HPDI(lZ.D[i,])[1]
     zlh[i]<- HPDI(lZ.D[i,])[2]
     
     zam[i]<- mean(aZ.D[i,],na.rm=TRUE)
     zal[i]<- HPDI(aZ.D[i,])[1]
     zah[i]<- HPDI(aZ.D[i,])[2]
   }

predframe <- data.frame(Survival_Probability=c(zam,zlm),Low=c(zal,zll),High=c(zah,zlh),
                        Rmin=Rmin,Type=c(rep("Adaptive",length(Rmin)),rep("Lévy",length(Rmin)))) 
a2 <- ggplot(predframe, aes(Rmin, Survival_Probability, colour=Type))+
            geom_path(size=2)+ ggtitle("Destructive")+  ylim(0,1)+ 
            geom_ribbon(data=predframe,aes(ymin=Low,ymax=High, colour=NULL, fill=Type),alpha=0.3)         
                           
###############################   
            predframe <- data.frame(Difference=c(m.Zdif.D,m.Zdif.d),Low=c(l.Zdif.D,l.Zdif.d),High=c(h.Zdif.D,h.Zdif.d),
                        Rmin=Rmin,Type=c(rep("Destructive \n(Adaptive v. Lévy)",length(Rmin)),rep("Non-Destructive \n(Adaptive v. Lévy)",length(Rmin))))
                                
 a3 <-      ggplot(predframe, aes(Rmin, Difference, colour=Type))+
            geom_path(size=2)+ ggtitle("Difference in Mean")+  
            geom_ribbon(data=predframe,aes(ymin=Low,ymax=High, colour=NULL, fill=Type),alpha=0.3) + scale_color_manual(values=c("#984ea3", "#ff7f00")) +         
            scale_fill_manual(values=c("#984ea3", "#ff7f00"))     
  
AA<-  grid.arrange(a1, a2,a3, nrow = 1)                   
  ggsave("RiskA_Torus.pdf",AA,height=4, width=12)      

################################################################################     

#############################
zbm<-zbl<-zbh<-zlm<-zll<-zlh<-zam<-zal<-zah<-c() 

   for(i in 1:length(Rmin)){
     zlm[i]<- mean(1-Risk.dh[i,],na.rm=TRUE)
     zll[i]<- HPDI(1-Risk.dh[i,])[1]
     zlh[i]<- HPDI(1-Risk.dh[i,])[2]
     
     zam[i]<- mean(1-Risk.dH[i,],na.rm=TRUE)
     zal[i]<- HPDI(1-Risk.dH[i,])[1]
     zah[i]<- HPDI(1-Risk.dH[i,])[2]
     
   }

predframe <- data.frame(Survival_Probability=c(zam,zlm),Low=c(zal,zll),High=c(zah,zlh),
                        Rmin=Rmin,Type=c(rep("Adaptive",length(Rmin)),rep("Lévy",length(Rmin)))) 
a1<- ggplot(predframe, aes(Rmin, Survival_Probability, colour=Type))+
            geom_path(size=2)+ ggtitle("Non-Destructive")+  ylim(0,1)+
            geom_ribbon(data=predframe,aes(ymin=Low,ymax=High, colour=NULL, fill=Type),alpha=0.3)         
                           
###############################
zbm<-zbl<-zbh<-zlm<-zll<-zlh<-zam<-zal<-zah<-c()   

   for(i in 1:length(Rmin)){
     zlm[i]<- mean(1-Risk.Dh[i,],na.rm=TRUE)
     zll[i]<- HPDI(1-Risk.Dh[i,])[1]
     zlh[i]<- HPDI(1-Risk.Dh[i,])[2]
     
     zam[i]<- mean(1-Risk.DH[i,],na.rm=TRUE)
     zal[i]<- HPDI(1-Risk.DH[i,])[1]
     zah[i]<- HPDI(1-Risk.DH[i,])[2]
     
   }

predframe <- data.frame(Survival_Probability=c(zam,zlm),Low=c(zal,zll),High=c(zah,zlh),
                        Rmin=Rmin,Type=c(rep("Adaptive",length(Rmin)),rep("Lévy",length(Rmin)))) 
a2 <- ggplot(predframe, aes(Rmin, Survival_Probability, colour=Type))+
            geom_path(size=2)+ ggtitle("Destructive")+ ylim(0,1)+  
            geom_ribbon(data=predframe,aes(ymin=Low,ymax=High, colour=NULL, fill=Type),alpha=0.3)         
                           
###############################   
            m.Risk.D <- h.Risk.D <- l.Risk.D <- c()
             m.Risk.d <- h.Risk.d <- l.Risk.d <- c()
             
             bm.Risk.D <- bh.Risk.D <- bl.Risk.D <- c()
             bm.Risk.d <- bh.Risk.d <- bl.Risk.d <- c()
             
                 for(i in 1:length(Rmin)){
                     m.Risk.D[i] <- (t.test((1-Risk.DH[i,]),(1-Risk.Dh[i,])))$estimate[1]-(t.test((1-Risk.DH[i,]),(1-Risk.Dh[i,])))$estimate[2]
                     h.Risk.D[i] <- (t.test((1-Risk.DH[i,]),(1-Risk.Dh[i,])))$conf.int[1] 
                     l.Risk.D[i] <- (t.test((1-Risk.DH[i,]),(1-Risk.Dh[i,])))$conf.int[2] 
                     
                     m.Risk.d[i] <- (t.test((1-Risk.dH[i,]),(1-Risk.dh[i,])))$estimate[1]-(t.test((1-Risk.dH[i,]),(1-Risk.dh[i,])))$estimate[2]
                     h.Risk.d[i] <- (t.test((1-Risk.dH[i,]),(1-Risk.dh[i,])))$conf.int[1] 
                     l.Risk.d[i] <- (t.test((1-Risk.dH[i,]),(1-Risk.dh[i,])))$conf.int[2] 
                 
                          }                      
            
                                      
            
                     
                predframe <- data.frame(Difference=c(m.Risk.D,m.Risk.d),Low=c(l.Risk.D,l.Risk.d),High=c(h.Risk.D,h.Risk.d),
                        Rmin=Rmin,Type=c(rep("Destructive \n(Adaptive v. Lévy)",length(Rmin)),rep("Non-Destructive \n(Adaptive v. Lévy)",length(Rmin))))
                                
       a3<-     ggplot(predframe, aes(Rmin, Difference, colour=Type))+
            geom_path(size=2)+ ggtitle("Difference in Mean")+ 
            geom_ribbon(data=predframe,aes(ymin=Low,ymax=High, colour=NULL, fill=Type),alpha=0.3) + scale_color_manual(values=c("#984ea3", "#ff7f00")) +         
            scale_fill_manual(values=c("#984ea3", "#ff7f00"))          
  
AA<-  grid.arrange(a1, a2,a3, nrow = 1)                  
  ggsave("RiskB_Torus.pdf",AA,height=4, width=12) 
                  
  
             
     
