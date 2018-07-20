############################################################################### Then load the results
 load("RES-LN.RData")
 load("RES-LD.RData")
 load("RES-AN.RData")
 load("RES-AD.RData")

############################################################### Now plot results
Xs<- matrix(NA, ncol=14,nrow=200)
for(i in 1:14){
 Xs[,i] <- rep(i,200)
          }

################################################## Distributions from simulation
sample_eff<-apply(RES.L.D,2,quantile,probs=c(0.05,0.5,0.95))
df_ld<-data.frame(Res=c(1:14),Group="Lévy",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])

sample_eff<-apply(RES.L.N,2,quantile,probs=c(0.05,0.5,0.95))
df_ln<-data.frame(Res=c(1:14),Group="Lévy",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])

sample_eff<-apply(RES.A.D,2,quantile,probs=c(0.05,0.5,0.95))
df_ad<-data.frame(Res=c(1:14),Group="Adaptive",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])

sample_eff<-apply(RES.A.N,2,quantile,probs=c(0.05,0.5,0.95))
df_an<-data.frame(Res=c(1:14),Group="Adaptive",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])



####### Ratio of in-patch to total variance for single spatial dimension in each simulation
Rat <-c()

 for(sims in 1:14){
 X_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)
 Y_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)

 N_Patches<-13

 X_Mushrooms<-vector("list",N_Patches)                                          # Location of prey
 Y_Mushrooms<-vector("list",N_Patches)                                          #

 set.seed(123456)                                                               # Reset seed
 N_PerPatch<-rpois(N_Patches, 200)                                              # Choose number of items per patch

 for(i in 1:N_Patches){
  X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, X_spread[sims]))+100  # Make some patches of prey
  Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, Y_spread[sims]))+100  #
   }

  Rat[sims] <-  (X_spread[sims])^2/var(unlist(X_Mushrooms))
      }

############## 
sims<-1
 X_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)
 Y_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)

 N_Patches<-13

 X_Mushrooms<-vector("list",N_Patches)                                          # Location of prey
 Y_Mushrooms<-vector("list",N_Patches)                                          #

 set.seed(123456)                                                               # Reset seed
 N_PerPatch<-rpois(N_Patches, 200)                                              # Choose number of items per patch

 for(i in 1:N_Patches){
  X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, X_spread[sims]))+100  # Make some patches of prey
  Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, Y_spread[sims]))+100  #
   }
 
 RESxy <- data.frame(X=unlist(X_Mushrooms),Y=unlist(Y_Mushrooms))   
   ggplot(data=RESxy,aes(X, Y)) +
   geom_point(colour = "tan3", size = 3, alpha=0.39,shape=16) +
   theme(legend.text=element_text(size=14)) + theme(text = element_text(size=16)) +
    theme(axis.text=element_blank(), axis.ticks=element_blank())
     ggsave("Robust-L.pdf",height=8, width=8)
     
sims<-13
 X_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)
 Y_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)

 N_Patches<-13

 X_Mushrooms<-vector("list",N_Patches)                                          # Location of prey
 Y_Mushrooms<-vector("list",N_Patches)                                          #

 set.seed(123456)                                                               # Reset seed
 N_PerPatch<-rpois(N_Patches, 200)                                              # Choose number of items per patch

 for(i in 1:N_Patches){
  X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, X_spread[sims]))+100  # Make some patches of prey
  Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, Y_spread[sims]))+100  #
   }
 
 RESxy <- data.frame(X=unlist(X_Mushrooms),Y=unlist(Y_Mushrooms))   
   ggplot(data=RESxy,aes(X, Y)) +
   geom_point(colour = "tan3", size = 3, alpha=0.39,shape=16) +
   theme(legend.text=element_text(size=14)) + theme(text = element_text(size=16)) +
    theme(axis.text=element_blank(), axis.ticks=element_blank())
     ggsave("Robust-H.pdf",height=8, width=8)
     
    sims<-5
 X_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)
 Y_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)

 N_Patches<-13

 X_Mushrooms<-vector("list",N_Patches)                                          # Location of prey
 Y_Mushrooms<-vector("list",N_Patches)                                          #

 set.seed(123456)                                                               # Reset seed
 N_PerPatch<-rpois(N_Patches, 200)                                              # Choose number of items per patch

 for(i in 1:N_Patches){
  X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, X_spread[sims]))+100  # Make some patches of prey
  Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, Y_spread[sims]))+100  #
   }
 
 RESxy <- data.frame(X=unlist(X_Mushrooms),Y=unlist(Y_Mushrooms))   
   ggplot(data=RESxy,aes(X, Y)) +
   geom_point(colour = "tan3", size = 3, alpha=0.39,shape=16) +
   theme(legend.text=element_text(size=14)) + theme(text = element_text(size=16)) +
    theme(axis.text=element_blank(), axis.ticks=element_blank())
     ggsave("Robust-M1.pdf",height=8, width=8)
     
        sims<-9
 X_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)
 Y_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)

 N_Patches<-13

 X_Mushrooms<-vector("list",N_Patches)                                          # Location of prey
 Y_Mushrooms<-vector("list",N_Patches)                                          #

 set.seed(123456)                                                               # Reset seed
 N_PerPatch<-rpois(N_Patches, 200)                                              # Choose number of items per patch

 for(i in 1:N_Patches){
  X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, X_spread[sims]))+100  # Make some patches of prey
  Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, Y_spread[sims]))+100  #
   }
 
 RESxy <- data.frame(X=unlist(X_Mushrooms),Y=unlist(Y_Mushrooms))   
   ggplot(data=RESxy,aes(X, Y)) +
   geom_point(colour = "tan3", size = 3, alpha=0.39,shape=16) +
   theme(legend.text=element_text(size=14)) + theme(text = element_text(size=16)) +
    theme(axis.text=element_blank(), axis.ticks=element_blank())
     ggsave("Robust-M2.pdf",height=8, width=8)

################################################################## Plot Results
 df<-rbind(df_ld,df_ad)
 df$Res<-rep(Rat,2)
 gg<-ggplot(df, aes(Res, Median, colour=Group))+
            geom_path(size=2) + ylab("Encounter rate") +  xlab("Ratio of in-patch to total variance in x-dimension")+
            geom_ribbon(data=df,aes(ymin=LI,ymax=HI, colour=NULL, fill=Group),alpha=0.3)
            ggsave("SimsD.pdf",gg,height=6, width=8)

 df<-rbind(df_ln,df_an)
 df$Res<-rep(Rat,2)
 gg2<-ggplot(df, aes(Res, Median, colour=Group))+
            geom_path(size=2) + ylab("Encounter rate") +  xlab("Ratio of in-patch to total variance in x-dimension")+
            geom_ribbon(data=df,aes(ymin=LI,ymax=HI, colour=NULL, fill=Group),alpha=0.3)
            ggsave("SimsN.pdf",gg2,height=6, width=8)


################################################################ Look at difference in means

T.N <-matrix(NA,ncol=3, nrow=14)
 for( i in 1:14){
 TT<-t.test( RES.A.N[,i], RES.L.N[,i])
 T.N[i,]<-c(TT$conf.int[1],TT$estimate[1]-TT$estimate[2], TT$conf.int[2])
         }

T.D <-matrix(NA,ncol=3, nrow=14)
 for( i in 1:14){
 TT<-t.test( RES.A.D[,i], RES.L.D[,i])
 T.D[i,]<-c(TT$conf.int[1],TT$estimate[1]-TT$estimate[2], TT$conf.int[2])
         }

 predframe <- data.frame(Difference=c(T.D[,2],T.N[,2]),Low=c(T.D[,1],T.N[,1]),High=c(T.D[,3],T.N[,3]),
                        Rmin=(Rat),Type=c(rep("Destructive",length(T.D[,2])),rep("Non-Destructive",length(T.N[,2]))))

gg3 <- ggplot(predframe, aes(Rmin, Difference, colour=Type))+
            geom_path(size=2)+ ylab("Mean difference in encounter rate") +  xlab("Ratio of in-patch to total variance in x-dimension")+
            geom_ribbon(data=predframe,aes(ymin=Low,ymax=High, colour=NULL, fill=Type),alpha=0.3)  + scale_color_manual(values=c("#984ea3", "#ff7f00")) +         
            scale_fill_manual(values=c("#984ea3", "#ff7f00"))          
             ggsave("SimsDiff.pdf",gg3,height=6, width=8)
