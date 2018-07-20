                                                                    
################# Example 1
Thresh<-Thresh                                                                  # Threshold of visual range
Lags<-Lags                                                                      # Lags at which there are effects of encounters on movement
steps<-Steps                                                                    # Length of simulation
Reps <-200                                                                      # Number of replications
N_Patches<-13                                                                   # Number of patches in environment. Note that we replace the GRF model from
                                                                                # above with separate bivariate normals, due to the infeasible computational
                                                                                # time required to simulate from a GRF on this grid
                                                    
X_Mushrooms<-vector("list",N_Patches)                                           # Location of prey
Y_Mushrooms<-vector("list",N_Patches)                                           #

set.seed(123456)                                                                # Set Seed

################################################################################ Create some prey
N_PerPatch<-rpois(N_Patches, 200)
for(i in 1:N_Patches){
X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, 350))+100
Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, 350))+100
}

X_Mushrooms_per_step<-vector("list",steps)                                      # Location of prey
Y_Mushrooms_per_step<-vector("list",steps)                                      #

for(i in 1:steps){
 X_Mushrooms_per_step[[i]]<-X_Mushrooms
 Y_Mushrooms_per_step[[i]]<-Y_Mushrooms
}

################################################################################ Adaptive model parameters
AlphaDist <- 3.7                                                                     
AlphaAngle <- -0.5                                                                   
                                                                                     
BetaDist  <- c(-1.2, -1, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1)  
                                                                                     
BetaAngle <- c(0.37, 0.33, 0.325, 0.2, 0.17, 0.15, 0.13, 0.11, 0.09, 0.08)           
                                                                                     
SDDist <- 0.35                                                                          
DAngle <- 1.5   

################################################################################ Start Model
MeanSpeed<-SDSpeed<-MeanDHeading<-DDHeading<-SDHits<-MeanHits<-c()############## Create storage

StoreSpeed  <- matrix(NA,ncol=Reps,nrow=steps-1)
StoreAngDiff <- matrix(NA,ncol=Reps,nrow=steps-1)
StoreHits <- matrix(NA,ncol=Reps,nrow=steps-1)

for(q in 1:Reps){                                                               # Run the simulation several times
set.seed(123456)                                                                # Reset seed

N_PerPatch<-rpois(N_Patches, 200)                                               # Choose number of items per patch

for(i in 1:N_Patches){
X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, 350))+100  # Make some patches of prey
Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, 350))+100  #
}

X_Mushrooms_per_step<-vector("list",steps)                                      # Location of prey
Y_Mushrooms_per_step<-vector("list",steps)                                      #
                                                                                #
for(i in 1:steps){                                                              #
 X_Mushrooms_per_step[[i]]<-X_Mushrooms                                         #
 Y_Mushrooms_per_step[[i]]<-Y_Mushrooms                                         #
}                                                                               #
    
loc.x<-c()                                                                      # Location of forager
loc.y<-c()                                                                      #

speed<-c()                                                                      # Speed or step size
heading<-c()                                                                    # Absolute heading
d.heading<-c()                                                                  # Heading change

Hits<-c()                                                                       # Binary vector of encounters

loc.x[1:Lags]<-rep(0,Lags)                                                      # Intialize vectors
loc.y[1:Lags]<-rep(0,Lags)                                                      #
heading[1:Lags]<-rep(0,Lags)                                                    #
speed[1:Lags]<-rep(0,Lags)                                                      #
Hits[1:Lags]<-rep(0,Lags)                                                       #

plot(loc.x,loc.y,typ="l",ylim=c(-3500,3500),xlim=c(-3500,3500))                 # Plot prey items
 for(i in 1:N_Patches){                                                         #
  points(X_Mushrooms[[i]],Y_Mushrooms[[i]], col="red",pch=".")                  #
   }                                                                            #

################################################################################ Now model forager movement
set.seed(q*100)

for(s in (Lags+1): (steps-1)){
 X_Mushrooms <- X_Mushrooms_per_step[[s]]
 Y_Mushrooms <- Y_Mushrooms_per_step[[s]]

 PredDist <- AlphaDist;                                                         # First calculate mean prediction conditional on encounters
for(k in 1:Lags){                                                               #
 PredDist <- PredDist + BetaDist[k]*ifelse(Hits[s-k]>0,1,0);                                 #
       }                                                                        #
       
 R<- exp(rnorm(1,PredDist,SDDist))*7.5                                          # Then simulate a step distance.
 # Note that the 7.5 just scales the step sizes to better fit the grid.
    
 PredAngle <- AlphaAngle;                                                       # Again calculate mean prediction conditional on encounters
for(k in 1:Lags){                                                               #
 PredAngle <- PredAngle + BetaAngle[k]*ifelse(Hits[s-k]>0,1,0);                               #
        }                                                                       #
                
 Theta<- rbeta(1,inv_logit(PredAngle)*DAngle,                                   # And then simulate a directional change
          (1-inv_logit(PredAngle))*DAngle)*180*ifelse(runif(1,0,1)>0.5,1,-1)    # The 180 shifts from the unit to the maximum absolute distance
                                                                                # The ifelse just chooses a random direction to turn
                                                                             
 heading[s]<-(heading[s-1]+Theta)%%360                                          # Store new heading. Note that the %% is the mod operation to wrap around if needed
 d.heading[s] <- abs(Theta)/180                                                 # Also store just the delta heading
 speed[s] <- R                                                                  # And the speed slash step size
   
 ynew <- R * sin(deg2rad(heading[s]))                                           # Now convert polar to Cartesian to get the offset for a new x and y pair
 xnew <- R * cos(deg2rad(heading[s]))                                           #
   
 loc.x[s]<-loc.x[s-1]+xnew                                                      # Make the new x and y pair
 loc.y[s]<-loc.y[s-1]+ynew                                                      #

############################################################################### Now check for an encounter 
 Scrap2<-c()
 for(i in 1:N_Patches){                                                         # For each patch
  Scrap<-rep(NA,length(X_Mushrooms[[i]]))
 for(j in 1:length(X_Mushrooms[[i]])){                                          # For each mushroom in patch
 Scrap[j]<- dist(loc.x[s],X_Mushrooms[[i]][j],loc.y[s],Y_Mushrooms[[i]][j]);    # Calculate the distance from the forager to the mushroom
 if(Scrap[j]<Thresh){                                                           # If the forager is closer than the visual radius slash encounter threshold
 points(X_Mushrooms[[i]][j],Y_Mushrooms[[i]][j], col="blue",pch=20)             # Plot a hit

       for(allgone in s: (steps-1)){
       X_Mushrooms_per_step[[allgone]][[i]][j]<-99999                           # If this run is for destructive foraging
       X_Mushrooms_per_step[[allgone]][[i]][j]<-99999                           # disappear food for all future timesteps
      }
 }}
 
 Scrap2[i]<- ifelse(sum(ifelse(Scrap<Thresh,1,0))>0,sum(ifelse(Scrap<Thresh,1,0)),0)      # Check for hits, also can replace sum(ifelse(Scrap<Thresh,1,0)) with 1
 }
 
 Hits[s]<-ifelse(sum(Scrap2)==0,0,sum(Scrap2))                                            # If there is a hit, then set encounters to 1
 
 lines(loc.x,loc.y,ylim=c(-2500,2500),xlim=c(-2500,2500))                       # Plot updates to the foragers path
}


 MeanHits[q]<-mean(Hits[(Lags+1):length(Hits)],na.rm=T)                         # For simulation q, calculate mean hits, speed, and delta heading
 MeanSpeed[q]<-mean(log(speed)[(Lags+1):length(Hits)],na.rm=T)

 FF<-fitdistr(d.heading[(Lags+1):length(Hits)],"beta",start=list(shape1=1,shape2=1))$estimate
 
 MeanDHeading[q]<-(FF[1]/sum(FF))
 
 SDHits[q]<-sd(Hits[(Lags+1):length(Hits)],na.rm=T)                             # As well as the dispesion metrics
 SDSpeed[q]<-sd(log(speed)[(Lags+1):length(Hits)],na.rm=T)
 DDHeading[q]<-sum(FF) 
 
 StoreAngDiff[,q] <- d.heading                                                  # And store distributions
 StoreSpeed[,q] <- speed
 StoreHits[,q] <- Hits
   
 print(q)
 }

################################################################################ Store Each Simulation Here
 StoreHits.D.H.E1 <- StoreHits   
 StoreAngDiff.D.H.E1 <- StoreAngDiff
 StoreSpeed.D.H.E1  <- StoreSpeed
                                      
############################################################# Full Distributions  Example 1
################################################################################
DF.DH<-melt(as.data.frame((StoreSpeed.D.H.E1)))
DF.Dh<-melt(as.data.frame((StoreSpeed.D.h)))

DF.DH$Search_Mode<-rep("Adaptive",length(DF.DH$variable))
DF.Dh$Search_Mode<-rep("Lévy",length(DF.Dh$variable))

DF1<-rbind(DF.DH,DF.Dh)

DF1$ID<-paste0(DF1$variable,DF1$Search_Mode)

ggplot(DF1) + geom_line(aes(x = value, group = ID,color=Search_Mode),stat='density',alpha=I(0.25),bw=10) + labs(x = "Step-size",y="Density") +
      geom_hline(yintercept = 0) + theme(legend.text=element_text(size=14)) + theme(text = element_text(size=16)) + guides(colour = guide_legend(override.aes = list(alpha=1,size=2.5))) + xlim(-5,1000)
      ggsave("P-D-S-1.pdf",height=6, width=8)                            
                     
################################################################################
DF.DH<-melt(as.data.frame((StoreAngDiff.D.H.E1)))
DF.Dh<-melt(as.data.frame((StoreAngDiff.D.h)))

DF.DH$Search_Mode<-rep("Adaptive",length(DF.DH$variable))
DF.Dh$Search_Mode<-rep("Lévy",length(DF.Dh$variable))

DF1<-rbind(DF.DH,DF.Dh)

DF1$ID<-paste0(DF1$variable,DF1$Search_Mode)
DF1$value <- DF1$value * (rbinom(length(DF1$value),prob=0.5,size=1)*2-1)


id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=200)

for(i in 1:200){
zz<-na.omit(DF1$value[which(DF1$Search_Mode== "Adaptive" & DF1$variable==names(table(DF1$variable))[i])])
zz <- c(zz, zz+2,zz-2)
densityX[,i] <- density(zz,bw=0.05)$x
densityY[,i] <- density(zz,bw=0.05)$y
id[,i] <- rep(i,512)
               }
               
DFa <-data.frame(Heading=c(densityX),Density=c(densityY),id=c(id),Search_Mode="Adaptive")

id<-densityX<-densityY<-matrix(NA,nrow=512,ncol=200)

for(i in 1:200){
zz<-na.omit(DF1$value[which(DF1$Search_Mode != "Adaptive" & DF1$variable==names(table(DF1$variable))[i])])
zz <- c(zz, zz+2,zz-2)
densityX[,i] <- density(zz,bw=0.05)$x
densityY[,i] <- density(zz,bw=0.05)$y
id[,i] <- rep(i,512)
               }
               
DFl <-data.frame(Heading=c(densityX),Density=c(densityY),id=c(id),Search_Mode="Lévy")

DF<-rbind(DFa,DFl)
DF$group2<-paste0(DF$id,DF$Search_Mode)

#ggplot object
require(ggplot2)
g = ggplot(DF,aes(x=Heading,y=Density))
#Density function
g = g + geom_line(aes(group=group2,color=Search_Mode),alpha=I(0.25))
g = g + ylim(0,max(DF$Density))
g = g + xlim(-1,1)
#polar coordinates
g = g + coord_polar()   + theme(legend.text=element_text(size=14)) + theme(text = element_text(size=16)) + guides(colour = guide_legend(override.aes = list(alpha=1,size=2.5)))
g    
     ggsave("P-D-A-1.pdf",g,height=6, width=8) 
     