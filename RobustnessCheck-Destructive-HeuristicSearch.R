################################################################################
################################################################################
# Part4 - Extra Simulation
Thresh<-Thresh                                      # Threshold of visual range
Lags<-10                                            # Lags at which there are effects of encounters on movement
steps<-500                                          # Length of simulation
N_Patches<-13                                       # Number of patches in environment. Note that we replace the GRF model from above with separate
                                                    # bivariate normals, due to increased compuational time with this larger grid
RES <- vector("list",14)

X_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)
Y_spread <- c(100,150,200,250,300,350,400,450,500,550,600,650,700,750)

X_Mushrooms<-vector("list",N_Patches)               # Location of prey
Y_Mushrooms<-vector("list",N_Patches)               #

set.seed(123456)                                    # Reset Seed
N_PerPatch<-rpois(N_Patches, 200)                   # Create some prey
for(i in 1:N_Patches){
X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, 350))+100
Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, 350))+100
}

X_Mushrooms_per_step<-vector("list",steps)               # Location of prey
Y_Mushrooms_per_step<-vector("list",steps)               #

for(i in 1:steps){
X_Mushrooms_per_step[[i]]<-X_Mushrooms
Y_Mushrooms_per_step[[i]]<-Y_Mushrooms
}

################################################################ AIBH parameters     
AlphaDist <- Phi0A
AlphaAngle <- Psi0A

BetaDist  <- PhiA 

BetaAngle <- PsiA

SDDist <- OmegaA
DAngle <- EtaA

#################################################################### Start Model
for(sims in 1:14){
MeanSpeed<-SDSpeed<-MeanDHeading<-DDHeading<-SDHits<-MeanHits<-c()   # Create storage

for(q in 1:200){                                                     # Rep 200 times

set.seed(123456)                                                     # Reset seed

N_PerPatch<-rpois(N_Patches, 200)                                    # Choose number of items per patch

for(i in 1:N_Patches){
X_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, X_spread[sims]))+100  # Make some patches of prey
Y_Mushrooms[[i]]<-rnorm(N_PerPatch[i], runif(1,-2500,2500), rpois(1, Y_spread[sims]))+100  #
}

for(i in 1:steps){
 X_Mushrooms_per_step[[i]]<-X_Mushrooms
 Y_Mushrooms_per_step[[i]]<-Y_Mushrooms
}

loc.x<-c()                                                           # Location of forager
loc.y<-c()                                                           #

speed<-c()                                                           # Speed slash step size
heading<-c()                                                         # Absolute heading
d.heading<-c()                                                       # Heading change

Hits<-c()                                                            # Binary vector of encounters

loc.x[1:Lags]<-rep(0,Lags)                                           # Intialize vectors
loc.y[1:Lags]<-rep(0,Lags)                                           #
heading[1:Lags]<-rep(0,Lags)                                         #
speed[1:Lags]<-rep(0,Lags)                                           #
Hits[1:Lags]<-rep(0,Lags)                                            #

set.seed(q*100)
###################################################################### Now model forager movement
for(s in (Lags+1): (steps-1)){                                       # First calculate mean prediction conditional on encounters
 X_Mushrooms <- X_Mushrooms_per_step[[s]]                            #
 Y_Mushrooms <- Y_Mushrooms_per_step[[s]]                            #
                                                                     #
 PredDist <- AlphaDist;                                              #
for(k in 1:Lags){                                                    #
 PredDist <- PredDist + BetaDist[k]*ifelse(Hits[s-k]>0,1,0);         #
       }                                                             #

 R<- exp(rnorm(1,PredDist,SDDist))*7.5                               # Then simulate a step distance. Note that the 7.5 just scales the step sizes to better fit the grid.

 PredAngle <- AlphaAngle;                                            # Again calculate mean prediction conditional on encounters
for(k in 1:Lags){                                                    #
 PredAngle <- PredAngle + BetaAngle[k]*ifelse(Hits[s-k]>0,1,0);      #
        }                                                            #

 Theta<- rbeta(1,inv_logit(PredAngle)*DAngle,                                # And then simulate a directional change
          (1-inv_logit(PredAngle))*DAngle)*180*ifelse(runif(1,0,1)>0.5,1,-1) # The 180 shifts from the unit to the maximum absolute distance
                                                                             # The ifelse just chooses a random direction

 heading[s]<-(heading[s-1]+Theta)%%360                               # Store new heading. Note that the %% is the mod operation to wrap around if needed
 d.heading[s] <- abs(Theta)/180                                      # Also store just the delta heading
 speed[s] <- R                                                       # And the speed slash step size

 ynew <- R * sin(deg2rad(heading[s]))                                # Now convert polar to Cartesian, to get the offset for a new x and y pair
 xnew <- R * cos(deg2rad(heading[s]))                                #

 loc.x[s]<-loc.x[s-1]+xnew                                           # Make the new x and y pair
 loc.y[s]<-loc.y[s-1]+ynew                                           #

################################################################################ Now check for an encounter
 Scrap2<-c()
 for(i in 1:N_Patches){                                                      # For each patch
  Scrap<-rep(NA,length(X_Mushrooms[[i]]))
 for(j in 1:length(X_Mushrooms[[i]])){                                       # For each mushroom in patch
 Scrap[j]<- dist(loc.x[s],X_Mushrooms[[i]][j],loc.y[s],Y_Mushrooms[[i]][j]); # Calculate the distance from the forager to the mushroom
 if(Scrap[j]<Thresh){                                                        # If the forager is closer than the visual radius slash encounter threshold
                                                                               
       for(allgone in s: (steps-1)){
       X_Mushrooms_per_step[[allgone]][[i]][j]<-99999                                      #2 If this run is for destructive foraging
       X_Mushrooms_per_step[[allgone]][[i]][j]<-99999                                      #2 disappear food for all future timesteps
      }
 }}

 Scrap2[i]<- ifelse(sum(ifelse(Scrap<Thresh,1,0))>0,sum(ifelse(Scrap<Thresh,1,0)),0)      # Check for hits, also can replace sum(ifelse(Scrap<Thresh,1,0)) with 1
 }                                                                             #

 Hits[s]<-ifelse(sum(Scrap2)==0,0,sum(Scrap2))                                            # If there is a hit, then set encounters to 1
}                                                                              #


 MeanHits[q]<-mean(Hits[(Lags+1):length(Hits)],na.rm=T)                        # For simulation q, calculate mean hits, speed, and delta heading
 MeanSpeed[q]<-mean(log(speed)[(Lags+1):length(Hits)],na.rm=T)

 FF<-fitdistr(d.heading[(Lags+1):length(Hits)],"beta",start=list(shape1=1,shape2=1))$estimate

 MeanDHeading[q]<-(FF[1]/sum(FF))

 SDHits[q]<-sd(Hits[(Lags+1):length(Hits)],na.rm=T)                            # As well as the dispesion metrics
 SDSpeed[q]<-sd(log(speed)[(Lags+1):length(Hits)],na.rm=T)
 DDHeading[q]<-sum(FF)
 print(q)
 }

 RES[[sims]]<-data.frame(MeanSpeed,SDSpeed,MeanDHeading,DDHeading,SDHits,MeanHits)
 print(sims)
}

################################################################################ Save each R space from each of the 4 simulations (one at a time)

RES.A.D <- matrix(NA, ncol=14,nrow=200)
for(i in 1:14){
 RES.A.D[,i] <- RES[[i]]$MeanHits
          }
           save(list = "RES.A.D", file = "RES-AD.RData", envir = .GlobalEnv)

