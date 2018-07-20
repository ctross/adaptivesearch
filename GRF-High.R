################################################################################
# Part 1 - Gaussian Random Field Figures 
         
BIG<-40                                             # Size of array

Xs <- matrix(NA,nrow=BIG,ncol=BIG)                  # Create data arrays
Ys <-matrix(NA,nrow=BIG,ncol=BIG)                   #

for(i in 1:BIG){                                    # Fills arrays with X and Y coordinates
Xs[i,]<- rep(i, BIG)                                #
Ys[,i] <- rep(i, BIG)                               #
  }                                                 #
  
D2<-matrix(NA,nrow=(BIG*BIG),ncol=(BIG*BIG))        # Calculate all pairwise distances
cXs<-c(Xs)                                          #
cYs<-c(Ys)                                          #
 for (i in 1:((BIG*BIG)-1)){
 for (j in (i+1):(BIG*BIG)){
  D2[i,j]<-dist(cXs[i],cXs[j],cYs[i],cYs[j])^2
  D2[j,i] <- D2[i,j];
                       }}

for (i in 1:(BIG*BIG)){
         D2[i,i] <- 0;
                 }


D2<-D2/max(D2)

ell<- 0.1

RHO<-exp(-D2/(ell*ell))                             # Correlation values

RHO <- RHO + diag(ncol(RHO))*0.5

 set.seed(37453)
                                                    
GGG<-matrix(MASS::mvrnorm(1,rep(-0.5,BIG*BIG),RHO), # Simulate from multivariate normal     
            nrow=BIG,ncol=BIG)
            
GGG2<-ceiling(ifelse(GGG>0,GGG,0)*2)

XXX<-matrix(NA,nrow=(BIG),ncol=(BIG))
YYY<-matrix(NA,nrow=(BIG),ncol=(BIG))

for(i in 1:BIG){
 XXX[i,] <- 1:BIG
 YYY[,i] <- 1:BIG
}

ResX<-array(NA,c(BIG,BIG,max(GGG2)))
ResY<-array(NA,c(BIG,BIG,max(GGG2)))

for(i in 1:BIG){
for(j in 1:BIG){
 if(GGG2[i,j]>0){
 for(k in 1:GGG2[i,j]){
ResX[i,j,k] <- XXX[i,j]+runif(1,0,0.85)              # Jitter
ResY[i,j,k] <- YYY[i,j]+runif(1,0,0.85)
}}}}

RESX<-na.omit(c(ResY))
RESY<-na.omit(c(ResX))
RES <- data.frame(X=RESX,Y=RESY)


 ggplot(data=RES,aes(X, Y)) +
   geom_point(colour = "tan3", size = 3, alpha=0.39,shape=16) +
   theme(legend.text=element_text(size=14)) + theme(text = element_text(size=16)) +
   theme(axis.text=element_blank(), axis.ticks=element_blank())
   ggsave("GRF-H.pdf",height=8, width=8)
     
     