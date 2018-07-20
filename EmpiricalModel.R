############################################################ Load Libraries
 library(rethinking)
 library(MASS)
 library(maps)
 library(maptools)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 library(xtable)

############################################################ Load Functions
ScaleBeta <- function(X){
 a<-0
 b<-pi
 y<-X
 Samp<-50

 y2 <- (y-a)/(b-a)
 y3<-(y2*(Samp - 1) + 0.5)/Samp
 y3}

rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}
ang.dif <- function(x,y) {min((2 * pi) - abs(x - y), abs(x - y))}

dist<-function(x2a,x1a,y2a,y1a){
   return(sqrt((x2a-x1a)^2 + (y2a-y1a)^2))}

ang<-function(x2,x1,y2,y1){
  Theta<-seq(0,2*pi,length.out=100)
  DB<-cbind(cos(Theta),sin(Theta))
   r <- dist(x2,x1,y2,y1)
   x0<-(x2-x1)/r
   y0<-(y2-y1)/r

   (atan2(y0,x0) + pi )
   }

createScaleBar <- function(lon,lat,distanceLon,distanceLat,distanceLegend, dist.units = "km"){
	# First rectangle
	bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon, dist.units = dist.units, model = "WGS84")

	topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLat, dist.units = dist.units, model = "WGS84")
	rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"], bottomRight[1,"long"], lon),
	lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
	rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)

	# Second rectangle t right of the first rectangle
	bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon*2, dist.units = dist.units, model = "WGS84")
	rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"], bottomRight2[1,"long"], bottomRight2[1,"long"], bottomRight[1,"long"]),
	lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
	rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)

	# Now let's deal with the text
	onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLegend, dist.units = dist.units, model = "WGS84")
	onTop2 <- onTop3 <- onTop
	onTop2[1,"long"] <- bottomRight[1,"long"]
	onTop3[1,"long"] <- bottomRight2[1,"long"]

	legend <- rbind(onTop, onTop2, onTop3)
	legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)), stringsAsFactors = FALSE, row.names = NULL)
	return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}
           scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend, dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill = "black", rec2.colour = "black", legend.colour = "black", legend.size = 3, orientation = TRUE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){
	laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend = distanceLegend, dist.unit = dist.unit)
	# First rectangle
	rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)

	# Second rectangle
	rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)

	# Legend
	scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"], dist.unit, sep=""), x = laScaleBar$legend[,"long"], y = laScaleBar$legend[,"lat"], size = legend.size, colour = legend.colour)

	res <- list(rectangle1, rectangle2, scaleBarLegend)

	if(orientation){# Add an arrow pointing North
		coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, length = arrow.length, distance = arrow.distance, dist.unit = dist.unit)
		arrow <- list(geom_segment(data = coordsArrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coordsArrow$coordsN[1,"x"], y = coordsArrow$coordsN[1,"y"], size = arrow.North.size, colour = "black"))
		res <- c(res, arrow)
	}
	return(res)
}
                                           
############################################################ Prepare Data
 Trip <- dim(StoreHits)[2]  
 MaxTicks<-dim(StoreHits)[1]
    
 Yield <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)
 Distance <-matrix(NA,ncol=max(Trip),nrow=MaxTicks) 
 Ang <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)
 AngDiff <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)

 N<-c()
 for(j in 1:max(Trip)){
 X <- StoreX[,j]
 Y <- StoreY[,j]
 N[j] <- length(X)
 Yieldq<-ifelse(StoreHits[,j]>=1,1,0)
 Yieldq[is.na(Yieldq)]<-0
 Yield[1:N[j],j]<-Yieldq     
 
 Dist<-c()  
 ang1<-c()
 angdif<-c()
  
 for(i in 2:N[j]){
 Dist[i]<- dist(X[i],X[i-1],Y[i],Y[i-1]);
 ang1[i]<- ang(X[i],X[i-1],Y[i],Y[i-1])
 if(i>3){
 angdif[i]<- ang.dif(ang1[i],ang1[i-1])
         }}    
         
 Distance[1:N[j],j]<-Dist
 Ang[1:N[j],j]<-ang1       
 AngDiff[1:N[j],j]<-angdif
 }

 for(i in 1:MaxTicks){
 for(j in 1:max(Trip)){
  Distance[i,j] <- ifelse(Distance[i,j]==0,runif(1,0,0.1),Distance[i,j])
      }}
      
 plot(density((c(na.omit(c(log(Distance)))))) )

 Distance[is.na(Distance)] <- 999999

 Yield <- ifelse(Yield>=1,1,0)
 Yield[is.na(Yield)] <- 999999

 EmptyAngle <-  fitdistr( ScaleBeta(c(na.omit(c(AngDiff)))),start=list(shape1=1,shape2=1),"beta")$estimate

 for(i in 1:MaxTicks){
 for(j in 1:max(Trip)){
  AngDiff[i,j] <- ifelse(is.nan(AngDiff[i,j]), rbeta(1,EmptyAngle[1], EmptyAngle[2])*pi,AngDiff[i,j])
      }}
      
 AngDiff <- ScaleBeta(AngDiff)
 AngDiff[is.na(AngDiff)] <- 999999

 AngDiff <- AngDiff[c(-1,-2,-3),]
 Distance <- Distance[c(-1,-2,-3),]
 Yield <- Yield[c(-1,-2,-3),]
 N <- N-3
 MaxTicks<-MaxTicks-3
 Lags<-10

########################################################## Extract data for Stan    
model_dat=list(
Distance=Distance, 
AngDiff=ScaleBeta(AngDiff),
Yield=Yield, 
N=N,
Lags=Lags,
MaxTrip=max(Trip),
MaxTicks=MaxTicks)

################################################################# Fit Stan Model
model_code_1 <- '
functions{
//######################################### Quick Gaussian Process Cholesky Factor
matrix Quick_GP_L(int SIZE, real COR, real DECAY, real SCALE){
 matrix[SIZE,SIZE] Rho;                       //# Cholesky factor to be made
 real RealSIZE;                               //# Used to normalize distance
 real G;                                      //# Intermediate storage

 RealSIZE = SIZE;                             //# Convert Int to Real

 for(i in 1:(SIZE-1)){
 for(j in (i+1):SIZE){
  G = ((j-i) * (j-i))/(RealSIZE*RealSIZE);    //# Calculate normalized sq distance
  Rho[i,j] = COR * exp(-DECAY * G);           //# Estimate correlations
  Rho[j,i] = Rho[i,j];                        //# Fill other triangle
                      }}

 for (i in 1:SIZE){
  Rho[i,i] = 1;                               //# Fill diagonal
                  }

  Rho = SCALE*cholesky_decompose(Rho);        //# Decompose Rho

return Rho;
}
}

data{
int MaxTrip;
int MaxTicks;
int N[MaxTrip];
int Lags;

real Distance[MaxTicks,MaxTrip];
real AngDiff[MaxTicks,MaxTrip];
int Yield[MaxTicks,MaxTrip];
}

parameters {
real AlphaDist;
real MuBetaDist;
real<lower=0> SDDist; 

real AlphaAngle;
real MuBetaAngle;
real<lower=0> DAngle; 

real<lower=0, upper=1> Rho[2];
real<lower=0> Decay[2];
real<lower=0> Scale[2];

vector[Lags] Delta[2];
}

transformed parameters{
vector[Lags] BetaDist;
vector[Lags] BetaAngle;

BetaDist = MuBetaDist + Delta[1];
BetaAngle = MuBetaAngle + Delta[2];
}


model{
Rho ~ beta(12,2);
Decay ~ normal(0,5);
Scale ~ normal(0,5);

MuBetaDist ~ normal(0,2.5);
MuBetaAngle ~ normal(0,2.5);

Delta[1] ~ multi_normal_cholesky(rep_vector(0,Lags), Quick_GP_L(Lags, Rho[1], Decay[1], Scale[1]));
Delta[2] ~ multi_normal_cholesky(rep_vector(0,Lags), Quick_GP_L(Lags, Rho[2], Decay[2], Scale[2]));

AlphaDist ~ normal(0,5);
SDDist ~ cauchy(0,1);  

AlphaAngle ~ normal(0,5);
DAngle ~ cauchy(0,1);   

for(j in 1:MaxTrip){
{

 vector[N[j]-Lags] PredAngle;
 vector[N[j]-Lags] PredDist;
 vector[N[j]-Lags] Dist;
 vector[N[j]-Lags] AngleDifference;
  
 for(i in (Lags+1):N[j]){
  PredDist[i-Lags] = AlphaDist; 
     for(k in 1:Lags){
    PredDist[i-Lags] = PredDist[i-Lags] + BetaDist[k]*Yield[i-k,j];  
        }} 
        
  for(i in (Lags+1):N[j]){
  PredAngle[i-Lags] = AlphaAngle; 
     for(k in 1:Lags){
    PredAngle[i-Lags] = PredAngle[i-Lags] + BetaAngle[k]*Yield[i-k,j];  
        }}       
              
   for(i in (Lags+1):N[j]){            
Dist[i-Lags] = Distance[i,j];              
AngleDifference[i-Lags] = AngDiff[i,j];
              }
Dist ~ lognormal(PredDist,SDDist);
AngleDifference ~ beta(inv_logit(PredAngle)*DAngle, (1-inv_logit(PredAngle))*DAngle);
          }}    

}

'

 m1 <- stan( model_code=model_code_1, data=model_dat,refresh=1,chains=1, control = list(adapt_delta = 0.9, max_treedepth = 15))
 m1
 windows();plot(m1,pars="BetaDist") 
 windows();plot(m1,pars="BetaAngle") 

############################################################## Plot main results
# Make caterpillar plot
 windows()
 m1a<-rstan::extract(m1,pars="BetaAngle")
 sample_eff<-apply(m1a$BetaAngle,2,quantile,probs=c(0.05,0.5,0.95))
 df_angle<-data.frame(Lags=c(1:10),Group="Heading Change",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,],
                      Real=BetaAngle)
                      
 m1d<-rstan::extract(m1,pars="BetaDist")
 sample_eff<-apply(m1d$BetaDist,2,quantile,probs=c(0.05,0.5,0.95))
 df_dist<-data.frame(Lags=c(1:10),Group="Step-Size",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,],
                      Real=BetaDist)
                      
 df_all<-rbind(df_angle,df_dist)

 ggplot(df_all,aes(x=Lags,y=Median))+geom_point()+geom_point(aes(x=Lags,y=Real),color="red")+
 geom_linerange(aes(ymin=LI,ymax=HI))+facet_wrap(~Group,scales="free")+
 geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
 labs(y="Regression parameters") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) + 
scale_x_continuous(breaks= pretty_breaks())

 ggsave("LagParams.pdf",width=8.5,height=4)
    
