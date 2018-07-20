################################################################################
#
# Foraging Simulation Code
#
# Cody T. Ross
#

# This code was run on a Windows machine, if some of the plotting doesnt work, 
# then replace the accented e in Levy with a normal e in the source files:
# EncounterResults.R, ParameterResults.R, RiskAnalysis.R, and ExamplePlots.R

# Prep workstation
 rm(list=ls(all=TRUE))
 setwd('C:\\Users\\cody_ross\\Dropbox\\Completed and Published Projects\\1-papers\\Levy Flights\\JTBsubmission - Revision\\RevisedSoM')
             
# Load libraries
 library(MASS)
 library(mvtnorm)
 library(fields)
 library(ggplot2)
 library(rethinking)  
 library(sfsmisc)
 library(ash)
 library(reshape)
 library(maptools)
 library(gridExtra)
 library(scales)
 library(msm)

# Define functions
 dist2<-function(a,b){return(sqrt( (b[2]-a[2])^2   + (b[1]-a[1])^2 ))}  
 
 dist<-function(x2a,x1a,y2a,y1a){return(sqrt((x2a-x1a)^2 + (y2a-y1a)^2))}  
 
 rad2deg <- function(rad) {(rad * 180) / (pi)}
 
 deg2rad <- function(deg) {(deg * pi) / (180)}
 
 ang.dif <- function(x,y) {min((2 * pi) - abs(x - y), abs(x - y))}
 
 lp_dist <-function(a,b,c){ 
                           # Return distance between a point and line segment 
                           t <- -(((a[1]-c[1])*(b[1]-a[1]) + (a[2]-c[2])*(b[2]-a[2])) / ((b[1]-a[1])^2 + (b[2] - a[2])^2 ))
 
                           if(t >0 & t <1){    
                            numer <- abs(  (b[2]-a[2])*c[1] -(b[1]-a[1])*c[2] + b[1]*a[2] - b[2]*a[1])
                            denom <- sqrt( (b[2]-a[2])^2   + (b[1]-a[1])^2 ) 
                            return(numer/denom)  
                             }
        
                           else{
                            d1 <- dist2(a,c)
                            d2 <- dist2(b,c)   
                            return( min(d1,d2))                              
                             }                                                
                           }
 
# Check some math
 source('CheckDifferenceOfUniformVariatesOnCircle.R')

# Replicate GRF figures
 source('GRF-High.R')
 source('GRF-Low.R')

################################################################################
# Analysis from main paper

################################################################# Set parameters
# Threshold of visual range
Thresh <- 50
 # Adaptive model parameters
Phi0A <- 2.8
Psi0A <- 0
PhiA  <- c(-0.76, -0.65, -0.58, -0.43, -0.29, -0.15, -0.03,0,0,0)
PsiA <- c(0.5, 0.3, 0.25, 0.2, 0.17, 0.15, 0.13, 0.11, 0.09, 0.08)
OmegaA <- 1
EtaA <- 2
 # Levy parameters
Phi0L <- 2.8
Psi0L <- 0
PhiL  <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PsiL <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
OmegaL <- 1
EtaL <- 2

Lags<-10             # Lags at which there are effects of encounters on movement, kinda hardcoded---careful with changes
Steps<-501           # Length of simulation
Reps <- 300          # Number of replications

# Replicate simulations
 source('Simulation-Destructive-HeuristicSearch.R')
 source('Simulation-Destructive-LevySearch.R')

 source('Simulation-NonDestructive-HeuristicSearch.R')
 source('Simulation-NonDestructive-LevySearch.R')

# Encounter plots
 source('EncounterResults_Main.R') 

# Parameter plots
 source('ParameterResults_Main.R')

# Risk plots
 source('RiskAnalysis_Main.R')

# Traceplots
 source('Simulation-Destructive-HeuristicSearch-Trace.R')
 source('Simulation-Destructive-LevySearch-Trace.R')
 source('Simulation-NonDestructive-HeuristicSearch-Trace.R')
 source('Simulation-NonDestructive-LevySearch-Trace.R')

# Examples Simulations
 source('Example1.R')
 source('Example2.R')
 source('Example3.R')
 source('Example3-Trace.R')

################################################################################
# Analysis from main paper but on torus
source('Simulation-Destructive-HeuristicSearch-Reflect.R')
source('Simulation-Destructive-LevySearch-Reflect.R')

source('Simulation-NonDestructive-HeuristicSearch-Reflect.R')
source('Simulation-NonDestructive-LevySearch-Reflect.R')

source('EncounterResults_Reflect.R') 
source('ParameterResults_Reflect.R')
source('RiskAnalysis_Reflect.R')

################################################################################
# Secondary analysis using a more efficient adaptive forager, and checking Brownian search

##### First, reset parameters
# Adaptive model parameters
Phi0A <- 3.2
Psi0A <- 0
PhiA  <- c(-0.76, -0.65, -0.58, -0.43, -0.29, -0.15, -0.03,0,0,0)
PsiA <- c(0.5, 0.3, 0.25, 0.2, 0.17, 0.15, 0.13, 0.11, 0.09, 0.08)
OmegaA <- 0.4
EtaA <- 2
 # Levy parameters
Phi0L <- 2.8
Psi0L <- 0
PhiL  <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PsiL <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
OmegaL <- 1
EtaL <- 2
 # Brownian parameters
Phi0B <- 25
Psi0B <- 0
PhiB  <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PsiB <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
OmegaB <- 15
EtaB <- 2

# Check stepsizes for same mean
mean( (exp(rnorm(100000,3.2,0.4))*7.5))   
mean( (exp(rnorm(100000,2.8,1))*7.5))  
mean( (rtnorm(100000,25,15,0,)*7.5))

# Now, replicate simulations
source('Simulation-Destructive-BrownianSearch.R')
source('Simulation-Destructive-HeuristicSearch.R')
source('Simulation-Destructive-LevySearch.R')

source('Simulation-NonDestructive-BrownianSearch.R')
source('Simulation-NonDestructive-HeuristicSearch.R')
source('Simulation-NonDestructive-LevySearch.R')

source('EncounterResults_Full.R') 
source('ParameterResults_Full.R')
source('RiskAnalysis_Full.R')

################################################################################
# Secondary analysis but on torus
source('Simulation-Destructive-BrownianSearch-Reflect.R')
source('Simulation-Destructive-HeuristicSearch-Reflect.R')
source('Simulation-Destructive-LevySearch-Reflect.R')

source('Simulation-NonDestructive-BrownianSearch-Reflect.R')
source('Simulation-NonDestructive-HeuristicSearch-Reflect.R')
source('Simulation-NonDestructive-LevySearch-Reflect.R')

source('EncounterResults_Reflect_Full.R') 
source('ParameterResults_Reflect_Full.R')
source('RiskAnalysis_Reflect_Full.R')

################################################################################
# Analysis from main paper, but with robustness check on spatial autocorrelation levels
################################################################# Set parameters
# Threshold of visual range
Thresh <- 50
 # Adaptive model parameters
Phi0A <- 2.8
Psi0A <- 0
PhiA  <- c(-0.76, -0.65, -0.58, -0.43, -0.29, -0.15, -0.03,0,0,0)
PsiA <- c(0.5, 0.3, 0.25, 0.2, 0.17, 0.15, 0.13, 0.11, 0.09, 0.08)
OmegaA <- 1
EtaA <- 2
 # Levy parameters
Phi0L <- 2.8
Psi0L <- 0
PhiL  <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PsiL <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
OmegaL <- 1
EtaL <- 2
source('RobustnessCheck-Destructive-HeuristicSearch.R')
source('RobustnessCheck-Destructive-LevySearch.R')
source('RobustnessCheck-NonDestructive-HeuristicSearch.R')
source('RobustnessCheck-NonDestructive-LevySearch.R')

source('RobustnessCheck-Plots.R')

################################################################################
################################################################################
# Emprical Model

# First make a small data-set of encounter-conditional movement sequences
source('Simulation-Destructive-HeuristicSearch-ForEmpirical.R')

# Then fit the emprical model to the simulated data
source('EmpiricalModel.R')

################################################################################
