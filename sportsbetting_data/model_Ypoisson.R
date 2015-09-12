# Jags-ExampleScript.R 
# Accompanies the book:
#   Kruschke, J. K. (2014). Doing Bayesian Data Analysis: 
#   A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier.

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Load the functions used below:
source("D:/nrl/DBDA2Eprograms/DBDA2Eprograms/DBDA2E-utilities.R") # Must be in R's current working directory.
require(rjags)               # Must have previously installed package rjags.

fileNameRoot="Jags-NRLModel" # For output file names.
graphFileType = "eps"


#===============================================================================


plotAllDists = function( codaSamples , data , compValMu , # must specify compValMu
                     ropeMu=NULL , 
                     compValSigma=NULL , ropeSigma=NULL , 
                     compValEff=0.0 , ropeEff=NULL ,
                     showCurve=FALSE , pairsPlot=FALSE ,
                     saveName=NULL , saveType="jpg",
                     paramPrefix="att",
                     labels=NULL) {
  # showCurve is TRUE or FALSE and indicates whether the posterior should
  #   be displayed as a histogram (by default) or by an approximate curve.
  # pairsPlot is TRUE or FALSE and indicates whether scatterplots of pairs
  #   of parameters should be displayed.
  #-----------------------------------------------------------------------------
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  Nparams = length(grep(paramPrefix,colnames(mcmcMat)))
  mu = mcmcMat[,"att[1]"]
  sigma = mcmcMat[,"att[2]"]
  #mu = mcmcMat[,"lambda_1[1]"]
  #sigma = mcmcMat[,"lambda_2[1]"]
  #-----------------------------------------------------------------------------
  if ( pairsPlot ) {
    # Plot the parameters pairwise, to see correlations:
    openGraph(width=5,height=5)
    nPtToPlot = 1000
    plotIdx = floor(seq(1,chainLength,by=chainLength/nPtToPlot))
    panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...) {
      usr = par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y))
      txt = format(c(r, 0.123456789), digits=digits)[1]
      txt = paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex=1.25 ) # was cex=cex.cor*r
    }
    pairs( cbind( mu , sigma )[plotIdx,] ,
           labels=c( expression(mu) , 
                     expression(sigma) ) , 
           lower.panel=panel.cor , col="skyblue" )
    if ( !is.null(saveName) ) {
      saveGraph( file=paste(saveName,"PostPairs",sep=""), type=saveType)
    }
  }
  #-----------------------------------------------------------------------------
  # Work out the dimensions of the plot.
  dim <- ceiling(sqrt(Nparams))
  # Set up window and layout:
  openGraph(width=6.0,height=8.0*2.5/5)
  #layout( matrix( c(2,3, 1,4) , nrow=2, byrow=FALSE ) )
  layout( matrix( c(1:Nparams) , nrow=dim, byrow=TRUE ) )
  par( mar=c(3.5,3.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  # Select thinned steps in chain for plotting of posterior predictive curves:
  nCurvesToPlot = 20
  stepIdxVec = seq( 1 , chainLength , floor(chainLength/nCurvesToPlot) )
  # Compute limits for plots of data with posterior pred. distributions
  y = data
  xLim = c( min(y)-0.1*(max(y)-min(y)) , max(y)+0.1*(max(y)-min(y)) )
  xBreaks = seq( xLim[1] , xLim[2] , 
                 length=ceiling((xLim[2]-xLim[1])/(sd(y)/4)) )
  histInfo = hist(y,breaks=xBreaks,plot=FALSE)
  yMax = 1.2 * max( histInfo$density )
  xVec = seq( xLim[1] , xLim[2] , length=501 )
  #-----------------------------------------------------------------------------
  if (FALSE) {
  # Plot data y and smattering of posterior predictive curves:
  histInfo = hist( y , prob=TRUE , xlim=xLim , ylim=c(0,yMax) , breaks=xBreaks,
                   col="red2" , border="white" , xlab="y" , ylab="" , 
                   yaxt="n" , cex.lab=1.5 , main="Data w. Post. Pred." )
  for ( stepIdx in 1:length(stepIdxVec) ) {
    lines(xVec, dnorm( xVec , mu[stepIdxVec[stepIdx]] , 
                       sigma[stepIdxVec[stepIdx]] ) , 
          type="l" , col="skyblue" , lwd=1 )
  }
  text( max(xVec) , yMax , bquote(N==.(length(y))) , adj=c(1.1,1.1) )
  }
  #-----------------------------------------------------------------------------
  for (i in 1:Nparams) {
    param <- paste(paramPrefix, "[", i, "]", sep="")
    mu = mcmcMat[,param]
    if (!is.null(labels)) {
      param <- labels[i]
    }
    #mu = mcmcMat[,"att[3]"]
    histInfo = plotPost( mu , cex.lab = 1.75 , showCurve=showCurve ,
                         compVal=compValMu , ROPE=ropeMu ,
                         xlab=paste(paramPrefix, "[", i, "]", sep=""),#bquote(mu) , 
                         main=param,#paste("Mean") , 
                         col="skyblue" )
  }
  #-----------------------------------------------------------------------------
  if (FALSE) {
  histInfo = plotPost( mu , cex.lab = 1.75 , showCurve=showCurve ,
                       compVal=compValMu , ROPE=ropeMu ,
                       xlab=bquote(mu) , main=paste("Mean") , 
                       col="skyblue" )
  }
  #-----------------------------------------------------------------------------
  if (FALSE) {
  histInfo = plotPost( sigma , cex.lab=1.75 , showCurve=showCurve ,
                       compVal=compValSigma , ROPE=ropeSigma , 
                       xlab=bquote(sigma) , main=paste("Std. Dev.") , 
                       col="skyblue" )
  }
  #-----------------------------------------------------------------------------
  if (FALSE) {
  effectSize = ( mu - compValMu ) / sigma
  histInfo = plotPost( effectSize , compVal=compValEff ,  ROPE=ropeEff ,
                       showCurve=showCurve , 
                       xlab=bquote( ( mu - .(compValMu) ) / sigma ),
                       cex.lab=1.75 , main="Effect Size" ,
                       col="skyblue" )
  }
  #-----------------------------------------------------------------------------  
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"Post",sep=""), type=saveType)
  }
}

#===============================================================================









# Load the data:
#myData = read.csv("C:/Users/Renee/Documents/paul/nrl/sportsbetting_data/scores_2014.csv")
myData = read.csv("D:/nrl/sportsbetting_data/scores_20150726.csv")
# Restrict the amount of data, to make it faster for testing.
myData = myData[1:50,]
y1 = myData$Home.Score
y2 = myData$Away.Score
Ngames = length(y1)
teams = myData$Home.Team
teams = c(teams, myData$Away.Team)
teams = unique(teams)
# Convert the teams to unique numerical values.
home_teams <- c()
away_teams <- c()
for (g in seq(1:nrow(myData))) {
  home_team <- match(myData$Home.Team[g], teams)
  away_team <- match(myData$Away.Team[g], teams)
  home_teams <- c(home_teams, home_team)
  away_teams <- c(away_teams, away_team)
}
home_teams <- as.numeric(myData$Home.Team)
away_teams <- as.numeric(myData$Away.Team)

#teams = myData$Home.Team
#teams = c(teams, myData$Away.Team)
#teams = unique(teams)
#teams = data.frame(teams)
#teams['i'] = 1:length(teams)
#team_numbers = as.numeric(teams)
#myData <- join(myData, teams)

Nteams = length(teams)
dataList = list(    # Put the information into a list.
  y1 = y1,
  y2 = y2,
  Ngames = Ngames,
  Nteams = Nteams,
  hometeam = home_teams,
  awayteam = away_teams
)

modelString = "
model {
  for ( g in 1:Ngames ) {
    y1[g] ~ dpois(lambda_1[g])
    y2[g] ~ dpois(lambda_2[g])

    log(lambda_1[g]) <- intercept + home + att[hometeam[g]] + def[awayteam[g]]
    log(lambda_2[g]) <- intercept + att[awayteam[g]] + def[hometeam[g]]
  }

  # Home team = Cowboys (10)
  # Away team = Titans (5)
  # Home team = Storm (7)
  # Away team = Warriors (8)
  ht <- 7
  at <- 8
  y_pred_1 ~ dpois(lambda_pred_1)
  y_pred_2 ~ dpois(lambda_pred_2)
  log(lambda_pred_1) <- intercept + home + att[ht] + def[at]
  log(lambda_pred_2) <- intercept + att[at] + def[ht]

  # Prior on intercept.
  intercept ~ dnorm(0, 0.0001)

  # Prior on home.
  home ~ dnorm(0, 0.0001)

  for (t in 1:Nteams) {
    att.star[t] ~ dnorm(mu.att, tau.att)
    def.star[t] ~ dnorm(mu.def, tau.def)
    
    att[t] <- att.star[t] - mean(att.star[])
    def[t] <- def.star[t] - mean(def.star[])
  }

  mu.att ~ dnorm(0, 0.0001)
  mu.def ~ dnorm(0, 0.0001)
  tau.att ~ dgamma(0.01, 0.01)
  tau.def ~ dgamma(0.01, 0.01)

  for ( g in 1:Ngames ) {
    ynew_1[g] ~ dpois(lambda_1[g])
    ynew_2[g] ~ dpois(lambda_2[g])
  }
}
"
writeLines( modelString , con="TEMPmodel.txt" )

# Initialize the chains based on MLE of data.
# Option: Use single initial value for all chains:
#  thetaInit = sum(y)/length(y)
#  initsList = list( theta=thetaInit )
# Option: Use function that generates random values for each chain:
#initsList = function() {
#  resampledY = sample( y , replace=TRUE )
#  thetaInit = sum(resampledY)/length(resampledY)
#  thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
#  return( list( theta=thetaInit ) )
#}
mu = mean(y1) 
sigma = sd(y1) 
initsList = list( mu = mu , sigma = sigma )
initsList = NULL
# Run the chains:
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
update( jagsModel , n.iter=500 )
#codaSamples = coda.samples( jagsModel , variable.names=c("theta") ,
#                            n.iter=3334 )
codaSamples = coda.samples( jagsModel , variable.names=c(
  "home", "intercept", "lambda_1", "lambda_2", "att", "def", "y1", "y2", "ynew_1", "ynew_2", "y_pred_1", "y_pred_2") ,
                            n.iter=2000)#3334 )
save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )

# Examine the chains:
# Convergence diagnostics:
#diagMCMC( codaObject=codaSamples , parName="lambda_1[2]" )
diagMCMC( codaObject=codaSamples , parName="home" )
diagMCMC( codaObject=codaSamples , parName="intercept" )
diagMCMC( codaObject=codaSamples , parName="y_pred_1" )
diagMCMC( codaObject=codaSamples , parName="y_pred_2" )
diagMCMC( codaObject=codaSamples , parName="y1[1]" )
diagMCMC( codaObject=codaSamples , parName="y2[1]" )
diagMCMC( codaObject=codaSamples , parName="ynew_1[1]" )
diagMCMC( codaObject=codaSamples , parName="ynew_2[1]" )
diagMCMC( codaObject=codaSamples , parName="y_pred_1" )
diagMCMC( codaObject=codaSamples , parName="y_pred_2" )
#diagMCMC( codaObject=codaSamples , parName="lambda_1[1]" )
#diagMCMC( codaObject=codaSamples , parName="lambda_2[1]" )

saveGraph( file=paste0(fileNameRoot,"LambdaDiag") , type="eps" )
# Posterior descriptives:
openGraph(height=3,width=4)
par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )

yData = myData$Home.Score

# Plot the attack of all teams.
plotAllDists(
          codaSamples , data=yData,#myData , 
          compValMu=0.0,#20.0,#100.0 , 
          ropeMu=NULL,#c(99.0,101.0) ,
          compValSigma=0.0,#NULL,#15.0 , 
          ropeSigma=NULL,#c(14,16) ,
          compValEff=0.0 , 
          ropeEff=c(-0.1,0.1) ,
          pairsPlot=TRUE , 
          showCurve=FALSE ,
          saveName=fileNameRoot , 
          saveType=graphFileType,
          paramPrefix = "att",
          labels=levels(myData$Home.Team))

# Plot the defence of all teams.
plotAllDists(
          codaSamples , data=yData,#myData , 
          compValMu=0.0,#20.0,#100.0 , 
          ropeMu=NULL,#c(99.0,101.0) ,
          compValSigma=0.0,#NULL,#15.0 , 
          ropeSigma=NULL,#c(14,16) ,
          compValEff=0.0 , 
          ropeEff=c(-0.1,0.1) ,
          pairsPlot=TRUE , 
          showCurve=FALSE ,
          saveName=fileNameRoot , 
          saveType=graphFileType,
          paramPrefix = "def",
          labels=levels(myData$Home.Team))

