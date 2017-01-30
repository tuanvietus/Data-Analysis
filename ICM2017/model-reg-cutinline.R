#Regular passengers, cut-in-line
#180 is in minutes = 3 hours

xbarRt <- 1:180
nRt <- 180
#CALCULATE RATES (people per minute) AT DIFFERENT MINUTE TIME STEP FOR 3 HOURS
#arrival
alphaRt <- rpois(nRt, 12.9)
alphaRt <- replace(alphaRt, alphaRt <= 0, 0)
alpha_rateRt <- convertToRate(alphaRt)
alpha_rateRt <- replace(alpha_rateRt, alpha_rateRt <= 0, 0)
alpha_rateRt[!is.finite(alpha_rateRt)] <- 0

#id check
betaRt <- rnorm(nRt, 11.2, 3.8)
betaRt <- replace(betaRt, betaRt <= 0, 0)
beta_rateRt <- convertToRate(betaRt)
beta_rateRt <- replace(beta_rateRt, beta_rateRt <= 0, 0)
beta_rateRt[!is.finite(beta_rateRt)] <- 0

#xray
xRt <- runif(nRt, 0 , 1)

deltaRt <- c()
delta_rateRt <- c()
stochasticRt <- function(xRt, deltaRt){
  for (i in xbarRt){
    if (xRt[i] <= 4/11){
      deltaRt[i] <<- rnorm(1, 15.6, 7.6)
    } else {
      deltaRt[i] <<- rnorm(1, 2.2, 0.5)
    }
  }
}
stochasticRt(xRt, deltaRt)
deltaRt <- replace(deltaRt, deltaRt <= 0, 0)
delta_rateRt <- convertToRate(deltaRt)
delta_rateRt <- replace(delta_rateRt, delta_rateRt <= 0, 0)
delta_rateRt[!is.finite(delta_rateRt)] <- 0

#wavescan
# gammaRt <- rnorm(nRt, 11.6, 5.6)
# gammaRt <- replace(gammaRt, gammaRt <= 0, 0)
# gamma_rateRt <- convertToRate(gammaRt)
# gamma_rateRt <- replace(gamma_rateRt, gamma_rateRt <= 0, 0)
# gamma_rateRt[!is.finite(gamma_rateRt)] <- 0

#wavescan-cut in line
gammaRt_cut <- pmin(rnorm(180, 11.6, 5.6), rnorm(180, 11.6, 5.6))
gammaRt_cut <- replace(gammaRt_cut, gammaRt_cut <= 0, 0)
gamma_rateRt_cut <- convertToRate(gammaRt_cut)
gamma_rateRt_cut <- replace(gamma_rateRt_cut, gamma_rateRt_cut <= 0, 0)
gamma_rateRt_cut[!is.finite(gamma_rateRt_cut)] <- 0

#grabbags
psiRt <- rnorm(nRt, 28, 14)
psiRt <- replace(psiRt, psiRt <= 0, 0)
psi_rateRt <- convertToRate(psiRt)
psi_rateRt <- replace(psi_rateRt, psi_rateRt <= 0, 0)
psi_rateRt[!is.finite(psi_rateRt)] <- 0

#extracheck = 2 minutes
epsilon97Rt <- pmin(delta_rateRt, gamma_rateRt_cut)
epsilon3Rt <- 60/(pmax(deltaRt, gammaRt_cut) + 120)

#function to convert sec/person to persons/min
convertToRate <- function(x){
  result <- 1/(x/60)
  return(result)
}

#Break into 2 lanes when AR goes over 100, BR into 2 when goes over 50
ARt <- 0
BRt <- 0
CRt <- 0
#note that people in D are included in B. Thus total pop at a time step = A + B + C + E.
DRt <- 0
ERt <- 0
popEachMinuteRt <- 0

ARt[1] <- alpha_rateRt[1] - beta_rateRt[1]

if (rbinom(1, 1, 0.03) == 0){
  BRt[1] <- beta_rateRt[1] - epsilon97Rt[1]
  CRt[1] <- epsilon97Rt[1] - psi_rateRt[1]
  #DRt[1] <- -0.5
} else {
  BRt[1] <- beta_rateRt[1] - epsilon3Rt[1]
  CRt[1] <- epsilon3Rt[1] - psi_rateRt[1]
  DRt[1] <- epsilon3Rt[1] - 0.5 
}
ERt[1] <- psi_rateRt[1]
if (ARt[1] <= 0){
  ARt[1] <- 0
}
if (BRt[1] <= 0){
  BRt[1] <- 0
}
if (CRt[1] <= 0){
  CRt[1] <- 0
}
if (DRt[1] <= 0){
  DRt[1] <- 0
}
if (ERt[1] <= 0){
  ERt[1] <- 0
}

popEachMinuteRt[1] <- sum(ARt[1], BRt[1], CRt[1], ERt[1])


for (i in 2:nRt){
  ARt[i] <- ARt[i-1] + alpha_rateRt[i] - beta_rateRt[i]
  
  if (rbinom(1, 1, 0.03) == 0){
    BRt[i] <- BRt[i-1] + beta_rateRt[i] - epsilon97Rt[i]
    CRt[i] <- CRt[i-1] + epsilon97Rt[i] - psi_rateRt[i]
    DRt[i] <- DRt[i-1] - 0.5
  } else {
    BRt[i] <- BRt[i-1] + beta_rateRt[i] - epsilon3Rt[i]
    CRt[i] <- CRt[i-1] + epsilon3Rt[i] - psi_rateRt[i]
    DRt[i] <- DRt[i-1] + epsilon3Rt[i] - 0.5
  }

  ERt[i] <- ERt[i-1] + psi_rateRt[i]
  
  if (ARt[i] <= 0){
    ARt[i] <- 0
  }
  if (BRt[i] <= 0){
    BRt[i] <- 0
  }
  if (CRt[i] <= 0){
    CRt[i] <- 0
  }
  if (DRt[i] <= 0){
    DRt[i] <- 0
  }
  if (ERt[i] <= 0){
    ERt[i] <- 0
  }
  
  popEachMinuteRt[i] <- sum(ARt[i], BRt[i], CRt[i], ERt[i])
}


plot(xbarRt, popEachMinuteRt, main = "Cumulative number of regular passengers entered Security Checkpoint in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarRt, ARt, main = "Number of regular passengers at the ID check queue in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarRt, BRt, main = "Number of regular passengers at the Xray/Wavescanner queue in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarRt, CRt, main = "Number of regular passengers queuing to grab their bags in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarRt, DRt, main = "Number of regular passengers queuing for pat-down inspection in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarRt, ERt, main = "Number of regular passengers getting through the Security Checkpoint in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers",col=4)

plot(xbarRt, popEachMinuteRt, type = "l", lwd=2, main="Number of regular passengers at different nodes", xlab = "Time (minute)", ylab="Number of passengers")
lines(xbarRt, ARt, type = "l", lwd=2, col=2)
lines(xbarRt, BRt, type = "l", lwd=2, col=3)
lines(xbarRt, CRt, type = "l", lwd=2, col=4)
lines(xbarRt, DRt, type = "l", lwd=2, col=5)
lines(xbarRt, ERt, type = "l", lwd=2, col=6)
stations <- c("Entered","ID Check","Xray/Wavescan","Grab bags","Pat-down","Through")
legend("topleft", legend=stations, lwd=2, col=c(1,2,3,4,5,6))


# #NO TITLE GRAPHS
# plot(xbarR, popEachMinuteR, xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbarR, AR, main="Zone A", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbarR, BR, main="Zone B", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbarR, CR, main="Zone C", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbarR, DR, main="Zone D", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbarR, ER, main="Zone E", xlab = "Time (minute)", ylab = "Number of passengers",col=4)

plot(xbarRt, popEachMinuteRt, type = "l", lwd=2, xlab = "Time (minute)", ylab="Number of passengers")
lines(xbarRt, ARt, type = "l", lwd=2, col=2)
lines(xbarRt, BRt, type = "l", lwd=2, col=3)
lines(xbarRt, CRt, type = "l", lwd=2, col=4)
lines(xbarRt, DRt, type = "l", lwd=2, col=5)
lines(xbarRt, ERt, type = "l", lwd=2, col=6)
stations <- c("Entered","ID Check","Xray/Wavescan","Grab bags","Pat-down","Through")
legend("topleft", legend=stations, lwd=2, col=c(1,2,3,4,5,6))