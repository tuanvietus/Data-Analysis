#Regular passengers
#180 is in minutes = 3 hours

xbarR <- 1:180
nR <- 180
#CALCULATE RATES (people per minute) AT DIFFERENT MINUTE TIME STEP FOR 3 HOURS
#arrival
alphaR <- rpois(nR, 12.9)
alpha_rateR <- convertToRate(alphaR)

#id check
betaR <- rnorm(nR, 11.2, 3.8)
beta_rateR <- convertToRate(betaR)

#xray
xR <- runif(nR, 0 , 1)

deltaR <- c()
delta_rateR <- c()
stochasticR <- function(xR, deltaR){
  for (i in xbarR){
    if (xR[i] <= 4/11){
      deltaR[i] <<- rnorm(1, 15.6, 7.6)
    } else {
      deltaR[i] <<- rnorm(1, 2.2, 0.5)
    }
  }
}
stochasticR(xR, deltaR)
deltaR <- replace(deltaR, deltaR <= 0, 0)
delta_rateR <- convertToRate(deltaR)
delta_rateR <- replace(delta_rateR, delta_rateR <= 0, 0)
delta_rateR[!is.finite(delta_rateR)] <- 0

#wavescan
gammaR <- rnorm(nR, 11.6, 5.6)
gammaR <- replace(gammaR, gammaR <= 0, 0)
gamma_rateR <- convertToRate(gammaR)
gamma_rateR <- replace(gamma_rateR, gamma_rateR <= 0, 0)
gamma_rateR[!is.finite(gamma_rateR)] <- 0

#grabbags
psiR <- rnorm(nR, 28, 14)
psiR <- replace(psiR, psiR <= 0, 0)
psi_rateR <- convertToRate(psiR)
psi_rateR <- replace(psi_rateR, psi_rateR <= 0, 0)
psi_rateR[!is.finite(psi_rateR)] <- 0

#extracheck = 2 - NOT YET TOUCHED the 97 and 3 percentage
epsilon97R <- pmin(delta_rateR, gamma_rateR)
epsilon3R <- 60/(pmax(deltaR, gammaR) + 120)

#function to convert sec/person to persons/min
convertToRate <- function(x){
  result <- 1/(x/60)
  return(result)
}

#CALCULATE No of PASSENGERS
AR <- 0
BR <- 0
CR <- 0
#note that people in D are included in B. Thus total pop at a time step = A + B + C + E.
DR <- 0
ER <- 0
popEachMinuteR <- 0

AR[1] <- alpha_rateR[1] - beta_rateR[1]

if (rbinom(1, 1, 0.03) == 0){
  BR[1] <- beta_rateR[1] - epsilon97R[1]
  CR[1] <- epsilon97R[1] - psi_rateR[1]
  #DR[1] <- -0.5
} else {
  BR[1] <- beta_rateR[1] - epsilon3R[1]
  CR[1] <- epsilon3R[1] - psi_rateR[1]
  DR[1] <- epsilon3R[1] - 0.5 
}
ER[1] <- psi_rateR[1]
if (AR[1] <= 0){
  AR[1] <- 0
}
if (BR[1] <= 0){
  BR[1] <- 0
}
if (CR[1] <= 0){
  CR[1] <- 0
}
if (DR[1] <= 0){
  DR[1] <- 0
}
if (ER[1] <= 0){
  ER[1] <- 0
}

popEachMinuteR[1] <- sum(AR[1], BR[1], CR[1], ER[1])


for (i in 2:nR){
  AR[i] <- AR[i-1] + alpha_rateR[i] - beta_rateR[i]
  if (rbinom(1, 1, 0.03) == 0){
    BR[i] <- BR[i-1] + beta_rateR[i] - epsilon97R[i]
    CR[i] <- CR[i-1] + epsilon97R[i] - psi_rateR[i]
    DR[i] <- DR[i-1] - 0.5
  } else {
    BR[i] <- BR[i-1] + beta_rateR[i] - epsilon3R[i]
    CR[i] <- CR[i-1] + epsilon3R[i] - psi_rateR[i]
    DR[i] <- DR[i-1] + epsilon3R[i] - 0.5
  }
  ER[i] <- ER[i-1] + psi_rateR[i]
  
  if (AR[i] <= 0){
    AR[i] <- 0
  }
  if (BR[i] <= 0){
    BR[i] <- 0
  }
  if (CR[i] <= 0){
    CR[i] <- 0
  }
  if (DR[i] <= 0){
    DR[i] <- 0
  }
  if (ER[i] <= 0){
    ER[i] <- 0
  }
  
  popEachMinuteR[i] <- sum(AR[i], BR[i], CR[i], ER[i])
}

plot(xbarR, popEachMinuteR, main = "Cumulative number of regular passengers entered Security Checkpoint in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarR, AR, main = "Number of regular passengers at the ID check queue in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarR, BR, main = "Number of regular passengers at the Xray/Wavescanner queue in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarR, CR, main = "Number of regular passengers queuing to grab their bags in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarR, DR, main = "Number of regular passengers queuing for pat-down inspection in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarR, ER, main = "Number of regular passengers getting through the Security Checkpoint in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers",col=4)

plot(xbarR, popEachMinuteR, type = "l", lwd=2, main="Number of regular passengers at different nodes", xlab = "Time (minute)", ylab="Number of passengers")
lines(xbarR, AR, type = "l", lwd=2, col=2)
lines(xbarR, BR, type = "l", lwd=2, col=3)
lines(xbarR, CR, type = "l", lwd=2, col=4)
lines(xbarR, DR, type = "l", lwd=2, col=5)
lines(xbarR, ER, type = "l", lwd=2, col=6)
stations <- c("Entered","ID Check","Xray/Wavescan","Grab bags","Pat-down","Through")
legend("topleft", legend=stations, lwd=2, col=c(1,2,3,4,5,6))

#NO TITLE GRAPHS
plot(xbarR, popEachMinuteR, xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarR, AR, main="Zone A", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarR, BR, main="Zone B", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarR, CR, main="Zone C", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarR, DR, main="Zone D", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbarR, ER, main="Zone E", xlab = "Time (minute)", ylab = "Number of passengers", col=4)

plot(xbarR, popEachMinuteR, type = "l", lwd=2, xlab = "Time (minute)", ylab="Number of passengers")
lines(xbarR, AR, type = "l", lwd=2, col=2)
lines(xbarR, BR, type = "l", lwd=2, col=3)
lines(xbarR, CR, type = "l", lwd=2, col=4)
lines(xbarR, DR, type = "l", lwd=2, col=5)
lines(xbarR, ER, type = "l", lwd=2, col=6)
stations <- c("Entered","ID Check","Xray/Wavescan","Grab bags","Pat-down","Through")
legend("topleft", legend=stations, lwd=2, col=c(1,2,3,4,5,6))