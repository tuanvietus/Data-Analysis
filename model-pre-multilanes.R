#Precheck passengers, multilanes
#180 is in minutes = 3 hours
xbart <- 1:180
nt <- 180
#CALCULATE RATES (people per minute) AT DIFFERENT MINUTE TIME STEP FOR 3 HOURS
#arrival
alphat <- rpois(nt, 9.2)
alphat <- replace(alphat, alphat <= 0, 0)
alpha_ratet <- convertToRate(alphat)
alpha_ratet <- replace(alpha_ratet, alpha_ratet <= 0, 0)
alpha_ratet[!is.finite(alpha_ratet)] <- 0

#id check
betat <- rnorm(nt, 11.2, 3.8)
betat <- replace(betat, betat <= 0, 0)
beta_ratet <- convertToRate(betat)
beta_ratet <- replace(beta_ratet, beta_ratet <= 0, 0)
beta_ratet[!is.finite(beta_ratet)] <- 0

#xray
xt <- runif(nt, 0 , 1)

deltat <- c()
delta_ratet <- c()
stochastict <- function(xt, deltat){
  for (i in xbart){
    if (xt[i] <= 4/11){
      deltat[i] <<- rnorm(1, 15.6, 7.6)
    } else {
      deltat[i] <<- rnorm(1, 2.2, 0.5)
    }
  }
}
stochastict(xt, deltat)
deltat <- replace(deltat, deltat <= 0, 0)
delta_ratet <- convertToRate(deltat)
delta_ratet <- replace(delta_ratet, delta_ratet <= 0, 0)
delta_ratet[!is.finite(delta_ratet)] <- 0

#full body scan
gammat <- rnorm(n, 11.6, 5.6)
gammat <- replace(gammat, gammat <= 0, 0)
gamma_ratet <- convertToRate(gammat)
gamma_ratet <- replace(gamma_ratet, gamma_ratet <= 0, 0)
gamma_ratet[!is.finite(gamma_ratet)] <- 0

#wavescan-cut in line
# gammat_cut <- pmin(rnorm(180, 11.6, 5.6), rnorm(180, 11.6, 5.6))
# gammat_cut <- replace(gammat_cut, gammat_cut <= 0, 0)
# gamma_ratet_cut <- convertToRate(gammat_cut)
# gamma_ratet_cut <- replace(gamma_ratet_cut, gamma_ratet_cut <= 0, 0)
# gamma_ratet_cut[!is.finite(gamma_ratet_cut)] <- 0

#grabbags
psit <- rnorm(n, 28, 14)
psit <- replace(psit, psit <= 0, 0)
psi_ratet <- convertToRate(psit)
psi_ratet <- replace(psi_ratet, psi_ratet <= 0, 0)
psi_ratet[!is.finite(psi_ratet)] <- 0

#extracheck = 2 - NOT YET TOUCHED the 97 and 3 percentage
epsilon97t <- pmin(delta_ratet, gamma_ratet)
epsilon3t <- 60/(pmax(deltat, gammat) + 120)

#function to convert sec/person to persons/min
convertToRate <- function(x){
  result <- 1/(x/60)
  return(result)
}

#CALCULATE No of PASSENGERS
At <- 0
Bt <- 0
Ct <- 0
#note that people in D are included in B. Thus total pop at a time step = A + B + C + E.
Dt <- 0
Et <- 0
popEachMinutet <- 0

At[1] <- alpha_ratet[1] - beta_ratet[1]

if (rbinom(1, 1, 0.03) == 0){
  Bt[1] <- beta_ratet[1] - epsilon97t[1]
  Ct[1] <- epsilon97t[1] - psi_ratet[1]
  #Dt[1] <- -0.5
} else {
  Bt[1] <- beta_ratet[1] - epsilon3t[1]
  Ct[1] <- epsilon3t[1] - psi_ratet[1]
  Dt[1] <- epsilon3t[1] - 0.5 
}
Et[1] <- psi_ratet[1]

if (At[1] <= 0){
  At[1] <- 0
}
if (Bt[1] <= 0){
  Bt[1] <- 0
}
if (Ct[1] <= 0){
  Ct[1] <- 0
}
if (Dt[1] <= 0){
  Dt[1] <- 0
}
if (Et[1] <= 0){
  Et[1] <- 0
}

popEachMinutet[1] <- sum(At[1], Bt[1], Ct[1], Et[1])


for (i in 2:nt){
  #added for multiple lanes
  At[i] <- 0
  Bt[i] <- 0
  if (At[i] >= 100){
    beta_ratet[i] <- beta_ratet[i]*2
  }
  if (Bt[i] >= 50){
    epsilon97t[i] <- epsilon97t[i]*2
    epsilon3t[i] <- epsilon3t[i]*2
  }
  #===
  At[i] <- At[i-1] + alpha_ratet[i] - beta_ratet[i]
  if (rbinom(1, 1, 0.03) == 0){
    Bt[i] <- Bt[i-1] + beta_ratet[i] - epsilon97t[i]
    Ct[i] <- Ct[i-1] + epsilon97t[i] - psi_ratet[i]
    Dt[i] <- Dt[i-1] - 0.5
  } else {
    Bt[i] <- Bt[i-1] + beta_ratet[i] - epsilon3t[i]
    Ct[i] <- Ct[i-1] + epsilon3t[i] - psi_ratet[i]
    Dt[i] <- Dt[i-1] + epsilon3t[i] - 0.5
  }
  Et[i] <- Et[i-1] + psi_ratet[i]
  
  if (At[i] <= 0){
    At[i] <- 0
  }
  if (Bt[i] <= 0){
    Bt[i] <- 0
  }
  if (Ct[i] <= 0){
    Ct[i] <- 0
  }
  if (Dt[i] <= 0){
    Dt[i] <- 0
  }
  if (Et[i] <= 0){
    Et[i] <- 0
  }
  
  popEachMinutet[i] <- sum(At[i], Bt[i], Ct[i], Et[i])
}

# plot(xbart, popEachMinutet, main = "Cumulative number of precheck passengers entered Security Checkpoint in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbart, At, main = "Number of precheck passengers at the ID check queue in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbart, Bt, main = "Number of precheck passengers at the Xray/Wavescanner queue in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbart, Ct, main = "Number of precheck passengers queuing to grab their bags in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbart, Dt, main = "Number of precheck passengers queuing for pat-down inspection in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbart, Et, main = "Number of precheck passengers getting through the Security Checkpoint in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers",col=4)

plot(xbart, popEachMinutet, type = "l", lwd=2, main="Number of precheck passengers at different nodes", xlab = "Time (minute)", ylab="Number of passengers")
lines(xbart, At, type = "l", lwd=2, col=2)
lines(xbart, Bt, type = "l", lwd=2, col=3)
lines(xbart, Ct, type = "l", lwd=2, col=4)
lines(xbart, Dt, type = "l", lwd=2, col=5)
lines(xbart, Et, type = "l", lwd=2, col=6)
stations <- c("Entered","ID Check","Xray/Wavescan","Grab bags","Pat-down","Through")
legend("topleft", legend=stations, lwd=2, col=c(1,2,3,4,5,6))

#NO TITLE GRAPHS

# plot(xbart, popEachMinutet, xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbar, At, main = "Zone A", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbar, Bt, main = "Zone B", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbar, Ct, main = "Zone C", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbar, D, main = "Zone D", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
# plot(xbar, E, main = "Zone E", xlab = "Time (minute)", ylab = "Number of passengers",col=4)

plot(xbart, popEachMinute, type = "l", lwd=2, xlab = "Time (minute)", ylab="Number of passengers")
lines(xbart, At, type = "l", lwd=2, col=2)
lines(xbart, Bt, type = "l", lwd=2, col=3)
lines(xbart, Ct, type = "l", lwd=2, col=4)
lines(xbart, Dt, type = "l", lwd=2, col=5)
lines(xbart, Et, type = "l", lwd=2, col=6)
stations <- c("Entered","ID Check","Xray/Wavescan","Grab bags","Pat-down","Through")
legend("topleft", legend=stations, lwd=2, col=c(1,2,3,4,5,6))
