#Precheck passengers
#180 is in minutes = 3 hours
xbar <- 1:180
n <- 180
#CALCULATE RATES (people per minute) AT DIFFERENT MINUTE TIME STEP FOR 3 HOURS
#arrival
alpha <- rpois(n, 9.2)
alpha_rate <- convertToRate(alpha)

#id check
beta <- rnorm(n, 11.2, 3.8)
beta_rate <- convertToRate(beta)

#xray
x <- runif(n, 0 , 1)

delta <- c()
delta_rate <- c()
stochastic <- function(x, delta){
  for (i in xbar){
    if (x[i] <= 4/11){
      delta[i] <<- rnorm(1, 15.6, 7.6)
    } else {
      delta[i] <<- rnorm(1, 2.2, 0.5)
    }
  }
}
stochastic(x, delta)
delta <- replace(delta, delta <= 0, 0)
delta_rate <- convertToRate(delta)
delta_rate <- replace(delta_rate, delta_rate <= 0, 0)
delta_rate[!is.finite(delta_rate)] <- 0

#full body scan
gamma <- rnorm(n, 11.6, 5.6)
gamma <- replace(gamma, gamma <= 0, 0)
gamma_rate <- convertToRate(gamma)
gamma_rate <- replace(gamma_rate, gamma_rate <= 0, 0)
gamma_rate[!is.finite(gamma_rate)] <- 0

#grabbags
psi <- rnorm(n, 28, 14)
psi <- replace(psi, psi <= 0, 0)
psi_rate <- convertToRate(psi)
psi_rate <- replace(psi_rate, psi_rate <= 0, 0)
psi_rate[!is.finite(psi_rate)] <- 0

#extracheck = 2 - NOT YET TOUCHED the 97 and 3 percentage
epsilon97 <- pmin(delta_rate, gamma_rate)
epsilon3 <- 60/(pmax(delta, gamma) + 120)

#function to convert sec/person to persons/min
convertToRate <- function(x){
  result <- 1/(x/60)
  return(result)
}

#CALCULATE No of PASSENGERS
A <- 0
B <- 0
C <- 0
#note that people in D are included in B. Thus total pop at a time step = A + B + C + E.
D <- 0
E <- 0
popEachMinute <- 0

A[1] <- alpha_rate[1] - beta_rate[1]

if (rbinom(1, 1, 0.03) == 0){
  B[1] <- beta_rate[1] - epsilon97[1]
  C[1] <- epsilon97[1] - psi_rate[1]
  #D[1] <- -0.5
} else {
  B[1] <- beta_rate[1] - epsilon3[1]
  C[1] <- epsilon3[1] - psi_rate[1]
  D[1] <- epsilon3[1] - 0.5 
}
E[1] <- psi_rate[1]

popEachMinute[1] <- sum(A[1], B[1], C[1], E[1])


for (i in 2:n){
  A[i] <- A[i-1] + alpha_rate[i] - beta_rate[i]
  if (rbinom(1, 1, 0.03) == 0){
    B[i] <- B[i-1] + beta_rate[i] - epsilon97[i]
    C[i] <- C[i-1] + epsilon97[i] - psi_rate[i]
    D[i] <- D[i-1] - 0.5
  } else {
    B[i] <- B[i-1] + beta_rate[i] - epsilon3[i]
    C[i] <- C[i-1] + epsilon3[i] - psi_rate[i]
    D[i] <- D[i-1] + epsilon3[i] - 0.5
  }
  E[i] <- E[i-1] + psi_rate[i]
  
  if (A[i] <= 0){
    A[i] <- 0
  }
  if (B[i] <= 0){
    B[i] <- 0
  }
  if (C[i] <= 0){
    C[i] <- 0
  }
  if (D[i] <= 0){
    D[i] <- 0
  }
  if (E[i] <= 0){
    E[i] <- 0
  }
  
  popEachMinute[i] <- sum(A[i], B[i], C[i], E[i])
}

plot(xbar, popEachMinute, main = "Cumulative number of precheck passengers entered Security Checkpoint in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbar, A, main = "Number of precheck passengers at the ID check queue in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbar, B, main = "Number of precheck passengers at the Xray/Wavescanner queue in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbar, C, main = "Number of precheck passengers queuing to grab their bags in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbar, D, main = "Number of precheck passengers queuing for pat-down inspection in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbar, E, main = "Number of precheck passengers getting through the Security Checkpoint in 3 hours", xlab = "Time (minute)", ylab = "Number of passengers",col=4)

plot(xbar, popEachMinute, type = "l", lwd=2, main="Number of precheck passengers at different nodes", xlab = "Time (minute)", ylab="Number of passengers")
lines(xbar, A, type = "l", lwd=2, col=2)
lines(xbar, B, type = "l", lwd=2, col=3)
lines(xbar, C, type = "l", lwd=2, col=4)
lines(xbar, D, type = "l", lwd=2, col=5)
lines(xbar, E, type = "l", lwd=2, col=6)
stations <- c("Entered","ID Check","Xray/Wavescan","Grab bags","Pat-down","Through")
legend("topleft", legend=stations, lwd=2, col=c(1,2,3,4,5,6))

#NO TITLE GRAPHS

plot(xbar, popEachMinute, xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbar, A, main = "Zone A", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbar, B, main = "Zone B", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbar, C, main = "Zone C", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbar, D, main = "Zone D", xlab = "Time (minute)", ylab = "Number of passengers", col=4)
plot(xbar, E, main = "Zone E", xlab = "Time (minute)", ylab = "Number of passengers",col=4)

plot(xbar, popEachMinute, type = "l", lwd=2, xlab = "Time (minute)", ylab="Number of passengers")
lines(xbar, A, type = "l", lwd=2, col=2)
lines(xbar, B, type = "l", lwd=2, col=3)
lines(xbar, C, type = "l", lwd=2, col=4)
lines(xbar, D, type = "l", lwd=2, col=5)
lines(xbar, E, type = "l", lwd=2, col=6)
stations <- c("Entered","ID Check","Xray/Wavescan","Grab bags","Pat-down","Through")
legend("topleft", legend=stations, lwd=2, col=c(1,2,3,4,5,6))
