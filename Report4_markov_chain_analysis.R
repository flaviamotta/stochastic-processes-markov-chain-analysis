

# Initialization function
F0 <- function(u) {
  if (u[1] <= 0.20) {
    return(0)
  }
  if ((u[1] > 0.20) & (u[1] <= 0.40)) {
    return(1)
  }
  if ((u[1] > 0.40) & (u[1] <= 0.60)) {
    return(2)
  }
  if ((u[1] > 0.60) & (u[1] <= 0.80)) {
    return(3)
  }
  if ((u[1] > 0.80) & (u[1] <= 1)) {
    return(4)
  }
}

# Stationary initialization function
F0es <- function(u) {
  if (u[1] <= 22/87) {
    return(0)
  }
  if ((u[1] > 22/87) & (u[1] <= 42/87)) {
    return(1)
  }
  if ((u[1] > 42/87) & (u[1] <= 69/87)) {
    return(2)
  }
  if ((u[1] > 69/87) & (u[1] <= 81/87)) {
    return(3)
  }
  if ((u[1] > 81/87) & (u[1] <= 1)) {
    return(4)
  }
}

# State update function
F <- function(i, u) {
  if (i == 0) {
    if (u <= 0.5) {
      return(1)
    }
    if ((u > 0.5) & (u <= 1)) {
      return(2)
    }
  }
  if (i == 1) {
    if (u <= 0.5) {
      return(0)
    }
    if ((u > 0.5) & (u <= 1)) {
      return(2)
    }
  }
  if (i == 2) {
    if (u <= 1/3) {
      return(0)
    }
    if ((u > 1/3) & (u <= 2/3)) {
      return(1)
    }
    if ((u > 2/3) & (u <= 1)) {
      return(3)
    }
  }
  if (i == 3) {
    if (u <= 0.5) {
      return(2)
    }
    if ((u > 0.5) & (u <= 1)) {
      return(4)
    }
  }
  if (i == 4) {
    if (u <= 0.5) {
      return(0)
    }
    if ((u > 0.5) & (u <= 1)) {
      return(3)
    }
  }
}

# Number of steps in the chain
N <- 30
ux <- runif(N, 0, 1)  # Uniform distribution for X
uy <- runif(N, 0, 1)  # Uniform distribution for X' (Y)
x <- vector()
y <- vector()
z <- vector()
x[1] <- F0(ux)
y[1] <- F0es(uy)
z[1] <- F0(ux)

for (n in 2:N) {
  r <- 0
  x[n] <- F(x[n - 1], ux[n])
  y[n] <- F(y[n - 1], uy[n])
  
  if (z[n - 1] == y[n - 1]) {
    r <- uy[n]
  } else {
    r <- ux[n]
  }
  
  z[n] <- F(z[n - 1], r)
}

# Plotting all chains
plot(x, col = "red", type = "b", xlab = "n", ylab = expression(x[n]), lty = 1, pch = 18, ylim = c(0, 4), yaxt = "n")
lines(y, col = "black", type = "b", lty = 2, pch = 19)
lines(z, pch = 0, col = "purple", type = "b", lty = 1)
axis(2, at = c(0, 1, 2, 3, 4), labels = c(0, 1, 2, 3, 4), col.axis = "black", las = 2)
legend("top", legend = c("X", "X'", "X''"), col = c("red", "black", "purple"),
       pch = c(18, 19, 0),
       title = "Legend",
       lty = c(1, 2, 1), cex = 0.8)

# Plotting two of the chains (X and X')
plot(x, col = "red", type = "b", xlab = "n", ylab = expression(x[n]), lty = 1, pch = 18, ylim = c(0, 4), yaxt = "n")
lines(y, col = "black", type = "b", lty = 2, pch = 19)
axis(2, at = c(0, 1, 2, 3, 4), labels = c(0, 1, 2, 3, 4), col.axis = "black", las = 2)
legend("top", legend = c("X", "X'"), col = c("red", "black"),
       pch = c(18, 19),
       title = "Legend",
       lty = c(1, 2), cex = 0.8)

nsimulations <- 2000
N <- 500
T <- vector()
simexc <- 0  # Counter for exceeding N

for (k in 1:nsimulations) {
  ux <- runif(N, 0, 1)  # Uniform distribution for X
  uy <- runif(N, 0, 1)  # Uniform distribution for Y
  x <- vector()
  y <- vector()
  z <- vector()
  g <- 0
  r <- 0
  x[1] <- F0(ux)
  y[1] <- F0es(uy)
  z[1] <- F0(ux)
  T[k] <- 0

  for (n in 2:N) {
    r <- 0
    x[n] <- F(x[n - 1], ux[n])
    y[n] <- F(y[n - 1], uy[n])
    
    if (z[n - 1] == y[n - 1]) {
      r <- uy[n]
    } else {
      r <- ux[n]
    }
    
    z[n] <- F(z[n - 1], r)
    
    if (x[1] == y[1]) {
      g <- 1
    }
    
    if (x[n] == y[n] & g == 0) {
      T[k] <- n - 1
      g <- 1
    }
    
    if (n == N & z[n] != y[n]) {
      simexc <- simexc + 1
    }
  }
}

mean_T <- mean(T)
df <- data.frame(Tempo_de_encontro = T)
F <- ecdf(T)
t <- seq(0, 40, 1)
plot(t, F(t), main = "Empirical Distribution Function \n Sample Size n = 2000")
grid()

# Calculation of P(T > n) for different values of n
S <- vector()
nsimulations <- 2000
N <- seq(1, 60, 2)

for (i in 1:length(N)) {
  print(N[i])
  simexc <- 0  # Counter for exceeding N
  
  for (k in 1:nsimulations) {
    ux <- runif(N[i], 0, 1)  # Uniform distribution for X
    uy <- runif(N[i], 0, 1)  # Uniform distribution for Y
    x <- vector()
    y <- vector()
    z <- vector()
    g <- 0
    r <- 0
    x[1] <- F0(ux)
    y[1] <- F0es(uy)
    z[1] <- F0(ux)
    
    if (N[i] >= 2) {
      for (n in 2:N[i]) {
        r <- 0
        x[n] <- F(x[n - 1], ux[n])
        y[n] <- F(y[n - 1], uy[n])
        
        if (z[n - 1] == y[n - 1]) {
          r <- uy[n]
        } else {
          r <- ux[n]
        }
        
        z[n] <- F(z[n - 1], r)
        
        if (n == N[i] & z[n] != y[n]) {
          simexc <- simexc + 1
        }
      }
    } else {
      if (z[1] != y[1]) {
        simexc <- simexc + 1
      }
    }
  }
  
  S[i] <- simexc / nsimulations
}

dt <- data.frame(N, S)

ggplot(dt, aes(x = N, y = S)) +
  geom_point(shape = 19) +
  xlab('n') +
  scale_x_continuous(breaks = seq(1, 60, 2)) +
  ylab('P(T > n)') +
  theme_bw()

# Estimation of marginal distributions of X and Y
S <- vector()
nsimulations <- 9000
marginalx <- matrix(nrow = 3, ncol = 5)
marginaly <- matrix(nrow = 3, ncol = 5)
N <- c(50, 100, 1000)

for (i in 1:length(N)) {
  print(N[i])
  X <- vector()
  Y <- vector()
  
  for (k in 1:nsimulations) {
    ux <- runif(N[i], 0, 1)  # Uniform distribution for X
    uy <- runif(N[i], 0, 1)  # Uniform distribution for Y
    x <- vector()
    y <- vector()
    
    x[1] <- F0(ux)
    y[1] <- F0es(uy)
    
    for (n in 2:N[i]) {
      x[n] <- F(x[n - 1], ux[n])
      y[n] <- F(y[n - 1], uy[n])
    }
    
    X[k] <- x[N[i]]
    Y[k] <- y[N[i]]
  }
  
  marginalx[i, ] <- table(X) / nsimulations
  marginaly[i, ] <- table(Y) / nsimulations
}

p <- c(22/87, 20/87, 27/87, 12/87, 6/87)
matrizestac <- rbind(p, p, p)
marginalx - matrizestac
marginaly - matrizestac
