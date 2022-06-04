df = read.csv("steam-200k.csv", header=FALSE)
df

get_games <- function(user, df){
  ug <- df[df$V1 == user, ]$V2
  return(unique(ug))
}

create_matrix <- function(df) {
  M <- rbind(c())
  games <- unique(df$V2)
  users <- unique(df$V1)
  for (u in users){
    user_games <- get_games(u, df)
    user_activities <- c()
    for (g in games){
      if (g %in% user_games) {
        temp <- df[df$V1 == u & df$V2 == g,]$V4
        if (length(temp) > 1) {
          temp <- df[df$V1 == u & df$V2 == g & df$V3 == "play",]$V4
        }
        user_activities <- c(user_activities, temp)}
      else {user_activities <- c(user_activities, 0.01)}
    }
    M <- rbind(M, c(user_activities))
  }
  return(M)
}

library(msm)

initialize_v <- function(l, N2, d) {
  M <- rbind(c())
  M <- sapply(1:N2, function(M){c(rtnorm(n=d, mean=0, sd=1/l, lower=0, upper = Inf))})
  return(M)
}

compute1 <- function(mij, ui, vj, sig2){
  return((1/(2*sig2))*abs(mij - t(ui)%*%vj)^2)
}

compute2 <- function(ai, lam){
  return((lam/2) * (ai%*%ai)^2)
}

convergence <- function(M1, U1, V1, sig2, lam) {
  s1 <- 0
  for (i in 1:nrow(M1)) {
    for (j in 1:ncol(M1)) {
      s1 <- s1 + compute1(M1[i, j], c(U1[i, ]), V1[, j], sig2)
    }
  }
  sum21 <- 0
  for (i in 1:nrow(M1)) {
    sum21 <- sum21 + compute2(c(U1[i, ]), lam)
  }
  sum22 <- 0
  for (i in 1:ncol(M1)) {
    sum22 <- sum22 + compute2(c(V1[, j]), lam)
  }
  return(s1 + sum21 + sum22)
}

difference_norm <- function(M, UV) {
  difference <- M - UV
  return (norm(difference))
}

pmf_helper <- function(M, d, lambd, sigma2, U, V, N1, N2) {
  U <- rbind(c())
  for (i in 1:N1) {
    ui <- c()
    sum1 <- 0
    for (j in 1:d) {
      sum1 <- sum1 + (V[, j] %*% t(V[, j]))
    }
    temp1 <- solve(lambd*sigma2*diag(d) + sum1)
    sum2 <- 0
    for (j1 in 1:d) {
      sum2 <- sum2 + M[i, j1]*V[, j1]#?
    }
    ui <- temp1 %*% sum2
    U <- rbind(U, c(ui))
  }
  V <- rbind(c())
  for (j in 1:N2) {
    vj <- c()
    sum11 <- 0
    for (i in 1:d) {
      sum11 <- sum11 + ui %*% t(ui)
    }
    temp11 <- solve(lambd*sigma2*diag(d) + sum11)
    sum12 <- 0
    for (i1 in 1:d) {
      sum12 <- sum12 + M[i1, j]*ui
    }
    vj <- temp11 %*% sum12
    V <- rbind(V, c(vj))
  }
  V <- t(V)
  par <- convergence(M, U, V, sigma2, lambd)
  iter <- 1
  while (par > 0.001) {
    print(paste("after iteration ", iter, "cost", par))
    U <- rbind(c())
    for (i in 1:N1) {
      ui <- c()
      sum1 <- 0
      for (j in 1:d) {
        sum1 <- sum1 + (V[, j] %*% t(V[, j]))
      }
      temp1 <- solve(lambd*sigma2*diag(d) + sum1)
      sum2 <- 0
      for (j1 in 1:d) {
        sum2 <- sum2 + M[i, j1]*V[, j1]#?
      }
      ui <- temp1 %*% sum2
      U <- rbind(U, c(ui))
    }
    V <- rbind(c())
    for (j in 1:N2) {
      vj <- c()
      sum11 <- 0
      for (i in 1:d) {
        sum11 <- sum11 + ui %*% t(ui)
      }
      temp11 <- solve(lambd*sigma2*diag(d) + sum11)
      sum12 <- 0
      for (i1 in 1:d) {
        sum12 <- sum12 + M[i1, j]*ui
      }
      vj <- temp11 %*% sum12
      V <- rbind(V, c(vj))
    }
    V <- t(V)
    temp <- convergence(M, U, V, sigma2, lambd)
    par <- par - temp
    iter <- iter + 1
  }
  return(U %*% V)
}

pmf <- function(M, d, lambd, sigma2) {
  N1 <- nrow(M)
  N2 <- ncol(M)
  U <- rbind(c())
  V <- initialize_v(lambd, N2, d)
  UV <- pmf_helper(M, d, lambd, sigma2, U, V, N1, N2)
  print("U*V")
  print(UV[1, ])
  print(paste("norm: ", difference_norm(M, UV)))
}

M <- create_matrix(df)
print(M[1, ])
#pmf(M, 5, 1, 1)
pmf(M, 20, 1, 1)

