require(rethinking)

# Simulates a world in which attraction is only dependent on observable features

a <- 0
b_c <- 0.5
b_pos <- 0.5
b_th <- 1
b_auth <- c(0, -0.5, -0.1)
b_dia <- c(0, -0.1)
b_dist <- -0.3
d_th <- c(0, 0.5, 0.5)
d_pos <- c(0, 0.5, 0.5)

n <- 300
dist <- rpois(n, 10)
auth <- sample(1:3, n, replace = TRUE)
cop <- rbern(n, prob = 0.3)
pos <- c()
for (i in cop) {
  pos <- c(
    pos,
    ifelse(i,
      sample(1:3, size = 1, prob = c(0.1, 0.5, 0.4)),
      sample(1:3, 1, prob = c(0.8, 0.099, 0.01))
    )
  )
}
gen <- ifelse(auth == 1, 1, ifelse(auth == 2, 2, 2 + rbern(1, 0.4)))
dia <- 1 + (gen %% 2)
th <- c()
for (i in 1:n) {
  th <- c(
    th,
    ifelse(cop[i] == 1,
      3,
      ifelse(gen[i] == 0,
        sample(1:3, 1, prob = c(0.40, 0.59, 0.01)),
        sample(1:3, 1, prob = c(0.30, 0.65, 0.05))
      )
    )
  )
}

p <- c()
for (i in 1:n) {
  p <- c(
    p,
    inv_logit(
      b_c * cop[i] + b_pos * sum(d_pos[1:pos[i]]) + b_th * sum(d_th[1:th[i]]) + b_auth[auth[i]] + b_dia[dia[i]] + b_dist * dist[i]
    )
  )
}
attr <- rbern(n, prob = p)

dat <- list(
  "A" = attr,
  "Th" = th,
  "PoS" = pos,
  "Cop" = cop,
  "Auth" = auth,
  "Dia" = dia,
  "Dist" = dist,
  alpha_pos = rep(3, 2),
  alpha_th = rep(3, 2)
)

flist <- alist(
  # Attraction Model
  A ~ bernoulli(p),
  logit(p) <- a +
    b_c * Cop +
    b_pos * sum(delta_j_pos[1:PoS]) +
    b_th * sum(delta_j_th[1:Th]) +
    z_auth[Auth] * s_auth +
    b_dia_bar + z_dia[Dia] * s_dia +
    b_dist * Dist,
  # base
  a ~ normal(0, 1),
  # copula
  b_c ~ normal(0, 1),
  # pos
  b_pos ~ lognormal(0, 1),
  vector[3]:delta_j_pos <<- append_row(0, delta_pos),
  simplex[2]:delta_pos ~ dirichlet(alpha_pos),
  transpars > real[1]:d_part <<- b_pos * 0,
  transpars > real[1]:d_adj <<- b_pos * delta_pos[1],
  transpars > real[1]:d_noun <<- b_pos * (delta_pos[1] + delta_pos[2]),
  # th
  b_th ~ lognormal(0, 1),
  vector[3]:delta_j_th <<- append_row(0, delta_th),
  simplex[2]:delta_th ~ dirichlet(alpha_th),
  transpars > real[1]:d_recip <<- b_th * 0,
  transpars > real[1]:d_exp <<- b_th * delta_th[1],
  transpars > real[1]:d_agent <<- b_th * (delta_th[1] + delta_th[2]),
  # auth
  z_auth[Auth] ~ normal(0, 1),
  s_auth ~ exponential(1),
  gq > vector[Auth]:b_auth <<- z_auth * s_auth,
  # dia
  b_dia_bar ~ normal(0, 1),
  z_dia[Dia] ~ normal(0, 1),
  s_dia ~ exponential(1),
  gq > vector[Dia]:b_dia <<- b_dia_bar + z_dia * s_dia,
  # Dist
  b_dist ~ normal(0, 1)
)

model <- ulam(
  flist = flist,
  data = dat,
  chains = 4, cores = 4,
  log_lik = TRUE,
)

precis(model, depth = 2)

# Simulates a world in which attraction is dependent on observable and unobservable features

a <- 0
b_al <- 0.1
b_co <- 0.1
b_do <- 0.1
b_c <- 0.5
b_pos <- 0.5
b_th <- 1
b_auth <- c(0, -0.5, -0.1)
b_dia <- c(0, -0.1)
b_dist <- -0.3
d_th <- c(0, 0.5, 0.5)
d_pos <- c(0, 0.5, 0.5)

n <- 300
al <- rnorm(n)
co <- rnorm(n)
do <- rnorm(n)
dist <- rpois(n, 10)
auth <- sample(1:3, n, replace = TRUE)
cop <- rbern(n, prob = 0.3)
pos <- c()
for (i in cop) {
  pos <- c(
    pos,
    ifelse(i,
      sample(1:3, size = 1, prob = c(0.1, 0.5, 0.4)),
      sample(1:3, 1, prob = c(0.8, 0.099, 0.01))
    )
  )
}
gen <- ifelse(auth == 1, 1, ifelse(auth == 2, 2, 2 + rbern(1, 0.4)))
dia <- 1 + (gen %% 2)
th <- c()
for (i in 1:n) {
  th <- c(
    th,
    ifelse(cop[i] == 1,
      3,
      ifelse(gen[i] == 0,
        sample(1:3, 1, prob = c(0.40, 0.59, 0.01)),
        sample(1:3, 1, prob = c(0.30, 0.65, 0.05))
      )
    )
  )
}

p <- c()
for (i in 1:n) {
  p <- c(
    p,
    inv_logit(
      b_c * cop[i] + b_pos * sum(d_pos[1:pos[i]]) + b_th * sum(d_th[1:th[i]]) + b_auth[auth[i]] + b_dia[dia[i]] + b_dist * dist[i] + b_al * al[i] + b_co * co[i] + b_do * do[i]
    )
  )
}
attr <- rbern(n, prob = p)

dat <- list(
  "A" = attr,
  "Th" = th,
  "PoS" = pos,
  "Cop" = cop,
  "Auth" = auth,
  "Dia" = dia,
  "Dist" = dist,
  alpha_pos = rep(3, 2),
  alpha_th = rep(3, 2)
)

flist <- alist(
  # Attraction Model
  A ~ bernoulli(p),
  logit(p) <- a +
    b_c * Cop +
    b_pos * sum(delta_j_pos[1:PoS]) +
    b_th * sum(delta_j_th[1:Th]) +
    z_auth[Auth] * s_auth +
    b_dia_bar + z_dia[Dia] * s_dia +
    b_dist * Dist,
  # base
  a ~ normal(0, 1),
  # copula
  b_c ~ normal(0, 1),
  # pos
  b_pos ~ lognormal(0, 1),
  vector[3]:delta_j_pos <<- append_row(0, delta_pos),
  simplex[2]:delta_pos ~ dirichlet(alpha_pos),
  transpars > real[1]:d_part <<- b_pos * 0,
  transpars > real[1]:d_adj <<- b_pos * delta_pos[1],
  transpars > real[1]:d_noun <<- b_pos * (delta_pos[1] + delta_pos[2]),
  # th
  b_th ~ lognormal(0, 1),
  vector[3]:delta_j_th <<- append_row(0, delta_th),
  simplex[2]:delta_th ~ dirichlet(alpha_th),
  transpars > real[1]:d_recip <<- b_th * 0,
  transpars > real[1]:d_exp <<- b_th * delta_th[1],
  transpars > real[1]:d_agent <<- b_th * (delta_th[1] + delta_th[2]),
  # auth
  z_auth[Auth] ~ normal(0, 1),
  s_auth ~ exponential(1),
  gq > vector[Auth]:b_auth <<- z_auth * s_auth,
  # dia
  b_dia_bar ~ normal(0, 1),
  z_dia[Dia] ~ normal(0, 1),
  s_dia ~ exponential(1),
  gq > vector[Dia]:b_dia <<- b_dia_bar + z_dia * s_dia,
  # Dist
  b_dist ~ normal(0, 1)
)

model_weak <- ulam(
  flist = flist,
  data = dat,
  chains = 4, cores = 4,
  log_lik = TRUE,
)

precis(model_weak, depth = 2)

# Model Strong

a <- 0
b_al <- 2
b_co <- 2
b_do <- 2
b_c <- 0.5
b_pos <- 0.5
b_th <- 1
b_auth <- c(0, -0.5, -0.1)
b_dia <- c(0, -0.1)
b_dist <- -0.3
d_th <- c(0, 0.5, 0.5)
d_pos <- c(0, 0.5, 0.5)

n <- 300
al <- rnorm(n)
co <- rnorm(n)
do <- rnorm(n)
dist <- rpois(n, 10)
auth <- sample(1:3, n, replace = TRUE)
cop <- rbern(n, prob = 0.3)
pos <- c()
for (i in cop) {
  pos <- c(
    pos,
    ifelse(i,
      sample(1:3, size = 1, prob = c(0.1, 0.5, 0.4)),
      sample(1:3, 1, prob = c(0.8, 0.099, 0.01))
    )
  )
}
gen <- ifelse(auth == 1, 1, ifelse(auth == 2, 2, 2 + rbern(1, 0.4)))
dia <- 1 + (gen %% 2)
th <- c()
for (i in 1:n) {
  th <- c(
    th,
    ifelse(cop[i] == 1,
      3,
      ifelse(gen[i] == 0,
        sample(1:3, 1, prob = c(0.40, 0.59, 0.01)),
        sample(1:3, 1, prob = c(0.30, 0.65, 0.05))
      )
    )
  )
}

p <- c()
for (i in 1:n) {
  p <- c(
    p,
    inv_logit(
      b_c * cop[i] + b_pos * sum(d_pos[1:pos[i]]) + b_th * sum(d_th[1:th[i]]) + b_auth[auth[i]] + b_dia[dia[i]] + b_dist * dist[i] + b_al * al[i] + b_co * co[i] + b_do * do[i]
    )
  )
}
attr <- rbern(n, prob = p)

dat <- list(
  "A" = attr,
  "Th" = th,
  "PoS" = pos,
  "Cop" = cop,
  "Auth" = auth,
  "Dia" = dia,
  "Dist" = dist,
  alpha_pos = rep(3, 2),
  alpha_th = rep(3, 2)
)

flist <- alist(
  # Attraction Model
  A ~ bernoulli(p),
  logit(p) <- a +
    b_c * Cop +
    b_pos * sum(delta_j_pos[1:PoS]) +
    b_th * sum(delta_j_th[1:Th]) +
    z_auth[Auth] * s_auth +
    b_dia_bar + z_dia[Dia] * s_dia +
    b_dist * Dist,
  # base
  a ~ normal(0, 1),
  # copula
  b_c ~ normal(0, 1),
  # pos
  b_pos ~ lognormal(0, 1),
  vector[3]:delta_j_pos <<- append_row(0, delta_pos),
  simplex[2]:delta_pos ~ dirichlet(alpha_pos),
  transpars > real[1]:d_part <<- b_pos * 0,
  transpars > real[1]:d_adj <<- b_pos * delta_pos[1],
  transpars > real[1]:d_noun <<- b_pos * (delta_pos[1] + delta_pos[2]),
  # th
  b_th ~ lognormal(0, 1),
  vector[3]:delta_j_th <<- append_row(0, delta_th),
  simplex[2]:delta_th ~ dirichlet(alpha_th),
  transpars > real[1]:d_recip <<- b_th * 0,
  transpars > real[1]:d_exp <<- b_th * delta_th[1],
  transpars > real[1]:d_agent <<- b_th * (delta_th[1] + delta_th[2]),
  # auth
  z_auth[Auth] ~ normal(0, 1),
  s_auth ~ exponential(1),
  gq > vector[Auth]:b_auth <<- z_auth * s_auth,
  # dia
  b_dia_bar ~ normal(0, 1),
  z_dia[Dia] ~ normal(0, 1),
  s_dia ~ exponential(1),
  gq > vector[Dia]:b_dia <<- b_dia_bar + z_dia * s_dia,
  # Dist
  b_dist ~ normal(0, 1)
)

model_strong <- ulam(
  flist = flist,
  data = dat,
  chains = 4, cores = 4,
  log_lik = TRUE,
)

round(precis(model_strong, depth = 2), 2)
#               mean   sd  5.5% 94.5% rhat ess_bulk
# a            -0.05 0.76 -1.25  1.14    1  1363.05
# b_c           0.13 0.35 -0.44  0.67    1  1298.27
# b_pos         0.29 0.17  0.09  0.58    1  2143.45
# delta_pos[1]  0.50 0.19  0.20  0.81    1  2340.83
# delta_pos[2]  0.50 0.19  0.19  0.80    1  2340.83
# b_th          0.46 0.28  0.12  0.97    1  1261.56
# delta_th[1]   0.43 0.18  0.15  0.76    1  2156.07
# delta_th[2]   0.57 0.18  0.24  0.85    1  2156.07
# z_auth[1]    -0.14 0.89 -1.56  1.28    1  1649.71
# z_auth[2]     0.38 0.81 -0.93  1.63    1  1441.34
# z_auth[3]    -0.28 0.82 -1.58  1.00    1  1791.64
# s_auth        0.41 0.42  0.02  1.16    1   651.31
# b_dia_bar    -0.05 0.76 -1.25  1.15    1  1354.81
# z_dia[1]      0.09 0.87 -1.30  1.47    1  1602.13
# z_dia[2]     -0.10 0.86 -1.47  1.26    1  1543.69
# s_dia         0.50 0.55  0.02  1.54    1   881.57
# b_dist       -0.13 0.04 -0.20 -0.06    1  1763.26
# b_dia[1]      0.00 0.78 -1.21  1.26    1  1366.10
# b_dia[2]     -0.09 0.80 -1.38  1.19    1  1359.95
# b_auth[1]    -0.06 0.39 -0.72  0.49    1  1559.92
# b_auth[2]     0.15 0.35 -0.28  0.74    1  1382.78
# b_auth[3]    -0.10 0.34 -0.66  0.38    1  1609.66
# d_agent       0.46 0.28  0.12  0.97    1  1261.56
# d_exp         0.19 0.13  0.04  0.43    1  1871.09
# d_recip       0.00 0.00  0.00  0.00   NA       NA
# d_noun        0.29 0.17  0.09  0.58    1  2143.45
# d_adj         0.14 0.10  0.03  0.32    1  2248.65
# d_part        0.00 0.00  0.00  0.00   NA       NA

compare(model, model_weak, model_strong)
