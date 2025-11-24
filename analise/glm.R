# GLMs
library(rethinking)
set_ulam_cmdstan(TRUE)
# Data
library(tidyverse)

# Loading data
dados <- read_csv("./data.csv") %>%
  mutate(
    pos_pred_a = factor(pred_a_pos, levels = c("P", "A", "N")),
    OBJ_TH = factor(OBJ_TH, levels = c("Recipient", "Experiencer", "Agent"))
  )

# Prepared data for Stan
dat <- list(
  "ID" = as.factor(dados$ID),
  "A" = dados$Attr,
  "Th" = dados$OBJ_TH,
  "PoS" = dados$pos_pred_a,
  "Cop" = dados$Vinf_Cop,
  "Auth" = as.factor(dados$AUTHOR),
  "Dia" = as.factor(dados$DIALECT),
  "Prec" = (dados$DIST_OBJ_PRED < 0),
  "Dist" = abs(dados$DIST_OBJ_PRED),
  alpha_pos = rep(3, 2),
  alpha_th = rep(3, 2)
)

# Mathematical model
flist <- alist(
  # Attraction Model
  A ~ bernoulli(p),
  logit(p) <- a_bar + z_sent[ID] * s_sent +
    b_c * Cop +
    b_pos * sum(delta_j_pos[1:PoS]) +
    b_th * sum(delta_j_th[1:Th]) +
    z_auth[Auth] * s_auth +
    b_dia_bar + z_dia[Dia] * s_dia +
    b_dist * Dist,
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
  # sent
  a_bar ~ normal(0, 1),
  z_sent[ID] ~ normal(0, 1),
  s_sent ~ exponential(1),
  gq > vector[ID]:a_sent <<- a_bar + z_sent * s_sent,
  # dia
  b_dia_bar ~ normal(0, 1),
  z_dia[Dia] ~ normal(0, 1),
  s_dia ~ exponential(1),
  gq > vector[Dia]:b_dia <<- b_dia_bar + z_dia * s_dia,
  # Dist
  b_dist ~ normal(0, 1)
)

# Training
model <- ulam(
  flist = flist,
  data = dat,
  chains = 8, cores = 8,
  iter = 1000,
  log_lik = TRUE,
  file = "glm",
  output_dir = "./ulam/"
)

# Checking
precis(model, depth = 2)
sink("../fala/tables/precis.R")
precis(model, depth = 2)
sink()

pdf("./check.pdf")
dashboard(model)
dev.off()

pdf("../fala/plots/trank.pdf")
trankplot(model, ask = FALSE)
dev.off()

# Link function for generating simulations
link <- function(model, data) {
  samples <- extract.samples(model)
  p <- matrix(nrow = 4e3)
  for (i in seq_along(data$ID)) {
    d_pos <- samples$delta_j_pos[, 1:data$PoS[i]]
    d_th <- samples$delta_j_th[, 1:data$Th[i]]
    p <- cbind(
      p,
      inv_logit(
        samples$a_sent[, data$ID[i]] +
          (samples$b_c * data$Cop[i]) +
          (samples$b_pos * ifelse((dims(d_pos) == 1), 0, apply(d_pos, 1, sum))) +
          (samples$b_th * ifelse((dims(d_th) == 1), 0, apply(d_th, 1, sum))) +
          (samples$b_auth[, data$Auth[i]]) +
          (samples$b_dia[, data$Dia[i]]) +
          (samples$b_dist * data$Dist[i])
      )
    )
  }
  p <- p[, -1]
  bern <- function(p) {
    rbern(1, prob = p)
  }
  matrix(as.logical(dplyr::map_dbl(sim, bern)), nrow = 4e3)
}
