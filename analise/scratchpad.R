f0_cop_pos <- alist(
  A ~ bernoulli(p),
  logit(p) <- a + b_c * Cop + b_p * P,
  a ~ normal(0, 1),
  b_c ~ normal(0, 1),
  b_p ~ normal(0, 1)
)

m0 <- ulam(
  flist = f0_cop_pos,
  data = data,
  chains = 4, cores = 4,
  log_lik = TRUE
)

f1_cop_pos <- alist(
  A ~ bernoulli(p),
  logit(p) <- a + b_c * Cop + b_p * sum(delta_j[1:P]),
  a ~ normal(0, 1),
  b_c ~ normal(0, 1),
  b_p ~ normal(0, 1),
  vector[3]:delta_j <<- append_row(0, delta),
  simplex[2]:delta ~ dirichlet(alpha)
)

m1 <- ulam(
  flist = f1_cop_pos,
  data = data,
  chains = 4, cores = 4,
  log_lik = TRUE
)

f2_cop_pos <- alist(
  A ~ bernoulli(p),
  logit(p) <- a + b_c * Cop + b_p * sum(delta_j[1:P]),
  a ~ normal(0, 1),
  b_c ~ normal(0, 1),
  b_p ~ normal(0, 1),
  vector[3]:delta_j <<- append_row(0, delta),
  simplex[2]:delta ~ dirichlet(alpha),
  P ~ dordlogit(phi, cutpoints),
  phi <- u + g_c * Cop,
  g_c ~ dnorm(0, 0.5),
  u ~ dnorm(0, 1),
  cutpoints ~ dnorm(0, 1.5)
)

m2 <- ulam(
  flist = f2_cop_pos,
  data = data,
  chains = 4, cores = 4,
  log_lik = TRUE
)

compare(m0, m1, m2)

# Independence tests
chisq.test(dados$GENRE, dados$Attr)

chisq.test(dados$GENRE, dados$Attr)

dados_modal_vm <- dados %>% filter(VM_MOD)
dados_not_modal_vm <- dados %>% filter(!VM_MOD)


chisq.test(dados_modal_vm$GENRE, dados_modal_vm$Attr)
chisq.test(dados_not_modal_vm$GENRE, dados_not_modal_vm$Attr)

chisq.test(dados_modal_vm$AUTHOR, dados_modal_vm$Attr)
chisq.test(dados_not_modal_vm$AUTHOR, dados_not_modal_vm$Attr)


dados_juridical <- dados %>% filter(GENRE == "juridical")
dados_drama <- dados %>% filter(GENRE == "drama")
dados_historiography <- dados %>% filter(GENRE == "historiography")
dados_philosophical <- dados %>% filter(GENRE == "philosophical_dialogue")
dados_other <- dados %>% filter(GENRE == "other")

chisq.test(dados_juridical$AUTHOR, dados_juridical$Attr)
chisq.test(dados_drama$AUTHOR, dados_drama$Attr)
chisq.test(dados_historiography$AUTHOR, dados_historiography$Attr)
chisq.test(dados_philosophical$AUTHOR, dados_philosophical$Attr)
chisq.test(dados_other$AUTHOR, dados_other$Attr)

chisq.test(dados$VM_MOD, dados$Vinf_Cop)
chisq.test(dados$AUTHOR, dados$Cop)

sim <- (link(model, dat))

require(bayesplot)
require(cmdstanr)

y <- bayesplot::example_y_data()
yrep <- bayesplot::example_yrep_draws()
ppc_error_hist(y, yrep[1:3, ])
suppressPackageStartupMessages(library(rstanarm))
suppressWarnings(example("example_model", package = "rstanarm"))
formula(example_model)

# get observed proportion of "successes"
y_prop <- as.numeric(dat$A) # matrix of "success" and "failure" counts

yrep_prop <- sweep(sim, 2, 296, "/")

ppc_error_hist(y_prop, yrep_prop[1:50, ])

sim %>%
  t() %>%
  as_tibble() %>%
  mutate(seen = dat$A, id = dat$ID) %>%
  pivot_longer(names_to = "sample", cols = !c(seen, id)) %>%
  filter(sample %in% str_c("V", 1:2000)) %>%
  ggplot(aes(x = as.numeric(value), y = as.numeric(seen))) +
  geom_density2d_filled()
