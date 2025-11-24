require(rethinking)
require(tidyverse)

source("./glm.R")
source("./fix_pairs_rethinking.R")

dados <- dados %>%
  add_column(
    k_psis = PSIS(model, pointwise = TRUE)$k,
    waic_penalty = WAIC(model, pointwise = TRUE)$penalty
  )

dados %>% ggplot(aes(x = k_psis, y = waic_penalty)) +
  geom_point(aes(color = k_psis > 0.5))

# Prepared data for Stan
dat_attir_hdt <- list(
  "ID" = as.factor(dados$ID),
  "Th" = dados$OBJ_TH,
  "PoS" = dados$pos_pred_a,
  "Cop" = dados$Vinf_Cop,
  "Auth" = as.factor(dados$AUTHOR),
  "Dia" = factor(rep("attic", nrow(dados)), levels = c("attic", "jonic")),
  "Prec" = (dados$DIST_OBJ_PRED < 0),
  "Dist" = abs(dados$DIST_OBJ_PRED),
  alpha_pos = rep(3, 2),
  alpha_th = rep(3, 2)
)


# link <- function(samples, data) {
#   p <- matrix(nrow = 4e3)
#   for (i in seq_along(data$ID)) {
#     d_pos <- samples$delta_j_pos[, 1:data$PoS[i]]
#     d_th <- samples$delta_j_th[, 1:data$Th[i]]
#     p <- cbind(
#       p,
#       inv_logit(
#         samples$a_sent[, data$ID[i]] +
#           (samples$b_c * data$Cop[i]) +
#           (samples$b_pos * ifelse((dims(d_pos) == 1), 0, apply(d_pos, 1, sum))) +
#           (samples$b_th * ifelse((dims(d_th) == 1), 0, apply(d_th, 1, sum))) +
#           (samples$b_auth[, data$Auth[i]]) +
#           (samples$b_dia[, data$Dia[i]]) +
#           (samples$b_dist * data$Dist[i])
#       )
#     )
#   }
#   p[, -1]
# }
# sim <- (link(extract.samples(model), dat))
#
# dados %>%
#   add_column(m_pred = apply(sim, 2, mean)) %>%
#   add_column(l_pred = apply(sim, 2, HPDI)[1, ]) %>%
#   add_column(h_pred = apply(sim, 2, HPDI)[2, ]) %>%
#   arrange(m_pred) %>%
#   add_column(id = 1:296) %>%
#   ggplot(aes(x = id, y = ifelse(Attr, 1, 0), color = abs(ifelse(Attr, 1, 0) - m_pred))) +
#   geom_point() +
#   geom_pointrange(aes(ymin = l_pred, ymax = h_pred), lwd = 10, alpha = 0.5) +
#   geom_point(aes(y = m_pred), shape = 2, size = 4)
#
# inv_logit(
#   samples$a_bar[, ] + samples$z_sent[, data$ID] * samples$s_sent[, ] +
#     (samples$b_c[, ] * data$Cop) +
#     (samples$b_th[, ] * sum(samples$delta_j_th[, 1:data$Th])) +
#     (samples$z_auth[, data$Auth] * samples$s_auth[, ]) +
#     (samples$b_dia_bar[, ] + samples$z_dia[, data$Dia] * samples$s_dia[, ]) +
#     (samples$b_dist[, ] * data$Dist)
# )
