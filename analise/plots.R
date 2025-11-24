library(rethinking)
library(tidyverse)
library(kableExtra)
require(gtools)
require(ggcheck)
require(vcd)
require(FactoMineR)
require(factoextra)
require(ca)
library(kableExtra)
library(ggridges)
library(bayesplot)
source("./fix_pairs_rethinking.R")
options(knitr.kable.NA = "--")

# Loading data
dados <- read_csv("./data.csv") %>%
  mutate(
    pos_pred_a = factor(pred_a_pos, levels = c("P", "A", "N")),
    OBJ_TH = factor(OBJ_TH, levels = c("Recipient", "Experiencer", "Agent"))
  )

png(
  "../fala/plots/authors.png",
  units = "px", width = 1600, height = 1600, res = 300
)
dados %>% ggplot(aes(fill = Attr, x = autor_legivel)) +
  geom_bar(position = "dodge2") +
  xlab("Autor") +
  ylab("Passagens") +
  guides(fill = guide_legend(title = "Atração")) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

png(
  "../fala/plots/dist.png",
  units = "px", width = 1600, height = 1600, res = 300
)
dados %>% ggplot(aes(x = Attr, y = DIST_OBJ_PRED, fill = Attr)) +
  geom_boxplot() +
  xlab("Atração") +
  ylab("Distância (n de palavras)") +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )
dev.off()

attach(dados)
mca <- MCA(
  dados[, c("Attr", "Vinf_Cop", "VM_MOD", "GENRE")],
  method = "Burt", graph = FALSE
)

# Eigen-values


fviz_screeplot(
  mca,
  addlabels = TRUE,
  ylim = c(0, 60),
  barfill = "#1B9E77",
  barcolor = "#1B9E77",
  main = "Eigenvalues",
  ylab = "% explained variance"
) +
  theme_bw()


ggsave(
  "mca.eigenvalues.png",
  path = "../fala/plots/",
  width = 15,
  height = 7.5,
  units = "cm",
  dpi = 600
)


# Dim1 and Dim2

png(
  "../fala/plots/mca.png",
  height = 12,
  width = 12,
  units = "cm",
  res = 600,
  pointsize = 10
)
plot(mca,
  invisible = "ind", graph.type = "ggplot",
  col.var = c(
    "#1B9E77", "#1B9E77",
    "#D95F02", "#D95F02",
    "#7570B3", "#7570B3",
    rep("#E7298A", 5),
    repel = TRUE
  )
) +
  theme_bw()
dev.off()

# Loading fitted model
source("./glm.R")
pairs(model)
# Sampling
samples <- extract.samples(model)

# PoS
(a <- precis(model, pars = c("b_pos", "delta_pos"), depth = 2))
sink("../fala/tables/model_domain_effects.tex")
tibble(
  Effect = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(Effect = c(
    "$\\beta_{\\text{PoS}}$",
    "$\\delta_{\\text{Adj}}$",
    "$\\delta_{\\text{Noun}}$"
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

(a <- precis(model, pars = c("d_part", "d_adj", "d_noun"), depth = 2))
sink("../fala/tables/model_domain_cummulative_effects.tex")
tibble(
  `Cummulative effect` = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(
    PoS = c("Participle", "Adjective", "Noun"),
    `Cummulative effect` = c(
      "$\\beta_{\\text{PoS}} * 0$",
      "$\\beta_{\\text{PoS}} * \\delta_{\\text{Adj}}$",
      "$\\beta_{\\text{PoS}} * (\\delta_{\\text{Adj}} + \\delta_{\\text{Noun}})$"
    )
  ) %>%
  relocate("PoS") %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()


png(
  "../fala/plots/model_domain_banddelta.png",
  units = "px", width = 1600, height = 1600, res = 300
)
pairs(
  model,
  pars = c("b_pos", "delta_pos"), labels = c("Participle", "Adjective", "Noun")
)
dev.off()


# Theta
(a <- precis(model, pars = c("b_th", "delta_th"), depth = 2))
sink("../fala/tables/model_theta_effects.tex")
tibble(
  Effect = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(Effect = c(
    "$\\beta_{\\text{Theta}}$",
    "$\\delta_{\\text{Exp}}$",
    "$\\delta_{\\text{Agent}}$"
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

(a <- precis(model, pars = c("d_recip", "d_exp", "d_agent"), depth = 2))
sink("../fala/tables/model_theta_cummulative_effects.tex")
tibble(
  `Cummulative effect` = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(
    Theta = c("Recipient", "Experiencer", "Agent"),
    `Cummulative effect` = c(
      "$\\beta_{\\text{Th}} * 0$",
      "$\\beta_{\\text{Th}} * \\delta_{\\text{Exp}}$",
      "$\\beta_{\\text{Th}} * (\\delta_{\\text{Exp}} + \\delta_{\\text{Agent}})$"
    )
  ) %>%
  relocate("Theta") %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()


png(
  "../fala/plots/model_theta_banddelta.png",
  units = "px", width = 1600, height = 1600, res = 300
)
pairs(
  model,
  pars = c("b_th", "delta_th"), labels = c("Recipient", "Experiencer", "Agent")
)
dev.off()

# Copula and Distance
(a <- precis(model, pars = c("b_c", "b_dist"), depth = 2))
sink("../fala/tables/model_copula_dist_effects.tex")
tibble(
  Effect = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(Effect = c(
    "$\\beta_{\\text{Copula}}$",
    "$\\beta_{\\text{Dist}}$"
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

png(
  "../fala/plots/posterior_b_copula.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_b_c <- density(samples$b_c, bw = 0.036)
density_b_c <- tibble(x = density_b_c$x, y = density_b_c$y)
boundaries <- HPDI(samples$b_c)
density_b_c %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_c,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_cop") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()

png(
  "../fala/plots/posterior_b_dist.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_b_dist <- density(samples$b_dist, bw = 0.0035)
density_b_dist <- tibble(x = density_b_dist$x, y = density_b_dist$y)
boundaries <- HPDI(samples$b_dist)
density_b_dist %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_dist,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_dist") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()


png(
  "../fala/plots/posterior_b_th.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_b_th <- density(samples$b_th, bw = 0.0035)
density_b_th <- tibble(x = density_b_th$x, y = density_b_th$y)
boundaries <- HPDI(samples$b_th)
density_b_th %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_th,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_th") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()


# Dialect
(a <- precis(model, pars = c("b_dia_bar", "s_dia", "b_dia"), depth = 2))
sink("../fala/tables/model_dialect_effects.tex")
tibble(
  Effect = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(Effect = c(
    "$\\overline{\\beta}_{\\text{Dia}}$",
    "$\\sigma_{\\text{Dia}}$",
    "$\\beta_{\\text{Attic}}$",
    "$\\beta_{\\text{Jonic}}$"
  )) %>%
  rows_append(tibble(
    Effect =
      "$\\beta_{\\text{Attic}} - \\beta_{\\text{Jonic}}$",
    mean = round(mean(samples$b_dia[, 1] - samples$b_dia[, 2]), 2),
    sd = round(sd(samples$b_dia[, 1] - samples$b_dia[, 2]), 2),
    `5.5\\%` = round(HPDI(samples$b_dia[, 1] - samples$b_dia[, 2])[1], 2),
    `94.5\\%` = round(HPDI(samples$b_dia[, 1] - samples$b_dia[, 2])[2], 2),
    rhat = NA
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

png(
  "../fala/plots/posterior_b_attic.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_b_attic <- density(samples$b_dia[, 1], bw = 0.06)
density_b_attic <- tibble(x = density_b_attic$x, y = density_b_attic$y)
boundaries <- HPDI(samples$b_dia[, 1])
density_b_attic %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_attic,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_attic") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()

png(
  "../fala/plots/posterior_b_jonic.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_b_jonic <- density(samples$b_dia[, 2], bw = 0.06)
density_b_jonic <- tibble(x = density_b_jonic$x, y = density_b_jonic$y)
boundaries <- HPDI(samples$b_dia[, 2])
density_b_jonic %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_jonic,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_jonic") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()

png(
  "../fala/plots/posterior_diff_dia.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_diff_dia <- density(samples$b_dia[, 1] - samples$b_dia[, 2], bw = 0.06)
density_diff_dia <- tibble(x = density_diff_dia$x, y = density_diff_dia$y)
boundaries <- HPDI(samples$b_dia[, 1] - samples$b_dia[, 2])
density_diff_dia %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_diff_dia,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  geom_vline(aes(xintercept = 0)) +
  xlab("b_attic - b_jonic") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()


# Author
(a <- precis(model, pars = c("s_auth", "b_auth"), depth = 2))
sink("../fala/tables/model_author_effects.tex")
tibble(
  Effect = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(Effect = c(
    "$\\sigma_{\\text{Auth}}$",
    "$\\beta_{\\text{Aeschines}}$",
    "$\\beta_{\\text{Aeschylus}}$",
    "$\\beta_{\\text{Andocides}}$",
    "$\\beta_{\\text{Antiphon}}$",
    "$\\beta_{\\text{Demosthenes}}$",
    "$\\beta_{\\text{Euripides}}$",
    "$\\beta_{\\text{Herodotus}}$",
    "$\\beta_{\\text{Isaeus}}$",
    "$\\beta_{\\text{Isocrates}}$",
    "$\\beta_{\\text{Lycurgus}}$",
    "$\\beta_{\\text{Lysias}}$",
    "$\\beta_{\\text{Plato}}$",
    "$\\beta_{\\text{Sophocles}}$",
    "$\\beta_{\\text{Thucydides}}$",
    "$\\beta_{\\text{Xenophon}}$"
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

png(
  "../fala/plots/precis_author.png",
  units = "px", width = 1600, height = 1600, res = 300
)
plot(precis(model, pars = "b_auth", depth = 2),
  labels = c(
    "Aeschines",
    "Aeschylus",
    "Andocides",
    "Antiphon",
    "Demosthenes",
    "Euripides",
    "Herodotus",
    "Isaeus",
    "Isocrates",
    "Lycurgus",
    "Lysias",
    "Plato",
    "Sophocles",
    "Thucydides",
    "Xenophon"
  )
)
dev.off()

png(
  "../fala/plots/dens_authors.png",
  units = "px", width = 1600, height = 1600, res = 300
)
my_hpdi <- function(x, probs) {
  HPDI(x, prob = probs[2])
}
as_tibble(samples$b_auth) %>%
  rename(
    "Aeschines" = V1,
    "Aeschylus" = V2,
    "Andocides" = V3,
    "Antiphon" = V4,
    "Demosthenes" = V5,
    "Euripides" = V6,
    "Herodotus" = V7,
    "Isaeus" = V8,
    "Isocrates" = V9,
    "Lycurgus" = V10,
    "Lysias" = V11,
    "Plato" = V12,
    "Sophocles" = V13,
    "Thucydides" = V14,
    "Xenophon" = V15
  ) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(
    name,
    level =
      sort(
        c(
          "Aeschines", "Aeschylus", "Andocides", "Antiphon", "Demosthenes", "Euripides", "Herodotus",
          "Isaeus", "Isocrates", "Lycurgus", "Lysias", "Plato", "Sophocles", "Thucydides", "Xenophon"
        ),
        decreasing = TRUE
      )
  )) %>%
  ggplot(aes(x = value, y = name, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.11, 0.89),
    quantile_fun = my_hpdi,
    scale = 2, bandwidth = 0.0500, show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c("0", fill_alpha("purple", 0.75), "0"),
  ) +
  xlim(-2, 2) +
  xlab("b_auth") +
  ylab("Autor") +
  scale_y_discrete(expand = expansion(mult = c(0.01, .13))) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16)
  ) +
  theme_ridges()
dev.off()

png(
  "../fala/plots/rainbow_author.png",
  units = "px", width = 1600, height = 2000, res = 300
)
as_tibble(samples$b_auth) %>%
  rename(
    "Aeschines" = V1,
    "Aeschylus" = V2,
    "Andocides" = V3,
    "Antiphon" = V4,
    "Demosthenes" = V5,
    "Euripides" = V6,
    "Herodotus" = V7,
    "Isaeus" = V8,
    "Isocrates" = V9,
    "Lycurgus" = V10,
    "Lysias" = V11,
    "Plato" = V12,
    "Sophocles" = V13,
    "Thucydides" = V14,
    "Xenophon" = V15
  ) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(
    name,
    level =
      sort(
        c(
          "Aeschines", "Aeschylus", "Andocides", "Antiphon", "Demosthenes", "Euripides", "Herodotus",
          "Isaeus", "Isocrates", "Lycurgus", "Lysias", "Plato", "Sophocles", "Thucydides", "Xenophon"
        ),
        decreasing = TRUE
      )
  )) %>%
  ggplot(aes(x = name, y = value, width = after_stat(density), fill = name)) +
  geom_vridgeline(
    stat = "ydensity",
    trim = FALSE,
    alpha = 0.4,
    scale = 2,
    show.legend = FALSE,
    lwd = 0.1
  ) +
  xlab("auth") +
  ylab("b_auth") +
  theme_ridges() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1))
dev.off()

# Average effect
(a <- precis(model, pars = c("a_bar", "s_sent")))
sink("../fala/tables/model_average_effect.tex")
tibble(
  `Average effect` = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(
    `Average effect` = c(
      "$\\bar{\\alpha}$",
      "$\\sigma_{\\text{sent}}$"
    )
  ) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

# Averages
png(
  "../fala/plots/posterior_a_bar.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_a <- density(samples$a_bar, bw = 0.086)
density_a <- tibble(x = density_a$x, y = density_a$y)
boundaries <- HPDI(samples$a_bar)
density_a %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_a,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("a") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()

png(
  "../fala/plots/alpha_cor.png",
  units = "px", width = 1600, height = 1600, res = 300
)
color_scheme_set("teal")
mcmc_scatter(
  model@cstanfit$draws(c("a_bar", "b_dia_bar"))
)
dev.off()

png(
  "../fala/plots/rainbow_sent.png",
  units = "px", width = 3200, height = 2000, res = 300
)
as_tibble(samples$a_sent) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name)) %>%
  ggplot(aes(x = name, y = value, width = after_stat(density), fill = name)) +
  geom_vridgeline(
    stat = "ydensity",
    trim = FALSE,
    alpha = 0.1,
    scale = 40,
    show.legend = FALSE,
    lwd = 0.01
  ) +
  geom_hline(yintercept = mean(samples$a_bar), alpha = 0.2) +
  xlab("sentence") +
  ylab("a_sent") +
  theme_ridges() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
dev.off()


tibble(b_dia_bar = samples$b_dia_bar, a_bar = samples$a_bar) %>%
  ggplot(aes(x = a_bar, y = b_dia_bar)) +
  geom_density2d_filled()
