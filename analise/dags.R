library(tidyverse)
library(kableExtra)
library(rethinking)
library(dagitty)
library(ggdag)

png(
  "../src/plots/dag_non_canonical_agreement.png",
  units = "px", width = 800, height = 800, res = 200
)
dagitty("dag{
 C -> A <- D
 Al -> A
}") %>%
  tidy_dagitty() %>%
  ggdag(text_size = 10) +
  theme_dag_blank()
dev.off()

# Loading data
dados <- read_csv("./data.csv")

# DAG EX
dag_ex <- dagitty::dagitty("
  dag{
    A
    Cop
    D
    D -> A <- C
    Al -> A
    Cop -> A
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "resultado",
      "D" = "não-observado",
      "Al" = "não-observado",
      "C" = "não-observado",
      "Cop" = "observado"
    )
  )
png("../src/plots/dag_ex.png", units = "px", width = 1600, height = 1600, res = 300)
dag_ex %>%
  ggdag() +
  geom_dag_node(aes(color = label)) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()


# Domain
dag_domain <- dagitty("
    dag {
    A
    D
    PoS
    C
    D -> A
    D -> C -> A
    PoS -> A <- PoS
    C -> PoS
    }
  ") %>%
  tidy_dagitty()


dag_cop <- dag_domain %>%
  dag_label(labels = c("A" = "resultado", "D" = "latente", "C" = "exposição"))
png("../src/plots/dag_cop.png", units = "px", width = 1600, height = 1600, res = 300)
dag_cop %>% ggdag_paths_fan(
  from = "PoS", to = "A", shadow = TRUE, node = FALSE, text = FALSE
) +
  geom_dag_point(aes(color = label)) +
  geom_dag_text(size = 5) +
  theme_dag_blank(legend.position = "none")
dev.off()

png("../src/plots/dag_pos.png", units = "px", width = 1600, height = 1600, res = 300)
dag_pos <- dag_domain %>%
  dag_label(labels = c("A" = "resultado", "D" = "latente", "PoS" = "exposição"))
dag_pos %>% ggdag_paths_fan(
  from = "PoS", to = "A", shadow = TRUE, node = FALSE, text = FALSE
) +
  geom_dag_point(aes(color = label)) +
  geom_dag_text(size = 5) +
  theme_dag_blank(legend.position = "none")
dev.off()

# Controller
dag_controller <- dagitty::dagitty("
  dag{
    A
    ModV
    Aut -> Dia
    Aut -> G
    Dia -> A
    G -> ModV
    ModV -> A
  }
")

a <- table(
  dados$DIALECT,
  dados$AUTHOR,
  dados$VM_MOD,
  dados$Attr
)
# Attic
chisq.test(a[1, , 1, ], correct = TRUE)
chisq.test(a[1, , 2, ], correct = TRUE)
# Jonic (Only Herodotus)
chisq.test(a[2, , 1, ], correct = TRUE)
chisq.test(a[2, , 2, ], correct = TRUE)

dag_general <- dagitty::dagitty("
  dag{
    A
    PoS
    ModV
    OP
    G -> Dia
    Aut -> G
    Aut -> OP
    Cop -> A
    Cop -> PoS
    Dia -> A
    G -> ModV
    PoS -> A
    ModV -> A
    OP -> A
  }
")



dag_a <- dagitty::dagitty("
  dag{
    A
    PoS
    Cop
    D
    D -> A <- PoS <- Cop
    Cop -> A
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "resultado",
      "D" = "não-observado",
      "Cop" = "observado",
      "PoS" = "observado"
    )
  )
png("../src/plots/dag_a.png", units = "px", width = 1600, height = 1600, res = 300)
dag_a %>%
  ggdag() +
  geom_dag_node(aes(color = label)) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()

dag_b <- dagitty::dagitty("
  dag{
    A
    PoS
    Cop
    D
    C -> A
    D -> A <- PoS <- Cop
    Cop -> A
    Cop -> Th -> A
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "resultado",
      "D" = "não-observado",
      "C" = "não-observado",
      "Cop" = "observado",
      "PoS" = "observado",
      "Th" = "observado"
    )
  )

png("../src/plots/dag_b.png", units = "px", width = 1600, height = 1600, res = 300)
dag_b %>%
  ggdag() +
  geom_dag_node(aes(color = label)) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()


dag_c <- dagitty::dagitty("
  dag{
    A
    PoS
    Cop
    D
    C -> A
    D -> A <- PoS <- Cop
    Cop -> A
    Cop -> Th -> A
    Aut -> G -> Th
    Aut -> A <- Dia <- Aut
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "resultado",
      "D" = "não-observado",
      "C" = "não-observado",
      "Cop" = "observado",
      "PoS" = "observado",
      "OP" = "observado",
      "Th" = "observado",
      "Aut" = "observado",
      "G" = "observado",
      "Dia" = "observado"
    )
  )

png("../src/plots/dag_c.png", units = "px", width = 1600, height = 1600, res = 300)
dag_c %>%
  ggdag() +
  geom_dag_node(aes(color = label)) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()


dag_d <- dagitty::dagitty("
  dag{
    A
    PoS
    Cop
    D
    C -> A
    D -> A <- PoS <- Cop
    Cop -> A
    Cop -> Th -> A
    Al -> A
    Aut -> G -> Th
    Aut -> A <- Dia <- G
    Al -> OP -> A <- Dist
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "resultado",
      "D" = "não-observado",
      "Al" = "não-observado",
      "C" = "não-observado",
      "Cop" = "observado",
      "PoS" = "observado",
      "OP" = "observado",
      "Th" = "observado",
      "Aut" = "observado",
      "G" = "observado",
      "Dia" = "observado",
      "Dist" = "observado"
    )
  )

png("../src/plots/dag_d.png", units = "px", width = 1600, height = 1600, res = 300)
dag_d %>%
  ggdag() +
  geom_dag_node(aes(color = label)) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()


dag_e <- dagitty::dagitty("
  dag{
    A
    PoS
    Cop
    D
    C -> A
    D -> A <- PoS <- Cop
    Cop -> A
    Cop -> Th -> A
    Al -> A
    Aut -> G -> Th
    Aut -> A <- Dia <- G
    Al -> OP -> A <- Dist
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "resultado",
      "D" = "não-observado",
      "Al" = "não-observado",
      "C" = "não-observado",
      "Cop" = "exposição",
      "PoS" = "exposição",
      "OP" = "não-ajustado",
      "Th" = "exposição",
      "Aut" = "exposição",
      "Dia" = "exposição",
      "Dist" = "exposição",
      "G" = "exposição"
    )
  )

png("../src/plots/dag_e.png", units = "px", width = 1600, height = 1600, res = 300)
dag_e %>%
  ggdag() +
  geom_dag_node(aes(color = label)) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()

dag_e <- dagitty::dagitty("
  dag{
    A [outcome]
    Dia[exposure]
    PoS
    Cop
    D
    C -> A
    D -> A <- PoS <- Cop
    Cop -> A
    Cop -> Th -> A
    Al -> A
    Aut -> G -> Th
    Aut -> A <- Dia <- G
    Al -> OP <- Dist -> A
  }
")

dagitty::adjustmentSets(dag_e)
