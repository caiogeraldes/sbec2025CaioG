library(tidyverse)
library(kableExtra)
options(knitr.kable.NA = "--")

# Loading data
dados <- read_csv("./data.csv") %>%
  mutate(
    pos_pred_a = factor(pred_a_pos, levels = c("P", "A", "N")),
    OBJ_TH = factor(OBJ_TH, levels = c("Recipient", "Experiencer", "Agent")),
    GENRE = factor(GENRE),
    autor_legivel = fct_recode(autor_legivel,
      "Ésquines" = "Aeschines",
      "Ésquilo" = "Aeschylus",
      "Andócides" = "Andocides",
      "Antifonte" = "Antiphon",
      "Demóstenes" = "Demosthenes",
      "Eurípides" = "Euripides",
      "Heródoto" = "Herodotus",
      "Iseu" = "Isaeus",
      "Isócrates" = "Isocrates",
      "Licurgo" = "Lycurgus",
      "Lísias" = "Lysias",
      "Platão" = "Plato",
      "Sófocles" = "Sophocles",
      "Tucídides" = "Thucydides",
      "Xenofonte" = "Xenophon"
    ),
    genre = fct_recode(genre,
      "drama" = "drama",
      "historiografia" = "historiography",
      "diálogo filosófico" = "philosophical_dialogue",
      "outro" = "other",
      "jurídico" = "juridical"
    )
  )



ld <- dados %>%
  filter(ID %in% c(35, 2859, 4524, 4586, 6178, 7279, 9055, 10057, 10220))
sink("../src/tables/data_anot_sample_a.tex")
cat("% TeX root=../main.tex\n\n\\scalebox{0.7}{")
ld %>%
  select(
    !c(autor_legivel, CHECK, Anot, pred_a_pos, pred_c_pos, pred_b_pos, pred_d_pos)
  ) %>%
  select(ID:VM_PERSONAL) %>%
  arrange(ID) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("r", "l", "l", "l", "p{12cm}", "l", "l", "l")
  )
cat("}\n")
sink()
sink("../src/tables/data_anot_sample_b.tex")
cat("% TeX root=../main.tex\n\n\\scalebox{0.7}{")
ld %>%
  select(
    !c(autor_legivel, CHECK, Anot, pred_a_pos, pred_c_pos, pred_b_pos, pred_d_pos)
  ) %>%
  arrange(ID) %>%
  select(ID, VM_MOD:OBJ_C) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
  )
cat("}\n")
sink()
sink("../src/tables/data_anot_sample_c.tex")
cat("% TeX root=../main.tex\n\n\\scalebox{0.7}{")
ld %>%
  select(
    !c(autor_legivel, CHECK, Anot, pred_a_pos, pred_c_pos, pred_b_pos, pred_d_pos)
  ) %>%
  arrange(ID) %>%
  select(ID, Pred:Variation) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
  )
cat("}\n")
sink()


sink("../src/tables/per_author.tex")
cat("% TeX root=../main.tex\n\n")
dados %>%
  select(autor_legivel) %>%
  arrange(autor_legivel) %>%
  table() %>%
  as_tibble() %>%
  mutate(prop = str_c(round(n / sum(n) * 100, 2), "%")) %>%
  add_row(autor_legivel = "Total", "n" = nrow(dados), prop = NA) %>%
  kable(
    format = "latex",
    col.names = c("Autor", "Freq.", "Prop."),
    booktabs = TRUE,
    caption = "Sentenças selecionadas por autor",
    label = "porautor",
    position = "!htb"
  ) %>%
  stringr::str_replace("\\\\addlinespace\nTotal", "\\\\midrule\nTotal") %>%
  cat() %>%
  cat("\n")
sink()

sink("../src/tables/per_genre.tex")
cat("% TeX root=../main.tex\n\n")
dados %>%
  select(genre) %>%
  table() %>%
  as_tibble() %>%
  mutate(prop = str_c(round(n / sum(n) * 100, 2), "%")) %>%
  kable(
    format = "latex",
    col.names = c("Gênero", "Freq.", "Prop."),
    booktabs = TRUE,
    caption = "Sentenças selecionadas por gênero textual",
    label = "porgenero",
    position = "!htb"
  )
sink()

sink("../src/tables/freq_attr.tex")
cat("% TeX root=../main.tex\n\n")
a <- dados %>%
  select(Attr) %>%
  table() %>%
  chisq.test()
dados %>%
  select(Attr) %>%
  table() %>%
  as_tibble() %>%
  mutate(prop = str_c(round(n / sum(n) * 100, 2), "%")) %>%
  kable(
    format = "latex",
    col.names = c("Atração", "Freq.", "Prop."),
    booktabs = TRUE,
    caption = "Frequência da atração de caso",
    label = "atração",
    position = "!htb"
  ) %>%
  str_replace(
    "\\\\bottomrule\n\\\\end\\{tabular\\}\n\\\\end\\{table\\}",
    "\\\\midrule\n"
  ) %>%
  stringr::str_c(
    "\\multicolumn{3}{c}{$\\chi^2$ = ",
    round(a$statistic, 2),
    " (1) p = ",
    round(a$p.value, 2),
    "}\\\\"
  ) %>%
  str_c("\n\\bottomrule\n\\end{tabular}\n\\end{table}\n") %>%
  cat()
sink()
