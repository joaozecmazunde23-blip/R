==============================================================================
  # Trabalho Prático I - Análise de Dados (Análise Exploratória)
  # Tema: Análise dos Factores Associados ao Efeito do Novo Tratamento na Perda de Peso
  # Objectivos: OE1 perfil | OE2 comparar perda entre grupos | OE3 factores associados
  # Entrada : BasedeDados.txt (TAB, formato longo) | Saídas: ./tabelas/*.csv, ./fig/*.pdf
  # ==============================================================================
library(dplyr); library(tidyr); library(readr); library(ggplot2)
dir.create("tabelas", showWarnings = FALSE); dir.create("fig", showWarnings = FALSE)
G <- c("Controlo", "Placebo", "Tratamento")

# ---------------------------------------------------------------- 1. Leitura e consistência
bruto <- read.delim("C:/Users/asus/Downloads/BasedeDados.txt") %>%
  mutate(across(where(is.character), trimws))
cat("Linhas:", nrow(bruto), " | Valores omissos:", sum(is.na(bruto)), "\n")
cat("Medições por indivíduo (deve ser só 2):\n"); print(table(table(bruto$ID)))
cat("Duplicados ID x VISIT:", sum(duplicated(bruto[, c("ID", "VISIT")])), "\n")
const <- bruto %>% group_by(ID) %>%
  summarise(across(c(TREAT, AGE, LIVINGSTATUS, SMOKESTATUS, GENDER), n_distinct), .groups = "drop")
cat("Indivíduos com covariáveis inconsistentes:", sum(const[, -1] != 1), "\n")

# ---------------------------------------------------------------- 2. Formato largo e variáveis derivadas
d <- bruto %>%
  pivot_wider(id_cols = c(ID, TREAT, AGE, LIVINGSTATUS, SMOKESTATUS, GENDER),
              names_from = VISIT, values_from = WEIGHT) %>%
  mutate(
    PERDA     = PRE - POST,
    PERDA_PCT = 100 * PERDA / PRE,
    RESP      = PERDA_PCT >= 5,
    RESULTADO = case_when(PERDA > 0 ~ "Perdeu peso", PERDA < 0 ~ "Ganhou peso", TRUE ~ "Manteve o peso"),
    TREAT     = factor(recode(TREAT, CONTROL = "Controlo", PLACEBO = "Placebo", TREATMENT = "Tratamento"), levels = G),
    GENERO    = recode(GENDER, FEMALE = "Feminino", MALE = "Masculino"),
    FUMO      = factor(recode(SMOKESTATUS, SMOKER = "Fumador", `QUIT-SMOKER` = "Ex-fumador", `NON-SMOKER` = "Não fumador"),
                       levels = c("Fumador", "Ex-fumador", "Não fumador")),
    VIVE      = factor(recode(LIVINGSTATUS,
                              `ALONE (SINGLE)` = "Sozinho (solteiro)", `ALONE (DIVORCED)` = "Sozinho (divorciado)",
                              `ALONE (WIDOWER)` = "Sozinho (viúvo)", `WITH FRIENDS` = "Com amigos",
                              `WITH PARTNER` = "Com parceiro(a)", `WITH PARTNER AND CHILDREN` = "Com parceiro(a) e filhos"),
                       levels = c("Sozinho (solteiro)", "Sozinho (divorciado)", "Sozinho (viúvo)",
                                  "Com amigos", "Com parceiro(a)", "Com parceiro(a) e filhos")),
    FAIXA     = cut(AGE, breaks = c(19, 34, 49, 65), labels = c("20-34", "35-49", "50-65"))
  ) %>% rename(IDADE = AGE)
write_csv(d, "tabelas/dados_largos.csv")

# ---------------------------------------------------------------- OE1. PERFIL DOS INDIVÍDUOS
resumo <- function(x) sprintf("%.1f (%.1f)", mean(x), sd(x))
perfil_cont <- d %>% group_by(TREAT) %>% summarise(Idade = resumo(IDADE), Peso_inicial = resumo(PRE), .groups = "drop")
print(perfil_cont)
print(d %>% summarise(Idade = resumo(IDADE), Peso_inicial = resumo(PRE), min_idade = min(IDADE), max_idade = max(IDADE)))
for (v in c("GENERO", "FUMO", "VIVE")) {
  cat("\n---", v, "---\n"); tab <- table(d[[v]], d$TREAT); print(tab)
  print(round(100 * prop.table(tab, 2), 1))
}
write_csv(perfil_cont, "tabelas/perfil_continuas.csv")

## Gráficos circulares do perfil
pie_perfil <- function(var, titulo, ficheiro) {
  freq <- d %>% count(nivel = .data[[var]]) %>%
    mutate(pct = round(100 * n / sum(n), 1),
           rotulo = paste0(nivel, " (", pct, "%)"))
  p <- ggplot(freq, aes(x = "", y = n, fill = nivel)) +
    geom_bar(width = 1, stat = "identity", colour = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = rotulo), position = position_stack(vjust = 0.5), size = 3) +
    labs(title = titulo, fill = NULL) + theme_void()
  ggsave(ficheiro, p, width = 6.5, height = 5, dpi = 150)
}
pie_perfil("GENERO", "Perfil da amostra: género", "fig/pie_genero.pdf")
pie_perfil("FUMO", "Perfil da amostra: estado de fumador", "fig/pie_fumo.pdf")
pie_perfil("VIVE", "Perfil da amostra: condição de vida", "fig/pie_vive.pdf")

# ---------------------------------------------------------------- OE2. COMPARAR PERDA ENTRE GRUPOS
global <- d %>% summarise(Media_PRE = mean(PRE), Media_POST = mean(POST), Perda_media = mean(PERDA),
                          DP_perda = sd(PERDA), Maior_perda = max(PERDA), Menor_perda = min(PERDA))
print(round(global, 2)); write_csv(global, "tabelas/global.csv")

por_grupo <- d %>% group_by(TREAT) %>%
  summarise(n = n(), PRE = mean(PRE), POST = mean(POST), Media = mean(PERDA), DP = sd(PERDA),
            Mediana = median(PERDA), Minimo = min(PERDA), Maximo = max(PERDA),
            Perda_pct = mean(PERDA_PCT), Sem_perda = sum(PERDA <= 0),
            Resp5 = sum(RESP), Resp5_pct = 100 * mean(RESP), .groups = "drop")
print(por_grupo %>% mutate(across(where(is.numeric), ~ round(.x, 2))))
write_csv(por_grupo, "tabelas/por_grupo.csv")

## Identificação explícita: quem GANHOU e quem PERDEU peso
d %>% count(TREAT, RESULTADO) %>% group_by(TREAT) %>% mutate(pct = round(100 * n / sum(n), 1))
ganhou <- d %>% filter(PERDA < 0) %>%
  select(ID, TREAT, GENERO, IDADE, FUMO, VIVE, PRE, POST, PERDA) %>% arrange(TREAT, PERDA)
perdeu <- d %>% filter(PERDA > 0) %>%
  select(ID, TREAT, GENERO, IDADE, FUMO, VIVE, PRE, POST, PERDA) %>% arrange(TREAT, desc(PERDA))
print(ganhou); print(perdeu)
write_csv(ganhou, "tabelas/ganhou_peso.csv")
write_csv(perdeu, "tabelas/perdeu_peso.csv")

tema <- theme_minimal(base_size = 10) + theme(legend.position = "bottom", legend.title = element_blank())
cores <- c(Controlo = "#8c8c8c", Placebo = "#e0a030", Tratamento = "#1f6fb2")

## Fig 1a: boxplots do peso por grupo e visita
longo <- d %>% pivot_longer(c(PRE, POST), names_to = "Visita", values_to = "Peso") %>%
  mutate(Visita = factor(recode(Visita, PRE = "Inicial", POST = "6 meses"), levels = c("Inicial", "6 meses")))
f1a <- ggplot(longo, aes(TREAT, Peso, fill = Visita)) + geom_boxplot(width = .6, outlier.size = .8) +
  scale_fill_manual(values = c("#bcd3e8", "#1f6fb2")) +
  labs(x = NULL, y = "Peso (kg)", title = "(a) Peso inicial e aos 6 meses") + tema

## Fig 1b: perda por grupo
f1b <- ggplot(d, aes(TREAT, PERDA, fill = TREAT)) + geom_boxplot(width = .5, outlier.shape = NA, alpha = .8) +
  geom_jitter(width = .15, size = 1, alpha = .5) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  scale_fill_manual(values = cores) +
  labs(x = NULL, y = "Perda de peso (kg)", title = "(b) Perda de peso por grupo") + tema + theme(legend.position = "none")

## Fig 1c: interacção tratamento x tempo (médias ± dp)
media_peso <- longo %>% group_by(TREAT, Visita) %>% summarise(m = mean(Peso), dp = sd(Peso), .groups = "drop")
f1c <- ggplot(media_peso, aes(Visita, m, group = TREAT, colour = TREAT)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  geom_errorbar(aes(ymin = m - dp, ymax = m + dp), width = .08) +
  scale_colour_manual(values = cores) +
  labs(x = NULL, y = "Peso médio (kg)", title = "(c) Interacção tratamento × tempo", colour = NULL) + tema
ggsave("fig/fig1a.pdf", f1a, width = 5, height = 3.9)
ggsave("fig/fig1b.pdf", f1b, width = 5, height = 3.9)
ggsave("fig/fig1c_interaccao.pdf", f1c, width = 5, height = 3.9)

# ---------------------------------------------------------------- OE3. FACTORES ASSOCIADOS
subgrupo <- function(v) {
  d %>% group_by(TREAT, nivel = .data[[v]]) %>%
    summarise(n = n(), media = mean(PERDA), dp = sd(PERDA), mediana = median(PERDA),
              resp_pct = 100 * mean(RESP), .groups = "drop") %>% mutate(factor = v)
}
sub_all <- bind_rows(lapply(c("GENERO", "FUMO", "VIVE", "FAIXA"),
                            function(v) subgrupo(v) %>% mutate(nivel = as.character(nivel))))
print(sub_all %>% mutate(across(where(is.numeric), ~ round(.x, 2))), n = 100)
write_csv(sub_all, "tabelas/subgrupos.csv")

trat <- d %>% filter(TREAT == "Tratamento")
print(table(trat$GENERO, trat$FUMO))
print(d %>% group_by(TREAT) %>%
        summarise(r_idade = round(cor(IDADE, PERDA), 2),
                  r_peso_inicial = round(cor(PRE, PERDA), 2), .groups = "drop"))

## Fig 2: perda por grupo segundo género, tabagismo e condição de vida
lg <- d %>% select(TREAT, PERDA, Género = GENERO, Tabagismo = FUMO, `Condição de vida` = VIVE) %>%
  mutate(across(c(Género, Tabagismo, `Condição de vida`), as.character)) %>%
  pivot_longer(c(Género, Tabagismo, `Condição de vida`), names_to = "factor", values_to = "nivel") %>%
  mutate(factor = factor(factor, levels = c("Género", "Tabagismo", "Condição de vida")))
f2 <- ggplot(lg, aes(TREAT, PERDA, fill = nivel)) + geom_boxplot(outlier.size = .6, linewidth = .3) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed", linewidth = .3) +
  facet_wrap(~ factor, scales = "free_x") + labs(x = NULL, y = "Perda de peso (kg)") + tema +
  guides(fill = guide_legend(nrow = 3))
ggsave("fig/fig2_subgrupos.pdf", f2, width = 11, height = 4.4)

## Fig 3: perda vs idade e vs peso inicial
lc <- d %>% pivot_longer(c(IDADE, PRE), names_to = "var", values_to = "x") %>%
  mutate(var = recode(var, IDADE = "(a) Idade (anos)", PRE = "(b) Peso inicial (kg)"))
f3 <- ggplot(lc, aes(x, PERDA, colour = TREAT)) + geom_point(size = 1.2, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = .7) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed", linewidth = .3) +
  scale_colour_manual(values = cores) +
  facet_wrap(~ var, scales = "free_x") + labs(x = NULL, y = "Perda de peso (kg)") + tema
ggsave("fig/fig3_continuas.pdf", f3, width = 10, height = 3.8)

## Fig 4: respondedores (>=5%) no grupo tratamento — VERSÃO ÚNICA, cores correctas
lr <- trat %>% select(RESP, Género = GENERO, Tabagismo = FUMO, `Condição de vida` = VIVE) %>%
  mutate(across(-RESP, as.character)) %>%
  pivot_longer(-RESP, names_to = "factor", values_to = "nivel") %>%
  group_by(factor, nivel) %>% summarise(Responderam = 100 * mean(RESP), n = n(), .groups = "drop") %>%
  mutate(`Não responderam` = 100 - Responderam, etiqueta = paste0(nivel, " (n=", n, ")")) %>%
  pivot_longer(c(Responderam, `Não responderam`), names_to = "res", values_to = "pct") %>%
  mutate(res = factor(res, levels = c("Não responderam", "Responderam")),
         factor = factor(factor, levels = c("Género", "Tabagismo", "Condição de vida")))
rotulos <- lr %>% filter(res == "Responderam") %>%
  mutate(txt = paste0(round(pct), "%"), posicao = 100 - pct / 2)
f4 <- ggplot(lr, aes(pct, etiqueta, fill = res)) + geom_col(width = .7) +
  geom_text(data = rotulos, aes(x = posicao, y = etiqueta, label = txt),
            inherit.aes = FALSE, colour = "black", size = 3, fontface = "bold") +
  scale_fill_manual(values = c(Responderam = "#1f6fb2", `Não responderam` = "#d9d9d9")) +
  facet_grid(factor ~ ., scales = "free_y", space = "free_y") +
  labs(x = "% dos indivíduos do grupo tratamento", y = NULL) + tema
ggsave("fig/fig4_resposta.pdf", f4, width = 7, height = 5.2)

cat("\nConcluído: tabelas em ./tabelas e figuras em ./fig\n")
'''