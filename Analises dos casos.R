# ==============================================================================
# SCRIPT COMPLETO: ANÁLISE DE SÉRIE TEMPORAL INTERROMPIDA (TB PEDIÁTRICA)
# ==============================================================================
# Objetivo: 
# 1. Analisar o impacto GERAL da pandemia (todas as crianças < 5 anos).
# 2. Analisar o impacto ESTRATIFICADO por raça/cor.
# Metodologia: ITS com agregação TRIMESTRAL e modelo Quasipoisson.
# ==============================================================================

# --- 1. CONFIGURAÇÃO INICIAL E PACOTES ---
if(!require(pacman)) install.packages("pacman")
pacman::p_load(read.dbc, dplyr, tidyr, purrr, lubridate, broom, ggplot2, scales)

# DEFINA SEU DIRETÓRIO AQUI
setwd("/home/Ramos/Documentos/R/SHORT TB/arquivo/DADOS BRUTOS/SINAN/")
arquivos <- list.files(pattern = "\\.dbc$", full.names = TRUE)

# --- 2. LEITURA E LIMPEZA DOS DADOS (SINAN) ---
dados_todos <- arquivos |>
  map_df(~ {
    message(paste("Lendo:", basename(.x)))
    df <- read.dbc(.x)
    # Tratamento seguro para idade
    if("NU_IDADE_N" %in% names(df)) df$NU_IDADE_N <- as.character(df$NU_IDADE_N)
    df
  })

# Função para converter idade
converter_idade_anos <- function(x) {
  x <- as.numeric(as.character(x))
  unidade <- floor(x / 1000)
  valor   <- x %% 1000
  case_when(
    is.na(x) ~ NA_real_,
    unidade == 5 ~ 100 + valor,
    unidade == 4 ~ valor,
    unidade == 3 ~ valor / 12,
    unidade == 2 ~ valor / 365,
    unidade == 1 ~ valor / (365 * 24),
    TRUE ~ NA_real_
  )
}

dados_processados <- dados_todos |>
  mutate(idade_anos = converter_idade_anos(NU_IDADE_N)) |>
  filter(idade_anos < 5) |>
  mutate(
    data_diagnostico = as.Date(DT_DIAG),
    # AGREGACAO TRIMESTRAL (Essencial para estabilidade)
    data_trimestre = floor_date(data_diagnostico, "quarter"),
    ano = year(data_diagnostico),
    raca_descricao = case_when(
      as.character(CS_RACA) == "1" ~ "Branca",
      as.character(CS_RACA) == "2" ~ "Preta",
      as.character(CS_RACA) == "3" ~ "Amarela",
      as.character(CS_RACA) == "4" ~ "Parda",
      as.character(CS_RACA) == "5" ~ "Indígena",
      TRUE ~ "Ignorado/Outros"
    )
  )

# --- 3. PREPARAÇÃO DA POPULAÇÃO E BASES DE DADOS ---
setwd("/home/Ramos/Documentos/R/SHORT TB/arquivo/")
pop <- read.csv2("pop.csv") 

pop_longa <- pop |>
  pivot_longer(
    cols = c(branca, preta, amarela, parda, indigenea),
    names_to = "raca_origem",
    values_to = "populacao"
  ) |>
  mutate(
    raca_descricao = case_when(
      raca_origem == "branca" ~ "Branca",
      raca_origem == "preta" ~ "Preta",
      raca_origem == "amarela" ~ "Amarela",
      raca_origem == "parda" ~ "Parda",
      raca_origem == "indigenea" ~ "Indígena",
      TRUE ~ raca_origem
    )
  ) |>
  select(Ano, raca_descricao, populacao)

# --- 4. CRIAÇÃO DA BASE TRIMESTRAL COMPLETA (ZERO-FILLING) ---
min_data <- min(dados_processados$data_diagnostico, na.rm = TRUE)
max_data <- max(dados_processados$data_diagnostico, na.rm = TRUE)
seq_trimestres <- seq(floor_date(min_data, "quarter"), floor_date(max_data, "quarter"), by = "quarter")

# Contagem inicial
contagem_raca <- dados_processados |>
  count(data_trimestre, raca_descricao) |>
  rename(data_base = data_trimestre)

# Base Final para Análise (Preenchendo vazios e juntando pop)
dados_its_completo <- contagem_raca |>
  ungroup() |>
  complete(raca_descricao, data_base = seq_trimestres, fill = list(n = 0)) |>
  mutate(ano = year(data_base)) |>
  left_join(pop_longa, by = c("ano" = "Ano", "raca_descricao" = "raca_descricao")) |>
  group_by(raca_descricao) |>
  fill(populacao, .direction = "downup") |>
  ungroup() |>
  filter(raca_descricao != "Ignorado/Outros") |>
  arrange(raca_descricao, data_base) |>
  group_by(raca_descricao) |>
  mutate(
    taxa_100k = (n / populacao) * 100000,
    tempo = row_number(),
    # Intervenção no Q2 (Abril) de 2020
    intervencao = if_else(data_base >= "2020-04-01", 1, 0),
    tempo_apos = cumsum(intervencao)
  ) |>
  ungroup()

# ==============================================================================
# ANÁLISE 1: PANORAMA GERAL (TOTAL BRASIL < 5 ANOS)
# ==============================================================================

# Agregar todas as raças
dados_geral <- dados_its_completo |>
  group_by(data_base, tempo, intervencao, tempo_apos) |>
  summarise(
    n = sum(n),
    populacao = sum(populacao),
    .groups = "drop"
  ) |>
  mutate(taxa_100k = (n / populacao) * 100000)

# Modelo Geral
modelo_geral <- glm(n ~ tempo + intervencao + tempo_apos + offset(log(populacao)),
                    family = quasipoisson(link = "log"), data = dados_geral)

# Resultados Gerais
res_geral <- tidy(modelo_geral, exponentiate = TRUE, conf.int = TRUE) |>
  filter(term == "intervencao") |>
  mutate(
    Cenario = "TOTAL BRASIL (<5 anos)",
    Queda_Percentual = round((1 - estimate) * 100, 2),
    Significativo = ifelse(p.value < 0.05, "Sim", "Não")
  ) |>
  select(Cenario, RR = estimate, IC_Inf = conf.low, IC_Sup = conf.high, P_valor = p.value, Queda_Percentual)

print("=== RESULTADO 1: IMPACTO GERAL (MÉDIA NACIONAL) ===")
print(res_geral)

# Gráfico Geral
ggplot(dados_geral, aes(x = data_base, y = taxa_100k)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"), 
              aes(group = intervencao), color = "black", se = TRUE) +
  geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dashed", color = "red") +
  labs(title = "Impacto Geral da Pandemia na TB Pediátrica (0-4 anos)",
       subtitle = "Brasil, 2014-2023 (Agregação Trimestral)",
       y = "Taxa por 100.000 hab.", x = "Ano") +
  theme_minimal()

# ==============================================================================
# ANÁLISE 2: ESTRATIFICADO POR ETNIA (AS DESIGUALDADES)
# ==============================================================================

modelos_etnia <- dados_its_completo |>
  nest(dados = -raca_descricao) |>
  mutate(
    modelo = map(dados, ~ glm(n ~ tempo + intervencao + tempo_apos + offset(log(populacao)), 
                              family = quasipoisson(link = "log"), data = .x)),
    resultados = map(modelo, ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE)),
    predicoes = map2(dados, modelo, ~ mutate(.x, predito = predict(.y, type = "response") / populacao * 100000))
  )

tabela_etnia <- modelos_etnia |>
  unnest(resultados) |>
  filter(term == "intervencao") |>
  select(Raca = raca_descricao, RR = estimate, IC_Inf = conf.low, IC_Sup = conf.high, P_valor = p.value) |>
  mutate(
    Queda_Percentual = round((1 - RR) * 100, 2),
    Significativo = ifelse(P_valor < 0.05, "Sim", "Não")
  )

print("=== RESULTADO 2: IMPACTO POR ETNIA (DESIGUALDADES) ===")
print(tabela_etnia)

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(lubridate)

# 1. Carregar os dados
dados <- read.csv("casos_trimestrais_corrigido.csv") |>
  mutate(data_base = as.Date(data_base))

# 2. Criar o Dataset "TOTAL"
dados_total <- dados |>
  group_by(data_base, tempo, intervencao, tempo_apos) |>
  summarise(
    n = sum(n),
    populacao = sum(populacao),
    .groups = "drop"
  ) |>
  mutate(raca_descricao = "Total")

# 3. Juntar e Traduzir
dados_completo <- bind_rows(dados_total, dados) |>
  mutate(raca_descricao = case_when(
    raca_descricao == "Total" ~ "Total",
    raca_descricao == "Indígena" ~ "Indigenous",
    raca_descricao == "Preta" ~ "Black",
    raca_descricao == "Parda" ~ "Brown",
    raca_descricao == "Branca" ~ "White",
    raca_descricao == "Amarela" ~ "Asian",
    TRUE ~ raca_descricao
  )) |>
  mutate(raca_descricao = factor(raca_descricao, levels = c(
    "Total", "Indigenous", "Black", "Brown", "White", "Asian"
  ))) |>
  filter(!is.na(raca_descricao))

# 4. Ajustar Modelos e Calcular Intervalos de Confiança (A PARTE IMPORTANTE)
dados_final <- dados_completo |>
  nest(dados = -raca_descricao) |>
  mutate(
    modelo = map(dados, ~ glm(n ~ tempo + intervencao + tempo_apos + offset(log(populacao)), 
                              family = quasipoisson(link = "log"), data = .x)),
    
    predicoes = map2(dados, modelo, ~ {
      # Predizer na escala do Link (Log) para calcular IC corretamente
      pred_link <- predict(.y, type = "link", se.fit = TRUE)
      
      .x |> mutate(
        # Valor ajustado (linha)
        fit_resp = exp(pred_link$fit),
        taxa_pred = (fit_resp / populacao) * 100000,
        
        # Intervalo de Confiança (95%)
        # Calculamos no log e depois exponencializamos para não gerar valor negativo
        upr_link = pred_link$fit + (1.96 * pred_link$se.fit),
        lwr_link = pred_link$fit - (1.96 * pred_link$se.fit),
        
        taxa_upr = (exp(upr_link) / populacao) * 100000,
        taxa_lwr = (exp(lwr_link) / populacao) * 100000,
        
        # Taxa real observada
        taxa_obs = (n / populacao) * 100000
      )
    })
  ) |>
  select(raca_descricao, predicoes) |>
  unnest(predicoes)

# 5. Gráfico "Limpo" com Intervalo de Confiança
ggplot(dados_final, aes(x = data_base)) +
  
  # A. A Faixa de Confiança (Sombra) - Vem primeiro para ficar no fundo
  geom_ribbon(aes(ymin = taxa_lwr, ymax = taxa_upr, group = intervencao), 
              fill = "firebrick", alpha = 0.15) + # Alpha baixo para ficar suave
  
  # B. Pontos Observados (Mais discretos agora)
  geom_point(aes(y = taxa_obs), color = "gray70", alpha = 0.5, size = 1.8) +
  
  # C. Linha de Tendência (Forte)
  geom_line(aes(y = taxa_pred, group = intervencao), 
            color = "firebrick", linewidth = 1) +
  
  # D. Linha da Intervenção
  geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dashed", color = "gray30") +
  
  # Configurações de Facetas e Texto
  facet_wrap(~raca_descricao, scales = "free_y", ncol = 2) +
  labs(
    title = "Solid line: Model fit | Shaded area: 95% Confidence Interval",
    
    x = "Year",
    y = "Rate per 100,000 inhabitants"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank(), # Remove grades pequenas para limpar
    panel.grid.major.x = element_blank() # Remove grades verticais (opcional, limpa mais)
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years")

ggsave("ITS_Final_Ribbon.png", width = 10, height = 12, dpi = 300)


