library(read.dbc)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(broom)
library(ggplot2)
setwd("/home/Ramos/Documentos/R/SHORT TB/arquivo/DADOS BRUTOS/SINAN/")
arquivos <- list.files(pattern = "\\.dbc$", full.names = TRUE)
# --- 1.2 Leitura dos arquivos DBC ---
dados_todos <- arquivos |>
    map_df(~ {
        message(paste("Lendo:", basename(.x)))
        df <- read.dbc(.x)
        df$arquivo_origem <- basename(.x)
    
    # Tratamento de segurança para tipos
        if("NU_IDADE_N" %in% names(df)) df$NU_IDADE_N <- as.character(df$NU_IDADE_N)
    
    df
    
  })

# --- 1.3 Tratamento da Idade (Função Segura) ---

dados_todos$NU_IDADE_N <- as.numeric(as.character(dados_todos$NU_IDADE_N))
converter_idade_anos <- function(x) {
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

dados_limpos <- dados_todos |>
   mutate(idade_anos = converter_idade_anos(NU_IDADE_N)) |>
    # Filtro: Crianças menores de 5 anos (0 a 4.99)
    filter(idade_anos < 5) |>
    mutate(
        data_diagnostico = as.Date(DT_DIAG),
        ano = year(data_diagnostico),
        mes = month(data_diagnostico),
        # Categorização de Raça/Cor
        raca_descricao = case_when(
            as.character(CS_RACA) == "1" ~ "Branca",
            as.character(CS_RACA) == "2" ~ "Preta",
            as.character(CS_RACA) == "3" ~ "Amarela",
            as.character(CS_RACA) == "4" ~ "Parda",
            as.character(CS_RACA) == "5" ~ "Indígena",
            TRUE ~ "Ignorado/Outros"
      
    )
    
  )


# --- 1.4 Contagem Inicial de Casos ---

resumo_casos <- dados_limpos |>
    count(ano, mes, raca_descricao)

# ==============================================================================

# ETAPA 2: POPULAÇÃO E PREPARAÇÃO PARA ITS (ZERO-FILLING)

# ==============================================================================



# --- 2.1 Leitura e Transformação da População ---

# Ajuste o read.csv2 ou o sep=";" conforme seu arquivo
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



# --- 2.2 Join e Preenchimento de Meses Vazios (Essencial para ITS) ---

min_data <- min(dados_limpos$data_diagnostico, na.rm = TRUE)
max_data <- max(dados_limpos$data_diagnostico, na.rm = TRUE)



# Arredondar para o dia 1 do mês para criar a sequência

seq_datas <- seq(floor_date(min_data, "month"), floor_date(max_data, "month"), by = "month")



dados_its <- resumo_casos |>
  
  # Garante que a data base existe
  
  mutate(data_base = as.Date(paste0(ano, "-", mes, "-01"))) |>
  
  ungroup() |>
  
  # Cruza todas as raças com todos os meses (preenche n=0 se não houver casos)
  
  complete(raca_descricao, data_base = seq_datas, fill = list(n = 0)) |>
  
  mutate(
        ano = year(data_base),
        mes = month(data_base)
    
  ) |>
  
  # Traz a população
  
  left_join(pop_longa, by = c("ano" = "Ano", "raca_descricao" = "raca_descricao")) |>
  
  # Preenche população se houver falhas (ex: meses criados onde o join falhou)
  
  group_by(raca_descricao) |>
    fill(populacao, .direction = "downup") |>
    ungroup() |>
  
  # Remove ignorados para a modelagem
    filter(raca_descricao != "Ignorado/Outros") |>
  
  # Cria Variáveis ITS
  
  arrange(raca_descricao, data_base) |>
    group_by(raca_descricao) |>
    mutate(
    taxa_100k   = (n / populacao) * 100000,
        tempo       = row_number(),
        intervencao = if_else(data_base >= "2020-03-01", 1, 0),
        tempo_apos  = cumsum(intervencao)
    
  ) |>
  
  ungroup()



# ==============================================================================

# ETAPA 3: MODELAGEM ESTATÍSTICA (QUASIPOISSON)

# ==============================================================================



modelos_its <- dados_its |>
    nest(dados = -raca_descricao) |>
    mutate(
    
    # Roda o modelo
    
    modelo_ajustado = map(dados, ~ glm(n ~ tempo + intervencao + tempo_apos + offset(log(populacao)), 
                                                                         family = quasipoisson(link = "log"), 
                                       
                                       data = .x)),
    
    
    
    # Extrai Tabela de Resultados (RR e IC)
    
    tabela_coeficientes = map(modelo_ajustado, ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE)),
    
    
    
    # Gera Predições para o Gráfico (Valores esperados pelo modelo)
    
    # type = "response" retorna a contagem esperada (n). Depois dividimos pela pop.
    
    dados_com_predicao = map2(dados, modelo_ajustado, ~ {
      
      .x |> mutate(predito_contagem = predict(.y, type = "response"),
                   
                   predito_taxa = (predito_contagem / populacao) * 100000)
      
    })
    
  )



# --- 3.1 Exibir Tabela Final de Resultados ---

tabela_final <- modelos_its |>
  
  unnest(tabela_coeficientes) |>
  
  select(Raca = raca_descricao, Termo = term, RR = estimate, 
         
         IC_Inf = conf.low, IC_Sup = conf.high, P_valor = p.value) |>
  
  filter(Termo == "intervencao") |>
  
  mutate(
    
    Queda_Percentual = round((1 - RR) * 100, 2),
    
    Significativo = ifelse(P_valor < 0.05, "Sim", "Não")
    
  )



print("=== RESULTADOS DA INTERVENÇÃO (MARÇO/2020) ===")

print(tabela_final)



# ==============================================================================

# ETAPA 4: GRÁFICOS CLÁSSICOS DE ITS

# ==============================================================================



# Preparar dados para plotagem (desaninhando as predições)

dados_grafico <- modelos_its |>
  
  select(raca_descricao, dados_com_predicao) |>
  
  unnest(dados_com_predicao)



# Criar o Gráfico

grafico_its <- ggplot(dados_grafico, aes(x = data_base, y = taxa_100k)) +
  
  
  
  # 1. Pontos Reais (Taxas observadas) - cinza claro para não poluir
  
  geom_point(alpha = 0.4, color = "gray50", size = 1.5) +
  
  
  
  # 2. Linha de Tendência do Modelo (Vermelha)
  
  # A interação group = intervencao força a quebra da linha em Março/2020
  
  geom_line(aes(y = predito_taxa, group = intervencao), 
            
            color = "firebrick", size = 1) +
  
  
  
  # 3. Linha Vertical da Intervenção
  
  geom_vline(xintercept = as.Date("2020-03-01"), 
             
             linetype = "dashed", color = "black") +
  
  
  
  # 4. Facetas (Um gráfico por etnia)
  
  # scales = "free_y" é crucial pois a taxa da Indígena é mto diferente da Branca
  
  facet_wrap(~raca_descricao, scales = "free_y", ncol = 2) +
  
  
  
  # 5. Estética e Textos
  
  labs(
    
    title = "Série Temporal Interrompida: Diagnósticos por Etnia",
    
    subtitle = "Linha Vermelha: Modelo Ajustado (Quasipoisson) | Pontos: Taxa Observada",
    
    x = "Ano",
    
    y = "Taxa por 100.000 habitantes"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    strip.text = element_text(face = "bold", size = 12),
    
    axis.title = element_text(size = 11)
    
  )



# Exibir gráfico

print(grafico_its)library(read.dbc)

library(dplyr)

library(tidyr)

library(purrr)

library(lubridate)

library(broom)

library(ggplot2)



# --- 1.2 Leitura dos arquivos DBC ---

dados_todos <- arquivos |>
  
  map_df(~ {
    
    message(paste("Lendo:", basename(.x)))
    
    df <- read.dbc(.x)
    
    df$arquivo_origem <- basename(.x)
    
    
    
    # Tratamento de segurança para tipos
    
    if("NU_IDADE_N" %in% names(df)) df$NU_IDADE_N <- as.character(df$NU_IDADE_N)
    
    
    
    df
    
  })



# --- 1.3 Tratamento da Idade (Função Segura) ---

dados_todos$NU_IDADE_N <- as.numeric(as.character(dados_todos$NU_IDADE_N))



converter_idade_anos <- function(x) {
  
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



dados_limpos <- dados_todos |>
  
  mutate(idade_anos = converter_idade_anos(NU_IDADE_N)) |>
  
  # Filtro: Crianças menores de 5 anos (0 a 4.99)
  
  filter(idade_anos < 5) |>
  
  mutate(
    
    data_diagnostico = as.Date(DT_DIAG),
    
    ano = year(data_diagnostico),
    
    mes = month(data_diagnostico),
    
    # Categorização de Raça/Cor
    
    raca_descricao = case_when(
      
      as.character(CS_RACA) == "1" ~ "Branca",
      
      as.character(CS_RACA) == "2" ~ "Preta",
      
      as.character(CS_RACA) == "3" ~ "Amarela",
      
      as.character(CS_RACA) == "4" ~ "Parda",
      
      as.character(CS_RACA) == "5" ~ "Indígena",
      
      TRUE ~ "Ignorado/Outros"
      
    )
    
  )



# --- 1.4 Contagem Inicial de Casos ---

resumo_casos <- dados_limpos |>
  
  count(ano, mes, raca_descricao)



# ==============================================================================

# ETAPA 2: POPULAÇÃO E PREPARAÇÃO PARA ITS (ZERO-FILLING)

# ==============================================================================



# --- 2.1 Leitura e Transformação da População ---

# Ajuste o read.csv2 ou o sep=";" conforme seu arquivo

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



# --- 2.2 Join e Preenchimento de Meses Vazios (Essencial para ITS) ---

min_data <- min(dados_limpos$data_diagnostico, na.rm = TRUE)

max_data <- max(dados_limpos$data_diagnostico, na.rm = TRUE)



# Arredondar para o dia 1 do mês para criar a sequência

seq_datas <- seq(floor_date(min_data, "month"), floor_date(max_data, "month"), by = "month")



dados_its <- resumo_casos |>
  
  # Garante que a data base existe
  
  mutate(data_base = as.Date(paste0(ano, "-", mes, "-01"))) |>
  
  ungroup() |>
  
  # Cruza todas as raças com todos os meses (preenche n=0 se não houver casos)
  
  complete(raca_descricao, data_base = seq_datas, fill = list(n = 0)) |>
  
  mutate(
    
    ano = year(data_base),
    
    mes = month(data_base)
    
  ) |>
  
  # Traz a população
  
  left_join(pop_longa, by = c("ano" = "Ano", "raca_descricao" = "raca_descricao")) |>
  
  # Preenche população se houver falhas (ex: meses criados onde o join falhou)
  
  group_by(raca_descricao) |>
  
  fill(populacao, .direction = "downup") |>
  
  ungroup() |>
  
  # Remove ignorados para a modelagem
  
  filter(raca_descricao != "Ignorado/Outros") |>
  
  # Cria Variáveis ITS
  
  arrange(raca_descricao, data_base) |>
  
  group_by(raca_descricao) |>
  
  mutate(
    
    taxa_100k   = (n / populacao) * 100000,
    
    tempo       = row_number(),
    
    intervencao = if_else(data_base >= "2020-03-01", 1, 0),
    
    tempo_apos  = cumsum(intervencao)
    
  ) |>
  
  ungroup()



# ==============================================================================

# ETAPA 3: MODELAGEM ESTATÍSTICA (QUASIPOISSON)

# ==============================================================================



modelos_its <- dados_its |>
  
  nest(dados = -raca_descricao) |>
  
  mutate(
    
    # Roda o modelo
    
    modelo_ajustado = map(dados, ~ glm(n ~ tempo + intervencao + tempo_apos + offset(log(populacao)), 
                                       
                                       family = quasipoisson(link = "log"), 
                                       
                                       data = .x)),
    
    
    
    # Extrai Tabela de Resultados (RR e IC)
    
    tabela_coeficientes = map(modelo_ajustado, ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE)),
    
    
    
    # Gera Predições para o Gráfico (Valores esperados pelo modelo)
    
    # type = "response" retorna a contagem esperada (n). Depois dividimos pela pop.
    
    dados_com_predicao = map2(dados, modelo_ajustado, ~ {
      
      .x |> mutate(predito_contagem = predict(.y, type = "response"),
                   
                   predito_taxa = (predito_contagem / populacao) * 100000)
      
    })
    
  )



# --- 3.1 Exibir Tabela Final de Resultados ---

tabela_final <- modelos_its |>
  
  unnest(tabela_coeficientes) |>
  
  select(Raca = raca_descricao, Termo = term, RR = estimate, 
         
         IC_Inf = conf.low, IC_Sup = conf.high, P_valor = p.value) |>
  
  filter(Termo == "intervencao") |>
  
  mutate(
    
    Queda_Percentual = round((1 - RR) * 100, 2),
    
    Significativo = ifelse(P_valor < 0.05, "Sim", "Não")
    
  )



print("=== RESULTADOS DA INTERVENÇÃO (MARÇO/2020) ===")

print(tabela_final)



# ==============================================================================

# ETAPA 4: GRÁFICOS CLÁSSICOS DE ITS

# ==============================================================================



# Preparar dados para plotagem (desaninhando as predições)

dados_grafico <- modelos_its |>
  
  select(raca_descricao, dados_com_predicao) |>
  
  unnest(dados_com_predicao)



# Criar o Gráfico

grafico_its <- ggplot(dados_grafico, aes(x = data_base, y = taxa_100k)) +
  
  
  
  # 1. Pontos Reais (Taxas observadas) - cinza claro para não poluir
  
  geom_point(alpha = 0.4, color = "gray50", size = 1.5) +
  
  
  
  # 2. Linha de Tendência do Modelo (Vermelha)
  
  # A interação group = intervencao força a quebra da linha em Março/2020
  
  geom_line(aes(y = predito_taxa, group = intervencao), 
            
            color = "firebrick", size = 1) +
  
  
  
  # 3. Linha Vertical da Intervenção
  
  geom_vline(xintercept = as.Date("2020-03-01"), 
             
             linetype = "dashed", color = "black") +
  
  
  
  # 4. Facetas (Um gráfico por etnia)
  
  # scales = "free_y" é crucial pois a taxa da Indígena é mto diferente da Branca
  
  facet_wrap(~raca_descricao, scales = "free_y", ncol = 2) +
  
  
  
  # 5. Estética e Textos
  
  labs(
    
    title = "Série Temporal Interrompida: Diagnósticos por Etnia",
    
    subtitle = "Linha Vermelha: Modelo Ajustado (Quasipoisson) | Pontos: Taxa Observada",
    
    x = "Ano",
    
    y = "Taxa por 100.000 habitantes"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    strip.text = element_text(face = "bold", size = 12),
    
    axis.title = element_text(size = 11)
    
  )



# Exibir gráfico

print(grafico_its)

write.csv(dados_its, "casos mensais.csv")
