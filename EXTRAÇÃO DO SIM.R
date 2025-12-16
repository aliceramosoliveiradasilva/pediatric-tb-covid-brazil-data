## ============================================================
## 0. Pacotes
## ============================================================
library(microdatasus)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(readr)
library(writexl)

## ============================================================
## 1. Configurações gerais
## ============================================================
# 👉 Ajuste para o caminho da sua pasta de trabalho
setwd("C:/Users/alice/Documents/R/TB/arquivo/DADOS BRUTOS")

# Vetor de UFs
ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG",
         "MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR",
         "RS","SC","SE","SP","TO")

# Intervalo de anos
anos <- 2015:2024

# Pasta onde serão salvos os CSVs por UF/ano
out_dir <- "SIM"
dir.create(out_dir, showWarnings = FALSE)

## ============================================================
## 2. Função robusta para minerar SIM-DO (TB <5 anos) por UF/ano
##    - Sem retrabalho
##    - Com tratamento de erro
##    - Sem salvar arquivos vazios
## ============================================================
processar_uf_ano <- function(uf, ano){
  
  arquivo_saida <- file.path(out_dir, sprintf("tb_%s_%d.csv", uf, ano))
  
  # 1. Se já existe, não refaz
  if (file.exists(arquivo_saida)) {
    message("⚠️ Já existe: ", arquivo_saida, " — pulando.")
    return(NULL)
  }
  
  message("📥 Baixando dados: UF=", uf, " Ano=", ano)
  
  # 2. Tenta baixar; se der erro, pula
  bruto <- tryCatch(
    {
      fetch_datasus(
        year_start = ano,
        year_end   = ano,
        uf = uf,
        information_system = "SIM-DO"
      )
    },
    error = function(e){
      message("❌ Erro em fetch_datasus para UF=", uf, " Ano=", ano,
              " — pulando. Detalhe: ", e$message)
      return(NULL)
    }
  )
  
  # Se não conseguiu baixar nada
  if (is.null(bruto)) {
    return(NULL)
  }
  
  if (nrow(bruto) == 0) {
    message("🚫 SIM sem linhas para UF=", uf, " Ano=", ano, " — pulando.")
    return(NULL)
  }
  
  # 3. Tenta processar SIM
  proc <- tryCatch(
    {
      process_sim(bruto)
    },
    error = function(e){
      message("❌ Erro em process_sim para UF=", uf, " Ano=", ano,
              " — pulando. Detalhe: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(proc)) {
    return(NULL)
  }
  
  # 4. Garante que colunas de idade existem
  colunas_idade_necessarias <- c("IDADEanos", "IDADEmeses", "IDADEdias", "IDADEhoras")
  colunas_faltando <- setdiff(colunas_idade_necessarias, colnames(proc))
  
  if (length(colunas_faltando) > 0) {
    message("⚠️ Estrutura inesperada (faltam colunas de idade: ",
            paste(colunas_faltando, collapse = ", "),
            ") para UF=", uf, " Ano=", ano, " — pulando.")
    return(NULL)
  }
  
  # 5. Filtrar óbitos por TB em menores de 5 anos
  tb <- proc |>
    mutate(
      # converter idades
      IDADEanos_num   = suppressWarnings(as.numeric(IDADEanos)),
      IDADEmeses_num  = suppressWarnings(as.numeric(IDADEmeses)),
      IDADEdias_num   = suppressWarnings(as.numeric(IDADEdias)),
      IDADEhoras_num  = suppressWarnings(as.numeric(IDADEhoras)),
      
      # menor de 5 anos:
      # - anos < 5
      # - ou sem anos, mas com info em meses/dias/horas (bebês <1 ano)
      eh_menor5 = case_when(
        !is.na(IDADEanos_num) & IDADEanos_num < 5 ~ TRUE,
        is.na(IDADEanos_num) & (
          !is.na(IDADEmeses_num) |
            !is.na(IDADEdias_num)  |
            !is.na(IDADEhoras_num)
        ) ~ TRUE,
        TRUE ~ FALSE
      ),
      
      data_obito = as.Date(DTOBITO),
      mes_ano    = floor_date(data_obito, "month"),
      raca_final = ifelse(is.na(RACACOR), "Ignorado", as.character(RACACOR)),
      ano_obito  = year(data_obito),
      UF         = uf
    ) |>
    filter(
      eh_menor5,
      !is.na(CAUSABAS),
      stringr::str_detect(CAUSABAS, "^A1[5-9]") | stringr::str_detect(CAUSABAS, "^B90")
    ) |>
    count(UF, ano_obito, mes_ano, raca_final, name = "obitos_tb")
  
  # 6. Se não há nenhum óbito, não cria arquivo
  if (nrow(tb) == 0) {
    message("🚫 Sem óbitos por TB em menores de 5 anos para UF=", uf,
            " Ano=", ano, " — nenhum arquivo será criado.")
    return(NULL)
  }
  
  # 7. Salva arquivo
  write.csv(tb, arquivo_saida, row.names = FALSE)
  message("💾 Arquivo salvo: ", arquivo_saida)
  
  return(tb)
}

## ============================================================
## 3. Loop geral: minera tudo sem retrabalho
## ============================================================
for (uf in ufs) {
  for (ano in anos) {
    processar_uf_ano(uf, ano)
  }
}

message("✅ Mineração por UF/ano finalizada.")

## ============================================================
## 4. Juntar tudo que foi minerado em um único banco
## ============================================================
arquivos <- list.files(out_dir, full.names = TRUE, pattern = "^tb_.*\\.csv$")

if (length(arquivos) == 0) {
  message("⚠️ Nenhum arquivo tb_*.csv encontrado em ", out_dir,
          " — verifique se a mineração gerou resultados.")
} else {
  # Ler todos e empilhar
  dados_tb <- purrr::map_dfr(arquivos, readr::read_csv, show_col_types = FALSE)
  
  # Salvar banco final longo
  write.csv(dados_tb,
            file = "tb_menores5_BR_2015_2024_longo.csv",
            row.names = FALSE)
  
  # Versão larga: uma coluna por raça/cor
  dados_tb_wide <- dados_tb |>
    tidyr::pivot_wider(
      names_from  = raca_final,
      values_from = obitos_tb,
      values_fill = 0
    )
  
  write.csv(dados_tb_wide,
            file = "tb_menores5_BR_2015_2024_largo.csv",
            row.names = FALSE)
  
  # Opcional: XLSX com abas
  writexl::write_xlsx(
    list(
      "tb_longo" = dados_tb,
      "tb_largo" = dados_tb_wide
    ),
    path = "tb_menores5_BR_2015_2024.xlsx"
  )
  
  message("📦 Arquivos finais criados: 
          - tb_menores5_BR_2015_2024_longo.csv
          - tb_menores5_BR_2015_2024_largo.csv
          - tb_menores5_BR_2015_2024.xlsx")
}

