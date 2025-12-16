############################################################
# Project: Pediatric TB during COVID-19 in Brazil
# Script: 01_extract_sim_tb_u5.R
# Purpose: Download + process SIM-DO (TB deaths <5y) by UF/year and export
# Output: data/processed/SIM/tb_menores5_BR_2015_2024_{longo,largo}.csv + .xlsx
#
# Data source: DataSUS (SIM-DO) via microdatasus
# Ethics: Public, de-identified secondary data
############################################################

## ============================================================
## 0) Pacotes
## ============================================================
pkgs <- c(
  "microdatasus","dplyr","lubridate","stringr","tidyr","readr",
  "writexl","purrr","here"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

## ============================================================
## 1) Configurações gerais (editáveis)
## ============================================================

# UFs
ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG",
         "MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR",
         "RS","SC","SE","SP","TO")

# Intervalo de anos (SIM disponível por ano)
anos <- 2015:2024

# Pasta de saída dentro do repositório
out_dir <- here::here("data", "processed", "SIM")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Controle de cache: se TRUE, não rebaixa/processa quando já existe o CSV por UF/ano
skip_if_exists <- TRUE

# (opcional) salvar também o arquivo bruto baixado? (recomendo NÃO versionar no GitHub)
save_raw_download <- FALSE
raw_dir <- here::here("data", "raw", "SIM")
if (save_raw_download) dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

## ============================================================
## 2) Função robusta para minerar SIM-DO (TB <5 anos) por UF/ano
## ============================================================
processar_uf_ano <- function(uf, ano){

  arquivo_saida <- file.path(out_dir, sprintf("tb_%s_%d.csv", uf, ano))

  # 1) Se já existe, não refaz
  if (skip_if_exists && file.exists(arquivo_saida)) {
    message("⚠️ Já existe: ", basename(arquivo_saida), " — pulando.")
    return(invisible(NULL))
  }

  message("📥 Baixando SIM-DO: UF=", uf, " Ano=", ano)

  # 2) Download
  bruto <- tryCatch(
    {
      microdatasus::fetch_datasus(
        year_start = ano,
        year_end   = ano,
        uf = uf,
        information_system = "SIM-DO"
      )
    },
    error = function(e){
      message("❌ Erro em fetch_datasus (UF=", uf, ", Ano=", ano, "): ", e$message)
      return(NULL)
    }
  )

  if (is.null(bruto) || nrow(bruto) == 0) {
    message("🚫 SIM sem linhas para UF=", uf, " Ano=", ano, " — pulando.")
    return(invisible(NULL))
  }

  # (opcional) salvar bruto localmente
  if (save_raw_download) {
    raw_file <- file.path(raw_dir, sprintf("SIMDO_%s_%d.rds", uf, ano))
    saveRDS(bruto, raw_file)
  }

  # 3) Processamento padronizado
  proc <- tryCatch(
    {
      microdatasus::process_sim(bruto)
    },
    error = function(e){
      message("❌ Erro em process_sim (UF=", uf, ", Ano=", ano, "): ", e$message)
      return(NULL)
    }
  )

  if (is.null(proc) || nrow(proc) == 0) {
    message("🚫 process_sim retornou vazio para UF=", uf, " Ano=", ano)
    return(invisible(NULL))
  }

  # 4) Checar colunas esperadas
  colunas_necessarias <- c("DTOBITO","RACACOR","CAUSABAS","IDADEanos","IDADEmeses","IDADEdias","IDADEhoras")
  faltando <- setdiff(colunas_necessarias, names(proc))
  if (length(faltando) > 0) {
    message("⚠️ Estrutura inesperada (faltam: ", paste(faltando, collapse = ", "),
            ") UF=", uf, " Ano=", ano, " — pulando.")
    return(invisible(NULL))
  }

  # 5) Filtrar TB em <5 anos
  tb <- proc |>
    mutate(
      IDADEanos_num   = suppressWarnings(as.numeric(IDADEanos)),
      IDADEmeses_num  = suppressWarnings(as.numeric(IDADEmeses)),
      IDADEdias_num   = suppressWarnings(as.numeric(IDADEdias)),
      IDADEhoras_num  = suppressWarnings(as.numeric(IDADEhoras)),

      eh_menor5 = dplyr::case_when(
        !is.na(IDADEanos_num) & IDADEanos_num < 5 ~ TRUE,
        is.na(IDADEanos_num) & (!is.na(IDADEmeses_num) | !is.na(IDADEdias_num) | !is.na(IDADEhoras_num)) ~ TRUE,
        TRUE ~ FALSE
      ),

      data_obito = as.Date(DTOBITO),
      mes_ano    = lubridate::floor_date(data_obito, "month"),
      raca_final = dplyr::if_else(is.na(RACACOR) | RACACOR == "", "Ignorado", as.character(RACACOR)),
      ano_obito  = lubridate::year(data_obito),
      UF         = uf
    ) |>
    filter(
      eh_menor5,
      !is.na(CAUSABAS),
      stringr::str_detect(CAUSABAS, "^A1[5-9]") | stringr::str_detect(CAUSABAS, "^B90")
    ) |>
    count(UF, ano_obito, mes_ano, raca_final, name = "obitos_tb")

  if (nrow(tb) == 0) {
    message("🚫 Sem óbitos por TB <5 para UF=", uf, " Ano=", ano, " — não cria arquivo.")
    return(invisible(NULL))
  }

  # 6) Exportar
  readr::write_csv(tb, arquivo_saida)
  message("💾 Salvo: ", basename(arquivo_saida))

  invisible(tb)
}

## ============================================================
## 3) Loop geral
## ============================================================
for (uf in ufs) {
  for (ano in anos) {
    processar_uf_ano(uf, ano)
  }
}
message("✅ Mineração por UF/ano finalizada.")

## ============================================================
## 4) Juntar tudo em um banco final (longo e largo)
## ============================================================
arquivos <- list.files(out_dir, full.names = TRUE, pattern = "^tb_.*\\.csv$")

if (length(arquivos) == 0) {
  message("⚠️ Nenhum tb_*.csv encontrado em ", out_dir)
} else {

  dados_tb <- purrr::map_dfr(arquivos, readr::read_csv, show_col_types = FALSE)

  # Salvar banco final longo
  out_long <- here::here("data", "processed", "tb_menores5_BR_2015_2024_longo.csv")
  readr::write_csv(dados_tb, out_long)

  # Versão larga (uma coluna por raça/cor)
  dados_tb_wide <- dados_tb |>
    tidyr::pivot_wider(
      names_from  = raca_final,
      values_from = obitos_tb,
      values_fill = 0
    )

  out_wide <- here::here("data", "processed", "tb_menores5_BR_2015_2024_largo.csv")
  readr::write_csv(dados_tb_wide, out_wide)

  # XLSX com abas
  out_xlsx <- here::here("data", "processed", "tb_menores5_BR_2015_2024.xlsx")
  writexl::write_xlsx(
    list("tb_longo" = dados_tb, "tb_largo" = dados_tb_wide),
    path = out_xlsx
  )

  message("📦 Outputs finais criados em data/processed/:",
          "\n- ", basename(out_long),
          "\n- ", basename(out_wide),
          "\n- ", basename(out_xlsx))
}
