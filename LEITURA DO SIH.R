dir_sih <- "/home/Ramos/Documentos/R/SHORT TB/arquivo/DADOS BRUTOS/SIH/"
# ============================================================
# 1) Pacotes
# ============================================================
library(readr)
library(dplyr)
library(purrr)

# ============================================================
# 2) Listar CSVs
# ============================================================
arquivos_csv_sih <- list.files(
  path = dir_sih,
  pattern = "\\.csv$",
  full.names = TRUE
)

length(arquivos_csv_sih)
arquivos_csv_sih

# ============================================================
# 3) Ler + concatenar
# ============================================================
library(readr)
library(dplyr)
library(purrr)

dados_sih_consolidados <- map_df(
  arquivos_csv_sih,
  ~ read_csv(.x,
             show_col_types = FALSE,
             col_types = cols(.default = col_character())) %>%
    mutate(arquivo_origem = basename(.x))
)

glimpse(dados_sih_consolidados)
write_csv(dados_sih_consolidados, file.path(dir_sih, "SIH_consolidado.csv"))

library(dplyr)
library(readr)

library(dplyr)
library(readr)

# 1) Criar mes_ano e padronizar raça/cor
sih_base <- dados_sih_consolidados %>%
  mutate(
    ano = as.integer(ano),
    mes = as.integer(mes),
    mes_ano = as.Date(sprintf("%04d-%02d-01", ano, mes)),
    raca_cor_final = case_when(
      is.na(RACA_COR) ~ "Ignorado",
      RACA_COR == ""  ~ "Ignorado",
      RACA_COR %in% c("00", "99") ~ "Ignorado",
      TRUE ~ RACA_COR
    )
  )

# 2) Sumarizar por mes_ano e raça/cor (contando internações = n())
sih_mes_raca <- sih_base %>%
  group_by(mes_ano, raca_cor_final) %>%
  summarise(internacoes_tb = n(), .groups = "drop") %>%
  arrange(mes_ano, raca_cor_final)

# 3) Sumarizar total por mes_ano
sih_mes_total <- sih_base %>%
  group_by(mes_ano) %>%
  summarise(internacoes_tb = n(), .groups = "drop") %>%
  arrange(mes_ano)

# 4) Exportar
write_csv(sih_mes_raca,  file.path(dir_sih, "SIH_internacoes_tb_por_mes_raca_cor.csv"))
write_csv(sih_mes_total, file.path(dir_sih, "SIH_internacoes_tb_por_mes_total.csv"))
