# ============================================================
# 1) Pacotes
# ============================================================
library(readr)
library(dplyr)
library(purrr)

# ============================================================
# 2) Pasta
# ============================================================
dir_base <- "/home/Ramos/Documentos/R/SHORT TB/arquivo/DADOS BRUTOS/SIM/SIM/"

# ============================================================
# 3) Listar CSVs
# ============================================================
arquivos_csv <- list.files(
  path = dir_base,
  pattern = "\\.csv$",
  full.names = TRUE
)

length(arquivos_csv)
arquivos_csv

# ============================================================
# 4) Ler + concatenar (robusto)
# - guess_max aumenta a chance de acertar tipos em arquivos grandes
# ============================================================
dados_consolidados <- map_df(
  arquivos_csv,
  ~ read_csv(.x, show_col_types = FALSE, guess_max = 200000) %>%
    mutate(arquivo_origem = basename(.x))
)

# Conferir
glimpse(dados_consolidados)

# ============================================================
# 5) Salvar consolidado
# ============================================================
write_csv(
  dados_consolidados,
  file.path(dir_base, "SIM_consolidado.csv")
)


library(dplyr)
library(readr)

tb_mes_raca <- dados_consolidados %>%
  group_by(mes_ano, raca_final) %>%
  summarise(obitos_tb = sum(obitos_tb, na.rm = TRUE), .groups = "drop") %>%
  arrange(mes_ano, raca_final)

write_csv(tb_mes_raca, file.path(dir_base, "SIM_obitos_tb_por_mes_raca.csv"))


tb_mes_total <- dados_consolidados %>%
  group_by(mes_ano) %>%
  summarise(obitos_tb = sum(obitos_tb, na.rm = TRUE), .groups = "drop") %>%
  arrange(mes_ano)

write_csv(tb_mes_total, file.path(dir_base, "SIM_obitos_tb_por_mes_total.csv"))



