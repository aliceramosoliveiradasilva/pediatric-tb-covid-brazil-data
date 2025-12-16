## =====================================================================
## 0. Pacotes
## =====================================================================

library(microdatasus)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

## =====================================================================
## 1. Caminhos e pasta de saída
## =====================================================================

# Raiz dos dados brutos (ajuste se seu caminho for outro)
dir_base  <- "/home/Ramos/Documentos/R/SHORT TB/arquivo/DADOS BRUTOS"

# Pasta para guardar os dados de internação por TB
dir_sih   <- file.path(dir_base, "SIH")

if (!dir.exists(dir_sih)) dir.create(dir_sih)

## =====================================================================
## 2. Parâmetros: anos, UFs e definição de TB
## =====================================================================

anos <- 2015:2024

ufs  <- c(
  "AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT",
  "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"
)

# Definição de TB: CID-10 A15–A19 (principal)
codigos_tb_3d <- paste0("A1", 5:9)  # "A15","A16","A17","A18","A19"

## =====================================================================
## 3. Função para converter idade do SIH em anos
## =====================================================================

converter_idade_anos_sih <- function(idade, cod_idade) {
  idade_num <- suppressWarnings(as.numeric(idade))
  
  case_when(
    cod_idade == "Anos"                          ~ idade_num,
    cod_idade == "Meses"                         ~ idade_num / 12,
    cod_idade == "Dias"                          ~ idade_num / 365,
    cod_idade == "Horas"                         ~ idade_num / (365 * 24),
    cod_idade == "Centena de anos (100 + idade)" ~ 100 + idade_num,
    TRUE                                         ~ NA_real_
  )
}

## =====================================================================
## 4. Loop sobre ano x UF: baixar, processar, filtrar TB < 5 anos e salvar
## =====================================================================

# Tabela de combinações ano x UF
plano_sih <- tidyr::expand_grid(ano = anos, uf = ufs)

dados_tb_0a4_lista <- plano_sih |>
  mutate(dados = pmap(
    list(ano, uf),
    function(ano, uf) {
      
      message(glue::glue("Baixando SIH-RD: ano = {ano}, UF = {uf}"))
      
      # ---------------------------------------------------------------
      # 4.1 Download SIH bruto para o ano completo da UF
      # ---------------------------------------------------------------
      dados_brutos <- fetch_datasus(
        year_start        = ano,
        month_start       = 1,
        year_end          = ano,
        month_end         = 12,
        uf                = uf,
        information_system = "SIH-RD"
      )
      
      if (nrow(dados_brutos) == 0) {
        message(glue::glue("Nenhum registro SIH para {uf} em {ano}."))
        return(NULL)
      }
      
      # ---------------------------------------------------------------
      # 4.2 Processamento padrão do microdatasus
      # ---------------------------------------------------------------
      dados_tratados <- process_sih(dados_brutos)
      
      # ---------------------------------------------------------------
      # 4.3 Calcular idade em anos e filtrar < 5 anos
      # ---------------------------------------------------------------
      dados_tratados <- dados_tratados |>
        mutate(
          idade_anos = converter_idade_anos_sih(IDADE, COD_IDADE),
          
          # TB como diagnóstico principal (A15–A19, 3 primeiros caracteres)
          tb_principal = str_sub(DIAG_PRINC, 1, 3) %in% codigos_tb_3d,
          
          faixa_0a4 = idade_anos < 5
        ) |>
        filter(tb_principal, faixa_0a4)
      
      # Se depois de filtrar não sobrou nada, só retorna NULL
      if (nrow(dados_tratados) == 0) {
        message(glue::glue("Sem internações por TB <5 anos para {uf} em {ano}."))
        return(NULL)
      }
      
      # ---------------------------------------------------------------
      # 4.4 Selecionar colunas essenciais e criar ano/mes/UF explícitos
      #     (ajuste as colunas conforme o que você queira guardar)
      # ---------------------------------------------------------------
      dados_tratados <- dados_tratados |>
        mutate(
          ano = as.integer(ANO_CMPT),
          mes = as.integer(MES_CMPT),
          uf_sih = UF_ZI         # ou munResUf, se preferir UF de residência
        ) |>
        select(
          uf_sih, ano, mes,
          DIAG_PRINC, IDADE, COD_IDADE, idade_anos,
          RACA_COR, ETNIA,
          DT_INTER, DT_SAIDA,
          MUN_RES = MUNIC_RES,
          everything()
        )
      
      # ---------------------------------------------------------------
      # 4.5 Salvar um CSV por UF x ano na pasta SIH
      # ---------------------------------------------------------------
      nome_arquivo <- glue::glue("sih_tb_0a4_{uf}_{ano}.csv")
      caminho_arquivo <- file.path(dir_sih, nome_arquivo)
      
      readr::write_csv(dados_tratados, caminho_arquivo)
      
      message(glue::glue("Salvo: {caminho_arquivo} ({nrow(dados_tratados)} linhas)"))
      
      return(dados_tratados)
    }
  ))

## =====================================================================
## 5. Juntar tudo em um único banco Brasil (opcional, mas recomendo)
## =====================================================================

dados_tb_0a4_sih <- dados_tb_0a4_lista |>
  pull(dados) |>
  bind_rows()

# Salvar o consolidado Brasil
arquivo_brasil <- file.path(dir_sih, "sih_tb_0a4_2015_2024_brasil.csv")
readr::write_csv(dados_tb_0a4_sih, arquivo_brasil)

message(glue::glue("Arquivo consolidado salvo em: {arquivo_brasil}"))
