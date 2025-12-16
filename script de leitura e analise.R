library(read.dbc)
library(dplyr)
library(purrr)

caminho <- "/home/ramos/Documentos/arquivo"

arquivos <- list.files(
  path = caminho,
  pattern = "\\.dbc$",
  full.names = TRUE
)

# Ler arquivos
dados_todos <- arquivos |>
  map_df(~ {
    message(paste("Lendo:", basename(.x)))
    df <- read.dbc(.x)
    df$arquivo_origem <- basename(.x)
    
    # TRUQUE IMPORTANTE: Converter colunas chave para character logo na leitura
    # para evitar conflitos de tipos entre arquivos diferentes
    if("NU_IDADE_N" %in% names(df)) {
      df$NU_IDADE_N <- as.character(df$NU_IDADE_N)
    }
    
    df
  })

# --- DIAGNÓSTICO ---
# Veja se aparecem códigos começando com 2 (dias), 3 (meses) ou 4 (anos)
print("Amostra dos códigos brutos de idade:")
print(table(substr(dados_todos$NU_IDADE_N, 1, 1))) 
# Esperado: números 1, 2, 3, 4 e talvez 5. Se der só 0 ou 1, o dado veio errado.

# Converter para numérico da forma segura
dados_todos$NU_IDADE_N <- as.numeric(as.character(dados_todos$NU_IDADE_N))

# Função ajustada (trata o 0 como NA explicitamente apenas se não for codigo valido)
converter_idade_anos <- function(x) {
  
  # Primeiro dígito (unidade de tempo)
  unidade <- floor(x / 1000)
  # Últimos 3 dígitos (valor)
  valor   <- x %% 1000
  
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    
    # Idade maior que 100 anos (código 5)
    unidade == 5 ~ 100 + valor,
    
    # Anos (código 4)
    unidade == 4 ~ valor,
    
    # Meses (código 3) -> divide por 12
    unidade == 3 ~ valor / 12,
    
    # Dias (código 2) -> divide por 365
    unidade == 2 ~ valor / 365,
    
    # Horas (código 1) -> divide por 365*24
    unidade == 1 ~ valor / (365 * 24),
    
    # Ignorado ou inconsistente
    TRUE ~ NA_real_
  )
}

# Aplicar a conversão
dados_todos <- dados_todos |>
  mutate(idade_anos = converter_idade_anos(NU_IDADE_N))

library(dplyr)

# 1. Filtra quem tem menos de 4 anos completos (0 a 3.999...)
dados_criancas <- dados_todos |> 
  filter(idade_anos < 5)

# 2. Verificação rápida: Quantas crianças temos por ano de idade?
# O 'floor' arredonda para baixo para agrupar (0 = 0 a 11 meses, 1 = 1 ano, etc)
dados_criancas |>
  count(idade_inteira = floor(idade_anos)) |>
  mutate(descricao = case_when(
    idade_inteira == 0 ~ "Menores de 1 ano (bebês)",
    idade_inteira == 1 ~ "1 ano",
    idade_inteira == 2 ~ "2 anos",
    idade_inteira == 3 ~ "3 anos",
    idade_inteira == 4 ~ "4 anos"
  ))

# 3. Ver estatísticas descritivas (mínimo deve ser perto de 0, máx perto de 3.99)
summary(dados_criancas$idade_anos)

library(dplyr)
library(lubridate) # Se não tiver, instale: install.packages("lubridate")

dados_criancas <- dados_criancas |> 
  mutate(
    # 1. Converter texto para Objeto de Data (caso ainda não seja)
    data_diagnostico = as.Date(DT_DIAG),
    
    # 2. Extrair o Ano (ex: 2015)
    ano = year(data_diagnostico),
    
    # 3. Extrair o Mês numérico (ex: 2)
    mes = month(data_diagnostico),
    
    # OPCIONAL: Cria uma coluna "Ano-Mês" (útil para gráficos de linha temporal)
    ano_mes = format(data_diagnostico, "%Y-%m")
  )

# Verificar como ficou
head(dados_criancas |> select(DT_DIAG, ano, mes, ano_mes))
unique(dados_criancas$CS_RACA)
library(dplyr)

# 1. Criar rótulos para a raça (Opcional, mas recomendado)
# Códigos padrão SUS: 1-Branca, 2-Preta, 3-Amarela, 4-Parda, 5-Indígena
dados_criancas <- dados_criancas |>
  mutate(
    raca_descricao = case_when(
      as.character(CS_RACA) == "1" ~ "Branca",
      as.character(CS_RACA) == "2" ~ "Preta",
      as.character(CS_RACA) == "3" ~ "Amarela",
      as.character(CS_RACA) == "4" ~ "Parda",
      as.character(CS_RACA) == "5" ~ "Indígena",
      TRUE ~ "Ignorado/Outros" # Pega NA, 0, 9, etc.
    )
  )

# 2. Sumarizar (contagem)
resumo_raca <- dados_criancas |>
  count(ano, mes, raca_descricao) |>
  arrange(ano, mes, desc(n)) # Ordena por data e quem tem mais casos

# Visualizar o topo da tabela
head(resumo_raca)
pop <- read.csv2("pop.csv")
# Verifica se carregou certo
head(pop)
head(resumo_raca)
