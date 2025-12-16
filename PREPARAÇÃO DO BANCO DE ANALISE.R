## =====================================================================
## 0. Limpar ambiente e carregar pacotes
## =====================================================================

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(broom)
library(purrr)
library(readr)
library(stringr)

## =====================================================================
## 1. Ler base consolidada
## =====================================================================

base_dir <- "C:/Users/alice/Downloads/TB/arquivo/DADOS TRATADOS"

dados <- read.csv(file.path(base_dir, "base_trimestral_tb_0a4_raca_brasil.csv"))

## Garantir tipos corretos
dados <- dados |>
  mutate(
    data_base = as.Date(data_base),
    raca_cat  = factor(
      raca_cat,
      levels = c("Indígena", "Preta", "Parda", "Branca", "Amarela", "Ignorado")
    )
  )

## Filtrar período do paper (2015–2023, último trimestre completo = 2023-10-01)
dados <- dados |>
  filter(data_base >= as.Date("2015-01-01"),
         data_base <= as.Date("2023-10-01"))

## Opcional: excluir "Ignorado" das análises principais
dados <- dados |>
  filter(raca_cat != "Ignorado")

## =====================================================================
## 2. Versão em inglês dos nomes de raça (para gráficos)
## =====================================================================

dados <- dados |>
  mutate(
    race_en = case_when(
      raca_cat == "Indígena" ~ "Indigenous",
      raca_cat == "Preta"    ~ "Black",
      raca_cat == "Parda"    ~ "Brown",
      raca_cat == "Branca"   ~ "White",
      raca_cat == "Amarela"  ~ "Asian",   # nunca "Yellow"
      TRUE                   ~ "Unknown"
    ),
    race_en = factor(
      race_en,
      levels = c("Indigenous", "Black", "Brown", "White", "Asian", "Unknown")
    )
  )

## =====================================================================
## 3. Criar pastas de saída
## =====================================================================

fig_dir     <- file.path(base_dir, "FIGURAS")
results_dir <- file.path(base_dir, "RESULTADOS_MODELOS")

dir.create(fig_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

## =====================================================================
## 4. Função genérica para ajustar ITS por raça e indicador
## =====================================================================

fit_its_by_race <- function(data, outcome, offset_var = "populacao") {
  # data: data.frame filtrado por indicador/período
  # outcome: nome da coluna de contagem ("casos", "internacoes_tb", "obitos_tb")
  # offset_var: denominador ("populacao")
  
  data |>
    group_by(raca_cat, race_en) |>
    group_modify(~{
      df <- .x
      
      # remover linhas com NA no desfecho ou offset
      df <- df |>
        filter(!is.na(.data[[outcome]]),
               !is.na(.data[[offset_var]]),
               .data[[offset_var]] > 0)
      
      # ajustar modelo ITS
      mod <- glm(
        formula = as.formula(
          paste0(outcome, " ~ tempo + intervencao + tempo_apos")
        ),
        family = quasipoisson(link = "log"),
        offset = log(df[[offset_var]]),
        data   = df
      )
      
      # resíduos de Pearson e parâmetro de dispersão
      res_pearson <- residuals(mod, type = "pearson")
      disp        <- sum(res_pearson^2) / df.residual(mod)
      
      # teste de autocorrelação dos resíduos (Ljung-Box, lag 4)
      lb_test <- tryCatch(
        {
          bt <- Box.test(res_pearson, lag = 4, type = "Ljung-Box")
          c(statistic = unname(bt$statistic), p_value = bt$p.value)
        },
        error = function(e) c(statistic = NA, p_value = NA)
      )
      
      # coeficientes em escala log, SE e RRs com IC95% (Wald)
      tidy_raw <- broom::tidy(mod)  # sem conf.int, sem exponentiate
      
      tidy_mod <- tidy_raw |>
        mutate(
          rr      = exp(estimate),
          rr_low  = exp(estimate - 1.96 * std.error),
          rr_high = exp(estimate + 1.96 * std.error)
        )
      
      glance_mod <- broom::glance(mod)
      
      tibble(
        outcome      = outcome,
        model        = list(mod),
        data_used    = list(df),
        tidy         = list(tidy_mod),
        glance       = list(glance_mod),
        dispersion   = disp,
        lb_statistic = lb_test["statistic"],
        lb_p_value   = lb_test["p_value"]
      )
    }) |>
    ungroup()
}

## =====================================================================
## 5. Ajustar modelos ITS para cada indicador
## =====================================================================

## 5.1. Notificações (casos)
models_cases <- fit_its_by_race(dados, outcome = "casos", offset_var = "populacao")

## 5.2. Internações
models_intern <- fit_its_by_race(dados, outcome = "internacoes_tb", offset_var = "populacao")

## 5.3. Óbitos
models_deaths <- fit_its_by_race(dados, outcome = "obitos_tb", offset_var = "populacao")

## Guardar lista completa de modelos em RDS (se quiser reusar posteriormente)
saveRDS(
  list(
    cases       = models_cases,
    internacoes = models_intern,
    obitos      = models_deaths
  ),
  file = file.path(results_dir, "ITS_models_pediatric_TB_0a4_by_race.rds")
)

## =====================================================================
## 6. Exportar tabelas de resultados (RR, IC, qualidade)
## =====================================================================

## Função auxiliar para “achatar” as listas de tidy e glance
flatten_results <- function(models_tbl, indicador_label) {
  tidy_df <- models_tbl |>
    select(raca_cat, race_en, outcome, tidy) |>
    unnest(tidy) |>
    mutate(
      indicator = indicador_label
      # columns: term, estimate (log), std.error, statistic, p.value,
      #          rr, rr_low, rr_high
    )
  
  glance_df <- models_tbl |>
    select(raca_cat, race_en, outcome, glance, dispersion, lb_statistic, lb_p_value) |>
    unnest(glance) |>
    mutate(indicator = indicador_label)
  
  list(tidy = tidy_df, glance = glance_df)
}


res_cases   <- flatten_results(models_cases,   "Notifications")
res_intern  <- flatten_results(models_intern,  "Hospitalizations")
res_deaths  <- flatten_results(models_deaths,  "Mortality")

## Juntar tudo
tidy_all <- bind_rows(
  res_cases$tidy,
  res_intern$tidy,
  res_deaths$tidy
)

glance_all <- bind_rows(
  res_cases$glance,
  res_intern$glance,
  res_deaths$glance
)

## Salvar em CSV
write_csv(tidy_all,
          file.path(results_dir, "ITS_coefficients_RR_by_indicator_and_race.csv"))

write_csv(glance_all,
          file.path(results_dir, "ITS_model_glance_and_quality_by_indicator_and_race.csv"))

## =====================================================================
## 7. Criar data.frames com valores ajustados para gráficos
## =====================================================================

## Função que gera valores previstos (taxas por 100.000) para cada raça e indicador
## =====================================================================
## 7. Criar data.frames com valores ajustados para gráficos
## =====================================================================

## Função que gera valores previstos (taxas por 100.000) para cada raça e indicador
make_predictions_df <- function(models_tbl, outcome_var, label_outcome) {
  models_tbl |>
    mutate(
      pred_df = map2(
        model,
        data_used,
        ~{
          df <- .y
          
          # predições no escore de resposta (contagens)
          pr <- predict(.x, type = "response", se.fit = TRUE)
          
          df |>
            mutate(
              indicator_lab = label_outcome,
              fitted        = pr$fit,
              se_fit        = pr$se.fit,
              # taxa observada e ajustada por 100.000 habitantes
              rate_obs      = .data[[outcome_var]] / populacao * 1e5,
              rate_fit      = fitted / populacao * 1e5
            )
        }
      )
    ) |>
    select(raca_cat, race_en, pred_df) |>
    unnest(pred_df)
}

## Predições por indicador
pred_cases  <- make_predictions_df(models_cases,  "casos",         "Notification rate")
pred_intern <- make_predictions_df(models_intern, "internacoes_tb","Hospitalization rate")
pred_deaths <- make_predictions_df(models_deaths, "obitos_tb",     "Mortality rate")

## Unir tudo em um único data.frame
pred_all <- bind_rows(pred_cases, pred_intern, pred_deaths) |>
  mutate(
    indicator_lab = factor(
      indicator_lab,
      levels = c("Notification rate", "Hospitalization rate", "Mortality rate")
    )
  )


pred_cases  <- make_predictions_df(models_cases,  "casos",         "Notification rate")
pred_intern <- make_predictions_df(models_intern, "internacoes_tb","Hospitalization rate")
pred_deaths <- make_predictions_df(models_deaths, "obitos_tb",     "Mortality rate")

pred_all <- bind_rows(pred_cases, pred_intern, pred_deaths)

## =====================================================================
## 8. Função de plotagem e exportação (PDF + PNG, alta resolução)
## =====================================================================

plot_indicator <- function(df_indicator, indicator_label, fig_dir) {
  
  p <- ggplot(df_indicator,
              aes(x = data_base)) +
    geom_point(aes(y = rate_obs), alpha = 0.6) +
    geom_line(aes(y = rate_fit), linewidth = 0.9) +
    geom_vline(xintercept = as.Date("2020-04-01"),
               linetype = "dashed") +
    facet_wrap(~ race_en, ncol = 2) +
    labs(
      x = "Quarter (2015–2023)",
      y = "Rate per 100,000 children (0–4 years)",
      title = indicator_label,
      subtitle = "Interrupted time series by race/ethnicity, Brazil, 2015–2023"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ## nomes de arquivo: tirar espaços e deixar tudo minúsculo
  fname_base <- indicator_label |>
    stringr::str_replace_all(" ", "_") |>
    tolower()
  
  # PDF
  ggsave(
    filename = file.path(fig_dir, paste0("ITS_", fname_base, ".pdf")),
    plot     = p,
    device   = cairo_pdf,  # melhor renderização de texto
    width    = 8,
    height   = 6,
    units    = "in"
  )
  
  # PNG em alta resolução
  ggsave(
    filename = file.path(fig_dir, paste0("ITS_", fname_base, ".png")),
    plot     = p,
    device   = "png",
    width    = 8,
    height   = 6,
    units    = "in",
    dpi      = 600
  )
}

## =====================================================================
## 9. Gerar e exportar figuras para cada indicador
## =====================================================================

## Notificações
pred_cases_plot <- pred_all |>
  filter(indicator_lab == "Notification rate") |>
  arrange(race_en, data_base)  # só para garantir ordem bonita

plot_indicator(pred_cases_plot, "Notification rate", fig_dir)

## Internações
pred_intern_plot <- pred_all |>
  filter(indicator_lab == "Hospitalization rate") |>
  arrange(race_en, data_base)

plot_indicator(pred_intern_plot, "Hospitalization rate", fig_dir)

## Óbitos
pred_deaths_plot <- pred_all |>
  filter(indicator_lab == "Mortality rate") |>
  arrange(race_en, data_base)

plot_indicator(pred_deaths_plot, "Mortality rate", fig_dir)
