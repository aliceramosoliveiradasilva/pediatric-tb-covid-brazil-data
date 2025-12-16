library(nlme)
library(dplyr)
library(purrr)
library(rlang)   # para sym()
library(tidyr)

## =====================================================================
## 4A. Função ITS com correlação AR(1) para cada raça e indicador
## =====================================================================

fit_its_ar1_by_race <- function(data, outcome, offset_var = "populacao") {
  
  data %>%
    group_by(race_en, raca_cat) %>%
    group_modify(~{
      df <- .x
      
      ## criar variável de taxa para GLS (pois GLS não aceita offset)
      df <- df %>%
        mutate(
          rate = (!!sym(outcome)) / (!!sym(offset_var)) * 1e5
        )
      
      ## modelo GLS com erro AR(1)
      mod <- tryCatch(
        gls(
          rate ~ tempo + intervencao + tempo_apos,
          data = df,
          correlation = corAR1(form = ~ tempo),
          method = "ML"
        ),
        error = function(e) NULL
      )
      
      if (is.null(mod)) {
        return(
          tibble(
            outcome   = outcome,
            model     = list(NULL),
            data_used = list(df),
            tidy      = list(NULL),
            glance    = list(NULL)
          )
        )
      }
      
      ## ---------- COEFICIENTES + IC95% (estilo tidy) ----------
      sum_mod <- summary(mod)
      coefs   <- as.data.frame(sum_mod$tTable)
      coefs$term <- rownames(coefs)
      
      # renomear colunas
      coefs <- coefs %>%
        dplyr::rename(
          estimate  = Value,
          std.error = Std.Error,
          statistic = `t-value`,  # <- aqui com hífen e crase
          p.value   = `p-value`
        )
      
      # graus de liberdade aproximados
      df_res <- mod$dims$N - mod$dims$p
      t_crit <- qt(0.975, df_res)
      
      coefs <- coefs %>%
        mutate(
          conf.low  = estimate - t_crit * std.error,
          conf.high = estimate + t_crit * std.error
        ) %>%
        select(term, estimate, std.error, statistic, p.value,
               conf.low, conf.high)
      
      ## ---------- “glance” simples ----------
      glance_tbl <- tibble(
        sigma   = mod$sigma,
        logLik  = as.numeric(logLik(mod)),
        AIC     = AIC(mod),
        BIC     = BIC(mod),
        nobs    = nrow(df)
      )
      
      tibble(
        outcome   = outcome,
        model     = list(mod),
        data_used = list(df),
        tidy      = list(coefs),
        glance    = list(glance_tbl)
      )
    }) %>%
    ungroup()
}
## =====================================================================
## 5A. Ajustar modelos ITS AR(1)
## =====================================================================

models_cases_ar1  <- fit_its_ar1_by_race(dados, "casos")
models_intern_ar1 <- fit_its_ar1_by_race(dados, "internacoes_tb")
models_deaths_ar1 <- fit_its_ar1_by_race(dados, "obitos_tb")

## =====================================================================
## 6A. Exportar resultados dos modelos AR(1)
## =====================================================================

flatten_ar1_results <- function(models_tbl, indicador_label) {
  
  tidy_df <- models_tbl %>%
    select(raca_cat, race_en, outcome, tidy) %>%
    unnest(tidy) %>%
    mutate(indicator = indicador_label)
  
  glance_df <- models_tbl %>%
    select(raca_cat, race_en, outcome, glance) %>%
    unnest(glance) %>%
    mutate(indicator = indicador_label)
  
  list(tidy = tidy_df, glance = glance_df)
}

res_cases_ar1  <- flatten_ar1_results(models_cases_ar1,  "Notifications")
res_intern_ar1 <- flatten_ar1_results(models_intern_ar1, "Hospitalizations")
res_deaths_ar1 <- flatten_ar1_results(models_deaths_ar1, "Mortality")

tidy_all_ar1 <- bind_rows(
  res_cases_ar1$tidy,
  res_intern_ar1$tidy,
  res_deaths_ar1$tidy
)

glance_all_ar1 <- bind_rows(
  res_cases_ar1$glance,
  res_intern_ar1$glance,
  res_deaths_ar1$glance
)

readr::write_csv(
  tidy_all_ar1,
  file.path(results_dir, "AR1_coefficients_by_indicator_and_race.csv")
)

readr::write_csv(
  glance_all_ar1,
  file.path(results_dir, "AR1_model_glance_by_indicator_and_race.csv")
)

## =====================================================================
## 7A. Criar dataframe de predições para gráficos (taxas ajustadas AR(1))
## =====================================================================

make_predictions_df_ar1 <- function(models_tbl, outcome, label_outcome) {
  
  models_tbl %>%
    mutate(
      pred_df = map2(model, data_used, ~{
        if (is.null(.x)) return(NULL)
        
        df <- .y
        fitted_vals <- as.numeric(predict(.x))
        
        df %>%
          mutate(
            indicator_lab = label_outcome,
            fitted        = fitted_vals,
            rate_obs      = rate,         # taxa observada (já criada na função)
            rate_fit      = fitted        # taxa ajustada
          )
      })
    ) %>%
    select(raca_cat, race_en, pred_df) %>%
    unnest(pred_df)
}

pred_cases_ar1  <- make_predictions_df_ar1(models_cases_ar1,  "casos",         "Notification rate")
pred_intern_ar1 <- make_predictions_df_ar1(models_intern_ar1, "internacoes_tb","Hospitalization rate")
pred_deaths_ar1 <- make_predictions_df_ar1(models_deaths_ar1, "obitos_tb",     "Mortality rate")

pred_all <- bind_rows(pred_cases_ar1, pred_intern_ar1, pred_deaths_ar1)

flatten_ar1_results <- function(models_tbl, indicador_label) {
  
  tidy_df <- models_tbl %>%
    select(raca_cat, race_en, outcome, tidy) %>%
    unnest(tidy) %>%
    mutate(
      indicator = indicador_label,
      sig_flag  = ifelse(p.value < 0.05, "significant", "ns"),
      sig_symbol = ifelse(p.value < 0.05, "*", "")
    )
  
  glance_df <- models_tbl %>%
    select(raca_cat, race_en, outcome, glance) %>%
    unnest(glance) %>%
    mutate(indicator = indicador_label)
  
  list(tidy = tidy_df, glance = glance_df)
}

res_cases_ar1  <- flatten_ar1_results(models_cases_ar1,  "Notifications")
res_intern_ar1 <- flatten_ar1_results(models_intern_ar1, "Hospitalizations")
res_deaths_ar1 <- flatten_ar1_results(models_deaths_ar1, "Mortality")

tidy_all_ar1 <- bind_rows(
  res_cases_ar1$tidy,
  res_intern_ar1$tidy,
  res_deaths_ar1$tidy
)
write.csv(tidy_all_ar1, "ARIMA.csv")
