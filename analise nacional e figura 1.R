library(dplyr)
library(stringr)
library(readr)

## ---------------------------------------------------------
## 1.1. Selecionar termos importantes e formatar
## ---------------------------------------------------------

# Termos de interesse no ITS
terms_of_interest <- c("intervencao", "tempo_apos")

table_article_notifications <- tidy_all_ar1 %>%
  filter(
    indicator == "Notifications",  # só notificações
    term %in% terms_of_interest
  ) %>%
  mutate(
    # rótulo bonitinho pro termo
    effect_label = case_when(
      term == "intervencao" ~ "Level change at Q2 2020",
      term == "tempo_apos"  ~ "Trend change after Q2 2020",
      TRUE                  ~ term
    ),
    # nome de raça em inglês já vem em race_en
    race_label = as.character(race_en),
    # estimativa em escala de taxa (beta): converter para % de mudança
    # Aqui: change% ≈ (exp(beta) - 1) * 100
    perc_change = (exp(estimate) - 1) * 100,
    perc_ci_low = (exp(conf.low)  - 1) * 100,
    perc_ci_high= (exp(conf.high) - 1) * 100
  ) %>%
  select(
    indicator,
    race_label,
    effect_label,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value,
    perc_change,
    perc_ci_low,
    perc_ci_high,
    sig_flag,
    sig_symbol
  ) %>%
  arrange(effect_label, race_label)

## ---------------------------------------------------------
## 1.2. Arredondar e formatar colunas numéricas
## ---------------------------------------------------------

table_article_notifications_fmt <- table_article_notifications %>%
  mutate(
    estimate   = round(estimate,   3),
    std.error  = round(std.error,  3),
    conf.low   = round(conf.low,   3),
    conf.high  = round(conf.high,  3),
    p.value    = signif(p.value,   3),
    perc_change   = round(perc_change,   1),
    perc_ci_low   = round(perc_ci_low,   1),
    perc_ci_high  = round(perc_ci_high,  1)
  )

## ---------------------------------------------------------
## 1.3. Exportar tabela em CSV “pronto para artigo”
## ---------------------------------------------------------

readr::write_csv(
  table_article_notifications_fmt,
  file.path(results_dir, "Table_AR1_ITS_notifications_by_race_for_article.csv")
)


## Quais efeitos são significativos?
sig_effects <- tidy_all_ar1 %>%
  filter(
    term %in% c("intervencao", "tempo_apos"),
    p.value < 0.05
  ) %>%
  select(indicator, outcome, race_en, raca_cat, term, p.value)

# Combinações indicador-raça com pelo menos UM termo significativo
sig_combos <- sig_effects %>%
  distinct(indicator, outcome, race_en, raca_cat)

pred_sig <- pred_all %>%
  inner_join(
    sig_combos,
    by = c("indicator_lab" = "indicator",
           "race_en",
           "raca_cat")
  )

library(ggplot2)
library(stringr)

plot_indicator_ar1 <- function(df_indicator, indicator_label, fig_dir, fname_suffix) {
  
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
      subtitle = "Interrupted time series with AR(1) errors, by race/ethnicity, Brazil, 2015–2023"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text   = element_text(face = "bold"),
      plot.title   = element_text(face = "bold"),
      axis.text.x  = element_text(angle = 45, hjust = 1)
    )
  
  # base do nome de arquivo
  fname_base <- fname_suffix %>%
    stringr::str_replace_all(" ", "_") %>%
    tolower()
  
  ## PDF
  ggsave(
    filename = file.path(fig_dir, paste0("ITS_AR1_", fname_base, ".pdf")),
    plot     = p,
    device   = cairo_pdf,
    width    = 8,
    height   = 6,
    units    = "in"
  )
  
  ## PNG alta resolução
  ggsave(
    filename = file.path(fig_dir, paste0("ITS_AR1_", fname_base, ".png")),
    plot     = p,
    device   = "png",
    width    = 8,
    height   = 6,
    units    = "in",
    dpi      = 600
  )
}


library(ggplot2)
library(stringr)

plot_indicator_ar1 <- function(df_indicator, indicator_label, fig_dir, fname_suffix) {
  
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
      subtitle = "Interrupted time series with AR(1) errors, by race/ethnicity, Brazil, 2015–2023"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text   = element_text(face = "bold"),
      plot.title   = element_text(face = "bold"),
      axis.text.x  = element_text(angle = 45, hjust = 1)
    )
  
  # base do nome de arquivo
  fname_base <- fname_suffix %>%
    stringr::str_replace_all(" ", "_") %>%
    tolower()
  
  ## PDF
  ggsave(
    filename = file.path(fig_dir, paste0("ITS_AR1_", fname_base, ".pdf")),
    plot     = p,
    device   = cairo_pdf,
    width    = 8,
    height   = 6,
    units    = "in"
  )
  
  ## PNG alta resolução
  ggsave(
    filename = file.path(fig_dir, paste0("ITS_AR1_", fname_base, ".png")),
    plot     = p,
    device   = "png",
    width    = 8,
    height   = 6,
    units    = "in",
    dpi      = 600
  )
}

pred_hosp_sig <- pred_sig %>%
  filter(indicator_lab == "Hospitalization rate") %>%
  arrange(race_en, data_base)

plot_indicator_ar1(
  df_indicator   = pred_hosp_sig,
  indicator_label = "Hospitalization rate (significant models)",
  fig_dir         = fig_dir,
  fname_suffix    = "hospitalization_rate_significant_races"
)

pred_death_sig <- pred_sig %>%
  filter(indicator_lab == "Mortality rate") %>%
  arrange(race_en, data_base)

plot_indicator_ar1(
  df_indicator   = pred_death_sig,
  indicator_label = "Mortality rate (significant models)",
  fig_dir         = fig_dir,
  fname_suffix    = "mortality_rate_significant_races"
)


library(dplyr)

## Termos ITS de interesse
terms_of_interest <- c("intervencao", "tempo_apos")

## Combinações indicador × raça com pelo menos um termo significativo
sig_combos <- tidy_all_ar1 %>%
  filter(
    term %in% terms_of_interest,
    p.value < 0.05
  ) %>%
  mutate(
    # mapear o nome do indicador (igual ao que está em pred_all$indicator_lab)
    indicator_lab = dplyr::case_when(
      indicator == "Notifications"     ~ "Notification rate",
      indicator == "Hospitalizations"  ~ "Hospitalization rate",
      indicator == "Mortality"         ~ "Mortality rate",
      TRUE                             ~ indicator
    )
  ) %>%
  distinct(indicator, indicator_lab, race_en, raca_cat)


pred_sig <- pred_all %>%
  inner_join(
    sig_combos,
    by = c("indicator_lab", "race_en", "raca_cat")
  ) %>%
  arrange(indicator_lab, race_en, data_base)

library(ggplot2)
library(stringr)

plot_indicator_sig <- function(df_indicator, indicator_label, fig_dir, fname_suffix) {
  
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
      title = paste0(indicator_label, " – significant ITS models"),
      subtitle = "Interrupted time series with AR(1) errors, only race/ethnicity with p < 0.05"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text   = element_text(face = "bold"),
      plot.title   = element_text(face = "bold"),
      axis.text.x  = element_text(angle = 45, hjust = 1)
    )
  
  fname_base <- fname_suffix %>%
    str_replace_all(" ", "_") %>%
    tolower()
  
  ## PDF
  ggsave(
    filename = file.path(fig_dir, paste0("ITS_AR1_sig_", fname_base, ".pdf")),
    plot     = p,
    device   = cairo_pdf,
    width    = 8,
    height   = 6,
    units    = "in"
  )
  
  ## PNG alta resolução
  ggsave(
    filename = file.path(fig_dir, paste0("ITS_AR1_sig_", fname_base, ".png")),
    plot     = p,
    device   = "png",
    width    = 8,
    height   = 6,
    units    = "in",
    dpi      = 600
  )
}

pred_notifications_sig <- pred_sig %>%
  filter(indicator_lab == "Notification rate") %>%
  arrange(race_en, data_base)

plot_indicator_sig(
  df_indicator   = pred_notifications_sig,
  indicator_label = "Notification rate",
  fig_dir         = fig_dir,
  fname_suffix    = "notification_rate_significant_races"
)


pred_hosp_sig <- pred_sig %>%
  filter(indicator_lab == "Hospitalization rate") %>%
  arrange(race_en, data_base)

plot_indicator_sig(
  df_indicator   = pred_hosp_sig,
  indicator_label = "Hospitalization rate",
  fig_dir         = fig_dir,
  fname_suffix    = "hospitalization_rate_significant_races"
)
pred_death_sig <- pred_sig %>%
  filter(indicator_lab == "Mortality rate") %>%
  arrange(race_en, data_base)

# Só plota se tiver alguma linha
if (nrow(pred_death_sig) > 0) {
  plot_indicator_sig(
    df_indicator   = pred_death_sig,
    indicator_label = "Mortality rate",
    fig_dir         = fig_dir,
    fname_suffix    = "mortality_rate_significant_races"
  )
}

library(dplyr)

dados_total <- dados %>%
  group_by(data_base) %>%
  summarise(
    casos = sum(casos),
    populacao = sum(populacao),
    tempo = first(tempo),
    intervencao = first(intervencao),
    tempo_apos = first(tempo_apos)
  ) %>%
  ungroup()

modelo_total <- glm(
  casos ~ tempo + intervencao + tempo_apos,
  offset = log(populacao),
  family = quasipoisson(link = "log"),
  data = dados_total
)
library(broom)

res_total <- tidy(modelo_total, exponentiate = TRUE, conf.int = TRUE)

res_total
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)

p <- ggplot(pred_plot, aes(x = data_base)) +
  # pontos observados
  geom_point(aes(y = rate_obs), colour = "grey60", alpha = 0.6) +
  # área do IC95% do modelo
  geom_ribbon(aes(ymin = rate_low, ymax = rate_high),
              fill = "red", alpha = 0.18) +
  # linha do modelo ajustado
  geom_line(aes(y = rate_fit),
            colour = "red", linewidth = 1) +
  # linha vertical da intervenção (Q2 2020)
  geom_vline(xintercept = as.Date("2020-04-01"),
             linetype = "dashed") +
  facet_wrap(
    ~ race_en,
    ncol   = 2,
    scales = "free_y"   # 👉 cada raça com seu próprio eixo Y
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Solid line: Model fit  |  Shaded area: 95% Confidence Interval",
    x     = "Year",
    y     = "Rate per 100,000 children (0–4 years)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text  = element_text(face = "bold"),
    plot.title  = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p


library(dplyr)
library(ggplot2)
library(lubridate)

## -----------------------------------------------------------------
## 1. Resumir por ano e raça (mín, máx, mediana)
## -----------------------------------------------------------------

dados_ano <- dados %>%
  mutate(
    ano  = year(data_base),
    taxa = casos / populacao * 1e5
  ) %>%
  group_by(race_en, raca_cat, ano) %>%
  summarise(
    rate_min = min(taxa, na.rm = TRUE),
    rate_max = max(taxa, na.rm = TRUE),
    rate_med = median(taxa, na.rm = TRUE),
    n_trimestres = sum(!is.na(taxa)),
    .groups = "drop"
  )

## -----------------------------------------------------------------
## 2. Paleta de cores (coisa fina)
## -----------------------------------------------------------------
paleta <- c(
  "Indigenous" = "#D73027",  # vermelho queimado
  "Black"      = "#7B3294",  # azul royal
  "Brown"      = "#66BD63",  # verde limão sofisticado
  "White"      = "pink",  # coral
  "Asian"      = "#74ADD1"   # azul pastel elegante
)


## -----------------------------------------------------------------
## 3. Figura A – pontos + mínimo–máximo, tudo no mesmo painel
## -----------------------------------------------------------------

pos_dodge <- position_dodge(width = 0.6)

figA <- ggplot(dados_ano,
               aes(x = factor(ano), color = race_en)) +
  
  # linha vertical ligando mínimo–máximo
  geom_linerange(
    aes(ymin = rate_min, ymax = rate_max),
    position = pos_dodge,
    linewidth = 0.9,
    alpha = 0.6
  ) +
  
  # ponto na mediana anual
  geom_point(
    aes(y = rate_med),
    position = pos_dodge,
    size = 2.6
  ) +
  
  scale_color_manual(values = paleta) +
  labs(
    
    x = "Year",
    y = "Rate per 100,000 children",
    color = "Race/Ethnicity"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title  = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

figA

paleta_lancet <- c(
  "Indigenous" = "#A50026",  # vinho profundo
  "Black"      = "#313695",  # azul escuro
  "Brown"      = "#1A9850",  # verde editorial
  "White"      = "#F46D43",  # coral quente
  "Asian"      = "#7B3294"   # roxo elegante
)
