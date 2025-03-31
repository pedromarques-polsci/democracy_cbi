# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(PanelMatch)
library(tidyverse)

rm(list = ls())

# Adicionando tratamento de poliarquia
final_dataset <- read_rds("processed_data/final_dataset.rds") %>% 
  mutate(treatment_polyarchy = ifelse(v2x_polyarchy >= 0.5, 1, 0))

# Transformações antes de implementação
final_dataset <- as.data.frame(final_dataset)
final_dataset$year <- as.integer(final_dataset$year)
final_dataset$cowcode <- as.integer(final_dataset$cowcode)

# Funcoes customizadas
custom_estimate <- function(data, vtreat, vout, cov.f, qoi) {
  
  panel_data <- PanelData(data, unit.id = "cowcode", 
                          time.id = "year", 
                          treatment = vtreat, 
                          outcome = vout)
  
  matched_obj <- PanelMatch(
    lag = 4, 
    refinement.method = "CBPS.weight",
    panel.data = panel_data, 
    match.missing = TRUE, 
    covs.formula = cov.f, 
    qoi = qoi, 
    lead = 0:4, 
    forbid.treatment.reversal = TRUE, 
    listwise.delete = FALSE
  )
  
  results <- PanelEstimate(sets = matched_obj, 
                           panel.data = panel_data, 
                           number.iterations = 1000, 
                           se.method = "bootstrap")
  
  df_estimate <- data.frame(
    time = names(results$estimate),
    estimate = unlist(results$estimate),
    std.error = unlist(results$standard.error),
    row.names = NULL
  ) %>% 
    mutate(outcome = vout,
           treatment = vtreat)
  return(df_estimate)
}
custom_get_balance <- function(data, vtreat, vout, cov.f, qoi, cov.v){
  
  panel_data <- PanelData(data, unit.id = "cowcode", 
                          time.id = "year", 
                          treatment = vtreat, 
                          outcome = vout)
  
  matched_obj <- PanelMatch(
    lag = 4, 
    refinement.method = "CBPS.weight",
    panel.data = panel_data, 
    match.missing = TRUE, 
    covs.formula = cov.f, 
    qoi = qoi, 
    lead = 0:4, 
    forbid.treatment.reversal = TRUE, 
    listwise.delete = FALSE
  )
  
  cov.balance <- get_covariate_balance(matched_obj, panel.data = panel_data, 
                                       covariates = cov.v) %>% 
    as.data.frame() %>% 
    rownames_to_column("time") %>% 
    mutate(outcome = vout, treatment = vtreat)
  
  return(cov.balance)
}
plot_cov_balance <- function(data, var_labels) {
  df <- data %>% pivot_longer(
    cols = !c(time),
    values_to = "value",
    names_to = "variavel"
  ) %>% 
    mutate(time = case_when(time == "t_4" ~ -4,
                            time == "t_3" ~ -3,
                            time == "t_2" ~ -2,
                            time == "t_1" ~ -1,
                            time == "t_0" ~ 0),
           variavel = str_remove(variavel, "^att\\.|^art\\."))
  
  df %>% ggplot(aes(x = time, y = value, group = variavel)) +
    geom_line(aes(colour = variavel)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    scale_colour_manual(name = "Variable", labels = var_labels,
                        values = scales::hue_pal()(n_distinct(df$variavel))) +
    theme_minimal() +
    xlab("Time") +
    ylab("Standard Deviation")
}
get_matches <- function(data, vtreat, vout, cov.f, qoi) {
  
  panel_data <- PanelData(data, unit.id = "cowcode", 
                          time.id = "year", 
                          treatment = vtreat, 
                          outcome = vout)
  
  matched_obj <- PanelMatch(
    lag = 4, 
    refinement.method = "CBPS.weight",
    panel.data = panel_data, 
    match.missing = TRUE, 
    covs.formula = cov.f, 
    qoi = qoi, 
    lead = 0:4, 
    forbid.treatment.reversal = TRUE, 
    listwise.delete = FALSE
  )
  
  if(qoi == "att") {
    return(list(print(vout), matched_obj$att))
  } else if(qoi == "art") {
    return(list(print(vout), matched_obj$art))
  }
  
}

# Preliminares
covariate.vector <- c("v2x_feduni", "flex", "unflex", "gini_disp", "ind", 
                      "pop", "lpop", "ecopen", "real_gdp_pcp_ppp", 
                      "inf_avg_cpi", "unemp", "population")

label_mapping <- c(
  "v2x_feduni"       = "Division of Power Index",
  "flex"             = "Exchange Regime Relaxation",
  "unflex"           = "Exchange Regime Tightening",
  "gini_disp"        = "Disposable Gini",
  "ecopen"           = "Economic Openness",
  "real_gdp_pcp_ppp" = "Real GDP",
  "inf_avg_cpi"      = "Inflation",
  "unemp"            = "Unemployment",
  "population"       = "Population",
  "lpop"             = "Leftist Populist",
  "pop"              = "Populist")

covariate.formula <-  ~ v2x_feduni + flex + unflex + gini_disp + ind + 
  pop + lpop + ecopen + real_gdp_pcp_ppp + inf_avg_cpi + unemp + population

vd <- c("lvau_garriga", "lvaw_garriga", "cuk_ceo", "cuk_obj", "cuk_pol", 
        "cuk_limlen")

# Estimacao de efeitos
att <- map(.x = vd, ~custom_estimate(vout = .x, 
                                     data = final_dataset, 
                                     vtreat = "treatment_polyarchy",
                                     cov.f = covariate.formula, 
                                     qoi = "att")) %>% 
  list_rbind()

art <- map(.x = vd, ~custom_estimate(vout = .x, 
                                     data = final_dataset, 
                                     vtreat = "treatment_polyarchy",
                                     cov.f = covariate.formula, 
                                     qoi = "art")) %>% 
  list_rbind()

# Estimation Plots

att %>% 
  group_by(outcome, treatment) %>% 
  mutate(low.ci = estimate - 1.96 * std.error,
         high.ci = estimate + 1.96 * std.error) %>% 
  ungroup() %>% 
  ggplot(aes(x = time, y = estimate, color = treatment)) +
  geom_point(size = 3) +  # Aumenta o tamanho dos pontos
  geom_errorbar(aes(ymin = low.ci, ymax = high.ci), width = 0.2, size = 1) +  # Barras de erro mais visíveis
  geom_hline(yintercept = 0, linetype = 'dashed', color = "black") +  # Linha de referência
  facet_wrap(~outcome) +  # Mantendo a escala do eixo Y padronizada
  theme_minimal(base_size = 14) +  # Tema mais limpo
  labs(x = "", y = "Treatment Effect", color = "Treatment") +  # Rótulos informativos
  scale_color_manual(values = c("black", "red")) +  # Define cores para os tratamentos
  theme(
    legend.position = "bottom",  # Move a legenda para baixo
    strip.text = element_text(face = "bold", size = 14),  # Títulos dos facetados mais destacados
    axis.text = element_text(size = 12),  # Melhor visibilidade dos eixos
    axis.title = element_text(face = "bold"),  # Destaca os títulos dos eixos
    panel.spacing = unit(1, "lines"),  # Aumenta o espaço entre os gráficos
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Adiciona bordas ao redor das facetas
  )

art %>% 
  group_by(outcome, treatment) %>% 
  mutate(low.ci = estimate - 1.96 * std.error,
         high.ci = estimate + 1.96 * std.error) %>% 
  ungroup() %>% 
  ggplot(aes(x = time, y = estimate, color = treatment)) +
  geom_point(size = 3) +  # Aumenta o tamanho dos pontos
  geom_errorbar(aes(ymin = low.ci, ymax = high.ci), width = 0.2, size = 1) +  # Barras de erro mais visíveis
  geom_hline(yintercept = 0, linetype = 'dashed', color = "black") +  # Linha de referência
  facet_wrap(~outcome) +  # Mantendo a escala do eixo Y padronizada
  theme_minimal(base_size = 14) +  # Tema mais limpo
  labs(x = "", y = "Treatment Reversal Effect", color = "Treatment") +  # Rótulos informativos
  scale_color_manual(values = c("black", "red")) +  # Define cores para os tratamentos
  theme(
    legend.position = "bottom",  # Move a legenda para baixo
    strip.text = element_text(face = "bold", size = 14),  # Títulos dos facetados mais destacados
    axis.text = element_text(size = 12),  # Melhor visibilidade dos eixos
    axis.title = element_text(face = "bold"),  # Destaca os títulos dos eixos
    panel.spacing = unit(1, "lines"),  # Aumenta o espaço entre os gráficos
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Adiciona bordas ao redor das facetas
  )


# Matches
get_matches(vout = "lvaw_garriga", 
            data = final_dataset, 
            vtreat = "treatment_polyarchy",
            cov.f = covariate.formula, 
            qoi = "att")

# Covariance balance
att_balance <- map(.x = vd, ~custom_get_balance(vout = .x, 
                                          data = final_dataset, 
                                          vtreat = "treatment_polyarchy",
                                          cov.f = covariate.formula, 
                                          qoi = "att",
                                          cov.v = covariate.vector)) %>% 
  list_rbind()

art_balance <- map(.x = vd, ~custom_get_balance(
  vout = .x, 
  data = final_dataset, 
  vtreat = "treatment_polyarchy",
  cov.f = covariate.formula, 
  qoi = "art",
  cov.v = covariate.vector)) %>% 
  list_rbind()

# Covariance balance plots
att_balance_plot <- att_balance %>% 
  group_nest(outcome, treatment) %>% 
  mutate(
  plot = map(data, var_labels = label_mapping, plot_cov_balance))

## Example
att_balance_plot$plot[1]

art_balance_plot <- art_balance %>% 
  group_nest(outcome, treatment) %>% 
  mutate(
    plot = map(data, var_labels = label_mapping, plot_cov_balance))


# Heat Plot
generic_panel_dataset <- PanelData(final_dataset, unit.id = "cowcode", 
                                        time.id = "year", 
                                        treatment = "treatment_polyarchy", 
                                        outcome = "lvaw_garriga")

DisplayTreatment(
  panel.data = generic_panel_dataset,
  color.of.treated = "red",
  color.of.untreated = "lightblue",  # azul mais claro
  title = "Treatment Plot - Polyarchy",
  xlab = "",
  ylab = "",
  x.size = NULL,
  y.size = NULL,
  legend.position = "none",
  x.angle = NULL,
  y.angle = NULL,
  legend.labels = c("not treated", "treated"),
  decreasing = FALSE,
  matched.set = NULL,   # Usa apenas o primeiro conjunto
  show.set.only = FALSE,
  hide.x.tick.label = FALSE,
  hide.y.tick.label = TRUE,
  gradient.weights = FALSE,
  dense.plot = TRUE   # ativa o modo denso
)
