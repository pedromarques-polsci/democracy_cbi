# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(PanelMatch)
library(tidyverse)

rm(list = ls())


# 1. LOAD AND ADJUSTMENTS -------------------------------------------------
final_dataset <- read_rds("processed_data/final_dataset.rds") %>% 
  mutate(treatment_polyarchy = ifelse(v2x_polyarchy >= 0.5, 1, 0))

# Adjustments
final_dataset <- as.data.frame(final_dataset)
final_dataset$year <- as.integer(final_dataset$year)
final_dataset$cowcode <- as.integer(final_dataset$cowcode)


# 2. CUSTOM FUNCTIONS -------------------------------------------------------
# Coefficient estimates
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

# Covariance balance
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

# Covariance balance plots
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

# Obter numero de unidades pareadas
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


# 3. USEFUL OBJECTS -------------------------------------------------------
# Covariates' vector
covariate.vector <- c("v2x_feduni", "flex", "unflex", "gini_disp", "ind", 
                      "pop", "lpop", "ecopen", "real_gdp_pcp_ppp", 
                      "inf_avg_cpi", "unemp", "population")

# Covariates labels (for the balance plots)
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

# Covariate formulas for the panel matching
covariate.formula <-  ~ v2x_feduni + flex + unflex + gini_disp + ind + 
  pop + lpop + ecopen + real_gdp_pcp_ppp + inf_avg_cpi + unemp + population

# Outcome variables
vd <- c("lvau_garriga", "lvaw_garriga", "cuk_ceo", "cuk_obj", "cuk_pol", 
        "cuk_limlen")


# 4. RESULTS --------------------------------------------------------------
# Treatment effects for each outcome
att <- map(.x = vd, ~custom_estimate(vout = .x, 
                                     data = final_dataset, 
                                     vtreat = "treatment_polyarchy",
                                     cov.f = covariate.formula, 
                                     qoi = "att")) %>% 
  list_rbind()

# Treatment reversal effects for each outcome
art <- map(.x = vd, ~custom_estimate(vout = .x, 
                                     data = final_dataset, 
                                     vtreat = "treatment_polyarchy",
                                     cov.f = covariate.formula, 
                                     qoi = "art")) %>% 
  list_rbind()


# Causal plots
facet_labels <- c("Personnel independence", "Limits on lending", 
                  "Objectives", "Policy independence", 
                  "CBI (raw average)", "CBI (weighted average)")

names(facet_labels) <- c("cuk_ceo", "cuk_limlen", "cuk_obj", "cuk_pol", 
                         "lvau_garriga", "lvaw_garriga")

att_plot <- att %>% 
  mutate(low.ci = estimate - 1.96 * std.error,
         high.ci = estimate + 1.96 * std.error) %>% 
  ungroup() %>% 
  ggplot(aes(x = time, y = estimate)) +
  geom_point(size = 3) +  # Aumenta o tamanho dos pontos
  geom_errorbar(aes(ymin = low.ci, ymax = high.ci), width = 0.2, size = 1) +  # Barras de erro mais visíveis
  geom_hline(yintercept = 0, linetype = 'dashed', color = "black") +  # Linha de referência
  facet_wrap(~outcome,
             labeller = labeller(outcome = facet_labels)) +  # Mantendo a escala do eixo Y padronizada
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

att_plot

ggsave("plots/att_plot.jpeg", att_plot, 
       width = 10, height = 6, 
       dpi = 500)

art_plot <- art %>% 
  group_by(outcome, treatment) %>% 
  mutate(low.ci = estimate - 1.96 * std.error,
         high.ci = estimate + 1.96 * std.error) %>% 
  ungroup() %>% 
  ggplot(aes(x = time, y = estimate)) +
  geom_point(size = 3) +  # Aumenta o tamanho dos pontos
  geom_errorbar(aes(ymin = low.ci, ymax = high.ci), width = 0.2, size = 1) +  # Barras de erro mais visíveis
  geom_hline(yintercept = 0, linetype = 'dashed', color = "black") +  # Linha de referência
  facet_wrap(~outcome,
             labeller = labeller(outcome = facet_labels)) +  # Mantendo a escala do eixo Y padronizada
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

art_plot

ggsave("plots/art_plot.jpeg", art_plot, 
       width = 10, height = 6, 
       dpi = 500)

# Get number of matches
get_matches(vout = "lvaw_garriga", 
            data = final_dataset, 
            vtreat = "treatment_polyarchy",
            cov.f = covariate.formula, 
            qoi = "att")

# Estimate covariance balance
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

# Plot covariates' balance
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

heat_plot <- DisplayTreatment(
  panel.data = generic_panel_dataset,
  color.of.treated = "red",
  color.of.untreated = "lightblue",  # azul mais claro
  title = "",
  xlab = "",
  ylab = "Countries",
  x.size = NULL,
  y.size = NULL,
  legend.position = "bottom",
  x.angle = 90,
  y.angle = NULL,
  legend.labels = c("Not treated", "Treated"),
  decreasing = FALSE,
  matched.set = NULL,   # Usa apenas o primeiro conjunto
  show.set.only = FALSE,
  hide.x.tick.label = FALSE,
  hide.y.tick.label = TRUE,
  gradient.weights = FALSE,
  dense.plot = T   # ativa o modo denso
)

heat_plot

ggsave("plots/heat_plot.jpeg", heat_plot, 
       width = 8, height = 8, 
       dpi = 500)

# Histogram
lvaw_hist <- final_dataset %>% ggplot(aes(x = lvaw_garriga)) +
  geom_histogram(color="black", fill="grey", binwidth=0.05) +
  theme_minimal() +
  xlab("Central Bank Index (weighted average)") +
  ylab("Frequency")

ggsave("plots/lvaw_hist.jpeg", lvaw_hist, 
       width = 6, height = 3, 
       dpi = 500)

# leftover ----------------------------------------------------------------
x <- PanelData(final_dataset, unit.id = "cowcode", 
                        time.id = "year", 
                        treatment = "treatment_polyarchy", 
                        outcome = "cuk_limlen")

y <- PanelMatch(
  lag = 4, 
  refinement.method = "CBPS.weight",
  panel.data = x, 
  match.missing = TRUE, 
  covs.formula = covariate.formula, 
  qoi = "att", 
  lead = 0:4, 
  forbid.treatment.reversal = TRUE, 
  listwise.delete = FALSE
)

w <- PanelEstimate(sets = y, 
                         panel.data = x, 
                         number.iterations = 1000, 
                         se.method = "bootstrap")

w %>% plot()

df_estimate <- data.frame(
  time = names(w$estimate),
  estimate = unlist(w$estimate),
  std.error = unlist(w$standard.error),
  row.names = NULL
) %>% mutate(low.ci = estimate - 1.96 * std.error,
             high.ci = estimate + 1.96 * std.error)