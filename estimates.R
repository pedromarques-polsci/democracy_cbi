# Packages ----------------------------------------------------------------
library(countrycode)
library(dplyr)
library(janitor)
library(ggplot2)
library(PanelMatch)
library(tidyverse)

rm(list = ls())

# 1. LOAD AND ADJUSTMENTS -------------------------------------------------
final_dataset <- read_rds("processed_data/final_dataset.rds") %>% 
  mutate(treatment_polyarchy = ifelse(v2x_polyarchy >= 0.5, 1, 0),
         log_gdp = log(real_gdp_pcp_ppp),
         log_population = log(population))

# Adjustments
final_dataset <- as.data.frame(final_dataset)
final_dataset$year <- as.integer(final_dataset$year)
final_dataset$cowcode <- as.integer(final_dataset$cowcode)

# 2. DESCRIPTIVE STATISTICS -------------------------------------------------


## CBI CHANGE AFTER DEMOCRATIZING -------------------------------------------
desc_1y <- final_dataset %>% 
  group_by(cowcode) %>% 
  arrange(year) %>% 
  mutate(treat_timing = treatment_polyarchy - dplyr::lag(treatment_polyarchy),
         treat_lag = dplyr::lag(treat_timing),
         treat_lead = dplyr::lead(treat_timing)) %>% 
  ungroup()

desc_select_1y <- desc_1y %>% 
  filter(treat_timing == 1 | treat_lag == 1 | treat_lead == 1) %>% 
  group_by(cowcode) %>% 
  arrange(year) %>% 
  mutate(iso3c = countrycode(cowcode, origin = 'cown', 
                             destination = 'iso3c'),
         iso3c = ifelse(cowcode == 345, "YUG", iso3c),
         treat_id = case_when(
           treat_timing == 1 ~ paste0(iso3c, year),
           treat_lag == 1 ~ paste0(iso3c, dplyr::lag(year)),
           treat_lead == 1 ~ paste0(iso3c, dplyr::lead(year)))) %>% 
  group_by(treat_id) %>% 
  reframe(post_garriga = lvaw_garriga[treat_lag == 1] - 
            lvaw_garriga[treat_lead == 1])

desc_select_1y %>% 
  filter(!is.na(post_garriga)) %>% 
  reframe(mean = mean(post_garriga > 0, na.rm = T),
                           mean_not = mean(post_garriga == 0, na.rm = T))

cbi_oneyear <- desc_select_1y %>% 
  filter(post_garriga != 0) %>% 
  ggplot(aes(x = fct_reorder(treat_id, post_garriga), y = post_garriga)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  coord_flip() +
  ylab("Absolute Difference") +
  xlab("Democratization Spell (ISO3C/YEAR)") +
  theme(text = element_text(size = 18)) 

# Two years
desc_2y <- final_dataset %>% 
  group_by(cowcode) %>% 
  arrange(year) %>% 
  mutate(treat_timing = treatment_polyarchy - dplyr::lag(treatment_polyarchy),
         treat_lag = dplyr::lag(treat_timing, n = 2),
         treat_lead = dplyr::lead(treat_timing, n = 2)) %>% 
  ungroup()

desc_select_2y <- desc_2y %>% 
  filter(treat_timing == 1 | treat_lag == 1 | treat_lead == 1) %>% 
  group_by(cowcode) %>% 
  arrange(year) %>% 
  mutate(iso3c = countrycode(cowcode, origin = 'cown', 
                             destination = 'iso3c'),
         iso3c = ifelse(cowcode == 345, "YUG", iso3c),
         treat_id = case_when(
           treat_timing == 1 ~ paste0(iso3c, year),
           treat_lag == 1 ~ paste0(iso3c, year - 2),
           treat_lead == 1 ~ paste0(iso3c, year +2))) %>% 
  group_by(treat_id) %>% 
  reframe(post_garriga = lvaw_garriga[treat_lag == 1] - 
            lvaw_garriga[treat_lead == 1])

desc_select_2y %>% 
  filter(!is.na(post_garriga)) %>% 
  reframe(mean = mean(post_garriga > 0, na.rm = T),
          mean_not = mean(post_garriga == 0, na.rm = T))

desc_select_2y %>% 
  filter(!is.na(post_garriga)) %>% 
  reframe(n = n(post_garriga > 0),
          n_not = n(post_garriga == 0))

cbi_twoyear <- desc_select_2y %>% 
  filter(post_garriga != 0) %>% 
  ggplot(aes(x = fct_reorder(treat_id, post_garriga), y = post_garriga)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  coord_flip() +
  ylab("CBI Absolute Difference") +
  xlab("Democratization Spell (ISO3C/YEAR)") +
  theme(text = element_text(size = 18)) 

ggsave("plots/cbi_oneyear.jpeg", plot = cbi_oneyear, dpi = 500,
       width = 11, height = 7)

ggsave("plots/cbi_twoyear.jpeg", plot = cbi_twoyear, dpi = 500,
       width = 11, height = 7)

## CBI TRENDS --------------------------------------------------------------
final_dataset %>% filter(!is.na(lvaw_garriga), !is.na(treatment_polyarchy)) %>% 
  count(cowcode)

country_bg <- final_dataset %>% 
  filter(year == 1970, treatment_polyarchy == 0) %>% 
  pull(cowcode)

country_treat <- final_dataset %>% 
  filter(cowcode %in% country_bg, year == 2019, treatment_polyarchy == 1) %>% 
  pull(cowcode)

country_control <- final_dataset %>% 
  filter(cowcode %in% country_bg, year == 2019, treatment_polyarchy == 0) %>% 
  pull(cowcode)

cbi_trends <- final_dataset %>% filter(year %in% c(1970, 2019), 
                                       cowcode %in% country_bg) %>% 
  group_by(cowcode) %>% 
  mutate(treatment_polyarchy = max(treatment_polyarchy, na.rm = T)) %>% 
  group_by(treatment_polyarchy, year) %>% 
  reframe(mean_cbi = mean(lvaw_garriga, na.rm = T))

cbi_trends

cbi_trends_plot <- cbi_trends %>% 
  mutate(treatment_polyarchy = case_when(
    treatment_polyarchy == 1 ~ "Treated",
    treatment_polyarchy == 0 ~ "Not treated"
  )) %>% 
  ggplot(aes(x = year, y = mean_cbi, group = treatment_polyarchy)) +
  geom_line(aes(linetype = treatment_polyarchy), linewidth = 1) +
  geom_point(aes(shape = treatment_polyarchy), size = 5) +
  geom_text(aes(x = year, y = mean_cbi, 
                label = round(mean_cbi, 2)),
            vjust = 2) +
  theme_minimal() +
  labs(x = "Year", y = "CBI Mean Value", linetype = "Treatment",
       shape = "Treatment") +
  theme(text = element_text(size = 18)) +
  scale_x_continuous(breaks = c(1970, 2019))

cbi_trends_plot

ggsave("plots/cbi_trends_plot.jpeg", plot = cbi_trends_plot, dpi = 500,
       width = 11, height = 7)


## LATIN AMERICA TIME SERIES -----------------------------------------------
latam <- c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PRY",
           "PER", "SUR", "URY", "VEN", 
           "CRI", "CUB", "DOM", "SLV", "GUF", "GTM", "HTI",
           "HND", "MEX", "NIC", "PAN")

dem_timing_df <- final_dataset %>%
  group_by(cowcode) %>%
  arrange(year) %>% 
  mutate(treat_timing = treatment_polyarchy - dplyr::lag(treatment_polyarchy)) %>% 
  filter(treat_timing == 1) %>%
  summarise(dem_year = first(year))

facet_latam <- final_dataset %>% 
  left_join(dem_timing_df) %>% 
  mutate(iso3c = countrycode(cowcode, origin = 'cown', 
                             destination = 'iso3c'),
         iso3c = ifelse(cowcode == 345, "YUG", iso3c)) %>% 
  filter(iso3c %in% latam,
         !iso3c %in% c("CRI", "CUB", "HTI", "VEN", "ECU")) %>% 
  ggplot(aes(x = year, y = lvaw_garriga)) +
  geom_line() +
  geom_vline(aes(xintercept = dem_year), linetype = "dashed") +
  theme_minimal() +
  labs(x = "Year", y = "Weighted CBI") +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~iso3c, scales = "free_y")

ggsave("plots/facet_latam.jpeg", plot = facet_latam, dpi = 500,
       width = 11, height = 7)

# 3. CUSTOM FUNCTIONS -------------------------------------------------------
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
  
  df_estimate <- results %>% 
    summary() %>% 
    as.data.frame(row.names = NULL) %>% 
    mutate(outcome = vout,
           treatment = vtreat) %>% 
    rownames_to_column("time")
  
  # df_estimate <- data.frame(
  #   time = names(results$estimate),
  #   estimate = unlist(results$estimate),
  #   std.error = unlist(results$standard.error),
  #   row.names = NULL
  # ) %>% 
  #   mutate(outcome = vout,
  #          treatment = vtreat)
  
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


# 4. USEFUL OBJECTS -------------------------------------------------------
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


# 5. RESULTS --------------------------------------------------------------
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

# Adjustments
att <- att %>% clean_names() %>% rename(low.ci = x2_5_percent, 
                                          high.ci = x97_5_percent)

art <- art %>% clean_names() %>% rename(low.ci = x2_5_percent, 
                                        high.ci = x97_5_percent)

# Output
att %>% select(outcome, everything(), -treatment)
art %>% select(outcome, everything(), -treatment)

# Causal plots
facet_labels <- c("Personnel independence", "Limits on lending", 
                  "Objectives", "Policy independence", 
                  "CBI (raw average)", "CBI (weighted average)")

names(facet_labels) <- c("cuk_ceo", "cuk_limlen", "cuk_obj", "cuk_pol", 
                         "lvau_garriga", "lvaw_garriga")

att_plot <- teste %>% 
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
leftover_cov <- c("v2x_feduni", "flex", "unflex", "gini_disp", "ind", 
                      "pop", "lpop", "ecopen", "log_gdp", 
                      "inf_avg_cpi", "unemp", "log_population")

leftover_formula <-  ~ v2x_feduni + flex + unflex + gini_disp + ind + 
  pop + lpop + ecopen + log_gdp + inf_avg_cpi + unemp + 
  log_population

x <- PanelData(final_dataset, unit.id = "cowcode", 
                        time.id = "year", 
                        treatment = "treatment_polyarchy", 
                        outcome = "cuk_limlen")

y <- PanelMatch(
  lag = 4, 
  refinement.method = "CBPS.weight",
  panel.data = x, 
  match.missing = TRUE, 
  covs.formula = leftover_formula, 
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

teste <- w %>% 
  summary() %>% as.data.frame(row.names = NULL) %>% 
  rownames_to_column("time")

df_estimate <- data.frame(
  time = names(w$estimate),
  estimate = unlist(w$estimate),
  std.error = unlist(w$standard.error),
  row.names = NULL
) %>% mutate(low.ci = estimate - 1.96 * std.error,
             high.ci = estimate + 1.96 * std.error)