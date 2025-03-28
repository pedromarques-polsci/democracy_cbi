library(dplyr)
library(ggplot2)
library(PanelMatch)
library(tidyverse)

#adicionando tratamento de poliarquia
final_dataset <- final_dataset %>%
  mutate(treatment_polyarchy = ifelse(v2x_polyarchy <= 0.5, 0, 1))

# transformações antes de implementação
final_dataset <- as.data.frame(final_dataset)
final_dataset$year <- as.integer(final_dataset$year)
final_dataset$cowcode <- as.integer(final_dataset$cowcode)



#Treatment plot
DisplayTreatment(
  unit.id = "cowcode",
  time.id = "year",
  treatment = "treatment_polyarchy",
  data = final_dataset,
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

###distribuição da V.D

# Plotando histograma da variável lvau_garriga
hist(
  final_dataset$lvau_garriga, 
  main = "Distribution of Central Bank Index", 
  xlab = "Central Bank Index (raw average)", 
  ylab = "Frequêncy", 
  col = "grey", 
  border = "black"
)

#apagando valores duplicados
duplicates <- final_dataset %>%
  group_by(year, cowcode) %>%
  filter(n() > 1)

print(duplicates)

final_dataset <- final_dataset %>%
  distinct(year, cowcode, .keep_all = TRUE)



# Rodando PanelMatch
t1 <- PanelMatch(
  lag = 4, 
  time.id = "year", 
  unit.id = "cowcode", 
  treatment = "treatment_polyarchy",
  refinement.method = "CBPS.weight",
  data = final_dataset, 
  match.missing = TRUE, 
  covs.formula = ~ v2x_feduni + flex + gini_disp + ind + pop + lpop+ ecopen + real_gdp_pcp_ppp+ 
  gdp_current_dollar + inf_avg_cpi + unemp + population, 
  qoi = "att", 
  lead = 0:4, 
  outcome.var = "lvau_garriga", 
  forbid.treatment.reversal = TRUE, 
  listwise.delete = FALSE
)


t1_resultados <- PanelEstimate(sets = t1, data = final_dataset, number.iterations = 1000, 
                               se.method = "bootstrap")

msets_t1 <- t1$att
summary(t1_resultados$matched.sets)

get_covariate_balance(t1$att, data = final_dataset, 
                      covariates = c("v2x_feduni", "flex", "gini_disp", "ind", "pop", "lpop", "ecopen",
                                     "real_gdp_pcp_ppp", "gdp_current_dollar", "inf_avg_cpi", "unemp", "population"),
                      plot = TRUE, ylim = c(-2, 10))

plot(t1_resultados, ylim = c(-0.5, 0.5), 
     main = "Treatment Effects",
     ylab = "Estimated Treatment Effects", xlab = "Time")

# Rodando PanelMatch para estimar efeito de reversão de tratamento

t1 <- PanelMatch(
  lag = 4, 
  time.id = "year", 
  unit.id = "cowcode", 
  treatment = "treatment_polyarchy",
  refinement.method = "CBPS.weight",
  data = final_dataset, 
  match.missing = TRUE, 
  covs.formula = ~ v2x_feduni + flex + gini_disp + ind + pop + lpop + ecopen + real_gdp_pcp_ppp + 
    gdp_current_dollar + inf_avg_cpi + unemp + population, 
  qoi = "art",  # Mudança para efeito de reversão de tratamento
  lead = 0:4, 
  outcome.var = "lvau_garriga", 
  forbid.treatment.reversal = TRUE, 
  listwise.delete = FALSE
)

# Estimando os efeitos da reversão do tratamento
t1_resultados <- PanelEstimate(
  sets = t1, 
  data = final_dataset, 
  number.iterations = 1000, 
  se.method = "bootstrap"
)

# Obtendo conjuntos de emparelhamento
msets_t1 <- t1$art
summary(t1_resultados$matched.sets)

# Avaliação do balanceamento das covariáveis
get_covariate_balance(
  t1$art, data = final_dataset, 
  covariates = c("v2x_feduni", "flex", "gini_disp", "ind", "pop", "lpop", "ecopen",
                 "real_gdp_pcp_ppp", "gdp_current_dollar", "inf_avg_cpi", "unemp", "population"),
  plot = TRUE, ylim = c(-2, 10)
)

# Visualizando os efeitos estimados
plot(
  t1_resultados, ylim = c(-0.5, 0.5), 
  main = "Effects of Treatment Reversal",
  ylab = "Estimated Reversal Effects", xlab = "Time"
)
