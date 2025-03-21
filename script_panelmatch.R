library(dplyr)
library(ggplot2)
library(PanelMatch)

###gráfico de tratamento
DisplayTreatment(
  unit.id = "VARIÁVEL DE UNIDADE",
  time.id = "TEMPO",
  treatment = "VARIÁVEL DE TRATAMENTO",
  data = base_final_2,
  color.of.treated = "red",
  color.of.untreated = "lightblue",  # azul mais claro
  title = "",
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



# Rodando PanelMatch
t1 <- PanelMatch(
  lag = , 
  time.id = "", 
  unit.id = "", 
  treatment = "",
  refinement.method = "",
  size.match = 5,
  data = base_final, 
  match.missing = TRUE, 
  covs.formula = ~ , 
  qoi = "att", 
  lead = 0:6, 
  outcome.var = "", 
  forbid.treatment.reversal = FALSE, 
  listwise.delete = FALSE
)

t1_resultados <- PanelEstimate(sets = t1, data = base_final, number.iterations = 1000, 
                               se.method = "bootstrap")

msets_t1 <- t1$att
summary(t1_resultados$matched.sets)

get_covariate_balance(t1$att, data = base_final, 
                      covariates = c(""),
                      plot = TRUE, ylim = c(-2, 10))

plot(t1_resultados, ylim = c(-1, 1), 
     main = "Efeitos estimados do tratamento ao longo do tempo",
     ylab = "Efeito estimado do tratamento", xlab = "Tempo")

msets_t1[[1]]
print(msets_t1, verbose = TRUE)
attr(msets_t1[[2]], "weights")