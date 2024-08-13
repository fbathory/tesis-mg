library(tidyverse)
library(openxlsx)

# Resultados descomposición para rearmar gráfico
res_decomp_p <- read.xlsx("data/resultados descomposición base.xlsx")

# Resultados e20 para regraficar por sexo
e20_escenarios_p <- read.xlsx("data/e20 3 escenarios para presentar.xlsx",sep.names = " ")
