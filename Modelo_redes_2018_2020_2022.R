#install.packages("statnet")
#install.packages("xtable")
library("igraph")
library("tidyverse")
library("readxl")
library("openxlsx")
library("ergm")
library("statnet")
library("mice")
library("ggplot2")
library("GGally")
library("scales")

# @Importando datos #######################################################################################################################################################################
data_net_2018_2020_2022 <- read_excel("C:/Users/USUARIO/Desktop/Estadística/Maestría en Estadística/Social_networks/Proyecto/Data/Initial_data/data_net_2018_2020_2022.xlsx")
# data_net_2018_2020_2022 <- data_net_2018_2020_2022

ruta <- "C:/Users/USUARIO/Desktop/Estadística/Maestría en Estadística/Social_networks/Proyecto/data/Initial_data"
setwd(ruta)

# Aristas
data_net_2018 <- data_net_2018_2020_2022 %>%
                 select("ReporterISO3", "PartnerISO3", "2018")

# Atributos 
Atributos_2018 <- read_excel("Paises_atributos_imputed_2018.xlsx")
Atributos_2018 <- Atributos_2018[ , !(names(Atributos_2018) %in% c("Time", "Time Code", "Country Name"))]

# @Modelo ERGM 2018 #######################################################################################################################################################################
set.seed(0)

to_network_2018 <- data_net_2018[, c("ReporterISO3", "PartnerISO3")]
to_network_2018$ReporterISO3 <- as.character(data_net_2018$ReporterISO3)
to_network_2018$PartnerISO3 <- as.character(data_net_2018$PartnerISO3)

g_igraph_2018 <- graph_from_data_frame(data_net_2018, directed = T)

adjacency_matrix_2018 <- get.adjacency(g_igraph_2018)

nw_2018 <- network(adjacency_matrix_2018, directed = TRUE)
class(nw_2018); nw_2018

x11()
par(mfrow = c(1,1), mar = 0.2*c(1,1,1,1))
set.seed(42)
plot(nw_2018, label = network.vertex.names(nw_2018))

## @Agregando atributos #######################################################################################################################################################################

set.vertex.attribute(nw_2018, 'Agriculture (% of GDP)', as.numeric(Atributos_2018$`Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`))
set.vertex.attribute(nw_2018, 'Industria (% of GDP)', as.numeric(Atributos_2018$`Industry (including construction), value added (% of GDP) [NV.IND.TOTL.ZS]`))
set.vertex.attribute(nw_2018, 'Inversión_extranjera_directa (% of GDP)', as.numeric(Atributos_2018$`Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]`))
set.vertex.attribute(nw_2018, 'GDP per capita (Corriente US$)', as.numeric(Atributos_2018$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`))
set.vertex.attribute(nw_2018, 'Gasto_educación_gobierno (% of GDP)', as.numeric(Atributos_2018$`Government expenditure on education, total (% of GDP) [SE.XPD.TOTL.GD.ZS]`))
set.vertex.attribute(nw_2018, 'Formación_bruta_capital (% of GDP)', as.numeric(Atributos_2018$`Gross capital formation (% of GDP) [NE.GDI.TOTL.ZS]`))
set.vertex.attribute(nw_2018, 'Inflación_x_deflactor', as.numeric(Atributos_2018$`Inflation, GDP deflator (annual %) [NY.GDP.DEFL.KD.ZG]`))
set.vertex.attribute(nw_2018, 'Landlocked', as.numeric(Atributos_2018$landlocked))
set.vertex.attribute(nw_2018, 'Estabilidad_política', as.numeric(Atributos_2018$`Political Stability and Absence of Violence/Terrorism: Estimate [PV.EST]`))
set.vertex.attribute(nw_2018, 'Población_total', as.numeric(Atributos_2018$`Population, total [SP.POP.TOTL]`))
set.vertex.attribute(nw_2018, 'Servicios (% of GDP)', as.numeric(Atributos_2018$`Services, value added (% of GDP) [NV.SRV.TOTL.ZS]`))
set.vertex.attribute(nw_2018, 'Desempleo_total', as.numeric(Atributos_2018$`Unemployment, total (% of total labor force) (modeled ILO estimate) [SL.UEM.TOTL.ZS]`))

nw_2018
ergm_model_2018 <- formula(nw_2018 ~ edges + #mutual + # triangles +
                             nodemain('Agriculture (% of GDP)') +
                             nodemain('Industria (% of GDP)') +
                             nodemain('Inversión_extranjera_directa (% of GDP)') +
                             nodemain('GDP per capita (Corriente US$)') +
                             nodemain('Gasto_educación_gobierno (% of GDP)') +
                             nodemain('Formación_bruta_capital (% of GDP)') +
                             nodemain('Inflación_x_deflactor') +
                             nodemain('Estabilidad_política') +
                             nodemain('Población_total') +
                             nodemain('Servicios (% of GDP)') +
                             nodemain('Desempleo_total') +
                             nodematch('Agriculture (% of GDP)') +
                             nodematch('Industria (% of GDP)') +
                             nodematch('Inversión_extranjera_directa (% of GDP)') +
                             nodematch('GDP per capita (Corriente US$)') +
                             nodematch('Gasto_educación_gobierno (% of GDP)') + 
                             nodematch('Formación_bruta_capital (% of GDP)') +
                             nodematch('Inflación_x_deflactor') +
                             nodefactor('Landlocked') +
                             nodematch('Estabilidad_política') + 
                             nodematch('Población_total') +
                             nodematch('Servicios (% of GDP)') +
                             nodematch('Desempleo_total'))

summary(ergm_model_2018)
set.seed(42)
ergm_fit_2018 <- ergm(formula = ergm_model_2018)
summary(ergm_fit_2018)
ergm_fit_2018$coefficients

anova(ergm_fit_2018)

## @Convergencia ############################################################################################

# mcmc.diagnostics(ergm_fit_2018)

## @simulación ##############################################################################################
start_time <- Sys.time()
sim <- simulate(object = ergm_fit_2018, nsim = 1000, seed = 42)
end_time <- Sys.time()
time_taken <- end_time - start_time; time_taken
summary(sim)
rbind("Valoes obs." = summary(ergm_fit_2018),
      "Media  sim." = colMeans(attr(sim, "stats")))

# Bondad de ajuste del modelo
start_time <- Sys.time()
ergm_gof_2018 <- gof(object = ergm_fit_2018)
end_time <- Sys.time()
ergm_gof_2018
time_taken <- end_time - start_time; time_taken

# gráficos
x11()
par(mfrow = c(3,2), mar = c(4,4,4,2))
plot(ergm_gof_2018)


# @Modelo ERGM 2020 #######################################################################################################################################################################
data_net_2020 <- data_net_2018_2020_2022 %>%
  select("ReporterISO3", "PartnerISO3", "2020")

# Atributos 
Atributos_2020 <- read_excel("Paises_atributos_imputed_2020.xlsx")
Atributos_2020 <- Atributos_2020[ , !(names(Atributos_2020) %in% c("Time", "Time Code", "Country Name"))]

set.seed(0)

to_network_2020 <- data_net_2020[, c("ReporterISO3", "PartnerISO3")]
to_network_2020$ReporterISO3 <- as.character(data_net_2020$ReporterISO3)
to_network_2020$PartnerISO3 <- as.character(data_net_2020$PartnerISO3)

g_igraph_2020 <- graph_from_data_frame(data_net_2020, directed = T)

adjacency_matrix_2020 <- get.adjacency(g_igraph_2020)

nw_2020 <- network(adjacency_matrix_2020, directed = TRUE)
class(nw_2020); nw_2020

x11()
par(mfrow = c(1,1), mar = 0.2*c(1,1,1,1))
set.seed(42)
plot(nw_2020, label = network.vertex.names(nw_2020))

## @Agregando atributos #######################################################################################################################################################################

set.vertex.attribute(nw_2020, 'Agriculture (% of GDP)', as.numeric(Atributos_2020$`Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`))
set.vertex.attribute(nw_2020, 'Industria (% of GDP)', as.numeric(Atributos_2020$`Industry (including construction), value added (% of GDP) [NV.IND.TOTL.ZS]`))
set.vertex.attribute(nw_2020, 'Inversión_extranjera_directa (% of GDP)', as.numeric(Atributos_2020$`Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]`))
set.vertex.attribute(nw_2020, 'GDP per capita (Corriente US$)', as.numeric(Atributos_2020$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`))
set.vertex.attribute(nw_2020, 'Gasto_educación_gobierno (% of GDP)', as.numeric(Atributos_2020$`Government expenditure on education, total (% of GDP) [SE.XPD.TOTL.GD.ZS]`))
set.vertex.attribute(nw_2020, 'Formación_bruta_capital (% of GDP)', as.numeric(Atributos_2020$`Gross capital formation (% of GDP) [NE.GDI.TOTL.ZS]`))
set.vertex.attribute(nw_2020, 'Inflación_x_deflactor', as.numeric(Atributos_2020$`Inflation, GDP deflator (annual %) [NY.GDP.DEFL.KD.ZG]`))
set.vertex.attribute(nw_2020, 'Landlocked', as.numeric(Atributos_2020$landlocked))
set.vertex.attribute(nw_2020, 'Estabilidad_política', as.numeric(Atributos_2020$`Political Stability and Absence of Violence/Terrorism: Estimate [PV.EST]`))
set.vertex.attribute(nw_2020, 'Población_total', as.numeric(Atributos_2020$`Population, total [SP.POP.TOTL]`))
set.vertex.attribute(nw_2020, 'Servicios (% of GDP)', as.numeric(Atributos_2020$`Services, value added (% of GDP) [NV.SRV.TOTL.ZS]`))
set.vertex.attribute(nw_2020, 'Desempleo_total', as.numeric(Atributos_2020$`Unemployment, total (% of total labor force) (modeled ILO estimate) [SL.UEM.TOTL.ZS]`))

nw_2020
ergm_model_2020 <- formula(nw_2020 ~ edges + #mutual + # triangles +
                             nodemain('Agriculture (% of GDP)') +
                             nodemain('Industria (% of GDP)') +
                             nodemain('Inversión_extranjera_directa (% of GDP)') +
                             nodemain('GDP per capita (Corriente US$)') +
                             nodemain('Gasto_educación_gobierno (% of GDP)') +
                             nodemain('Formación_bruta_capital (% of GDP)') +
                             nodemain('Inflación_x_deflactor') +
                             nodemain('Estabilidad_política') +
                             nodemain('Población_total') +
                             nodemain('Servicios (% of GDP)') +
                             nodemain('Desempleo_total') +
                             nodematch('Agriculture (% of GDP)') +
                             nodematch('Industria (% of GDP)') +
                             nodematch('Inversión_extranjera_directa (% of GDP)') +
                             nodematch('GDP per capita (Corriente US$)') +
                             nodematch('Gasto_educación_gobierno (% of GDP)') + 
                             nodematch('Formación_bruta_capital (% of GDP)') +
                             nodematch('Inflación_x_deflactor') +
                             nodefactor('Landlocked') +
                             nodematch('Estabilidad_política') + 
                             nodematch('Población_total') +
                             # nodematch('Servicios (% of GDP)') + 
                             nodematch('Desempleo_total'))

summary(ergm_model_2020)
set.seed(42)
ergm_fit_2020 <- ergm(formula = ergm_model_2020)
summary(ergm_fit_2020)

anova(ergm_fit_2020)

## @Convergencia ############################################################################################

# mcmc.diagnostics(ergm_fit_2020)

## @simulación ##############################################################################################

start_time <- Sys.time()
sim <- simulate(object = ergm_fit_2020, nsim = 1000, seed = 42)
end_time <- Sys.time()
time_taken <- end_time - start_time; time_taken
summary(sim)
rbind("Valoes obs." = summary(ergm_fit_2020),
      "Media  sim." = colMeans(attr(sim, "stats")))

# Bondad de ajuste del modelo
start_time <- Sys.time()
ergm_gof_2020 <- gof(object = ergm_fit_2020)
end_time <- Sys.time()
ergm_gof_2020
time_taken <- end_time - start_time; time_taken

# gráficos
x11()
par(mfrow = c(3,2), mar = c(4,4,4,2))
plot(ergm_gof_2020)


# @Modelo ERGM 2022 #######################################################################################################################################################################
data_net_2022 <- data_net_2018_2020_2022 %>%
  select("ReporterISO3", "PartnerISO3", "2022")

# Atributos 
Atributos_2022 <- read_excel("Paises_atributos_imputed_2022.xlsx")
Atributos_2022 <- Atributos_2022[ , !(names(Atributos_2022) %in% c("Time", "Time Code", "Country Name"))]

set.seed(0)

to_network_2022 <- data_net_2022[, c("ReporterISO3", "PartnerISO3")]
to_network_2022$ReporterISO3 <- as.character(data_net_2022$ReporterISO3)
to_network_2022$PartnerISO3 <- as.character(data_net_2022$PartnerISO3)

g_igraph_2022 <- graph_from_data_frame(data_net_2022, directed = T)

adjacency_matrix_2022 <- get.adjacency(g_igraph_2022)

nw_2022 <- network(adjacency_matrix_2022, directed = TRUE)
class(nw_2022); nw_2022

x11()
par(mfrow = c(1,1), mar = 0.2*c(1,1,1,1))
set.seed(42)
plot(nw_2022, label = network.vertex.names(nw_2022))

## @Agregando atributos #######################################################################################################################################################################

set.vertex.attribute(nw_2022, 'Agriculture (% of GDP)', as.numeric(Atributos_2022$`Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]`))
set.vertex.attribute(nw_2022, 'Industria (% of GDP)', as.numeric(Atributos_2022$`Industry (including construction), value added (% of GDP) [NV.IND.TOTL.ZS]`))
set.vertex.attribute(nw_2022, 'Inversión_extranjera_directa (% of GDP)', as.numeric(Atributos_2022$`Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]`))
set.vertex.attribute(nw_2022, 'GDP per capita (Corriente US$)', as.numeric(Atributos_2022$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`))
set.vertex.attribute(nw_2022, 'Gasto_educación_gobierno (% of GDP)', as.numeric(Atributos_2022$`Government expenditure on education, total (% of GDP) [SE.XPD.TOTL.GD.ZS]`))
set.vertex.attribute(nw_2022, 'Formación_bruta_capital (% of GDP)', as.numeric(Atributos_2022$`Gross capital formation (% of GDP) [NE.GDI.TOTL.ZS]`))
set.vertex.attribute(nw_2022, 'Inflación_x_deflactor', as.numeric(Atributos_2022$`Inflation, GDP deflator (annual %) [NY.GDP.DEFL.KD.ZG]`))
set.vertex.attribute(nw_2022, 'Landlocked', as.numeric(Atributos_2022$landlocked))
set.vertex.attribute(nw_2022, 'Estabilidad_política', as.numeric(Atributos_2022$`Political Stability and Absence of Violence/Terrorism: Estimate [PV.EST]`))
set.vertex.attribute(nw_2022, 'Población_total', as.numeric(Atributos_2022$`Population, total [SP.POP.TOTL]`))
set.vertex.attribute(nw_2022, 'Servicios (% of GDP)', as.numeric(Atributos_2022$`Services, value added (% of GDP) [NV.SRV.TOTL.ZS]`))
set.vertex.attribute(nw_2022, 'Desempleo_total', as.numeric(Atributos_2022$`Unemployment, total (% of total labor force) (modeled ILO estimate) [SL.UEM.TOTL.ZS]`))

nw_2022
ergm_model_2022 <- formula(nw_2022 ~ edges + #mutual + # triangles +
                             nodemain('Agriculture (% of GDP)') +
                             nodemain('Industria (% of GDP)') +
                             nodemain('Inversión_extranjera_directa (% of GDP)') +
                             nodemain('GDP per capita (Corriente US$)') +
                             nodemain('Gasto_educación_gobierno (% of GDP)') +
                             nodemain('Formación_bruta_capital (% of GDP)') +
                             nodemain('Inflación_x_deflactor') +
                             nodemain('Estabilidad_política') +
                             nodemain('Población_total') +
                             nodemain('Servicios (% of GDP)') +
                             nodemain('Desempleo_total') +
                             nodematch('Agriculture (% of GDP)') +
                             nodematch('Industria (% of GDP)') +
                             nodematch('Inversión_extranjera_directa (% of GDP)') +
                             nodematch('GDP per capita (Corriente US$)') +
                             nodematch('Gasto_educación_gobierno (% of GDP)') + 
                             nodematch('Formación_bruta_capital (% of GDP)') +
                             nodematch('Inflación_x_deflactor') +
                             nodefactor('Landlocked') +
                             nodematch('Estabilidad_política') + 
                             nodematch('Población_total') +
                             # nodematch('Servicios (% of GDP)') + 
                             nodematch('Desempleo_total'))

summary(ergm_model_2022)
set.seed(42)
ergm_fit_2022 <- ergm(formula = ergm_model_2022)
summary(ergm_fit_2022)

anova(ergm_fit_2022)

## @Convergencia ############################################################################################

# mcmc.diagnostics(ergm_fit_2022)

## @simulación ##############################################################################################

start_time <- Sys.time()
sim <- simulate(object = ergm_fit_2022, nsim = 1000, seed = 42)
end_time <- Sys.time()
time_taken <- end_time - start_time; time_taken
summary(sim)
rbind("Valoes obs." = summary(ergm_fit_2022),
      "Media  sim." = colMeans(attr(sim, "stats")))

# Bondad de ajuste del modelo
start_time <- Sys.time()
ergm_gof_2022 <- gof(object = ergm_fit_2022)
end_time <- Sys.time()
ergm_gof_2022
time_taken <- end_time - start_time; time_taken

# gráficos
x11()
par(mfrow = c(3,2), mar = c(4,4,4,2))
plot(ergm_gof_2022)
