#install.packages(igraph)
#install.packages(sand)
#install.packages(readxl)
#install.packages(tidyverse)
#install.packages(openxlsx)
library(igraph)
library(sand)
library("readxl")
library("tidyverse")
library("openxlsx")
setwd("C:/Users/USUARIO/Desktop/Estadística/Maestría en Estadística/Social_networks/Proyecto/data/Initial_data")

#datos
#data_atributos_bloques_2018 <- read_excel("Paises_atributos_imputed_2018.xlsx")
data_net <- read_excel("data_net_2018_2020_2022.xlsx")

country_codes <- read_excel("country_codes.xlsx")

years = c('2018', '2020', '2022')
mayores_exportadores = list()
for (year in years) {
  mayores_exportadores[[year]] <- data_net %>% group_by(ReporterISO3) %>% summarise(total_exports = sum(!!sym(year), na.rm =  T)) 
  colnames(mayores_exportadores[[year]]) <- c(names(country_codes[2]),"total_exports")
  mayores_exportadores[[year]] <- mayores_exportadores[[year]] %>% filter(total_exports != 0)
}
  


#for (year in years) {
#  data_comercio_años[[year]] <- data_comercio_bloques[, c("ReporterISO3", "PartnerISO3", year)]
#  data_comercio_años[[year]] <- data_comercio_años[[year]][complete.cases(data_comercio_años[[year]]),] #borrar NA filas
#}
#generar grafo

g <- list()
for (year in years) {
  year_data <- data_net %>% select(ReporterISO3, PartnerISO3, !!year)
  year_data <- year_data %>% filter(!is.na(!!sym(year)))
  year_data <- year_data %>% filter(!!sym(year) != 0)
#  year_data[[year]] <- (year_data[[year]] - mean(year_data[[year]])) / sd(year_data[[year]])
  g[[year]] <- graph_from_data_frame(
    d = year_data,
    vertices = unique(c(year_data$ReporterISO3, year_data$PartnerISO3)),
    directed = TRUE
  )
  E(g[[year]])$weight <- year_data[[year]]
}

#Para obtener resultados del modelo con datos de 2018, 2020 y 2022 cambie el valor de la variable "year" por '2018',
# 2020' y 2022' respectivamente

#=====================================================
#@ Modelo 
#=====================================================
year = '2018'

grafo <- g[[year]]



vcount(grafo)
V(grafo)
is.weighted(grafo)
# paquete para ajustar SBMs
# install.packages('blockmodels')
suppressMessages(suppressWarnings(library(blockmodels)))
# matriz de adyacencia
A <- as.matrix(igraph::as_adjacency_matrix(grafo))


# formulación del modelo
set.seed(777)
comercio.sbm <- blockmodels::BM_bernoulli(membership_type = "SBM", adj = A, verbosity = 0, plotting = "")
#comercio.sbm <- BM_gaussian(membership_type = "SBM", adj = B, plotting = "")
# estimación
estimacion <- comercio.sbm$estimate()
# ICL  verosimilitud de clasificación de integración
ICLs <- comercio.sbm$ICL
ICLs
# n. de grupos optimo
Q <- which.max(ICLs)
Q
# gráfico del ICL
x11()
par(mfrow = c(1,1), mar = c(2.75,2.75,1.5,0.5), mgp = c(1.7,0.7,0))
plot(comercio.sbm$ICL, xlab = "Número de clases (Q)", ylab = "ICL", type = "b", pch = 16,
     main = "Calculo del ICL para cada número de clases")
lines(x = c(Q,Q), y = c(min(ICLs),max(ICLs)), col = "red1", lty = 2)
lines(x = c(4,4), y = c(min(ICLs),ICLs[4]), col = "grey40", lty = 2)
# probabilidades estimadas de pertenencia a las comunidades
Z <- comercio.sbm$memberships[[4]]$Z
# asignaciones
labs <- apply(X = Z, MARGIN = 1, FUN = which.max)
head(x = labs, n = 10)
tail(x = labs, n = 10)
length(labs)

# resumen de las probabilidades maximales
summary(Z[cbind(1:vcount(grafo), labs)])

#vertices_df <- as_data_frame(grafo, what = "vertices")
vertices_df <- data.frame("Country Code"= V(grafo)$name)
colnames(vertices_df) <- names(country_codes[2])

vertices_df <- left_join(x = vertices_df, y = country_codes, by = names(country_codes[2]))

agrupamiento <- cbind(vertices_df, as.data.frame(labs)) 
agrupamiento <- left_join(x = agrupamiento, y = mayores_exportadores[[year]], by = 'Country code')

top_agrupamiento <- agrupamiento %>%
  group_by(labs) %>%
  arrange(desc(total_exports)) %>%
  slice_head(n = 15)


ordenado_agrupamiento <- agrupamiento[order(-agrupamiento$total_exports),]

#nombre_excel_1 = paste("agrupamiento_", year, ".xlsx", sep = "")
#write.xlsx(agrupamiento, nombre_excel)
#write.xlsx(top_agrupamiento, "paises_representantes_2018.xlsx")

# tamaño de las comunidades
table(labs)
cl.cnts <- as.vector(table(labs))
nv = vcount(grafo)
# probabilidades de los grupos
alpha <- table(labs)/vcount(grafo)
round(alpha, 3)

# probabilidades de los grupos (ordenadas)
round(alpha[order(alpha, decreasing = T)], 3)
# matriz de probabilidades de interaccion
Pi <- comercio.sbm$model_parameters[[4]]$pi
round(Pi, 3)
# grafico
x11()
corrplot::corrplot(main = "Matriz de probabilidades de interacción", corr = Pi, type = "full", col.lim = c(0,1),  method = "shade", addgrid.col = "gray90", tl.col = "black")

edges <- as_edgelist(grafo ,names=FALSE)

neworder<-order(labs)

m<-t(matrix(order(neworder)[as.numeric(edges)],2))
x11()
plot(1, 1, xlim = c(0, nv + 1), ylim = c(nv + 1, 0),
      type = "n", axes= FALSE, xlab="Clases",
      ylab="Clases",
      main="Matriz de Adyacencia Organizada por Clases",
      )

rect(m[,2]-0.5,m[,1]-0.5,m[,2]+0.5,m[,1]+0.5,col = "blue1")
rect(m[,1]-0.5,m[,2]-0.5,m[,1]+0.5,m[,2]+0.5,col = "blue1")

cl.lim <- cl.cnts
cl.lim <- cumsum(cl.lim)[1:(length(cl.lim)-1)]+0.5

clip(0,nv+1,nv+1,0)

abline(v=c(0.5,cl.lim,nv+0.5),
       h=c(0.5,cl.lim,nv+0.5),col="grey40")


#ver interacciones entre clusters
Pi.mat <- comercio.sbm$model_parameters[[4]]$pi
Pi.mat[1,]
Pi.mat[2,]
Pi.mat[3,]
Pi.mat[4,]


top_agrupamiento <- agrupamiento %>%
  group_by(labs) %>%
  arrange(desc(total_exports)) %>%
  slice_head(n = 6)


data_net <- data_net %>%
  filter(data_net$ReporterISO3 %in% top_agrupamiento$"Country code")

data_net <- data_net %>%
  filter(data_net$PartnerISO3 %in% top_agrupamiento$"Country code")

data <- data_net %>% select(ReporterISO3, PartnerISO3, year)

subgrafo <- graph_from_data_frame(
  d = data,
  vertices = unique(c(data$ReporterISO3, data$PartnerISO3)),
  directed = TRUE)

# Grafico subgrafo de paises con mayores exportaciones
vcount(subgrafo)
V(subgrafo)$labs <- top_agrupamiento$labs[match(V(subgrafo)$name, top_agrupamiento$`Country code`)]


Vcol <- ifelse(V(subgrafo)$labs == 1, "#d7191c",
                   ifelse(V(subgrafo)$labs == 2, "#fdae61",
                          ifelse(V(subgrafo)$labs == 3, "#abdda4",
                                 ifelse(V(subgrafo)$labs == 4, "#2b83ba", "grey"))))
E(subgrafo)$color <- adjustcolor(Vcol[match(ends(subgrafo, E(subgrafo))[, 1], V(subgrafo)$name)], 0.55)

#le = layout_on_sphere(subgrafo)
lf = layout_with_fr(subgrafo)
set.seed(777)
x11()
par(mar = c(0, 0, 1.5, 0))
plot(subgrafo, main = "Red de comercio internacional con representantes notables de cada clase" , layout = lf,
     vertex.size = 10, vertex.frame.color = "grey30",
     vertex.color = adjustcolor(Vcol, 0.8 ),
     edge.color = E(subgrafo)$color, vertex.label.cex = 0.98,vertex.label.dist = 1.5,
     edge.arrow.size = 0.6, edge.arrow.width = 0.55, edge.width = 0.5,
     edge.label = NA, edge.label.cex = 0.73, vertex.label.color = 'black')
legend("topright", legend = c("1", "2", "3", "4"),
       fill = c("#d7191c", "#fdae61", "#abdda4", "#2b83ba"),
       title = "Clase", cex = 1, horiz = FALSE,box.lty = 0)

# install.packages("maps")
# install.packages("ggplot2")
# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
library(maps)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


set.seed(777)
agrupamiento <- agrupamiento %>%
  mutate(labs = ifelse(labs == 2, 3, ifelse(labs == 3, 2, labs)))



world <- ne_countries(scale = "medium", returnclass = "sf")

# Ensure Column Names Match
colnames(agrupamiento)[1] <- "iso_a3"  # Renaming 'Country.Code' to 'iso_a3' to match the world dataset

# Convert world to a data frame
world_df <- as.data.frame(world)

# Merge Your Data with World Map Data
world_merged <- left_join(world_df, agrupamiento, by = "iso_a3")

# Convert back to sf object
world_merged <- st_as_sf(world_merged)

colors <- c("#FF5733", "#FFC300", "#DAF7A6", "#C70039") 
colors <- c("1" = "#1f78b4", "2" = "#33a02c", "3" = "#e31a1c", "4" = "#ff7f00")
colors <- c("1" = "#d7191c", "2" = "#fdae61", "3" = "#abdda4", "4" = "#2b83ba")

set.seed(777)
x11()
ggplot(data = world_merged) +
  geom_sf(aes(fill = as.factor(labs))) +
  scale_fill_manual(values = alpha(colors, 0.7), na.value = "grey50", guide = guide_legend(title = "Clases")) +
  theme_minimal() +
  labs(title = paste("Mapa de clases de países en la red de comercio. Año",  year, sep=" "),
       subtitle = "Cada color representa una clase") +
  theme(legend.position = "bottom")






