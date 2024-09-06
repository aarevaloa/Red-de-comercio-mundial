# @Librerias ######################################################################################

#install.packages("tidyverse")
#install.packages("igraph")
#install.packages("readxl")
#install.packages("openxlsx")
#install.packages("VIM")
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("caret")
#install.packages("gridExtra")
#install.packages("statnet")
#install.packages("mice")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("scales")

library("tidyverse")
library("igraph")
library("readxl")
library("openxlsx")
library("VIM")
library("knitr")
library("kableExtra")
library("caret")
library("gridExtra")

# @Datos ######################################################################################

setwd("C:/Users/USUARIO/Desktop/Estadística/Maestría en Estadística/Social_networks/Proyecto/data/Initial_data")
initial_data <- read_csv("Initial_data_all_countrys.csv")


atributos <- read_excel("P_Data_Extract_From_World_Development_Indicators.xlsx")[1:749,]
na <- atributos %>% summarise_all(~ sum(is.na(.))) %>%
                    t()

country_codes <- read_excel("country_codes.xlsx")

## @Atributos cualitativos #####################################################################

cualitative_atributos <- read_excel("Cualitative_variables.xlsx") %>% select('Country Code', 'landlocked', 'continent', 'langoff_1')
cualitative_atributos <- cualitative_atributos %>% mutate(continent = recode(continent, "America" = 1, "Asia" = 2, 'Africa' = 3, 'Europe' = 4, 'Pacific' = 5))
cualitative_atributos = cualitative_atributos[!duplicated(cualitative_atributos$`Country Code`), ] 

na <- cualitative_atributos %>% summarise_all(~ sum(is.na(.))) %>%
  t()

atributos <- left_join(x = atributos, y = cualitative_atributos, by = 'Country Code') 

years <- list("2018" = "2018 in 1000 USD", "2020" = "2020 in 1000 USD", "2022" = "2022 in 1000 USD")
data_net <- data.frame()

for (year in names(years)) {
  year_export <- years[[year]]
  year_data <- initial_data %>%
    filter(TradeFlowName == "Export") %>%
    group_by(ReporterISO3, PartnerISO3) %>%
    summarise(!!year := sum(!!sym(year_export)), .groups = 'drop')
  
  if (nrow(data_net) == 0) {
    data_net <- year_data
  } else {
    data_net <- full_join(data_net, year_data, by = c("ReporterISO3", "PartnerISO3"))
  }
}

## @Países excluidos | ausencia de información en las expos ######################################################################################

ISO <- country_codes$`Country code`

`%notin%` <- Negate(`%in%`)

Filas_eliminadas <- atributos %>% 
  filter(atributos$`Country Code` %notin% ISO)

Paises_excluidos <- unique(Filas_eliminadas$`Country Code`)
length(Paises_excluidos)

## @Base filtrada #####################################################################################################################
Filas_eliminadas_1 <- data_net %>% 
  filter(data_net$ReporterISO3 %notin% ISO)

Filas_eliminadas_2 <- data_net %>% 
  filter(data_net$PartnerISO3 %notin% ISO)

Paises_excluidos <- union(Filas_eliminadas_1$ReporterISO3, Filas_eliminadas_2$PartnerISO3)
length(Paises_excluidos)

data_net <- data_net %>%
  filter(data_net$ReporterISO3 %in% ISO)

data_net <- data_net %>%
  filter(data_net$PartnerISO3 %in% ISO)

kable(summary(data_net), caption = "Table 1: Summary statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# write.xlsx(data_net, "data_net_2018_2020_2022.xlsx")

Paises_atributos_base_inicial <- atributos %>% 
  filter(atributos$`Country Code` %in% ISO)

na <- Paises_atributos_base_inicial %>% summarise_all(~ sum(is.na(.))) %>%
  t()

Paises_atributos_Segmentada_x_años <- list()
for (year in names(years)) {
    Paises_atributos_segmentado <- Paises_atributos_base_inicial[Paises_atributos_base_inicial$Time == as.character(year), ]
    Paises_atributos_segmentado <- Paises_atributos_segmentado[order(Paises_atributos_segmentado$`GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`, decreasing = TRUE), ]
    Paises_atributos_Segmentada_x_años[[as.character(year)]] <- Paises_atributos_segmentado
}


## @Rellenando espacios en Blanco con la media de los K vecinos más cercanos #########################################################
### @Estableciendo estableciendo el valor de k óptimo ################################################################################ 

# Usamos el data frame correspondiente al año 2018
df <- Paises_atributos_Segmentada_x_años[["2022"]] # Reemplace el valor "2022", por el valor del año a evaluar


# Filtrar filas con valores faltantes en la columna 'Population, total'
df <- df[!is.na(df$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`), ]

# Crear pliegues para validación cruzada
folds <- createFolds(df$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, k = 10, list = TRUE, returnTrain = TRUE)

# Lista de valores de k a probar
k_values <- 1:10

# Resultados de error para cada k
errors <- numeric(length(k_values))

# Función para calcular el error de imputación
imputation_error <- function(original, imputed) {
  sum((original - imputed)^2, na.rm = TRUE)
}

for (k in k_values) {
  fold_errors <- numeric(length(folds))
  for (i in 1:length(folds)) {
    train_indices <- folds[[i]]
    test_indices <- setdiff(1:nrow(df), train_indices)
    
    train_data <- df[train_indices, ]
    test_data <- df[test_indices, ]
    
    imputed_data <- kNN(train_data, k = k)
    # Seleccionar solo las columnas originales
    imputed_test <- imputed_data[test_indices, 1:ncol(df)]
    
    # Calcular el error de imputación solo para las columnas relevantes
    fold_errors[i] <- imputation_error(test_data$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`, imputed_test$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`)
  }
  errors[k] <- mean(fold_errors)
}

# Seleccionar el k óptimo
optimal_k <- k_values[which.min(errors)]

# Mostrar el k óptimo
print(optimal_k)

### @Filling Na's ###########################################################
# Reemplace el valor "2022", por el valor del año a imputar (2018,2020,2022)
Paises_atributos_2022 <- Paises_atributos_Segmentada_x_años[["2022"]]
Paises_atributos_imputed <- kNN(Paises_atributos_2022, k = 1)
Paises_atributos_imputed <- Paises_atributos_imputed[, 1:ncol(Paises_atributos_2022)]

write.xlsx(Paises_atributos_imputed, "Paises_atributos_imputed_2022.xlsx")

# @Grafos #############################################################################################################################

g <- list()

for (year in names(years)) {
  year_data <- data_net %>% select(ReporterISO3, PartnerISO3, !!year)
  year_data <- year_data %>% filter(!is.na(!!sym(year)))
  g[[year]] <- graph_from_data_frame(
    d = year_data,
    vertices = unique(c(year_data$ReporterISO3, year_data$PartnerISO3)),
    directed = TRUE
  )
  E(g[[year]])$weight <- year_data[[year]]
}


# @Medidas ######################################################################################

medidas <- data.frame(
  Año = character(),
  Cantidad_nodos = integer(),
  Cantidad_aristas = integer(),
  Grafo_dirigido = logical(),
  Grafo_ponderado = logical(),
  Grado_salida_promedio = numeric(),
  Grado_entrada_promedio = numeric(),
  Mediana_grado = integer(),
  sd_salida = numeric(),
  sd_entrada = numeric(),
  coef_variación = numeric(),
  Densidad = numeric(),
  Correlacion = numeric(),
  Peso_promedio = numeric(),
  Transitividad_global = numeric(),
  Reciprocidad_aristas = numeric(),
  Reciprocidad_diadas = numeric(),
  K_conectividad = integer(),
  Componentes = integer(),
  Tamaño_componente_gigante = integer(),
  Asortatividad = numeric(),
  #Numero_clanes = integer(),
  stringsAsFactors = FALSE
)

for (year in names(g)) {
  graph <- g[[year]]
  
  medidas <- rbind(medidas, data.frame(
    Año = year,
    Cantidad_nodos = vcount(graph),
    Cantidad_aristas = ecount(graph),
    Grafo_dirigido = is_directed(graph),
    Grafo_ponderado = is_weighted(graph),
    Grado_salida_promedio = mean(degree(graph, mode = "out")),
    Grado_entrada_promedio = mean(degree(graph, mode = "in")),
    Mediana_grado = median(degree(graph, mode = "in")),
    sd_salida = sd(degree(graph, mode = "out")),
    sd_entrada = sd(degree(graph, mode = "in")),
    coef_variación_in = (sd(degree(graph, mode = "out"))/mean(degree(graph, mode = "in"))),
    Densidad = edge_density(graph),
    Correlacion = cor(degree(graph, mode = "out"),degree(graph, mode = "in"), method = "pearson"),
    Peso_promedio = mean(E(graph)$weight, na.rm = TRUE),
    Transitividad_global = transitivity(graph, type = "global"),
    Reciprocidad_aristas = reciprocity(graph, mode = "default"),
    Reciprocidad_diadas = reciprocity(graph, mode = "ratio"),
    K_conectividad = vertex_connectivity(graph),
    Componentes = length(decompose(graph)),
    Tamaño_componente_gigante = max(sapply(X = decompose(graph), FUN = vcount)),
    Asortatividad = assortativity_degree(graph)
    #Numero_clanes = clique.number(graph)
  ))
}

medidas_transpuesta <- t(medidas)
medidas_transpuesta_df <- as.data.frame(medidas_transpuesta)
colnames(medidas_transpuesta_df) <- medidas_transpuesta_df[1, ]
medidas_transpuesta_df <- medidas_transpuesta_df[-1, ]

# @Analisis de Conectividad #############################################################################
## 2018 ################################################################################################
# Reemplace el valor "2022", por el valor del año a analizar (2018,2020,2022)
thresholds <- seq(min(E(g[["2022"]])$weight), max(E(g[["2022"]])$weight), length.out = 10000)
num_components <- sapply(thresholds, function(thresh) {
  g_sub <- delete_edges(g[["2022"]], E(g[["2022"]])[weight < thresh])
  components(g_sub)$no
})

df1 <- data.frame(Valor_Export=thresholds, Components=num_components)

giant_component_size <- sapply(thresholds, function(thresh) {
  g_sub <- delete_edges(g[["2022"]], E(g[["2022"]])[weight < thresh])
  max(components(g_sub)$csize) / vcount(g[["2022"]]) * 100
})

df2 <- data.frame(Valor_Export=thresholds, GiantComponent=giant_component_size)

p1 <- ggplot(df1, aes(x=Valor_Export, y=Components)) +
  geom_line() +
  scale_y_log10() +
  scale_x_log10() +
  labs(x="Valor Exportaciones", y="No.componentes conectadas") +
  theme_minimal()

p2 <- ggplot(df2, aes(x=Valor_Export, y=GiantComponent)) +
  geom_line() +
  geom_vline(xintercept = log10(8.5), linetype="dashed", color="blue") +
  scale_x_log10() +
  labs(x="Valor Exportaciones (Logs)", y="T. componente gigante") +
  theme_minimal()

grid.arrange(p1, p2, ncol=2)

# @vizualizaciones ###########################################################################################
### @Layouts ##################################################################################################
l <- list()
for (year in names(g)) {
  set.seed(123)
  grafo <- g[[year]]
  
  if (is_weighted(grafo)) {
    E(grafo)$weight[E(grafo)$weight <= 0] <- 1
  }
  
  l[[year]] <- list()
  l[[year]]$l_she <- layout_on_sphere(grafo)
  l[[year]]$l_nic <- layout_nicely(grafo)
  l[[year]]$l_fr <- layout_with_fr(grafo)
  l[[year]]$l_kk <- layout_with_kk(grafo)
  l[[year]]$l_cir <- layout_in_circle(grafo)
}

### @Cluters, community detection ######################################################
clousters <- list()
for (graph_name in names(g)){
  graph <- g[[graph_name]]
  clousters[[graph_name]] <- list()
  clousters[[graph_name]]$c_wt <- cluster_walktrap(graph)
  clousters[[graph_name]]$c_lp <- cluster_label_prop(graph)
  clousters[[graph_name]]$c_im <- cluster_infomap(graph)
  clousters[[graph_name]]$c_sg <- cluster_spinglass(graph)
}

## @Gráfico por año ###################################################################################################### 
year <- "2018" # Reemplace por el año a graficar (2018,2020,2022)
x11()
plot(g[[year]],
     layout = l[[year]]$l_she,
     vertex.color = adjustcolor('darkolivegreen4', 0.8),
     vertex.frame.color = 'darkolivegreen4',
     vertex.size = log(2 *  strength(g[[year]])),
     vertex.label.color = 'black',
     vertex.label.cex = 0.75,
     vertex.label.dist = 0.85,
     edge.width = 3 * E(g[[year]])$weight / max(E(g[[year]])$weight, na.rm = TRUE),
     main = as.character(year),
     cex.main = 0.5)

## @Graficando 20 países con mayores exportaciones ######################################
# Reemplace el valor "2022", por el valor del año a analizar (2018,2020,2022)
g_filtrado_x_peso <- data_net %>% 
  select(ReporterISO3, PartnerISO3, "2018") %>% # Reemplace por el año a graficar 
  arrange(desc(`2018`)) %>% # Reemplace por el año a graficar 
  distinct(ReporterISO3, .keep_all = TRUE) %>%
  head(80) %>%
  graph_from_data_frame(directed = TRUE)

l_g_filtrado <- layout_nicely(g_filtrado_x_peso)

c_im <- cluster_infomap(g_filtrado_x_peso)
set.seed(123)

x11()
par(mar = c(0, 0, 1.5, 0))
plot(x = c_im, y = g_filtrado_x_peso, 
     main = "Red de comercio internacional" , 
     layout = l_g_filtrado,
     mark.groups = NULL,
     vertex.size = 10, vertex.frame.color = "grey30",
     #vertex.color = adjustcolor(Vcol, 0.8 ),
     edge.color = "grey20", 
     vertex.label.cex = 0.98, 
     vertex.label.color = "black",
     vertex.label.dist = 1.5,
     edge.arrow.size = 0.6, 
     edge.arrow.width = 0.55, 
     edge.width = 0.5,
     edge.label = NA)

