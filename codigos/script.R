# Borrramos la memoria
rm(list = ls())

# Librerias
library(arules)
library(arulesViz)
library(arulesSequences)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)

# Abrimos el archivo
datos <- fread("archivos/e-shop clothing 2008.csv", sep = ";")

#==================================Consigna A===================================
# Exploramos los datos
summary(datos)
str(datos)
head(datos)

# Como vimos que la mayoria de las variables contenian numeros que asociaban a una categoria,
#decidimos transfomar estas variables

# Unimos las variables year, month, day y creamos una nueva variable "Fecha"
datos <- datos %>%
  mutate(Fecha = as.Date(paste(year, month, day, sep = "-")))
datos <- datos %>%
  select(-year, -month, -day)

# Mapeamos los id de los paises
paises <- c("1"="Australia", "2"="Austria", "3"="Belgium", "4"="British Virgin Islands", "5"="Cayman Islands",
            "6"="Christmas Island", "7"="Croatia", "8"="Cyprus", "9"="Czech Republic", "10"="Denmark",
            "11"="Estonia", "12"="unidentified", "13"="Faroe Islands", "14"="Finland", "15"="France", "16"="Germany",
            "17"= "Greece", "18"="Hungary", "19"="Iceland", "20"="India", "21"="Ireland", "22"="Italy", "23"="Latvia",
            "24"= "Lithuania", "25"="Luxembourg", "26"="Mexico", "27"="Netherlands", "28"= "Norway", "29"="Poland",
            "30" = "Portugal", "31" = "Romania", "32" = "Russia",
            "33" = "San Marino", "34" = "Slovakia", "35" = "Slovenia", "36" = "Spain",
            "37" = "Sweden", "38" = "Switzerland", "39" = "Ukraine", "40" = "United Arab Emirates",
            "41" = "United Kingdom", "42" = "USA", "43" = "biz (*.biz)", "44" = "com (*.com)",
            "45" = "int (*.int)", "46" = "net (*.net)", "47" = "org (*.org)")
datos$country <- paises[as.character(datos$country)]

# Mapeamos las categorias de los productos
categorias <- c(
  "1" = "trousers", "2" = "skirts", "3" = "blouses","4" = "sale")
datos$`page 1 (main category)` <- categorias[as.character(datos$`page 1 (main category)`)]

# Mapeamos los colores
colores <- c(
  "1" = "beige", "2" = "black", "3" = "blue", "4" = "brown", "5" = "burgundy",
  "6" = "gray", "7" = "green", "8" = "navy blue", "9" = "many colors", "10" = "olive",
  "11" = "pink", "12" = "red", "13" = "violet", "14" = "white")
datos$colour <- colores[as.character(datos$colour)]

# Mapeamos las posiciones en la pantalla
posicion <- c(  "1" = "top left", "2" = "top in the middle", "3" = "top right",
                "4" = "bottom left", "5" = "bottom in the middle", "6" = "bottom right")
datos$location <- posicion[as.character(datos$location)]

# Mapeamos las posiciones de la fotografia
fotos <- c("1"="en face", "2"="profile")
datos$`model photography` <- fotos[as.character(datos$`model photography`)]

# Mapeamos los precios que superen la media o no
precios <- c("1"="yes", "2"="no")
datos$`price 2` <- precios[as.character(datos$`price 2`)]

# Exploramos los datos con las variables ya transformadas
dim(datos)
summary(datos)
head(datos)

# Identificamos datos vacios en cada columna
colSums(is.na(datos))

# Contamos las filas duplicadas
sum(duplicated(datos))

# Cantidad de sesiones unicas
length(unique(datos$`session ID`)) # 24026 sesiones diferentes

# Cantidad de paises unicos
length(unique(datos$country)) # 47 paises diferentes

# Grafico de clicks en cada parte de la pantalla
clicks_por_ubicacion <- datos %>%
  count(location, name = "clicks")

ubicaciones <- tibble(
  location = c("top left", "top in the middle", "top right",
               "bottom left", "bottom in the middle", "bottom right"),
  x = c(1, 2, 3, 1, 2, 3),
  y = c(2, 2, 2, 1, 1, 1))

datos_plot <- left_join(ubicaciones, clicks_por_ubicacion, by = "location")

ggplot(datos_plot, aes(x = x, y = y)) +
  geom_tile(aes(fill = clicks), width = 0.9, height = 0.9, color = "white") +
  geom_text(aes(label = paste0(location, "\n", clicks)), size = 4, color = "white") +
  scale_fill_gradient(low = "#FF8C00", high = "#EE2C2C") +
  scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  labs(title = "Cantidad de clics por ubicación en pantalla") +
  coord_fixed() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# Grafico de los colores de los productos
top_colores <- datos %>%
  count(colour, sort = TRUE) %>%
  top_n(5)

colores_reales <- c(
  "beige" = "#F5F5DC","black" = "#000000","blue" = "#0000FF","brown" = "#8B4513",
  "burgundy" = "#800020","gray" = "#808080","green" = "#008000","navy blue" = "#000080",
  "many colors" = "#CCCCCC","olive" = "#808000","pink" = "#FFC0CB","red" = "#FF0000",
  "violet" = "#8A2BE2","white" = "#FFFFFF")

ggplot(top_colores, aes(x = reorder(colour, n), y = n, fill = colour)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = colores_reales) +
  labs(
    title = "Colores más frecuentes",
    x = "Color",
    y = "Cantidad"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#C1CDC1", color = NA),  # gris claro
    panel.background = element_rect(fill = "#C1CDC1", color = NA),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Distribucion de precios por categoria
ggplot(datos, aes(x = `page 1 (main category)`, y = price, fill = `page 1 (main category)`)) +
  geom_boxplot(color = "black", outlier.color = "black", outlier.shape = 20, outlier.size = 3) +
  scale_fill_manual(values = c(
    "trousers" = "#2980B9",
    "skirts" = "#C0392B",
    "blouses" = "#27AE60",
    "sale" = "#F1C40F"
  )) +
  labs(
    title = "Distribución del precio por categoría de producto",
    x = "Categorías",
    y = "Precio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none")

# Cantidad de productos que superan la media de la categoria o no
table(datos$`price 2`)

#==================================Consigna B===================================
# Grafico de clicks por sesión
clicks_por_sesion <- datos %>%
  group_by(`session ID`) %>%
  summarise(Clicks = max(order, na.rm = TRUE))

ggplot(clicks_por_sesion %>% filter(Clicks <= 60), aes(x = Clicks)) +
  geom_histogram(binwidth = 1, fill = "tomato", color = "black") +
  labs(title = "Distribución de Clicks por Sesión (hasta 60)",
       x = "Cantidad de clicks",
       y = "Número de sesiones") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Grafico de sesiones por país
sesiones_unicas_por_pais <- datos %>%
  group_by(country) %>%
  summarise(cantidad_sesiones = n_distinct(`session ID`)) %>%
  arrange(desc(cantidad_sesiones))

ggplot(sesiones_unicas_por_pais, aes(x = reorder(country, cantidad_sesiones), y = cantidad_sesiones)) +
  geom_col(fill = "#009ACD") +
  coord_flip() +
  labs(title = "Cantidad de sesiones únicas por país",
       x = "País",
       y = "Sesiones únicas") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Grafico de sesiones por país sin Polonia y los paises que tengan menos de 50 sesiones
sesiones_unicas_por_pais <- sesiones_unicas_por_pais %>%
  filter(country != "Poland", cantidad_sesiones >= 50)

ggplot(sesiones_unicas_por_pais, aes(x = reorder(country, cantidad_sesiones), y = cantidad_sesiones)) +
  geom_col(fill = "#009ACD") +
  coord_flip() +
  labs(title = "Cantidad de sesiones únicas por país (sin Polonia y con países con más de 50 sesiones)",
       x = "País",
       y = "Sesiones únicas") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Grafico de productos vistos por sesión
productos_por_sesion <- datos %>%
  group_by(productos = `page 2 (clothing model)`) %>%
  summarise(cantidad_sesiones = n_distinct(`session ID`)) %>%
  arrange(cantidad_sesiones) %>%
  slice_tail(n = 20)
productos_por_sesion <- productos_por_sesion %>%
  mutate(color = ifelse(row_number() == 20, "destacado", "normal"))

ggplot(productos_por_sesion, aes(x = reorder(productos, -cantidad_sesiones), 
                                 y = cantidad_sesiones, 
                                 fill = color)) +
  geom_col() + 
  scale_fill_manual(values = c("normal" = "#FF4040", "destacado" = "#A52A2A")) +
  labs(title = "Top 20 productos más vistos por sesión",
       x = "Producto",
       y = "Sesiones únicas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")  # Oculta la leyenda si no la querés

# Grafico de categoría de producto por sesión
categoria_por_sesion <- datos %>%
  group_by(`page 1 (main category)`) %>%
  summarise(cantidad_sesiones = n_distinct(`session ID`)) %>%
  arrange(desc(cantidad_sesiones))

ggplot(categoria_por_sesion, aes(x = reorder(`page 1 (main category)`, -cantidad_sesiones), y = cantidad_sesiones)) +
  geom_col(fill = c("#458B74", "#76EEC6", "#76EEC6", "#76EEC6")) + 
  labs(title = "Categoría de productos vistos por cada sesion",
       x = "Categorias",
       y = "Cantidad de sesiones únicas") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#==================================Consigna C===================================
# Agrupamos por mes y contamos los clicks
clicks_por_mes <- datos %>%
  mutate(Mes = lubridate::month(Fecha, label = TRUE, abbr = FALSE)) %>%
  group_by(Mes) %>%
  summarise(Clicks = n())

# Ordenamos los meses
clicks_por_mes$Mes <- factor(clicks_por_mes$Mes,
                             levels = c("abril", "mayo", "junio", "julio", "agosto"))
# Grafico
ggplot(clicks_por_mes, aes(x = Mes, y = Clicks)) +
  geom_bar(stat = "identity", fill = c("#68228B","#BF3EFF","#BF3EFF","#BF3EFF","#BF3EFF")) +
  labs(title = "Evolución mensual de clicks",
       x = "Meses",
       y = "Cantidad de clicks") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#==================================Consigna D===================================
# Número de transacciones (sesiones distintas)
n_transacciones <- datos %>%
  summarise(Transacciones = n_distinct(`session ID`))

# Número de ítems únicos (códigos de producto diferentes)
n_items <- datos %>%
  summarise(Items = n_distinct(`page 2 (clothing model)`))

print(n_transacciones) # Hay un total de 24026 transacciones
print(n_items) # Hay 217 items

#==================================Consigna E===================================
# Agrupamos items por session ID
transacciones_lista <- split(as.character(datos$`page 2 (clothing model)`), datos$`session ID`)

# Convertir a formato "transactions"
transacciones <- as(transacciones_lista, "transactions")

# Utilizamos el algoritmo Apriori para encontrar los itemsets frecuentes
itemsets_frecuentes <- apriori(transacciones,
                               parameter = list(supp = 0.02,
                                                target = "frequent itemsets", 
                                                minlen = 2))

# Vemos los itemsets frecuentes encontrados y los ordenamos por el soporte
summary(itemsets_frecuentes) # Hay 60 itemsets frecuentes
top_items <- head(sort(itemsets_frecuentes, by = "support", decreasing = TRUE),20)
inspect(top_items)

#==================================Consigna F===================================
# Filtramos los datos
datos_polonia <- datos %>% filter(country == "Poland", `page 1 (main category)`== "blouses")

# Convertimos a transacciones
transacciones_polonia <- as(split(datos_polonia$`page 2 (clothing model)`, datos_polonia$`session ID`), "transactions")

# Utilizamos el algoritmo Apriori para encontrar los itemsets frecuentes
reglas <- apriori(transacciones_polonia,
                               parameter = list(supp = 0.02,
                                                confidence = 0.2,
                                                target = "rules"))
# Hay 16 reglas encontradas
reglas

# Ordenamos las reglas por soporte
top_10 <- head(sort(reglas, decreasing = TRUE, by = "support"), 10)
inspect(top_10)

# Grafo de las reglas
plot(reglas, method = "graph", engine = "htmlwidget")

#==================================Consigna G===================================
# Filtramos los datos
datos_checa <- datos %>% filter(country == "Czech Republic", `page 1 (main category)`== "blouses")

# Convertimos a transacciones
transacciones_checa <- as(split(datos_checa$`page 2 (clothing model)`, datos_checa$`session ID`), "transactions")

# Utilizamos el algoritmo Apriori para encontrar los itemsets frecuentes
reglas <- apriori(transacciones_checa,
                  parameter = list(supp = 0.04,
                                   confidence = 0.25,
                                   target = "rules"))
# Hay 7 reglas encontradas
reglas

# Ordenamos las reglas por soporte
top_7_checa <- head(sort(reglas, decreasing = TRUE, by = "support"), 7)
inspect(top_7_checa)

# Grafo de las reglas
plot(reglas, method = "graph", engine = "htmlwidget")

#==================================Consigna H===================================
#Compare los resultados de los dos países. ¿Qué conclusión sobre los consumidores puede obtener de los dos resultados?
#"Los usuarios de Polonia que visitaban la categoría blusas, visitaban más ítems 
#y no se quedaban con el primero, sino que navegan por el sitio web.
#Pero esto no sucede con los usuarios de República Checa, ya que estos revelaron 
#ser más sencillos y compactos, las asociaciones encontradas forman pequeños grupos 
#muy definidos. Esto podría reflejar que los consumidores tienen una preferencia establecida.

#Con esta comparación podemos sacar información importante para aplicar diferentes
#estrategias de comercio y marketing. Por ejemplo, para los usuarios de Polonia podríamos 
#facilitarle una herramienta para que pueda comparar diferentes productos, ayudando en su 
#elección y facilitando la experiencia del usuario. Mientras que en los usuarios de 
#República Checa, se pueden implementar la personalización de ofertas entre distintos 
#productos de la misma categoría, incentivando la venta." 

#==================================Consigna I===================================
# Transformarmos los datos para poder buscar las frecuencias secuenciales
datos_secuenciales <- datos %>%
  group_by(`session ID`) %>% # Agrupamos por sesionID
  mutate(EventID = row_number()) %>% # Cada fila tendra un numero de evento secuencial
  ungroup() %>%
  select(sequenceID = `session ID`, # Renombramos las variables para buscar las frecuencias secuenciales
         EventID,
         item = `page 2 (clothing model)`)

# Guardamos las variables en un archivo txt para luego abrirlo en el formato que se necesita
write.table(datos_secuenciales, file = "archivos/datos_secuenciales.txt",
            sep = " ", quote = FALSE, row.names = FALSE, col.names = FALSE)

# Leemos el archivo
datos_secuenciales <- read_baskets("archivos/datos_secuenciales.txt", info = c("sequenceID", "eventID"))

# Algoritmo Spade
secuencias <- cspade(datos_secuenciales,
                     parameter = list(support = 0.02),
                     control = list(verbose = FALSE))
# Hay 138 secuencias
secuencias

# Filtramos las secuencias que tienen más de un ítem
secuencias_filtradas <- secuencias[size(secuencias) > 1]

# Nos quedamos con 18 secuencias
secuencias_filtradas

# Mostramos los items frecuentes, ordenamos por soporte
inspect(sort(secuencias_filtradas, by = "support", decreasing = TRUE))