# Paquetes
library(purrr)
library(ggplot2)
library(ggtext)
library(cowplot)
library(readr)
library(dplyr)
library(tidyverse)
library(ggtext)
library(readr)

# Ojo, todo esto no es general, es para k = 3 y el conjunto de datos tiene que
# tener los nombres de columnas que venimos usando

# Función para clasificar como indiqué antes
asignar_topico_individual <- function(x) {
  k <- 3
  x <- x[-1]
  if (any(x >= 0.5)) {
    color <- as.character(which.max(x))
  } else if (min(x) <= .2) {
    color <- paste(setdiff(1:k, which.min(x)), collapse = "_")
  } else {
    color <- paste(1:k, collapse = "_")
  }
  color
}

# Funcion para asignar topico/s y color a cada palabra
asignar_topico_y_color <- function(gamma) {
  gamma <- 
    gamma %>% 
    t() %>% 
    as.data.frame %>% 
    rownames_to_column() %>% 
    rename(palabra = rowname)
  gamma$topico <- apply(gamma, 1, asignar_topico_individual)
  gamma <- 
    gamma %>% 
    mutate(
      # Asignar colores a las categorias
      color = case_when(topico == "1" ~ "mediumorchid",
                        topico == "2" ~ "green4",
                        topico == "3" ~ "deepskyblue2", # yellow
                        topico == "1_2" ~ "darkkhaki",
                        topico == "1_3" ~ "yellow",
                        topico == "2_3" ~ "orange",
                        TRUE ~ ""),
      # Reemplazar por etiquetas de html para ggplot
      reemplazo = ifelse(color == "", 
                         palabra, 
                         paste0("<span style='color:", color, "'>", palabra, "</span>"))
    )
  gamma
}

# Funcion que genera el grafico
pintar_mensaje <- function(gamma, theta, datos, idm, size = 8, prop = 0.6) {
  # Busco el texto original
  nro <- which(datos$mens_id == idm)
  original <- datos$Message.y[nro]
  
  # Busco el texto procesado y obtengo sus palabras
  palabras <- str_split(datos$mensaje[nro], " ")[[1]]
  
  # me quedo con estas palabras y la info de gamma
  palabras <- 
    gamma %>% 
    filter(palabra %in% palabras)
  
  # Mensaje original, reemplazo palabras que llevan colores
  original_col <- reduce2(palabras$palabra, palabras$reemplazo, .init = original, str_replace_all)
  original_col
  
  # Graficos
  titulo <- paste(datos$Nombre_corr2[nro], "-", datos$bloque[nro])
  
  g1 <- 
    ggplot(NULL) +
    aes(x = 0, y = 0, label = original_col) +
    geom_textbox(width = .9, size = size) +
    theme_void() +
    ggtitle(titulo)
  
  g2 <- 
    tibble(
      Probabilidad = theta[idm, ], 
      Tópico = names(theta[idm, ])
    ) %>% 
    ggplot() +
    aes(x = Tópico, y = Probabilidad, fill = Tópico) +
    geom_bar(stat = "identity") +
    # OJO COLORES HARDCODEADOS
    scale_fill_manual(values = c("t_1" = "mediumorchid", "t_2" = "green4", "t_3" = "deepskyblue2"),
                      limits=c("t_1","t_2","t_3"),labels=c("1","2","3")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, .25)) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none", 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.border = element_blank(),
          axis.ticks = element_blank(), 
          plot.margin = unit(c(5.5, 50, 50, 50), "points"))
  g <- plot_grid(g1, g2, nrow = 2, rel_heights = c(prop, 1 - prop))
  return(g)
}






#================================================================================
# PASO A PASO DE COMO FUI PROBANDO HASTA FINALMENTE ESCRIBIR LAS FUNCIONES
#================================================================================
datos <- read_delim("Datos/Concejales/Mensajes_limpios.txt", delim = "\t")
load("topic modeling resultados/res_k3_n1.Rdata")
res <- res_k3_n1
rm(res_k3_n1)

# Recordar
# theta gives P(topick|documentd)
# documentos x topico, cada documento suma 1
theta <- res$mod_lda$theta
# gamma gives P(topick|tokenv)
# topico x palabra, cada columna suma 1
gamma <- res$mod_lda$gamma

# Reacomodo gamma
gamma <- 
  res$mod_lda$gamma %>% 
  t() %>% 
  as.data.frame %>% 
  rownames_to_column() %>% 
  rename(palabra = rowname)
head(gamma)

# Voy a usar las gammas para clasificar a las palabras como provenientes de uno
# u otro tópico. Sabemos que una misma palabra puede provenir de uno u otro
# tópico, pero para clasificar y pintar a una palabra, voy a considerar él o los
# tópicos que más probabilidad tengan dada esa palabra. Si no hay ninguno que se
# destaque (por ejemplo, dada una palabra, los 3 tópicos tienen prob similar, no
# la pinto). Esto es algo arbitrario, por eso Esto lo hago con fines meramente
# de visualización. Ver notas sobre las asignaciones posteriores.

# Si hay uno topico con prob >= 0.5, le asigno ese a la palabra. En cambio, si
# no hay ninguno con prob >0.5, puede que pinte dos de los otros o ninguno. Si
# hay uno de los tres tópicos con prob <= .20, los otros dos estan entre .3 y .5
# sí o sí, pinto ambos. Si no, no pinto ninguno. Es una simplificación,
# cualquier otra podría ser. Situaciones como (.3, .3, .4) o (.2, .35, .45) o
# (.25, .26, .49) quedan sin pintar, o sea estoy siendo conservador. Obs: si
# ninguno de los trse tiene >=.5, decir que haya uno solo <=.2 equivale a que el
# minimo de los tres sea <= .2.

# Función para clasificar como indiqué antes
asignar_color <- function(x) {
  x <- x[-1]
  if (any(x >= 0.5)) {
    color <- as.character(which.max(x))
  } else if (min(x) <= .2) {
    color <- paste(setdiff(1:k, which.min(x)), collapse = "_")
  } else {
    color <- paste(1:k, collapse = "_")
  }
  color
}
k <- 3
# Se lo aplico a todas las palabras
gamma$color_cat <- apply(gamma, 1, asignar_color)
head(gamma)

# Agrego colores propiamente dichos y genero etiquetas para poder pintar en ggplot
# Puse esto, se puede cambiar:
#   topico 1 violeta
#   topico 2 verde
#   topico 3 celeste
#   topicos 1 y 2 marron
#   topicos 1 y 3 amarillo
#   topicos 2 y 3 naranja
#   topicos 1, 2 y 3, no se pinta (sale negro)

gamma <- 
  gamma %>% 
  mutate(
    # Asignar colores a las categorias
    color = case_when(color_cat == "1" ~ "mediumorchid",
                      color_cat == "2" ~ "green4",
                      color_cat == "3" ~ "deepskyblue2", # yellow
                      color_cat == "1_2" ~ "darkkhaki",
                      color_cat == "1_3" ~ "yellow",
                      color_cat == "2_3" ~ "orange",
                      TRUE ~ ""),
    # Reemplazar por etiquetas de html para ggplot
    reemplazo = ifelse(color == "", 
                       palabra, 
                       paste0("<span style='color:", color, "'>", palabra, "</span>"))
  )
head(gamma)

# Ahora a ver cómo quedan los mensaje originales con las palabras pintadas.
# Quiero tomar para explorar docs theta = P(topic / doc) alta
# En cada topico, ordeno de mayor a menor los doc segun P(topico/doc)
top_docs_t1 <- theta[order(theta[, 1], decreasing = T), 1, drop = F]
top_docs_t2 <- theta[order(theta[, 2], decreasing = T), 2, drop = F]
top_docs_t3 <- theta[order(theta[, 3], decreasing = T), 3, drop = F]

# Como hay muchos con probs muy altas, no me quedo con el "top 10" ni nada de eso.
# Elijo un doc al azar que tenga prob mayor a 0.9 
# Al final probé varias hasta encontrar un mensaje que me resultará un poco  más
# interesante y a su vez corto
# Topico 3: 101
set.seed(400)
idx <- sample(sum(top_docs_t3 > 0.9), 1)
idm <- rownames(top_docs_t3)[idx]
idm
# O ir explorando así...
# datos$Message.y[datos$mens_id == rownames(top_docs_t3)[7]]

# Busco el texto original
original <- datos$Message.y[datos$mens_id == idm]
original

# Busco el texto procesado y obtengo sus palabras
datos$mensaje[datos$mens_id == idm]
palabras <- str_split(datos$mensaje[datos$mens_id == idm], " ")[[1]]
palabras

# me quedo con estas palabras y la info de gamma
palabras <- 
  gamma %>% 
  filter(palabra %in% palabras)
palabras

# Busco las probs de cada topico en este doc
theta[idm, ]

# Mensaje original, reemplazo palabras que llevan colores
original_col <- reduce2(palabras$palabra, palabras$reemplazo, .init = original, str_replace_all)
original_col

# Grafico
g1 <- 
  ggplot(NULL) +
  aes(x = 0, y = 0, label = original_col) +
  geom_textbox(width = .9, size = 8) +
  theme_void()
g1

g2 <- 
  tibble(
    Probabilidad = theta[idm, ], 
    Tópico = names(theta[idm, ])
  ) %>% 
  ggplot() +
  aes(x = Tópico, y = Probabilidad, fill = Tópico) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("t_1" = "mediumorchid", "t_2" = "green4", "t_3" = "deepskyblue2"),
                    labels=c("1","2","3")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, .25)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        plot.margin = unit(c(5.5, 50, 50, 50), "points"))
g2

plot_grid(g1, g2, nrow = 2, rel_heights = c(.6, .4))

# Pruebas previas para el grafico
df <- tibble(
  x = 0,
  y = 0,
  mens = "Esta es una prueba, veremos qué sale"
)

ggplot(df) +
  aes(x, y, label = mens) +
  geom_textbox(size = 10) +
  theme_void()

df <- tibble(
  x = 0,
  y = 0,
  mens = "Esta es una <span style='color:blue'> prueba</span>,  <span style='color:red'> veremos </span>qué sale"
)

ggplot(df) +
  aes(x, y, label = mens) +
  geom_textbox(size = 10) +
  theme_void()


