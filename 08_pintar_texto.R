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

# Lectura de datos
datos <- read_delim("Datos/Concejales/Mensajes_limpios.txt", delim = "\t")

# Cargar funciones auxiliares
source("07_func_aux_pintar_texto.R")

# Cargar resultados de k = 3
load("topic modeling resultados/res_k3_n1.Rdata")
res <- res_k3_n1
rm(res_k3_n1)
SummarizeTopics(res$mod_lda)

# Recordar
# theta gives P(topick|documentd)
# documentos x topico, cada documento suma 1
theta <- res$mod_lda$theta
# gamma gives P(topick|tokenv)
# topico x palabra, cada columna suma 1
gamma <- res$mod_lda$gamma

# Exploro gamma
gamma[, "trabajo"]
gamma[, "sueños"]
gamma[, "proyecto"]
gamma[, "mujer"]
gamma[, "género"]
gamma[, "violencia"]
gamma[, "seguridad"]
gamma[,"riquísimos"]
dim(gamma)
sum(apply(gamma, 2, max) > 0.5) / ncol(gamma)
sum(apply(gamma, 2, max) > 0.9) / ncol(gamma)



# Agregar info a gamma
gamma <- asignar_topico_y_color(res$mod_lda$gamma)
head(gamma)

# Ahora a ver cómo quedan los mensaje originales con las palabras pintadas.
# Con cual mensaje probar? Con alguno que donde haya un topico con alta theta = P(topic / doc). 
# En cada topico, ordeno de mayor a menor los doc segun P(topico/doc)
top_docs_t1 <- theta[order(theta[, 1], decreasing = T), 1, drop = F]
top_docs_t2 <- theta[order(theta[, 2], decreasing = T), 2, drop = F]
top_docs_t3 <- theta[order(theta[, 3], decreasing = T), 3, drop = F]
head(top_docs_t3)

# Hay muchos con probs muy altas! Elijo al azar alguno con prob > 0.40
# Voy probando hasta encontrar alguno corto o que parezca intersante
idx <- sample(sum(top_docs_t3 > 0.9), 1)
idm <- rownames(top_docs_t3)[idx]
original <- datos$Message.y[datos$mens_id == idm]
original

# Produzco graficos sobre el topico 3
pintar_mensaje(gamma, theta, datos, rownames(top_docs_t3)[101])

pintar_mensaje(gamma, theta, datos, rownames(top_docs_t3)[156], size = 10)

pintar_mensaje(gamma, theta, datos, rownames(top_docs_t3)[14], size = 4)

pintar_mensaje(gamma, theta, datos, rownames(top_docs_t3)[142], size = 8)

#Topico 2
idx <- sample(sum(top_docs_t2 > 0.9), 1)
idm <- rownames(top_docs_t2)[idx]
original <- datos$Message.y[datos$mens_id == idm]
original

pintar_mensaje(gamma, theta, datos, rownames(top_docs_t2)[142], size = 8)

pintar_mensaje(gamma, theta, datos, rownames(top_docs_t2)[41], size = 5)

# Publicaciones con probs intermedias -------------------------------------
head(theta)
intermedias <- apply(theta, 1, function(x) all(x >= 0.25 & x <= 0.40))
sum(intermedias)
ids_interm <- rownames(theta)[intermedias]

# Busco mensaje
idm <- sample(ids_interm, 1)
original <- datos$Message.y[datos$mens_id == idm]
original
which(datos$mens_id == idm)

pintar_mensaje(gamma, theta, datos, datos$mens_id[3230], size = 5)

pintar_mensaje(gamma, theta, datos, datos$mens_id[491], size = 15)

pintar_mensaje(gamma, theta, datos, datos$mens_id[3197], size = 5)

pintar_mensaje(gamma, theta, datos, datos$mens_id[91], size = 5)
ggsave("topic modeling resultados/texto_pintado_interm_ej1.png", height = 5, width = 8)


