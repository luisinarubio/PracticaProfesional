# Cargar función auxiliar
source("06_func_aux_ajuste_lda.R")
library(textmineR)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(janitor)

datos2 <- read_delim("Datos/Concejales/Mensajes_limpios.txt", delim = "\t")

# Ajustar con k = 3 tópicos ---------------------------------------------------
res_k3_n1 <- lda_textminer(datos2$mensaje, datos2$mens_id, 
                           ngram_window = c(1, 1),
                           seed = 528, k = 3)
res_k3_n1$mod_lda$r2
res_k3_n1$top_terms
mean(res_k3_n1$coh_prev$coherence)
res_k3_n1$top
res_k3_n1$coh_prev

# t1: jerga de campaña (juntos, futuro, gracias, siempre, construir, mejor)
# t2: Propuestas políticas, referencia a problemáticas (trabajo, seguridad, transporte, proyecto, concejo, políticas, estado, situación, salud, obras, estado)
# t3: igualdad de género y/o derechos humanos. Observar que es el único tópico donde no aparecen primero "rosario" o "ciudad" (mujeres, derechos, justicia, lucha, lxs, género, ley, violencia)

# theta gives P(topick|documentd)
# Medias
res_k3_n1$thetas_media_tabla
res_k3_n1$thetas_media_plot

# Principal tópico por mensaje
res_k3_n1$top_topico_tabla
res_k3_n1$top_topico_plot
res_k3_n1$top_topico_plot_periodo
res_k3_n1$mod_lda$r2
res_k3_n1$plot_topic
save(res_k3_n1, file = "topic modeling resultados/res_k3_n1.Rdata")

##2 topicos##
res_k2_n1 <- lda_textminer(datos2$mensaje, datos2$mens_id, 
                           ngram_window = c(1, 1),
                           seed = 528, k = 2)
## 4 tópicos ##
res_k4_n1 <- lda_textminer(datos2$mensaje, datos2$mens_id, 
                           ngram_window = c(1, 1),
                           seed = 528, k = 4)
res_k4_n1$coh_prev$prevalence

## 5 tópicos ##
res_k5_n1 <- lda_textminer(datos2$mensaje, datos2$mens_id, 
                           ngram_window = c(1, 1),
                           seed = 528, k = 5)
res_k5_n1$coh_prev$prevalence
res_k5_n1$plot_topic

mean(res_k2_n1$coh_prev$coherence) #0,046
mean(res_k4_n1$coh_prev$coherence) #0,063 sería el "optimo"
mean(res_k5_n1$coh_prev$coherence) #0,060



