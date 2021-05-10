library(readr) #1
library(tidyr) #2
library(tidyverse) #3
library(tidytext) #4
library(tm) #5
library(stringr) #6
library(widyr) #7
library(ggraph) #8
library(influential) #9



datos2 <- read_delim("Datos/Concejales/Mensajes_limpios.txt", delim = "\t")
datos2 <- datos2%>%separate( mens_id,into=c("id","Nombre","bloque","periodo","Pago"),sep = "_")

######## Funcion para los grafos #######
#count_bigrams <- function(dataset) {
#  dataset %>%
#    unnest_tokens(bigram, mens, token = "ngrams", n = 2) %>%
#    separate(bigram, c("word1", "word2"), sep = " ") %>%
#    filter(!word1 %in% word$word,
#           !word2 %in% word$word) %>%
#    count(word1, word2, sort = TRUE)
#}

#visualize_bigrams <- function(bigrams) {
#  set.seed(2016)
#  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
#  bigrams %>%
#    graph_from_data_frame() %>%
#    ggraph(layout = "fr") +
#    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
#    geom_node_point(color = "lightblue", size = 5) +
#    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
#    theme_void()
#}
tema1 <- theme(legend.position = "none",
               legend.text = element_text(size = 10),
               axis.title.x = element_text(size = 13, face = "bold"),
               axis.title.y = element_text(size = 13, face = "bold"),
               axis.text.x = element_text(size=10,face = "bold"),
               axis.text.y = element_text(size=12,face = "bold"))



####Cambiemos#######
######## Transformación de la variable en palabras y frecuencia
cambiemos <- subset(datos2, bloque=="Cambiemos")
camb_mens <- tibble(cambiemos$mensaje)
camb_mens <- rename(camb_mens, mens = "cambiemos$mensaje")

camb_mens2 <- camb_mens %>%
  unnest_tokens(word, mens)

camb_mens2 <- camb_mens2 %>%count(word, sort = TRUE) 
  

camb_mens2 <- subset(camb_mens2, word!="rosario" & word!="ciudad")

## Gráfico de frecuencias por palabra
grafico_frec_cambiemos <- head(camb_mens2,15)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, color="yellow", fill = "yellow")) +
  scale_color_manual(values = "black")+
  scale_fill_manual(values = "yellow")+
  geom_col() +
  xlab(NULL) +
  ylab("Frecuencia")+
  coord_flip()+
  theme_minimal()+tema1
  
ggsave("Gráficos/Palabras frecuentes cambiemos.jpg",grafico_frec_cambiemos, width = 15, height = 15, units = "cm")

## Transformación de la variable en palabras por documento
camb_mens3 <- camb_mens
camb_mens3$ID <- c(1:nrow(camb_mens3))

camb_post_words <- camb_mens3 %>%
  unnest_tokens(word, mens)

camb_post_words

##Calculo correlaciones y luego saco las palabras correlacionadas con
## ciudad, provincia y país
word_cors <- camb_post_words %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, ID, sort = TRUE)

word_cors

cor_ciudad <- word_cors %>%filter(item1 == "ciudad")

cor_prov <- word_cors%>%filter(item1 == "provincia")

cor_pais <- word_cors%>%filter(item1 == "país")

## Grafo de correlaciones
set.seed(2016)
grafo_cor_cambiemos<- word_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "yellow", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

ggsave("Gráficos/Grafo correlaciones cambiemos.jpg",grafo_cor_cambiemos, width = 25, height = 15, units = "cm")


#################################################
## Repito mismos pasos para todos los bloques ##
################################################

#### Ciudad Futura
cf <- subset(datos2, bloque=="Ciudad Futura")
cf <- tibble(cf$mensaje)
cf_mens <- rename(cf, mens = "cf$mensaje")
cf_mens2 <- cf_mens %>%
  unnest_tokens(word, mens)
cf_mens2 <- cf_mens2 %>%count(word, sort = TRUE)
   

cf_mens2 <- subset(cf_mens2, word!="rosario" & word!="ciudad")

##Gráfico de frecuencias
grafico_frec_cf <- head(cf_mens2,15)%>%
  #filter(n > 80) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, color="red",fill="red")) +
  scale_color_manual(values = "black")+
  scale_fill_manual(values = "red")+
  geom_col() +
  ylab("Frecuencia")+
  xlab(NULL) +
  coord_flip()+
  theme_minimal()+
  tema1
ggsave("Gráficos/Palabras frecuentes cf.jpg",grafico_frec_cf, width = 15, height = 15, units = "cm")


cf_mens3 <- cf_mens
cf_mens3$ID <- c(1:nrow(cf_mens3))

cf_post_words <- cf_mens3 %>%
  unnest_tokens(word, mens) 

cf_post_words

word_cors <- cf_post_words %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, ID, sort = TRUE)

word_cors

cor_ciudad <- word_cors %>%filter(item1 == "ciudad")

cor_prov <- word_cors %>%filter(item1 == "provincia")

cor_pais <- word_cors %>%filter(item1 == "país")

set.seed(2016)

##Grafo correlaciones
grafo_cor_cf<- word_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "red", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

ggsave("Gráficos/Grafo correlaciones cf.jpg",grafo_cor_cf, width = 25, height = 15, units = "cm")

####FNP#######
fnp <- subset(datos2, bloque=="Frente Nac. y Pop")
fnp_mens <- tibble(fnp$mensaje)
fnp_mens <- rename(fnp_mens, mens = "fnp$mensaje")
fnp_mens2 <- fnp_mens %>%
  unnest_tokens(word, mens)

fnp_mens2 <- fnp_mens2 %>%
  count(word, sort = TRUE) 

fnp_mens2 <- subset(fnp_mens2, word!="rosario" & word!="ciudad")

##Frecuencia de palabras
grafico_frec_fnp <- head(fnp_mens2,15)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n,color="blue",fill="blue")) +
  scale_color_manual(values = "black")+
  scale_fill_manual(values = "blue")+
  ylab("Frecuencia")+
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  theme_minimal()+
  tema1

ggsave("Gráficos/Grafico frecuencias fnp.jpg",grafico_frec_fnp, width = 15, height = 15, units = "cm")


fnp_mens3 <- fnp_mens
fnp_mens3$ID <- c(1:nrow(fnp_mens3))

fnp_post_words <- fnp_mens3 %>%
  unnest_tokens(word, mens) 

## Correlaciones
word_cors <- fnp_post_words %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, ID, sort = TRUE)

word_cors

cor_ciudad <- word_cors %>%filter(item1 == "ciudad")

cor_prov <- word_cors %>%filter(item1 == "provincia")

cor_pais <- word_cors %>%filter(item1 == "país")


##Grafo correlaciones
set.seed(2016)

grafo_cor_fnp<- word_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "blue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

ggsave("Gráficos/Grafo correlaciones fnp.jpg",grafo_cor_fnp, width = 25, height = 15, units = "cm")


####FSP#######
fsp <- subset(datos2, bloque=="Frente Soc. y Pop")
fsp <- tibble(fsp$mensaje)
fsp_mens <- rename(fsp, mens = "fsp$mensaje")
fsp_mens2 <- fsp_mens %>%
  unnest_tokens(word, mens)
fsp_mens2 <- fsp_mens2 %>%
  count(word, sort = TRUE) 

fsp_mens2 <- subset(fsp_mens2, word!="rosario" & word!="ciudad")

##Frecuencia de palabras
grafico_frec_fsp <- head(fsp_mens2,15)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n,color="violet",fill="violet")) +
  scale_color_manual(values = "black")+
  scale_fill_manual(values = "violet")+
  geom_col() +
  scale_y_continuous(limits = c(0,160),breaks = xbreaks <- seq(0,160,50))+
  ylab("Frecuencia")+
  xlab(NULL) +
  coord_flip()+
  theme_minimal()+
  tema1

ggsave("Gráficos/Grafo palabras frecuentes fsp.jpg",grafico_frec_fsp, width = 15, height = 15, units = "cm")


fsp_mens3 <- fsp_mens
fsp_mens3$ID <- c(1:nrow(fsp_mens3))

fsp_post_words <- fsp_mens3 %>%
  unnest_tokens(word, mens) 

## Correlaciones
word_cors <- fsp_post_words %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, ID, sort = TRUE)

word_cors

cor_ciudad <- word_cors %>%filter(item1 == "ciudad")

cor_prov <- word_cors %>%filter(item1 == "provincia")

cor_pais <- word_cors %>%filter(item1 == "país")

##Grafo correlaciones
set.seed(2016)

word_cors <- subset(word_cors, item1!="paula" & item1!="sarkissian")

grafo_cor_fsp<- word_cors %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "violet", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

ggsave("Gráficos/Grafo correlaciones fsp.jpg",grafo_cor_fsp, width = 25, height = 15, units = "cm")


####Frente progresista#######
fprog <- subset(datos2, bloque=="Frente Progresista")
fprog <- tibble(fprog$mensaje)
fprog_mens <- rename(fprog, mens = "fprog$mensaje")
fprog_mens2 <- fprog_mens %>%
  unnest_tokens(word, mens)
fprog_mens2 <- fprog_mens2 %>%
  count(word, sort = TRUE) 

fprog_mens2 <- subset(fprog_mens2, word!="rosario" & word!="ciudad")

##Frecuencia de palabras
grafico_frec_fprog <- head(fprog_mens2,15)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n,color="orange",fill="orange")) +
  scale_color_manual(values = "black")+
  scale_fill_manual(values = "orange")+
  geom_col() +
  xlab(NULL) +
  ylab("Frecuencia")+
  coord_flip()+
  theme_minimal()+
  tema1

ggsave("Gráficos/Grafico palabras frecuentes fprog.jpg",grafico_frec_fprog, width = 15, height = 15, units = "cm")


fprog_mens3 <- fprog_mens
fprog_mens3$ID <- c(1:nrow(fprog_mens3))

fprog_post_words <- fprog_mens3 %>%
  unnest_tokens(word, mens) 


## Correlaciones
word_cors <- fprog_post_words %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, ID, sort = TRUE)

word_cors

cor_ciudad <- word_cors %>%filter(item1 == "ciudad")

cor_prov <- word_cors %>%filter(item1 == "provincia")

cor_pais <- word_cors %>%filter(item1 == "país")


##Grafo correlaciones
set.seed(2016)

word_cors <- subset(word_cors, item1!="bit.ly" & item1!="https" & item1!="bit" & item1!="ly")

grafo_cor_fprog<- word_cors %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "orange", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

ggsave("Gráficos/Grafo correlaciones fprog.jpg",grafo_cor_fprog, width = 25, height = 15, units = "cm")

