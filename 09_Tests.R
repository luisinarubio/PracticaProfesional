library(readxl)
library(tidyr)
library(tidyverse)
library(kableExtra)
library(janitor)
library(ggplot2)
library(lattice)
library(tm)
library(cowplot)
library(agricolae)
library(coin)
library(dunn.test)

datos <- read_excel("Datos/Concejales/Posts/Datos de febrero a noviembre corregidos.xlsx")
datos <-  datos[!duplicated(datos$Permalink), ]
datos$Engagement <- as.numeric(datos$Engagement)
datos$Shares <- as.numeric(datos$Shares)
datos$Comments <- as.numeric(datos$Comments)
datos$Reactions <- as.numeric(datos$Reactions)
datos$`Reactions - Like` <- as.numeric(datos$`Reactions - Like`)
datos$`Reactions - Love` <- as.numeric(datos$`Reactions - Love`)
datos$`Reactions - Haha` <- as.numeric(datos$`Reactions - Haha`)
datos$`Reactions - Wow` <- as.numeric(datos$`Reactions - Wow`)
datos$`Reactions - Sad` <- as.numeric(datos$`Reactions - Sad`)
datos$`Reactions - Angry` <- as.numeric(datos$`Reactions - Angry`)

datos <- datos %>% rename(Concejal = Nombre_corr2) %>%
  mutate(bloque = recode(bloque,`Ciudad Futura` = "CF", 
                         `Frente Progresista` = "FP",
                         Cambiemos = "C", `Frente Nac. y Pop` = "FNyP", 
                         `Frente Soc. y Pop` = "FSyP"), 
         periodo = factor(periodo, labels = c("Electoral", "Post-Electoral")))


###### Kruskal-Wallis para comparar engagement entre bloques
datos_eng <- datos %>% select(Concejal, bloque, Pago, periodo, Engagement) %>%
  mutate(tipo = "General")

datos_eng <- datos_eng %>%
  bind_rows(datos_eng %>% filter(periodo == "Electoral") %>% 
              mutate(tipo = "Período Electoral")) %>% 
  bind_rows(datos_eng %>% filter(periodo =="Post-Electoral") %>%
              mutate(tipo = "Período Post-Electoral")) %>% 
  bind_rows(datos_eng %>%filter(Pago == "Si") %>% 
              mutate(tipo = "Publicación Paga"))%>% 
  bind_rows(datos_eng %>%filter(Pago == "No") %>% 
              mutate(tipo = "Publicación No Paga"))
kruskal_aux <- function(datos) {
  # Atención: esto es para comparar siempre entre bloques
  res <- kruskal.test(Engagement ~ bloque, data = datos)
  tibble(datos = datos$tipo[1], estadistica = res$statistic, pvalue = res$p.value)
}
res_bloque <- split(datos_eng, datos_eng$tipo) %>% map_df(kruskal_aux)
res_bloque
##Comparaciones múltiples
dunn_aux <- function(datos, vble) {
  res <- dunn.test(datos$Engagement, datos$bloque, method = "bh", table = F)
  tibble(datos = datos$tipo[1], comparacion = res$comparisons, 
         zeta = res$Z, pvalue = res$P.adjusted, 
         rechazo = ifelse(pvalue <= 0.025, "*", ""))
}

res_bloque_compmult <- split(datos_eng, datos_eng$tipo) %>% 
  map_df(dunn_aux, vble = "bloque")
res_bloque_compmult


### Wilcoxon para comparar engagement entre periodos
datos_eng <- datos %>% select(Concejal, bloque, Pago, periodo, Engagement) %>%
  mutate(tipo = "General")

datos_eng <- datos_eng %>% bind_rows(datos_eng %>% filter(Pago == "Si") %>% 
                                       mutate(tipo = "Publicación Paga"))%>%
  bind_rows(datos_eng %>% filter(Pago == "No") %>% 
              mutate(tipo = "Publicación No Paga")) %>%
  bind_rows(datos_eng %>% filter(bloque == "C") %>% 
              mutate(tipo = "C")) %>% bind_rows(datos_eng %>%
                                                  filter(bloque == "CF") %>% mutate(tipo = "CF")) %>% 
  bind_rows(datos_eng %>% filter(bloque =="FNyP") %>% 
              mutate(tipo = "FNyP")) %>% bind_rows(datos_eng %>% 
                                                     filter(bloque =="FP") %>% mutate(tipo = "FP")) %>% 
  bind_rows(datos_eng %>% filter(bloque == "FSyP") %>%
              mutate(tipo = "FSyP")) %>% 
  mutate(tipo = factor(tipo, levels = c("General", "Publicación Paga",
                                        "Publicación No Paga", "C", "CF", "FNyP", "FP", "FSyP")))   
wilcoxon_aux <- function(datos) {
  # Atencion: esto es para comparar entre periodos
  res <- wilcox.test(Engagement ~ periodo, data = datos)
  tibble(datos = datos$tipo[1], estadistica = res$statistic, pvalue = round(res$p.value, 
                                                                            5))
}
res_periodo <- split(datos_eng, datos_eng$tipo) %>% map_df(wilcoxon_aux)
res_periodo

datos_eng2<- datos_eng%>%group_by(tipo,periodo)%>%summarise(mediana=median(Engagement))

### Wilcoxon para comparar engagement entre posteos pagos o no
datos_eng <- datos %>% select(Concejal, bloque, Pago, periodo, Engagement) %>%
  mutate(tipo = "General")

datos_eng <- datos_eng %>% bind_rows(datos_eng %>% filter(periodo == "Electoral") %>% 
                                       mutate(tipo = "Periodo Electoral"))%>%
  bind_rows(datos_eng %>% filter(periodo == "Post-Electoral") %>% 
              mutate(tipo = "Periodo Post-Electoral")) %>%
  bind_rows(datos_eng %>% filter(bloque == "C") %>% 
              mutate(tipo = "C")) %>% bind_rows(datos_eng %>%
                                                  filter(bloque == "CF") %>% mutate(tipo = "CF")) %>% 
  bind_rows(datos_eng %>% filter(bloque =="FNyP") %>% 
              mutate(tipo = "FNyP")) %>% bind_rows(datos_eng %>% 
                                                     filter(bloque =="FP") %>% mutate(tipo = "FP")) %>% 
  bind_rows(datos_eng %>% filter(bloque == "FSyP") %>%
              mutate(tipo = "FSyP")) %>% 
  mutate(tipo = factor(tipo, levels = c("General", "Periodo Electoral",
                                        "Periodo Post-Electoral", "C", "CF", "FNyP", "FP", "FSyP")))%>%
  mutate(Pago = factor(Pago, levels = c("Si","No"), labels=c("Paga","No paga")))                                      

wilcoxon_aux2 <- function(datos) {
  # Atencion: esto es para comparar entre periodos
  res <- wilcox.test(Engagement ~ Pago, data = datos)
  tibble(datos = datos$tipo[1], estadistica = res$statistic, pvalue = round(res$p.value, 
                                                                            5))
}
res_pago <- split(datos_eng, datos_eng$tipo) %>% map_df(wilcoxon_aux2)
res_pago

datos_eng3<- datos_eng%>%group_by(tipo,Pago)%>%summarise(mediana=median(Engagement))


##### Corrección por múltiples testeos ###
res <- bind_rows(mutate(res_bloque, `Comparación entre:` = "Bloques"), 
                 mutate(res_periodo,`Comparación entre:` = "Períodos"),
                 mutate(res_pago, `Comparación entre:` = "Pagos o no")) %>%
  mutate(pvalue_ajustado = round(p.adjust(pvalue,"BH"), 5)) %>% 
  select(`Comparación entre:`, everything())
res


### Independencia bloque y periodo ###    Ver de hacer una funcion para
###### posteos bloque x periodo ######    unificar todo
periodo <- datos%>%group_by(bloque, periodo)%>%summarise(posteos=sum(n()))

periodo <- spread(periodo, key = "periodo", value = "posteos")

test1 <- chisq.test(periodo[,2:3])
cramer <- sqrt(test1$statistic/4781) 

### Posteos pagos vs periodo
tabla3 <- table(datos$Pago, datos$periodo) 

test3 <- chisq.test(tabla3)
cramer <- sqrt(test3$statistic/4781) 

RO <- (312*1851)/(2539*79)
### Posteos pagos vs periodo cambiemos
cambiemos <- subset(datos, bloque=="C")

tabla5 <- table(cambiemos$Pago, cambiemos$periodo) 

test5 <- chisq.test(tabla5)
cramer <- sqrt(test5$statistic/1053) 

RO <- (20*529)/(497*7)

### Posteos pagos vs periodo ciudad futura
cf <- subset(datos, bloque=="CF")

tabla6 <- table(cf$Pago, cf$periodo) 

test6 <- chisq.test(tabla6)
cramer <- sqrt(test6$statistic/412) 

RO <- (15.5*102.5)/(295.5*0.5)

### Posteos pagos vs periodo FNP
fnp <- subset(datos, bloque=="FNyP")

tabla7 <- table(fnp$Pago, fnp$periodo) 

test7 <- chisq.test(tabla7)
cramer <- sqrt(test7$statistic/1788) 

RO <- (181*691)/(899*17)

### Posteos pagos vs periodo Frente progresista
fprog <- subset(datos, bloque=="FP")

tabla8 <- table(fprog$Pago, fprog$periodo) 

test8 <- chisq.test(tabla8)
cramer <- sqrt(test8$statistic/1276) 

RO <- (84*415)/(735*42)

### Posteos pagos vs periodo Fsp
fsp <- subset(datos, bloque=="FSyP")

tabla8 <- table(fsp$Pago, fsp$periodo) 

test8 <- chisq.test(tabla8)
cramer <- sqrt(test8$statistic/252) 

RO <- (12*114)/(113*13)

### Contenido del posteo vs bloque
tabla9 <- table(datos$Type, datos$bloque)

test9 <- chisq.test(tabla9)
cramer <- sqrt(test8$statistic/252) 
