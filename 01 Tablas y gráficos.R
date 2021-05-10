library(readxl) 
library(tidyverse) 
library(kableExtra) 
library(lubridate) 
library(janitor)  
library(treemap) 

#Lectura de datos
datos <- read_excel("Datos/Concejales/Posts/Datos de febrero a noviembre corregidos.xlsx")
datos <-  datos[!duplicated(datos$Permalink), ]
overview <- read_excel("Datos/Concejales/Overview/Overview Total 2019.xlsx")

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

colores <- c(C = "yellow", CF = "red", FNyP = "blue", FP = "orange", FSyP = "violet")

## Tabla tipo de posteo y bloque
tipo <- table(datos$Type,datos$bloque)
kable(tipo, align = "c")%>%
  kable_styling(c("striped","bordered"), full_width = F) %>%
  column_spec(1:1, background = "lightgrey")%>%
  row_spec(0, background = "lightgrey")%>% 
    add_header_above(c("Posts y reacciones totales en cada bloque"=6))

## Para crear la vble dia de la semana
datos$mes2 <- month(datos$Fecha)
datos$dia <- day(datos$Fecha)
datos <- mutate(datos, dia2=if_else(mes2==2 & dia==1,"Viernes","z"),
                dia2=if_else(mes2==2 & dia==2,"Sábado",dia2),
                dia2=if_else(mes2==2 & dia==3,"Domingo",dia2),
                dia2=if_else(mes2==2 & dia==4,"Lunes",dia2),
                dia2=if_else(mes2==2 & dia==5,"Martes",dia2),
                dia2=if_else(mes2==2 & dia==6,"Miércoles",dia2),
                dia2=if_else(mes2==2 & dia==7,"Jueves",dia2))

fecha.inicio <- as.Date("2019-02-01")
fecha.inicio2 <- as.Date("2019-02-02")
fecha.inicio3 <- as.Date("2019-02-03")
fecha.inicio4 <- as.Date("2019-02-04")
fecha.inicio5 <- as.Date("2019-02-05")
fecha.inicio6 <- as.Date("2019-02-06")
fecha.inicio7 <- as.Date("2019-02-07")
for(i in 1:nrow(datos)){
  datos$dia2=if_else(datos$Fecha==fecha.inicio+7*i,"Viernes",datos$dia2)
  datos$dia2=if_else(datos$Fecha==fecha.inicio2+7*i,"Sábado",datos$dia2)
  datos$dia2=if_else(datos$Fecha==fecha.inicio3+7*i,"Domingo",datos$dia2)
  datos$dia2=if_else(datos$Fecha==fecha.inicio4+7*i,"Lunes", datos$dia2)
  datos$dia2=if_else(datos$Fecha==fecha.inicio5+7*i,"Martes",datos$dia2)
  datos$dia2=if_else(datos$Fecha==fecha.inicio6+7*i,"Miércoles",datos$dia2)
  datos$dia2=if_else(datos$Fecha==fecha.inicio7+7*i,"Jueves",datos$dia2)
}

table(datos$dia2)

datos <- mutate(datos, orden_dia=if_else(dia2=="Lunes",1,7),
                orden_dia=if_else(dia2=="Martes",2,orden_dia),
                orden_dia=if_else(dia2=="Miércoles",3,orden_dia),
                orden_dia=if_else(dia2=="Jueves",4,orden_dia),
                orden_dia=if_else(dia2=="Viernes",5,orden_dia),
                orden_dia=if_else(dia2=="Sábado",6,orden_dia))
datos <- rename(datos, Día="dia2")

## Tabla de posteos según bloque y período
periodo_bloque_post <- tabyl(datos, periodo, bloque) %>%
  adorn_totals()

kable(periodo_bloque_post, align = "c")%>%
  kable_styling(c("striped","bordered"), full_width = F) %>%
  column_spec(1:1, background = "lightgrey")%>%
  row_spec(0, background = "lightgrey") %>% 
  add_header_above(c("Cantidad de posts según bloque y periodo"=6)) 


##Tabla posteos por mes
datos <- mutate(datos, 
                mes2=if_else(mes=="marzo",3,0),
                mes2=if_else(mes=="abril",4,mes2),
                mes2=if_else(mes=="mayo",5,mes2),
                mes2=if_else(mes=="junio",6,mes2),
                mes2=if_else(mes=="julio",7,mes2),
                mes2=if_else(mes=="agosto",8,mes2),
                mes2=if_else(mes=="septiembre",9,mes2),
                mes2=if_else(mes=="octubre",10,mes2),
                mes2=if_else(mes=="noviembre",11,mes2))
z <- table(datos$bloque, reorder(datos$mes,datos$mes2))
kable(z, align = "c")%>%
  kable_styling(c("striped","bordered"), full_width = F) %>%
  column_spec(1:1, background = "lightgrey")%>%
  row_spec(0, background = "lightgrey")%>% 
  add_header_above(c("Posts por bloque por mes"=11))


##Agrupo los datos según bloque y tipo de posteo y grafico porcentaje de posteos pagos
post_pagos <- datos %>% 
  group_by(bloque,Pago) %>%
  summarise(Posts=sum(n()))

post_pagos <- spread(post_pagos,Pago,Posts)

post_pagos$total <- post_pagos$Si+post_pagos$No

post_pagos$porc <- (post_pagos$Si/post_pagos$total)*100


tema2 <- theme(legend.position = "none",
               legend.text = element_text(size = 8),
               axis.title.x = element_text(size = 13, face = "bold"),
               axis.title.y = element_text(size = 13, face = "bold"),
               plot.title = element_text(family="arial", size = 15, hjust = 0.5, face = "bold"))

grafico_pagos <- ggplot(post_pagos,aes(x=bloque, y=porc,  fill=bloque)) +
  geom_bar(stat='identity', position='dodge')+
  scale_fill_manual("Bloque",values = colores) +
  scale_y_continuous("Porcentaje de posteos pagos",limits = c(0,15),expand = c(0,0))+
  scale_x_discrete(labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)"))+
  xlab ("Bloque")+
  theme_bw() +
  geom_text(aes(y = porc, label = paste0(round(porc,0),"%")),  size=3, vjust=-0.3, hjust=0.2 ,col="black")+
  tema2+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "white", size=0.5))
ggsave("Gráficos/Porcentaje de posts pagos por bloque.jpg",grafico_pagos, width = 25, height = 15, units = "cm")


##Cálculo de estadísticas
tapply(datos$Engagement, datos$Pago, summary)


##Asigno los bloques en la base overview
overview <- mutate(overview, 
                   bloque=if_else(Name=="Agapito Blanco", "C", "z"),
                   bloque=if_else(Name=="Agustina Bouza","C",bloque),
                   bloque=if_else(Name=="Aldo Poy","FP",bloque),
                   bloque=if_else(Name=="Alejandro Rosello","C",bloque),
                   bloque=if_else(Name=="Andres Giménez","FNyP",bloque),
                   bloque=if_else(Name=="Anita Martinez","C",bloque),
                   bloque=if_else(Name=="Caren Tepp","CF",bloque),
                   bloque=if_else(Name=="Celeste Lepratti","FSyP",bloque),
                   bloque=if_else(Name=="Charly Cardozo","FNyP",bloque),
                   bloque=if_else(Name=="Eduardo Toniolli","FNyP",bloque),
                   bloque=if_else(Name=="Enrique Estévez","FP",bloque),
                   bloque=if_else(Name=="Fernanda Gigliani","FNyP",bloque),
                   bloque=if_else(Name=="Gabriel Chumpitaz","C",bloque),
                   bloque=if_else(Name=="Germana F Casas","C",bloque),
                   bloque=if_else(Name=="Horacio Ghirardi","FP",bloque),
                   bloque=if_else(Name=="Juan Monteverde","CF",bloque),
                   bloque=if_else(Name=="Lichu Zeno","FP",bloque),
                   bloque=if_else(Name=="María Eugenia Schmuck","FP",bloque),
                   bloque=if_else(Name=="Marina Magnani","FNyP",bloque),
                   bloque=if_else(Name=="Norma López","FNyP",bloque),
                   bloque=if_else(Name=="Osvaldo Miatello","FNyP",bloque),
                   bloque=if_else(Name=="Pablo Javkin","FP",bloque),
                   bloque=if_else(Name=="Renata Ghilotti","C",bloque),
                   bloque=if_else(Name=="Roberto Sukerman","FNyP",bloque),
                   bloque=if_else(Name=="Roy López Molina","C",bloque),
                   bloque=if_else(Name=="Verónica Irizar","FP",bloque)
                   
)

overview <- mutate(overview, mes=if_else(Mes=="enero",1,0),
                   mes=if_else(Mes=="febrero",2,mes),
                   mes=if_else(Mes=="marzo",3,mes),
                   mes=if_else(Mes=="abril",4,mes),
                   mes=if_else(Mes=="mayo",5,mes),
                   mes=if_else(Mes=="junio",6,mes),
                   mes=if_else(Mes=="julio",7,mes),
                   mes=if_else(Mes=="agosto",8,mes),
                   mes=if_else(Mes=="septiembre",9,mes),
                   mes=if_else(Mes=="octubre",10,mes),
                   mes=if_else(Mes=="noviembre",11,mes),
                   mes=if_else(Mes=="diciembre",12,mes))
overview <- subset(overview, mes!=1 & mes!=12)
## Guardo porque voy a modificar overview
overview2 <- overview

overview_bloque <- overview%>%group_by(bloque,mes) %>%
  summarise(Fans=sum(`Fan count evolution`),
            Posts=sum(`Posts evolution`),
            Engagement=sum(`Engagement evolution`),
            Engag_median=median(`Engagement evolution`))

overview_bloque <- mutate(overview_bloque, concejales=if_else(bloque=="C",9,1),
                                           concejales=if_else(bloque=="CF",2,concejales),
                                           concejales=if_else(bloque=="FP",7,concejales),
                                           concejales=if_else(bloque=="FNyP",7,concejales))
overview_bloque$Fans_prom=overview_bloque$Fans/overview_bloque$concejales
overview_bloque$Posts_prom=overview_bloque$Posts/overview_bloque$concejales
overview_bloque$Engagement_prom=overview_bloque$Engagement/overview_bloque$Posts

tema <- theme(legend.position = "right",
              legend.text = element_text(size = 10),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              axis.text.x = element_text(size = 10,face = "bold"),
              plot.title = element_text(family="arial", size = 15, hjust = 0.5, face = "bold"))

### Gráfico de evolución de fans mensual por bloque
grafico1 <- ggplot(overview_bloque, aes(x=mes, y=Fans_prom, group =bloque , color =bloque )) + 
  geom_line(size=1)  + 
  xlab("Mes")+
  ylab("Número medio de fans")+
  scale_color_manual("Bloque",values = colores,labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)")) +
  geom_point( size=2, shape=21, fill="white") + 
  scale_y_continuous(limits=c(0,25000), breaks = xbreaks <- seq(0,25000,5000),expand = c(0,0))+
  scale_x_discrete(breaks = c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "),
                   limits =c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "))+
  theme_minimal()+ tema
  
ggsave("Gráficos/Evolucion de fans por bloque.jpg",grafico1, width = 25, height = 15, units = "cm")

##Gráfico de evolución de fans mensual por bloque opción 2
grafico7 <- ggplot(overview_bloque, aes(x=mes, y=Fans_prom, group =bloque , color =bloque )) + 
  geom_rect(aes(xmin = 2,
                xmax = 6.5,
                ymin = -Inf, ymax = Inf, fill = 'Electoral'), alpha = .01,show.legend = F)+
  geom_rect(aes(xmin = 6.5,
                xmax = 11,
                ymin = -Inf, ymax = Inf, fill = 'Post-Electoral'), alpha = .01,show.legend = F)+
  geom_line(size=1)  + 
  geom_point( size=2, shape=21, fill="white") +
  scale_fill_manual(values = c("plum1","seagreen1"),limits = c("Electoral","Post-Electoral"),
                    labels = c("Electoral", "Post-Electoral"))+
  xlab("")+
  ylab("Número medio de fans")+
  scale_y_continuous(limits=c(0,30000), breaks = xbreaks <- seq(0,30000,5000),expand = c(0,0))+
  scale_color_manual("Bloque",values = colores,labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)")) +
  scale_x_discrete(breaks = c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "),
                   limits =c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "))+
  theme_minimal()+
  tema+
  ggplot2::annotate(geom="text", x = 3.75, y = 27500, label = "Período Electoral")+
  ggplot2::annotate(geom="text", x = 8.75, y = 27500, label = "Período Post-Electoral")

ggsave("Gráficos/Evolucion de fans por bloque (sombreado por periodo).jpg",grafico7, width = 25, height = 15, units = "cm")



##Gráfico de evolución de posteos mensual por bloque
grafico7 <- ggplot(overview_bloque, aes(x=mes, y=Posts_prom, group =bloque , color =bloque )) + 
  geom_line(size=1)  + 
  geom_point( size=2, shape=21, fill="white") +
  xlab("Mes")+
  ylab("Número medio de posteos")+
  scale_color_manual("Bloque",values = colores,labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)")) +
  scale_x_discrete(breaks = c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "),
                   limits =c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "))+
  theme_minimal()+
  tema

ggsave("Gráficos/Evolucion de posteos por bloque.jpg",grafico7, width = 25, height = 15, units = "cm")


##Gráfico de evolución de posteos mensual por bloque opción 2
grafico7 <- ggplot(overview_bloque, aes(x=mes, y=Posts_prom, group =bloque , color =bloque )) + 
  geom_rect(aes(xmin = 2,
                xmax = 6.5,
                ymin = -Inf, ymax = Inf, fill = 'Electoral'), alpha = .01,show.legend = F)+
  geom_rect(aes(xmin = 6.5,
                xmax = 11,
                ymin = -Inf, ymax = Inf, fill = 'Post-electoral'), alpha = .01,show.legend = F)+
  geom_line(size=1)  + 
  geom_point( size=2, shape=21, fill="white") +
  scale_fill_manual(values = c("plum1","seagreen1"),limits = c("Electoral","Post-electoral"),labels = c("Electoral",
                                                                                                        "Post-electoral"))+
  xlab("")+
  ylab("Número medio de posteos")+
  scale_color_manual("Bloque",values = colores,labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)")) +
  scale_x_discrete(breaks = c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "),
                   limits =c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "))+
  scale_y_continuous(limits=c(0,60), breaks = xbreaks <- seq(0,60,5),expand = c(0,0))+
  theme_minimal()+
  tema+
  ggplot2::annotate(geom="text", x = 3, y = 55, label = "Electoral")+
  ggplot2::annotate(geom="text", x = 8, y = 55, label = "Post-Electoral")

ggsave("Gráficos/Evolucion de posteos por bloque (sombreado por periodo).jpg",grafico7, width = 25, height = 15, units = "cm")


##Gráfico de evolución del engagement mensual promedio por post segun bloque 
grafico8 <- ggplot(overview_bloque, aes(x=mes, y=Engagement_prom, group =bloque , color =bloque )) + 
  geom_line(size=1)  + 
  xlab("Mes")+
  ylab("Número medio de engagement")+
  scale_color_manual("Bloque",values = colores,labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)")) +
  geom_point( size=2, shape=21, fill="white") + 
  scale_x_discrete(breaks = c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "),
                   limits =c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "))+
  theme_minimal()+
  tema

ggsave("Gráficos/Evolucion de engagement por bloque.jpg",grafico8, width = 25, height = 15, units = "cm")


##Gráfico de evolución de engagement mensual promedio por post segun bloque opción 2
grafico7 <- ggplot(overview_bloque, aes(x=mes, y=Engagement_prom, group =bloque , color =bloque )) + 
  geom_rect(aes(xmin = 2,
                xmax = 6.5,
                ymin = -Inf, ymax = Inf, fill = 'Electoral'), alpha = .01,show.legend = F)+
  geom_rect(aes(xmin = 6.5,
                xmax = 11,
                ymin = -Inf, ymax = Inf, fill = 'Post-electoral'), alpha = .01,show.legend = F)+
  geom_line(size=1)  + 
  geom_point( size=2, shape=21, fill="white") +
  scale_fill_manual(values = c("plum1","seagreen1"),limits = c("Electoral","Post-electoral"),labels = c("Electoral",
                                                                                                        "Post-electoral"))+
  xlab("")+
  ylab("Número medio de engagement por publicación")+
  scale_y_continuous(limits=c(0,500), breaks = xbreaks <- seq(0,500,50),expand = c(0,0))+
  scale_color_manual("Bloque",values = colores,labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)")) +
  scale_x_discrete(breaks = c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "),
                   limits =c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "))+
  theme_minimal()+
  tema+
  ggplot2::annotate(geom="text", x = 3, y = 20000, label = "Electoral")+
  ggplot2::annotate(geom="text", x = 8, y = 20000, label = "Post-Electoral")

ggsave("Gráficos/Evolucion de engagement por bloque (sombreado por periodo).jpg",grafico7, width = 25, height = 15, units = "cm")

##Gráfico de evolución de engagement mediano por bloque
grafico9 <- ggplot(overview_bloque, aes(x=mes, y=Engag_median, group =bloque , color =bloque )) + 
  geom_rect(aes(xmin = 2,
                xmax = 6.5,
                ymin = -Inf, ymax = Inf, fill = 'Período Electoral'), alpha = .01,show.legend = F)+
  geom_rect(aes(xmin = 6.5,
                xmax = 11,
                ymin = -Inf, ymax = Inf, fill = 'Período Post-electoral'), alpha = .01,show.legend = F)+
  geom_line(size=1)  + 
  geom_point( size=2, shape=21, fill="white") +
  scale_fill_manual(values = c("plum1","seagreen1"),limits = c("Período Electoral","Período Post-electoral"),labels = c("Electoral",
                                                                                                        "Post-electoral"))+
  xlab("")+
  ylab("Número mediano de engagement")+
  scale_y_continuous(limits=c(0,19000), breaks = xbreaks <- seq(0,19000,2000),expand = c(0,0))+
  scale_color_manual("Bloque",values = colores,labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)")) +
  scale_x_discrete(breaks = c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "),
                   limits =c(" ","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"," "))+
  theme_minimal()+
  tema+
  ggplot2::annotate(geom="text", x = 3.75, y = 18000, label = "Período Electoral")+
  ggplot2::annotate(geom="text", x = 8.75, y = 18000, label = "Período Post-Electoral")

ggsave("Gráficos/Evolucion de engagement mediano por bloque (sombreado por periodo).jpg",grafico9, width = 25, height = 15, units = "cm")



## Reordeno datos para hacer gráficos por período
overview2 <- mutate(overview2, periodo=if_else(mes==2 | mes==3 | mes==4 | mes==5 | mes==6,"Electoral","Post-Electoral"))

frecs <-
  overview2 %>%
  group_by(periodo, bloque) %>%
  summarise(Post = sum(`Posts evolution`) ,
            Eng=sum(`Engagement evolution`),
            Fans=sum(`Fan count evolution`))
frecs <- as.data.frame(frecs)

frecs2 <-
  overview2 %>%
  group_by(bloque) %>%
  summarise(Post = sum(`Posts evolution`) ,
            Eng=sum(`Engagement evolution`),
            Fans=sum(`Fan count evolution`))
periodo <- c("Total","Total","Total","Total","Total")
frecs2 <- cbind(periodo,frecs2)

## Sumo al dataset los nros del total
frecs <- rbind(frecs2,frecs)

## Promedio por concejales
frecs <- mutate(frecs, cant=if_else(bloque=="C",9,1),
                cant=if_else(bloque=="CF",2,cant),
                cant=if_else(bloque=="FP",7,cant),
                cant=if_else(bloque=="FNyP",7,cant))
frecs$promPost <- frecs$Post/frecs$cant
frecs$promEng <- frecs$Eng/frecs$Post
frecs$promFans <- frecs$Fans/frecs$cant


##Gráfico de promedio de posteos por bloque y período
frecs3 <- subset(frecs, periodo!="Total")
grafico10 <- ggplot(frecs3,aes(x=periodo, y=promPost, fill= bloque)) +
  geom_bar(stat='identity', position='dodge')+
  scale_fill_manual("Bloque",values = colores,labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)")) +
  scale_y_continuous("Número medio de posteos",limits = c(0,200),breaks = xbreaks <- seq(0, 200, 50),expand = c(0,0))+
  xlab ("Periodo")+
  theme_bw() +
  tema2+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "white", size=0.5))

ggsave("Gráficos/Promedio de posteos por bloque y periodo opcion 3 (promedio mensual).jpg",grafico10, width = 25, height = 15, units = "cm")


##Gráfico de posteos por día
post_dias <- datos %>% 
  group_by(Día) %>%
  summarise(Posts=sum(n()),
            Eng=sum(Engagement),
            Eng_median=median(Engagement))
post_dias<- mutate(post_dias, orden_dia=if_else(Día=="Lunes",1,7),
                   orden_dia=if_else(Día=="Martes",2,orden_dia),
                   orden_dia=if_else(Día=="Miércoles",3,orden_dia),
                   orden_dia=if_else(Día=="Jueves",4,orden_dia),
                   orden_dia=if_else(Día=="Viernes",5,orden_dia),
                   orden_dia=if_else(Día=="Sábado",6,orden_dia))

post_dias$razon <- post_dias$Eng/post_dias$Posts

post_dias <- arrange(post_dias,orden_dia)
sum(post_dias$Eng)/4781

grafico_dias <- ggplot(post_dias,aes(x=reorder(Día,orden_dia), y=Posts)) +
  geom_bar(stat='identity', position='dodge',color="forestgreen",fill="forestgreen")+
  scale_y_continuous("Número de publicaciones",limits = c(0,800),expand = c(0,0))+
  xlab ("Día de la semana")+
  theme_bw() +
  tema2+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "white", size=0.5))

ggsave("Gráficos/Cantidad de posts por día.jpg",grafico_dias, width = 25, height = 15, units = "cm")


## Gráfico de barras de fans por bloque
overview_bloque2 <- overview_bloque %>% 
  group_by(bloque) %>%
  summarise(Fans=mean(Fans),
            Posts=sum(Posts),
            Engagement=sum(Engagement),
            concejales=mean(concejales))
overview_bloque2$Fans_prom=overview_bloque2$Fans/overview_bloque2$concejales
overview_bloque2$Posts_prom=overview_bloque2$Posts/overview_bloque2$concejales
overview_bloque2$Engagement_prom=overview_bloque2$Engagement/overview_bloque2$Posts

grafico_fans2 <- ggplot(overview_bloque2, aes(x=bloque, y=Fans,fill=bloque)) +
  geom_bar(stat="identity", position=position_stack(reverse = F), width = 0.6  )+
  ylab("Número medio de fans") +
  xlab("Bloque")+
  scale_fill_manual("Bloque",values = colores) +
  scale_x_discrete(labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)"))+
  theme_bw() +
  tema2+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "white", size=0.5))+
  scale_y_continuous (limits = c(0,210000),breaks = xbreaks <- seq(0, 210000, 50000),
                      expand = c(0,0))

ggsave("Gráficos/Gráfico de barras promedio de fans por bloque.jpg",grafico_fans2, width = 25, height = 15, units = "cm")

## Gráfico de barras de fans por bloque promedio concejal
grafico_fans2 <- ggplot(overview_bloque2, aes(x=bloque, y=Fans_prom,fill=bloque)) +
  geom_bar(stat="identity", position=position_stack(reverse = F), width = 0.6  )+
  ylab("Número medio de fans") +
  xlab("Bloque")+
  scale_fill_manual("Bloque",values = colores) +
  scale_x_discrete(labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)"))+
  theme_bw() +
  tema2+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "white", size=0.5))+
  scale_y_continuous (limits = c(0,25000),breaks = xbreaks <- seq(0, 25000, 5000),
                      expand = c(0,0))

ggsave("Gráficos/Gráfico de barras promedio de fans por bloque por concejal.jpg",grafico_fans2, width = 25, height = 15, units = "cm")



##Treemap y gráfico de barras de promedio de publicaciones por bloque
total=sum(overview_bloque2$Posts)

overview_bloque2$Porc <- round((overview_bloque2$Posts/total)*100 ,1)

tabla02 <- data.frame("Bloque"=overview_bloque2$bloque,"Porcentaje"=overview_bloque2$Porc,"Frec"=overview_bloque2$Posts)

color <- c( "#F8ED05","#FF0000", "#2F16FC", "#FF6600","#FF62F3")
tabla02 <- cbind(tabla02, color)
tabla02$label <- paste0(paste(tabla02$Bloque,tabla02$Porc),"%")
bloque <- c("C","CF","FNyP","FP","FSyP")
tabla02 <- cbind(tabla02,bloque)
tabla02$label <- paste0(paste(tabla02$bloque,tabla02$Porc),"%")
  png(filename="Gráficos/treemap posteos2.png",width=600, height=300)
  grafico5 <- treemap(tabla02,
                      index="label",
                      vSize="Porcentaje",
                      type="index",
                      palette = color,
                      fontsize.legend = 20
  )
  dev.off()
grafico_post2 <- ggplot(tabla02, aes(x=Bloque, y=Porcentaje,fill=Bloque)) +
  geom_bar(stat="identity", position=position_stack(reverse = F), width = 0.6  )+
  ylab("Porcentaje de posteos") +
  xlab("Bloque")+
  scale_fill_manual("Bloque",values = colores) +
  scale_x_discrete(labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)"))+
  theme_bw() + 
  tema2+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "white", size=0.5))+
    geom_text(aes(y = Porcentaje, label = paste0(Porcentaje,"%")), vjust=-0.4,
             position = position_stack(reverse = F), size=3, col="black")+
  scale_y_continuous (limits = c(0,40),breaks = xbreaks <- seq(0, 40, 5),
                      expand = c(0,0))

ggsave("Gráficos/Gráfico de barras porcentaje de publicaciones por bloque.jpg",grafico_post2, width = 25, height = 15, units = "cm")

########## Grafico de promedio de publicaciones por concejal, en al año
concejales <- c(9,2,7,7,1)
tabla02 <- cbind(tabla02,concejales)
tabla02$Promedio <- tabla02$Frec/tabla02$concejales
grafico_fans2 <- ggplot(tabla02, aes(x=Bloque, y=Promedio,fill=Bloque)) +
  geom_bar(stat="identity", position=position_stack(reverse = F), width = 0.6  )+
  ylab("Promedio de posteos") +
  xlab("Bloque")+
  scale_fill_manual("Bloque",values = colores) +
  scale_x_discrete(labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)"))+
  theme_bw() + 
  tema2+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "white", size=0.5))+
  
  geom_text(aes(y = Promedio, label = round(Promedio,0)), vjust=-0.4,
            position = position_stack(reverse = F), size=3, col="black")+
  scale_y_continuous (limits = c(0,280),breaks = xbreaks <- seq(0, 280, 20),
                      expand = c(0,0))

ggsave("Gráficos/Gráfico de barras promedio anual de publicaciones por bloque (promedio concejal).jpg",grafico_fans2, width = 25, height = 15, units = "cm")


###### Posteos pagos #########
pagos <- subset(datos, Pago=="Si")
##Gráfico de sectores, solo posteos pagos según bloque
frecs4 <-
  pagos %>%
  group_by(bloque) %>%
  summarise(FrecAbs = n() )
# Definicion de la funcion para mejorar el grafico

tabla01 <- data.frame("Bloque" = frecs4$bloque, "Valores"=frecs4$FrecAbs)
tabla02 <- group_by(tabla01, Bloque) %>% summarise(Valores=sum(Valores)) %>% 
  mutate(porcentaje = round(Valores/sum(Valores),4)*100) %>% 
  arrange(desc(porcentaje))


## Para que los porcentajes sumen 100
sum_pct <- sum(tabla02$porcentaje)
maximo <- max(tabla02$porcentaje)

if (sum_pct == 100) {
  tabla02[1,3]=max(tabla02$porcentaje)
} else if (sum_pct < 100) {
  tabla02[1,3]=(max(tabla02$porcentaje)+(100-sum_pct))
}  else {
  tabla02[1,3]=max(tabla02$porcentaje)+(100-sum_pct)
}

# Cambio el punto por la coma en tabla02
tabla02$pct <- tabla02$porcentaje
tabla02$pct<- gsub("\\.", ",",tabla02$pct)
tabla02 <- tabla02 %>% 
  arrange(desc(Bloque)) %>%
  mutate(prop = Valores / sum(tabla02$Valores) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
grafico5 <- ggplot(tabla02, aes(x="", y= porcentaje, fill=Bloque)) + 
  geom_bar(width = 1, stat="identity", size = 0.5, color= "black") + 
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(y = porcentaje, label = pct), color = "black", size=3.5, position = position_stack(vjust = 0.5)) +
  labs(x=NULL, y=NULL) +
  scale_fill_manual("Bloque",values = colores,labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        plot.title = element_text(family="arial", size = 15, hjust = 15, face = "bold"))+
  theme(panel.grid.major =   element_line(colour = "white",size=0.5))

ggsave(("Gráficos/Porcentaje de post pagos por bloque grafico de sectores.jpg"),grafico5, width = 25, height = 15, units = "cm")


tema3 <- theme(legend.position = "none",
               legend.text = element_text(size = 10),
               axis.title.x = element_text(size = 13, face = "bold"),
               axis.title.y = element_text(size = 13, face = "bold"),
               axis.text.x = element_text("none"),
               plot.title = element_text(family="arial", size = 15, hjust = 0.5, face = "bold"))

##Gráfico de barras, porcentaje de posteos pagos según bloque
grafico_pago <- ggplot(tabla02, aes(x=Bloque, y=porcentaje,fill=Bloque)) +
  geom_bar(stat="identity", position=position_stack(reverse = F), width = 0.6  )+
  ylab("Porcentaje de posteos pagos") +
  xlab("Bloque")+
  scale_fill_manual("Bloque",values = colores) +
  scale_x_discrete(labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)"))+
  theme_bw() + #esto le da cierto formato estetico
  tema3+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "white", size=0.5))+
  
    geom_text(aes(y = porcentaje, label = paste0(porcentaje,"%")), vjust=-0.4,
             position = position_stack(reverse = F), size=3, col="black")+
  scale_y_continuous (limits = c(0,60),breaks = xbreaks <- seq(0,60, 5),
                      expand = c(0,0))

ggsave("Gráficos/Gráfico de barras porcentaje de posteos pagos.jpg",grafico_pago, width = 25, height = 15, units = "cm")

##Treemap porcentaje de posteos pagos según bloque
color <- c( "#F8ED05","#FF0000", "#2F16FC", "#FF6600","#FF62F3")
tabla02 <- cbind(tabla02, color)
tabla02$label <- paste(tabla02$Bloque,tabla02$pct)
tabla02$label2 <- paste0(tabla02$label,"%")

png(filename="Gráficos/treemap posteos pagos.png",width=600, height=300)
grafico <- treemap(tabla02,
                   index="label2",
                   vSize="porcentaje",
                   type="index",
                   palette =color
)
dev.off()

####### Porcentaje posteos pagos dentro de c/ bloque
total <- datos%>%group_by(bloque)%>%summarise(Total_post=sum(n()))
frecs4 <- cbind(frecs4,total[,2])
frecs4$porc <- (round(frecs4$FrecAbs/frecs4$Total_post,4))*100
##Gráfico de barras, porcentaje de posteos pagos según bloque
grafico_pago <- ggplot(frecs4, aes(x=bloque, y=porc,fill=bloque)) +
  geom_bar(stat="identity", position=position_stack(reverse = F), width = 0.6  )+
  #stat_bin(geom = "text", aes(label = y, vjust = 5))+
  ylab("Porcentaje de posteos pagos") +
  xlab("Bloque")+
  scale_fill_manual("Bloque",values = colores) +
  scale_x_discrete(labels=c("C (9)","CF (2)","FNyP (7)","FP (7)","FSyP (1)"))+
    theme_bw() + #esto le da cierto formato estetico
  tema3+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "white", size=0.5))+
  geom_text(aes(y = porc, label = paste0(porc,"%")), vjust=-0.4,
            position = position_stack(reverse = F), size=3, col="black")+
  scale_y_continuous (limits = c(0,15),breaks = xbreaks <- seq(0,15, 5),
                      expand = c(0,0))

ggsave("Gráficos/Gráfico de barras porcentaje de posteos pagos.jpg",grafico_pago, width = 25, height = 15, units = "cm")


##Tabla de engagement promedio según tipo de posteo y bloque
pago_bloque <- datos %>%
  group_by(bloque,Pago) %>%
  summarise(FrecAbs = n(),
            eng=sum(Engagement),
            eng_median=median(Engagement))

pago_bloque$eng_prom <- pago_bloque$eng/pago_bloque$FrecAbs

pago_bloque_si <- subset(pago_bloque, Pago=="Si")
pago_bloque_no <- subset(pago_bloque, Pago=="No")
pago_bloque_si <- select(pago_bloque_si, c(bloque, eng_median))
pago_bloque_si <- rename(pago_bloque_si, eng_median_pago="eng_median")
pago_bloque_total <- cbind(pago_bloque_si, pago_bloque_no[,5])
pago_bloque_total$incremento <- pago_bloque_total$eng_median_pago/pago_bloque_total$eng_median


kable(pago_bloque_total, align = "c")%>%
  kable_styling(c("striped","bordered"), full_width = F) %>%
  column_spec(1:1, background = "lightgrey")%>%
  row_spec(0, background = "lightgrey")


tabla <- table(datos$Pago, datos$periodo)

kable(tabla, align = "c")%>%
  kable_styling(c("striped","bordered"), full_width = F) %>%
  column_spec(1:1, background = "lightgrey")%>%
  row_spec(0, background = "lightgrey")



### Agrupo engagement por bloque ###
eng_bloque <- datos %>% group_by(bloque) %>% 
  summarise(mediana = median(Engagement),RI = IQR(Engagement), 
            media = mean(Engagement), desvio = sd(Engagement), n = n()) %>% 
  arrange(-mediana)
eng_bloque

### Box-plots de engagement por bloque con transformacion logaritmica
g1 <- 
  ggplot(datos) +
  aes(x = reorder(bloque, -Engagement, FUN = median), 
      y = Engagement + 1, fill = bloque) + # Se suma 1 para evitar el log 0
  geom_boxplot(outlier.size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = colores) +
  coord_trans(y = "log10") +
  scale_y_continuous("Engagement", breaks = c(1, 10, 100, 1000, 10000)) +
  labs(fill = "Bloque", x = "Bloque")+
  theme_bw() + #esto le da cierto formato estetico
  tema3
ggsave("Gráficos/Boxplots engagement por bloque.jpg",g1, width = 25, height = 15, units = "cm")

### Gráfico de barras mediana del enagement por bloque
g2 <- ggplot(eng_bloque) + 
  aes(x = reorder(bloque, -mediana), y = mediana, fill = bloque) + 
  geom_bar(stat = "identity", show.legend = F) + 
  scale_fill_manual(values = colores) + 
  labs(y = "Engagement mediano", fill = "Bloque", x = "Bloque") + 
  theme_bw() + 
  tema3
ggsave("Gráficos/Gráfico de barras mediana engagement por bloque.jpg",g2, width = 25, height = 15, units = "cm")

#### Promediando por concejal ###
eng_concejal <- datos %>% group_by(Concejal) %>% 
  summarise(bloque = first(bloque),
            mediana = median(Engagement), 
            RI = IQR(Engagement), 
            media = mean(Engagement),
            desvio = sd(Engagement), 
            n = n()) %>%
  arrange(-mediana) %>% ungroup()

eng_conc_bloque <- eng_concejal %>% ungroup() %>% 
  group_by(bloque) %>% summarise(mediana = median(mediana),
                                 media = mean(media), 
                                 nconcejal = n(), 
                                 nposteos = sum(n)) %>% arrange(-mediana)
eng_conc_bloque

#### Engagement mediano por concejal en cada bloque
g3 <- ggplot(eng_conc_bloque) + 
  aes(x = reorder(bloque, -mediana), y = mediana, fill = bloque) + 
  geom_bar(stat = "identity", show.legend = F) + 
  scale_fill_manual(values = colores) + 
  labs(y = "Engagement mediano", fill = "Bloque", x = "Bloque")+
  theme_bw() + 
  tema3
ggsave("Gráficos/Gráfico de barras engagement mediano por concejal y bloque.jpg",g3, width = 25, height = 15, units = "cm")

##### Box plots engagement por periodo y tipo de publicacion ####
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

### Boxplots engagement por tipo de publicacion y bloque (para los tests K-W)
g4 <- ggplot(datos_eng) +
  aes(x = bloque, 
      y = Engagement + 1, fill = bloque) + # Se suma 1 para evitar el log 0
  geom_boxplot(outlier.size = 0.5, show.legend = F) +
  coord_trans(y = "log10") +
  scale_y_continuous("Engagement", breaks = c(1, 10, 100, 1000, 10000)) +
  facet_wrap(~ tipo, nrow = 1) +
  scale_fill_manual(values = colores) +
  labs(x = "Bloque") +
  theme_bw() + 
  tema3
ggsave("Gráficos/Boxplots engagement mediano todas las comparaciones.jpg",g4, width = 25, height = 15, units = "cm")

#### Gráfico de barras engagement mediano, por tipo de publicación y bloque
g5<- ggplot(datos_eng) + 
  aes(x = bloque, y = Engagement, fill = bloque) + 
  geom_bar(stat = "summary",show.legend = F, fun = "median") +
  facet_wrap(~tipo, nrow = 1) + 
  scale_fill_manual(values = colores) + 
  labs(x = "Bloque", y = "Engagement mediano") + 
  theme_bw() + 
  tema3
ggsave("Gráficos/Gráfico de barras engagement mediano segun bloque y tipo.jpg",g5, width = 25, height = 15, units = "cm")


### Comparacion engagement entre periodos
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

## Boxplots (Para los resultados de Wilcoxon)##
g6 <- ggplot(datos_eng) +
  aes(x = periodo, 
      y = Engagement + 1, fill = periodo) + # Se suma 1 para evitar el log 0
  scale_fill_manual(values = c("coral","forestgreen"))+
  geom_boxplot(outlier.size = 0.5, show.legend = F) +
  coord_trans(y = "log10") +
  scale_y_continuous("Engagement", breaks = c(1, 10, 100, 1000, 10000)) +
  facet_wrap(~ tipo, nrow = 2) +
  labs(x = "Período") +
  theme_bw() + 
  tema3
ggsave("Gráficos/Boxplots engagement según periodo y bloque o tipo de publicacion.jpg",g6, width = 25, height = 15, units = "cm")

## Gráfico de barras medianas
g7 <- ggplot(datos_eng) +
  aes(x = periodo, y = Engagement, fill = periodo) + 
  geom_bar(stat = "summary",show.legend = F, fun = "median") + 
  facet_wrap(~tipo, nrow = 2) + labs(x = "Período",y = "Engagement mediano") +
  scale_fill_manual(values = c("coral","forestgreen"))+
  theme_bw() + 
  tema3
ggsave("Gráficos/Engagement mediano según periodo y bloque o tipo de publicacion.jpg",g7, width = 25, height = 15, units = "cm")


### Comparacion engagement entre pagos y no pagos
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

## Boxplots (Para los resultados de Wilcoxon)##
g8 <- ggplot(datos_eng) +
  aes(x = Pago, 
      y = Engagement + 1, fill = Pago) + # Se suma 1 para evitar el log 0
  scale_fill_manual(values = c("coral","forestgreen"))+
  geom_boxplot(outlier.size = 0.5, show.legend = F) +
  coord_trans(y = "log10") +
  scale_y_continuous("Engagement", breaks = c(1, 10, 100, 1000, 10000)) +
  facet_wrap(~ tipo, nrow = 2) +
  labs(x = "Tipo de publicación") +
  theme_bw() + 
  tema3
ggsave("Gráficos/Boxplots engagement según bloque o periodo y tipo de publicacion.jpg",g8, width = 25, height = 15, units = "cm")

## Gráfico de barras medianas
g9 <- ggplot(datos_eng) +
  aes(x = Pago, y = Engagement, fill = Pago) + 
  geom_bar(stat = "summary",show.legend = F, fun = "median") + 
  facet_wrap(~tipo, nrow = 2) + labs(x = "Tipo de publicación",y = "Engagement mediano") +
  scale_fill_manual(values = c("coral","forestgreen"))+
  theme_bw() + 
  tema3
ggsave("Gráficos/Engagement mediano según tipo de publi y bloque o periodo.jpg",g9, width = 25, height = 15, units = "cm")
