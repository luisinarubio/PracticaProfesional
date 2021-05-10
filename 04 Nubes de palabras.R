library(readr)
library(wordcloud)
library(tidyverse)
library(tm)
library(wordcloud2)

datos2 <- read_delim("Datos/Concejales/Mensajes_limpios.txt", delim = "\t")
datos3 <- datos2%>%separate(mens_id, into = c("id", "concejal", "bloque", "periodo","Pago"), sep = "_")

#######Cambiemos###############
### General ##################
cambiemos <- subset(datos3, bloque=="Cambiemos")
docs <- cambiemos$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df <- subset(df, word!="rosario" & word!="ciudad")
set.seed(1234) # for reproducibility 
dev.new(width = 200, height = 200, unit = "px")
pal=c("gold")
wordcloud(words = df$word, freq = df$freq, min.freq = 22,           
          max.words=500, random.order=T,           
          colors=pal,rot.per = .2)

df2 <- subset(df, freq>22)
wordcloud2(data = df2, shape = 'circle',     
          color="gold", size = 0.2)


## Periodo 1 ##############################
cambiemos_1 <- subset(cambiemos, periodo==1)
docs <- cambiemos_1$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
pal=c("gold","gold1")
wordcloud(words = df$word, freq = df$freq, min.freq = 10,           
          max.words=500, random.order=FALSE,           
          colors=pal)

######## Periodo 2 #########################
cambiemos_2 <- subset(cambiemos, periodo==2)
docs <- cambiemos_2$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
pal=c("gold","gold1")
wordcloud(words = df$word, freq = df$freq, min.freq = 10,           
          max.words=500, random.order=FALSE,           
          colors=pal)


#######Ciudad futura###############
#######General ###################
cf <- subset(datos3, bloque=="Ciudad Futura")
docs <- cf$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df <- subset(df, word!="rosario" & word!="ciudad")
set.seed(1234) # for reproducibility 
dev.new(width = 200, height = 200, unit = "px")
pal=c("red")
wordcloud(words = df$word, freq = df$freq, min.freq = 20,           
          max.words=500, random.order=FALSE,           
          colors=pal)

df2 <- subset(df, freq>20)
wordcloud2(data = df2, shape = 'circle',     
           color="red", size = 0.25)

### Periodo 1 ##################
cf_1 <- subset(cf, periodo==1)
docs <- cf_1$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
pal=c("red")
wordcloud(words = df$word, freq = df$freq, min.freq = 10,           
          max.words=500, random.order=FALSE,           
          colors=pal)

### Periodo 2 ######################
cf_2 <- subset(cf, periodo==2)
docs <- cf_2$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
pal=c("red")
wordcloud(words = df$word, freq = df$freq, min.freq = 5,           
          max.words=500, random.order=FALSE,           
          colors=pal)


############ FNP ###############
############ General ###########
fnp <- subset(datos3, bloque=="Frente Nac. y Pop")
docs <- fnp$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df <- subset(df, word!="rosario" & word!="ciudad")
set.seed(1234) # for reproducibility 
dev.new(width = 200, height = 200, unit = "px")
pal=c("blue")
wordcloud(words = df$word, freq = df$freq, min.freq = 45,           
          max.words=500, random.order=FALSE,           
          colors=pal)

df2 <- subset(df, freq>45)
wordcloud2(data = df2, shape = 'circle',     
           color="blue", size = 0.25)

#### Periodo 1 ##################
fnp_1 <- subset(fnp, periodo==1)
docs <- fnp_1$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
pal=c("blue","lightblue")
wordcloud(words = df$word, freq = df$freq, min.freq = 15,           
          max.words=500, random.order=FALSE,           
          colors=pal)

#### Periodo 2 ##################
fnp_2 <- subset(fnp, periodo==2)
docs <- fnp_2$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
pal=c("blue","lightblue")
wordcloud(words = df$word, freq = df$freq, min.freq = 15,           
          max.words=500, random.order=FALSE,           
          colors=pal)


######## Frente progresista ################
####### General ###########################
fprog <- subset(datos3, bloque=="Frente Progresista")
docs <- fprog$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df <- subset(df, word!="rosario" & word!="ciudad")
set.seed(1234) # for reproducibility 
dev.new(width = 100, height = 100, unit = "px")
pal=c("orange")
wordcloud(words = df$word, freq = df$freq, min.freq = 36,           
          max.words=500, random.order=FALSE,           
          colors=pal)

df2 <- subset(df, freq>36)
wordcloud2(data = df2, shape = 'circle',     
           color="orange", size = 0.25)

##### Periodo 1 #####################
fprog_1 <- subset(fprog, periodo==1)
docs <- fprog_1$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
pal=c("orange","blue","blue")
wordcloud(words = df$word, freq = df$freq, min.freq = 10,           
          max.words=500, random.order=FALSE,           
          colors=pal)

###### Periodo 2 ####################
fprog_2 <- subset(fprog, periodo==2)
docs <- fprog_2$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
pal=c("orange","blue","blue")
wordcloud(words = df$word, freq = df$freq, min.freq = 10,           
          max.words=500, random.order=FALSE,           
          colors=pal)

########## FSP ###################
######## General ##################
fsp <- subset(datos3, bloque=="Frente Soc. y Pop")
docs <- fsp$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df <- subset(df, word!="rosario" & word!="ciudad")
set.seed(1234) # for reproducibility 
dev.new(width = 200, height = 200, unit = "px")
pal=c("violet")
wordcloud(words = df$word, freq = df$freq, min.freq = 18,           
          max.words=500, random.order=FALSE,           
          colors=pal)

df2 <- subset(df, freq>18)
wordcloud2(data = df2, shape = 'circle',     
           color="violet", size = 0.35)

#### Periodo 1 ####################
fsp_1 <- subset(fsp, periodo==1)
docs <- fsp_1$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
pal=c("violet","skyblue2")
wordcloud(words = df$word, freq = df$freq, min.freq = 8,           
          max.words=500, random.order=FALSE,           
          colors=pal)

####### Periodo 2 #################
fsp_2 <- subset(fsp, periodo==2)
docs <- fsp_2$mensaje
docs <- Corpus(VectorSource(docs))
dtm2 <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm2)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
pal=c("violet","skyblue2")
wordcloud(words = df$word, freq = df$freq, min.freq = 8,           
          max.words=500, random.order=FALSE,           
          colors=pal)
