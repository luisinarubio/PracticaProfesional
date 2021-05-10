
# ------------------------------------------------------------------------------
# Función para aplicar LDA con el paquet textminer y manipular resultados
# ------------------------------------------------------------------------------

# Crear función auxiliar para correr varias veces con distintos parámetros
lda_textminer <- function(doc_vec, doc_names = 1:length(doc_vec),
                          ngram_window = c(1, 1), 
                          stopword_vec = c(),
                          min_frec = 0, seed = 0,
                          k = 2, it = 500, burnin = 180,
                          alpha = 0.1, beta = 0.05,
                          ntop = 15, min_car = 3) {

  # Este paquete tiene su propia función para crear la matrix dtm
  dtm <- CreateDtm(doc_vec = doc_vec, 
                   doc_names = doc_names,
                   ngram_window = ngram_window, 
                   stopword_vec = stopword_vec,
                   # Lo siguiente en FALSE porque ya lo hicimos
                   remove_punctuation = FALSE, # no perder los _ que dejamos a proposito
                   lower = FALSE,
                   remove_numbers = FALSE
  )
  # Sacar palabras con pocos caracteres
  dtm <- dtm[, nchar(colnames(dtm)) >= min_car]
  # Quedarse con palabras con una frec min deseada
  dtm <- dtm[, colSums(dtm) > min_frec]
  
  # Ajuste
  set.seed(seed)
  mod_lda <- FitLdaModel(dtm = dtm, k = k, iterations = it, burnin = burnin,
                         alpha = alpha, beta = beta, optimize_alpha = T,
                         calc_likelihood = T, calc_coherence = T, calc_r2 = T)
  
  # Top terms en cada tópico
  top_terms <- GetTopTerms(phi = mod_lda$phi, M = ntop)
  top_terms <- data.frame(
    topic = rep(paste0("t_", 1:k), each = ntop),
    terms = top_terms %>% as.vector(),
    phi = sapply(1:k, function(x) mod_lda$phi[x, as.character(top_terms[, x])]) %>% as.vector()
  )
  
  # Grafico top terms
  topic_labels <- c(t_1="Tópico 1: Género y DDHH",
                    t_2="Tópico 2: Jerga de campaña",
                    t_3="Tópico 3: Propuestas políticas")
   plot_topic <- 
    top_terms %>%
    mutate(terms = reorder_within(terms, phi, topic)) %>%
    ggplot(aes(terms, phi, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    xlab("Términos")+
    ylab(expression(phi["kj"]))+
    facet_wrap(~ topic, scales = "free",labeller = as_labeller(topic_labels)) +
    coord_flip() +
    scale_x_reordered()+
    scale_fill_manual(values = c("mediumorchid","green4","deepskyblue2"),limits=c("t_1","t_2","t_3"),
                      labels=c("Género y DDHH","Jerga de campaña","Políticas públicas"))+
    theme_bw()
  
  # Coherence y prevalence
  coh_prev <- data.frame(
    topic = factor(1:k, levels = 1:k, labels = paste0("t_", 1:k)),
    coherence = mod_lda$coherence, 
    prevalence = colSums(mod_lda$theta) / sum(mod_lda$theta) * 100, 
    row.names = NULL
  )
  
  # Grafico prevalencia y coherencia
  coh_prev_plot <- 
    coh_prev %>% 
    pivot_longer(cols = c(coherence, prevalence)) %>%
    ggplot() + 
    aes(x = topic, y = value, group = name) +
    geom_point() + 
    geom_line() +
    facet_wrap(~ name, scales = "free_y", nrow = 2)
  
  # Dendrograma
  res_hclust <- 
    mod_lda$phi %>% 
    CalcHellingerDist() %>% 
    as.dist() %>% 
    hclust("ward.D")
  
  # Más para analizar la prob de cada tópico por mensaje y en cada bloque
  # Promedio de prob de topicos en cada bloque
  # thetas son las gamma del paquete topicmodels
  thetas <- 
    mod_lda$theta %>% 
    data.frame(row.names = NULL) %>% 
    as_tibble() %>% 
    mutate(mens_id = rownames(mod_lda$theta)) %>% 
    separate(mens_id, into = c("id", "concejal", "bloque", "periodo","Pago"), sep = "_") %>% 
    pivot_longer(starts_with("t_"), names_to = "topic")%>%
    mutate(bloque = recode(bloque,`Ciudad Futura` = "CF", 
                           `Frente Progresista` = "FP",
                           Cambiemos = "C", `Frente Nac. y Pop` = "FNyP", 
                           `Frente Soc. y Pop` = "FSyP"), 
           periodo = factor(periodo, labels = c("Electoral", "Post-Electoral")))
  
  thetas_media <-
    thetas %>% 
    group_by(bloque, topic) %>% 
    summarise(media = mean(value)) %>% 
    ungroup()
  # Tabla para mostrar lo anterior
  thetas_media_tabla <- 
    thetas_media %>% 
    pivot_wider(names_from = topic, values_from = media, names_prefix = "topic_")
  # Grafico para mostrar lo anterior
  thetas_media_plot <- 
    ggplot(thetas_media) +
    aes(x = bloque, y = media, fill = factor(topic)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("mediumorchid","green4","deepskyblue2"),limits=c("t_1","t_2","t_3"),
                      labels=c("Género y DDHH","Jerga de campaña","Políticas públicas"))+
    labs(fill = "Tópico", x = "Bloque", 
         y = "Media de la probabilidad de\n tópico en cada post")+theme_bw()
  # Principal tópico en cada mensaje
  top_topico <- 
    thetas %>%
    group_by(id, concejal, bloque, periodo,Pago) %>%
    top_n(1, value) %>%
    ungroup()
  # Distribución de principal tópico en cada mensaje según bloque
  top_topico_plot <- 
    ggplot(top_topico) +
    aes(x = bloque, fill = factor(topic)) +
    geom_bar(position = "fill") +
    labs(fill = "Tópico principal", x = "Bloque", y = "Proporción")+
    scale_fill_manual(values = c("mediumorchid","green4","deepskyblue2"),limits=c("t_1","t_2","t_3"),
                      labels=c("Género y DDHH","Jerga de campaña","Políticas públicas"))+
    theme_bw()
  top_topico_tabla <- 
    top_topico %>% 
    tabyl(bloque, topic) %>% 
    adorn_totals(c("row", "col")) %>% 
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 2) %>%
    adorn_ns() %>% 
    adorn_title(placement = "combined", "Bloque", "Tópico")
  
  ############  tópicos por periodo
  periodo_label=c('Electoral'="Período Electoral",
                  'Post-Electoral'="Período Post-Electoral")
  top_topico_plot_periodo <- 
    ggplot(top_topico) +
    aes(x = bloque, fill = factor(topic)) +
    geom_bar(position = "fill") +
    labs(fill = "Tópico principal", x = "Bloque", y = "Proporción")+
    facet_wrap(~periodo, scale="free",labeller = as_labeller(periodo_label))+
    scale_x_discrete(limits=c("Cambiemos","Ciudad Futura","Frente Progresista","Frente Soc. y Pop","Frente Nac. y Pop"),
                     labels=c("Camb","CF","FProg","FSP","FNP"))+
    scale_fill_manual(values = c("mediumorchid","green4","deepskyblue2"),limits=c("t_1","t_2","t_3"),
                      labels=c("Género y DDHH","Jerga de campaña","Políticas públicas"))+
    theme_bw()+
    theme(legend.position = "right",
          legend.text = element_text(size = 8),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          plot.title = element_text(family="arial", size = 15, hjust = 0.5, face = "bold"))
  
  top_topico_plot_periodo2 <- 
    ggplot(top_topico) +
    aes(x = periodo, fill = factor(topic)) +
    geom_bar(position = "fill") +
    labs(fill = "Tópico", x = "Periodo", y = "Porcentaje")+
    facet_wrap(~bloque, scale="free")+
    scale_fill_manual(values = c("mediumorchid","green4","deepskyblue2"),limits=c("t_1","t_2","t_3"),
                      labels=c("Género y DDHH","Jerga de campaña","Políticas públicas"))+
    theme_bw()+
    theme(legend.position = "right",
          legend.text = element_text(size = 8),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          plot.title = element_text(family="arial", size = 15, hjust = 0.5, face = "bold"))
  
  thetas_media_2 <-
    thetas %>% 
    group_by(bloque,periodo,Pago, topic) %>% 
    summarise(media = mean(value)) %>% 
    ungroup()
  
  thetas_media_plot_periodo <- 
    ggplot(thetas_media_2) +
    aes(x = periodo, y = media, fill = factor(topic)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~bloque, scale="free")+
    labs(fill = "Tópico", x = "Periodo", 
         y = "Media de la probabilidad de\n tópico en cada post")+
    scale_fill_manual(values = c("mediumorchid","green4","deepskyblue2"),limits=c("t_1","t_2","t_3"),
                      labels=c("Género y DDHH","Jerga de campaña","Políticas públicas"))+
    theme_bw()+
    theme(legend.position = "right",
          legend.text = element_text(size = 8),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          plot.title = element_text(family="arial", size = 15, hjust = 0.5, face = "bold"))
  
  ############  tópicos por pago
  pago_labels <- c('Paga'="Publiación paga",'No paga'="Publicación no paga")
  
   top_topico_plot_pago <- 
    ggplot(top_topico) +
    aes(x = bloque, fill = factor(topic)) +
    geom_bar(position = "fill") +
    labs(fill = "Tópico principal", x = "Bloque", y = "Proporción")+
    facet_wrap(~Pago, scale="free",labeller = as_labeller(pago_labels))+
    scale_x_discrete(limits=c("Cambiemos","Ciudad Futura","Frente Progresista","Frente Soc. y Pop","Frente Nac. y Pop"),
                     labels=c("Camb","CF","FProg","FSP","FNP"))+
    scale_fill_manual(values = c("mediumorchid","green4","deepskyblue2"),limits=c("t_1","t_2","t_3"),
                      labels=c("Género y DDHH","Jerga de campaña","Políticas públicas"))+
    theme_bw()+
    theme(legend.position = "right",
          legend.text = element_text(size = 8),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          plot.title = element_text(family="arial", size = 15, hjust = 0.5, face = "bold"))
  
  top_topico_plot_pago2 <- 
    ggplot(top_topico) +
    aes(x = Pago, fill = factor(topic)) +
    geom_bar(position = "fill") +
    labs(fill = "Tópico", x = "Pago", y = "Porcentaje")+
    facet_wrap(~bloque, scale="free")+
    scale_fill_manual(values = c("mediumorchid","green4","deepskyblue2"),limits=c("t_1","t_2","t_3"),
                      labels=c("Género y DDHH","Jerga de campaña","Políticas públicas"))+
    theme_bw()+
    theme(legend.position = "right",
          legend.text = element_text(size = 8),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          plot.title = element_text(family="arial", size = 15, hjust = 0.5, face = "bold"))
  
  thetas_media_plot_pago <- 
    ggplot(thetas_media_2) +
    aes(x = Pago, y = media, fill = factor(topic)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~bloque, scale="free")+
    labs(fill = "Tópico", x = "Pago", 
         y = "Media de la probabilidad de\n tópico en cada post")+
    scale_fill_manual(values = c("mediumorchid","green4","deepskyblue2"),limits=c("t_1","t_2","t_3"),
                      labels=c("Género y DDHH","Jerga de campaña","Políticas públicas"))+
    theme_bw()+
    theme(legend.position = "right",
          legend.text = element_text(size = 8),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          plot.title = element_text(family="arial", size = 15, hjust = 0.5, face = "bold"))
  
  # Return: todo se puede sacar de mod_lda por fuera de la función, pero
  # devuelvo acá por comodidad
  return(list(
    dtm = dtm, mod_lda = mod_lda, top_terms = top_terms, coh_prev = coh_prev,
    coh_prev_plot = coh_prev_plot, hclust = res_hclust, plot_topic = plot_topic,
    thetas_media_tabla = thetas_media_tabla, 
    thetas_media_plot = thetas_media_plot,
    top_topico_tabla = top_topico_tabla, top_topico_plot = top_topico_plot,
    top_topico_plot_periodo = top_topico_plot_periodo,
    top_topico_plot_periodo2=top_topico_plot_periodo2,
    thetas_media_plot_periodo = thetas_media_plot_periodo,
    top_topico_plot_pago = top_topico_plot_pago,
    top_topico_plot_pago2 = top_topico_plot_pago2,
    thetas_media_plot_pago = thetas_media_plot_pago
  ))
}
