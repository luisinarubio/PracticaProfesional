library(tidytext) 
library(textclean) 
library(tm) 
library(readr) 
library(tidyr) 
library(purrr)

datos2 <- read_delim("Datos/Concejales/Mensajes_unidos.txt", delim = "\t")
para_reempl1 <- c("santa fe", 
                  "santa rosa de lima", "santa emilia", "santa lucia", "santa cruz",
                  "santa clara de patagones", "santa teresita", "santa clara", 
                  "san martín", "san luis"
)
reemplazos1 <- matrix(ncol = 2, byrow = F, data = c(para_reempl1, str_replace_all(para_reempl1, " ", "_")))

reemplazos2 <- matrix(ncol = 2, byrow = T, data = c(
  "concejo municipal de rosario", "concejo",
  "concejo municipal", "concejo",
  "santafe","santa_fe",
  "sta fe", "santa_fe",
  "transporte público", "transporte",
  "transporte publico", "transporte",
  "san martin", "san_martín",
  # Emojis que reemplazan la o
  "tod[^[:alpha:]]s", "todos",
  "junt❤️s", "juntos",
  # El resto de los emojis son eliminados con este reemplazo (conserva ñ, tildes, y ü:
  "[^áéíóúüñ[\x01-\x7F]]", " "
))

# Reemplazos de nombres de personas (en cada linea: original, nuevo)
reemplazos3 <- matrix(ncol = 2, byrow = T, data = c(
  "mauricio macri","mauricio_macri",
  "miguel ángel pichetto","miguel_ángel_pichetto",
  "miguel angel pichetto","miguel_ángel_pichetto",
  "roy lópez molina","roy_lopez_molina",
  "daniela león","daniela_león",
  "fede angelini","federico_angelini",
  "federico angelini","federico_angelini",
  "jose corral","josé_corral",
  "josé corral","josé_corral",
  "juan monteverde","juan_monteverde",
  "caren tepp","caren_tepp",
  "carlos del frade","carlos_del_frade",
  "fernanda gigliani","fernanda_gigliani",
  "cristina fernández de kirchner","cristina_fernández",
  "cristina fernandez de kirchner","cristina_fernández",
  "cristina fernandez","cristina_fernández",
  "cristina fernández","cristina_fernández",
  "cristina kirchner","cristina_fernández",
  "marina magnani","marina_magnani",
  "alejandra ródenas","alejandra_ródenas",
  "alejandra rodenas","alejandra_ródenas",
  "evo morales","evo_morales",
  "alberto fernández","alberto_fernández",
  "agustin rossi","agustín_rossi",
  "agustín rossi","agustín_rossi",
  "omar perotti","omar_perotti",
  "roberto sukerman","roberto_sukerman",
  "oscar cachi martinez","oscar_martínez",
  "oscar martínez","oscar_martínez",
  "oscar cachi martínez","oscar_martínez",
  "néstor kirchner","néstor_kirchner",
  "eduardo toniolli","eduardo_toniolli",
  "lucila de ponti", "lucila_de_ponti",
  "lucila ponti","lucila_ponti",
  "norma lópez","norma_lópez",
  "andrés giménez","andrés_giménez",
  "leandro busatto","leandro_busatto",
  "marcos cleri","marcos_cleri",
  "raúl alfonsín","raúl_alfonsín",
  "maría eugenia schmuck","maría_eugenia_schmuck",
  "enrique estévez","enrique_estévez",
  "aldo poy","aldo_poy",
  "aldo pedro poy","aldo_poy",
  "nico gianelloni","nico_gianelloni",
  "antonio bonfatti","antonio_bonfatti",
  "horacio ghirardi","horacio_ghirardi",
  "miguel lifschitz","miguel_lifschitz",
  "vero irizar","verónica_irizar",
  "verónica irizar","verónica_irizar",
  "pablo javkin","pablo_javkin",
  "mónica fein","mónica_fein",
  "roberto lavagna","roberto_lavagna",
  "celeste lepratti","celeste_lepratti",
  "paula perassi","paula_perassi",
  "lorena almiron","lorena_almirón",
  "rubén giustiniani","rubén_giustiniani",
  "silvia augsburger","silvia_augsburger",
  "miguel zamarini","miguel_zamarini"
))
# Junto todo en una matriz de reemplazos - está bueno tener registro de todas las modificaciones que hagamos
reemplazos <- rbind(reemplazos1, reemplazos2, reemplazos3)

# Palabras vacías   ### Por qué se eliminan esas palabras?
vacias <- base::setdiff(get_stopwords("es", source = "stopwords-iso")$word, c(
  "acuerdo","ahora","verdad","verdadera","verdadero","trabajo","trata","trabaja",
  "trabajamos","trabajan","trabajar","trabajas","todas","tiempo","siempre",
  "posible","podemos","puede","pueden","puedo","otros","otras","nunca","nuevo",
  "nosotras","nosotros","nadie","momento","modo","menos","mejor","mayor",
  "manera","lugar","mal","intento","intentar","intentamos","igual","estado",
  "empleo","ejemplo","ellas","ellos","diferentes","contra","bueno","buena",
  "buenos","buenas"
))

# Limpieza
datos2$mensaje <- 
  # datos2$Message.y[3401:3410] %>% # para probar 
  datos2$Message.y %>% 
  str_to_lower() %>% 
  # Hacer todos los reemplazos a la vez con reduce2
  reduce2(.x = reemplazos[, 1], .y = reemplazos[, 2],
          .f = str_replace_all, .init = .) %>%
  # posibles saltos de línea
  str_replace_all("\n", " ") %>% 
  # sacar direcciones html y urls
  replace_html() %>% 
  replace_url() %>% 
  # sacar menciones
  replace_tag() %>% 
  # Acortar expresiones como "muy buenoooo"
  replace_word_elongation() %>% 
  # Sacar fechas y horas
  replace_date(replacement = " ") %>% 
  replace_time(replacement = " ") %>% 
  # Remover puntuaciones, caracteres especiales, números, todo excepto guiones
  # bajos que usamos para los nombres y espacios
  str_replace_all("[^_ [:alpha:]]", replacement = " ") %>% 
  # Remover stopwords
  removeWords(words = vacias) %>% 
  # Remover espacios blancos repetidos
  str_squish() %>% 
  # Remover espacios blancos al principio y al final
  str_trim()

# Sacar mensajes que quedaron vacío luego de la limpieza (por ej, tenían links)
datos2 <- subset(datos2, !is.na(mensaje) | mensaje != "")

datos2$id <- c(1:nrow(datos2))

datos2 <- datos2%>%unite( "mens_id",c(id,Nombre_corr2,bloque,periodo,Pago),sep = "_")

write_delim(datos2, "Datos/Concejales/Mensajes_limpios.txt", "\t")