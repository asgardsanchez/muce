library(tidyverse)
library(tidytext)
library(stopwords)
library(plotly)

setwd("/Users/asgardsanchez/Documents/proyectoserre")
#cargar dataset
dataset_instagram_hashtag <- read_csv("dataset_instagram-hashtag-scraper_2024-04-02_19-05-51-924.csv")


#palabras inutiles english an espanish
esStopwords <- get_stopwords("es", "stopwords-iso")
enStopwords <- get_stopwords("en", "stopwords-iso")


#graficar palabras frecuentes
igPlot1 <- dataset_instagram_hashtag %>% 
  mutate(text=gsub("#", "", caption)) %>% 
  unnest_tokens(word, text) %>% 
  filter(!word%in%esStopwords$word) %>% 
  filter(!word%in%enStopwords$word) %>% 
  count(word, sort = TRUE) %>% 
  slice_max(n, n=10) %>% 
  mutate(text=paste0(word, " ", n)) %>% 
  ggplot(aes(n, reorder(as_factor(word), n), fill=n, text=text))+
  geom_col(show.legend = FALSE)+
  xlab(NULL)+
  ylab(NULL)

ggplotly(igPlot1, tooltip = c("text"))

#contar palabras por post
wordsByPost <- dataset_instagram_hashtag %>% 
  mutate(text=gsub("#", "", caption)) %>% 
  rowid_to_column() %>% 
  group_by(rowid) %>% 
  unnest_tokens(word, text) %>% 
  filter(!word%in%esStopwords$word) %>% 
  filter(!word%in%enStopwords$word) %>% 
  filter(nchar(word)>2) %>% 
  count(word, sort = TRUE)   %>% 
  ungroup()

#contar los pares de palabras
library(widyr)
igPairs <- pairwise_count(wordsByPost, word, rowid, wt = n) %>% 
  arrange(desc(n))

#cargar librerías para grafos
library(tidygraph)
library(ggraph)

#generar el grafo
set.seed(1234)
igPairsPlot <- igPairs %>%
  filter(n>30) %>% 
  as_tbl_graph(directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(centralidad=centrality_degree()) %>% 
  mutate(comunidad=as_factor(group_louvain())) %>%
  filter(centralidad>1) %>% 
  ggraph(layout = "stress")+
  geom_edge_link(aes(width=n), color="lightgrey")+
  geom_node_text(aes(label=name, 
                     size= centralidad,
                     color=comunidad),
                 check_overlap = TRUE)+
  scale_size_continuous(range = c(3,20))+
  theme_void()

ggplotly(igPairsPlot)

############/MR/WORLDWIDE/INTERNATIONAL/KILLAR##############

dataset_instagram_hashtag_2 <- read_csv("dataset_instagram-hashtag-scraper_2024-04-28_04-17-53-032.csv") %>% 
  tibble()

# Contar palabras por post del dataset2
wordsByPost2 <- dataset_instagram_hashtag_2 %>%
  mutate(text = gsub("#", "", caption)) %>%
  rowid_to_column() %>%
  group_by(rowid) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% esStopwords$word) %>%
  filter(!word %in% enStopwords$word) %>%
  filter(nchar(word) > 2) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Contar los pares de palabras del nuevo dataset
igPairs2 <- pairwise_count(wordsByPost2, word, rowid, wt = n) %>%
  arrange(desc(n))

# Generar el segundo grafo de co-ocurrencia
set.seed(1234)
igPairsPlot2 <- igPairs2 %>%
  filter(n > 30) %>%
  as_tbl_graph(directed = FALSE) %>%
  activate(nodes) %>%
  mutate(centralidad = centrality_degree()) %>%
  mutate(comunidad = as_factor(group_louvain())) %>%
  filter(centralidad > 1) %>%
  ggraph(layout = "stress") +
  geom_edge_link(aes(width = n), color = "lightgrey") +
  geom_node_text(aes(label = name, 
                     size = centralidad,
                     color = comunidad),
                 check_overlap = TRUE) +
  scale_size_continuous(range = c(3,20)) +
  theme_void()

ggplotly(igPairsPlot2)

#10k post sample
dataset2_selection <- dataset_instagram_hashtag_2 %>% 
  slice_sample(n=10000) 

#contar palabras más frecuentes de la selección
dataset2_words <- dataset2_selection %>% 
  mutate(text=gsub("#", "", caption)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(esStopwords)  %>% 
  count(word, sort = TRUE) 

#graficar 20 palabras más frecuentes
dataset2_words %>% 
  slice_max(n, n=20) %>% 
  mutate(text=paste0(word, " ", n)) %>% 
  ggplot(aes(n, reorder(as_factor(word), n), fill=n, text=text))+
  geom_col(show.legend = FALSE)+
  xlab(NULL)+
  ylab(NULL)

#buscar actores
dataset2_usuarios<- dataset_instagram_hashtag_2 %>% 
  mutate(text=gsub("#", "", caption)) %>% 
  mutate(alt=gsub("Photo shared", "Photo", alt)) %>% 
  mutate(usuario=gsub("Photo by ", "", alt)) %>% 
  mutate(usuario= sub(" on .*", "", usuario)) %>% 
  select(usuario, text)

#usuarios más productivos
dataset2_usuarios %>% 
  filter(!is.na(usuario)) %>% 
  count(usuario, sort = TRUE) %>% 
  arrange(desc(n)) %>% 
  slice_max(order_by = n, n= 20) %>% 
  ggplot(aes(n, reorder(as_factor(usuario),n), fill=n))+
  geom_col(show.legend = FALSE)+
  ylab(NULL)

#usuarios con más de 5 posts
usuariosTop5 <- dataset2_usuarios %>% 
  filter(!is.na(usuario)) %>% 
  count(usuario, sort = TRUE) %>% 
  filter(n>=5)

#palabras frecuentes de los top 5 usuarios
usariosWords <- dataset2_usuarios %>% 
  filter(usuario%in%usuariosTop5$usuario) %>% 
  #mutate(text=gsub("#", "", caption)) %>% 
  group_by(usuario) %>% 
  unnest_tokens(word, text) %>% 
  filter(nchar(word)>2) %>% 
  anti_join(stop_words) %>% 
  anti_join(esStopwords)  %>% 
  anti_join(enStopwords)  %>% 
  count(word, sort = TRUE) %>% 
  ungroup()


#graficar las palabras más frecuentes de los top 5
usariosWords %>% 
  count(word, sort = TRUE) %>% 
  slice_max(order_by = n, n = 20) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(n, reorder(as_factor(word), n), fill=n))+
  geom_col(show.legend = FALSE)+
  ylab(NULL)

#topic modeling
library(topicmodels)

#transformar formato de base de datos de formato tidy a document term matrix
usuariosDTM <- usariosWords %>% cast_dtm(usuario, word, n)

#calcular el mejor valor de k
mod_log_lik = numeric(10)
mod_perplexity = numeric(10)

for (i in 2:10) {
  mod = LDA(usuariosDTM, k=i, method="Gibbs", alpha=0.1,
            control=list(alpha=0.1, iter=1000, seed=1234, thin=1))
  mod_perplexity[i] = perplexity(mod, usuariosDTM)
}

mod_perplexity

plot(mod_perplexity, 
     xlab="Valores de k", 
     ylab="Perplejidad", 
     xlim = c(2, 15), 
     ylim = c(8500, 17200) #aquí poner el rango de mod
)

#aplicar para k=4
culturalHeritageLDA <- LDA(usuariosDTM, 
                           k = 4, 
                           control = list(seed = 1234))

#ordenar términos del modelo según beta
termsCH <- tidy(culturalHeritageLDA, matrix="beta")
#ordenar documentos del modelo según gamma
documentsCH <- tidy(culturalHeritageLDA, matrix="gamma")

#seleccionar primero términos más frecuentes por tópico
topTerms <- termsCH %>% 
  group_by(topic) %>% 
  slice_max(order_by = beta, n = 25) %>% 
  ungroup() 

#graficar vínculo entre tópicos y términos según beta
topTerms %>%
  mutate(topic = as.factor(topic),
         term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill=as_factor(topic)))+
  geom_col(show.legend = FALSE)+
  scale_y_reordered() +
  facet_wrap(~topic, scales = "free")


install.packages("highcharter")
#graficar vínculo entre tópicos y documentos según gamma
library(highcharter)
documentsCH %>% 
  group_by(topic) %>% 
  slice_max(order_by = gamma, n = 5) %>% 
  ungroup() %>% 
  select(-gamma) %>% 
  right_join(topTerms) %>% 
  filter(!term=="soulfulodisha") %>% 
  select(-beta) %>% 
  data_to_sankey() %>% 
  hchart("sankey")

