# Machine Learning - 2022-1 - Laboratório 06
# Dados textuais

# Bibliotecas -------------------------------------------------------------
library(tidyverse)
library(tm)
library(tidytext)
library(textdata)
library(wordcloud)


# Dados -------------------------------------------------------------------

# Amostra dos dados retirados de 
# https://archive.ics.uci.edu/ml/datasets/Drug+Review+Dataset+%28Drugs.com%29

load("amostra_drugs.RData")
glimpse(data)

# condicoes que mais aparecem nos dados
data$condition %>% 
  unique()

data %>% 
  group_by(condition) %>% 
  summarize(total = n()) %>% 
  slice_max(total, n = 10) %>% 
  ggplot(aes(x = reorder(condition, total), y = total))+
  geom_col() +
  coord_flip() +
  labs(x = "Condition", y = "Count") +
  theme_bw()

# remedios que mais aparecem
data$drugName %>% 
  unique()

data %>% 
  group_by(drugName) %>% 
  summarize(total = n()) %>% 
  slice_max(total, n = 10) %>% 
  ggplot(aes(x = reorder(drugName, total), y = total))+
  geom_col()+
  coord_flip()+
  labs(x = "Drug name", y = "Total")

# algumas avaliações
data %>% 
  select(condition, review) %>% 
  slice_head(n = 10) %>% 
  View()

# vamos remover códigos html que estão junto às avaliações
data <- data %>% 
  mutate(review = map_chr(review, 
                          ~ xml2::xml_text(xml2::read_html(paste0("<x>", .x, "</x>")))))

# resultado
data %>% 
  select(condition, review) %>% 
  slice_head(n = 10) %>% 
  View()

data$review[3]


# Análise textual ---------------------------------------------------------

# criação do corpus
corpus <- VCorpus(VectorSource(data$review), 
                  readerControl = list(language = "english"))

# processamento das reviews
corpus <- corpus %>% 
  tm_map(content_transformer(tolower)) %>%  # minusculas
  tm_map(stripWhitespace) %>%         # remove espaços em branco
  tm_map(removePunctuation) %>%       # remove pontuacao
  tm_map(removeWords, stopwords("english")) %>%  # remove stopwords
  tm_map(stemDocument)                # stemiza documento

# wordcloud
wordcloud(corpus, max.words = 60, random.order = FALSE,
          rot.per = 0, colors = brewer.pal(8, "Blues"))

# dtm
dtm <- DocumentTermMatrix(corpus)

dtm

inspect(dtm)

reviews <- tidy(dtm)

reviews

# merge para obter as condicoes de cada documento
data_merge <- data %>% 
  mutate(document = as.character(1:nrow(data))) %>% 
  left_join(reviews) %>% 
  select(document, term, count, condition, 
         drugName, rating, usefulCount)

# word cloud Birth Control
par(mar=rep(0,4))

data_merge %>% 
  filter(condition == "Birth Control") %>% 
  count(term) %>% 
  slice_max(`n`, n = 100) %>%
  with(wordcloud(term, n, random.order = FALSE, rot.per = 0, 
                 colors = brewer.pal(8, "Blues")))

# word cloud Depression
data_merge %>% 
  filter(condition == "Depression") %>% 
  count(term) %>% 
  slice_max(`n`, n = 100) %>%
  with(wordcloud(term, n, random.order = FALSE, rot.per = 0, 
                 colors = brewer.pal(8, "Blues")))

# word cloud Anxiety
data_merge %>% 
  filter(condition == "Anxiety") %>% 
  count(term) %>% 
  slice_max(`n`, n = 100) %>%
  with(wordcloud(term, n, random.order = FALSE, rot.per = 0, 
                 colors = brewer.pal(8, "Blues")))

# Análise de sentimentos --------------------------------------------------

get_sentiments("afinn") # valor numérico
get_sentiments("bing") # positivo/negativo
get_sentiments("nrc") # substatinvo

# quais os sentimos em nrc?
get_sentiments("nrc") %>% 
  select(sentiment) %>% 
  unique()

# vamos usar a base bing para avaliar o sentimento
# expressado nas avaliacoes das 5 condicoes mais
# frequentes

top_conditions <- data %>% 
  count(condition) %>%
  slice_max(`n`, n = 5) %>% 
  select(condition) %>% 
  pull()

sentimentos_bing <- data_merge %>% 
  select(document, word = term, count, condition) %>% 
  filter(condition %in% top_conditions) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(document, sentiment, condition) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive-negative)

sentimentos_bing %>% 
  ggplot(aes(x = sentiment, y = condition, 
             fill = condition)) +
  geom_vline(xintercept = 0, linetype = "dashed", 
             col = "blue") +
  scale_fill_brewer(palette = "Dark2") +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  labs(x = "Sentimento", y = "") +
  coord_flip() +
  theme_bw()

sentimentos_bing %>% 
  ggplot(aes(x = sentiment, fill = condition)) +
  geom_density(show.legend = FALSE, alpha = 0.7) +
  geom_vline(xintercept = 0, color = "gray", 
             linetype = "dashed")+
  scale_fill_brewer(palette = "Dark2")+
  facet_wrap(~ condition)+
  labs(x = "Sentimento", y = "Densidade")

# tarefa: fazer o mesmo com a base afinn
