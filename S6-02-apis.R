##########################################################################
# Jose Cajide - @jrcajide
# Curso de introducción a R: Apis
##########################################################################

rm(list=ls()) 
cat("\014")

install.packages(c("devtools", "rjson", "bit64", "httr"),  repos="https://cran.rstudio.com")
devtools::install_github("geoffjentry/twitteR")


# ¿Es posible detectar personas "diferentes" a la media por sus mensajes en redes sociales?

library(twitteR)

# https://apps.twitter.com/

api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""

# Autenticacion
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

termino_a_buscar <- "#DataScience"
numero_tweets <- 10

tweets <- searchTwitter(termino_a_buscar, 
                        n=numero_tweets, 
                        lang="es", 
                        resultType="recent")

# tweets <- readRDS('data/tweets.Rds')

(tweet <- tweets[[1]]) 
str(tweet)
texto_tweet <- tweet$getText()

# https://cloud.google.com/natural-language/?hl=es
# install.packages("googleLanguageR")
library(googleLanguageR)

# https://cloud.google.com/free/
gl_auth('cpb100-162913-faf075966c64.json')

resultado_api <- gl_nlp(texto_tweet)
(scoring <- resultado_api$documentSentiment$score)

# Procesando todos los Tweets

tweets.df <- twListToDF(tweets)

head(tweets.df)

texto_tweets <- lapply(tweets, function(t) t$getText())

sentimiento_tweets <- lapply(texto_tweets, function(t) gl_nlp(t))

puntuaciones_tweets <- sapply(sentimiento_tweets, function(t) t$documentSentiment$score)

tweets.df$puntuacion <- puntuaciones_tweets

tweets.df$sentimiento <- 'Neutro'
tweets.df$sentimiento <- ifelse(tweets.df$puntuacion > .2, "Positivo", tweets.df$sentimiento)
tweets.df$sentimiento <- ifelse(tweets.df$puntuacion < -.2, "Negativo", tweets.df$sentimiento)

tweets.df %>% select(screenName, text, puntuacion, sentimiento) %>% View()

mean(tweets.df$puntuacion)

barplot(table(tweets.df$sentimiento))

# Valores extremos
outliers <- boxplot(tweets.df$puntuacion)$out

tweets_outliers.df <- tweets.df %>% filter(puntuacion %in% outliers) 

library(ggrepel)
tweets.df %>% ggplot(aes(x=1, y=puntuacion)) + geom_boxplot(outlier.shape = NA) +
  geom_point(data = tweets_outliers.df) +
  # geom_jitter(position=position_jitter(width=0.01), data = tweets_outliers.df) +
  geom_label_repel(aes(label = paste0('@',screenName)),arrow = arrow(length = unit(0.0, "npc")),box.padding = unit(2, "lines"), data= tweets_outliers.df) + theme_bw()

# Bonus
install.packages('googlesheets')
library(googlesheets)
gs_auth() 

tweets_ss <- gs_new("Tweeter", 
                    ws_title = termino_a_buscar, 
                    input = tweets.df %>% select(screenName, text, puntuacion, sentimiento),
                    trim = TRUE, 
                    verbose = FALSE)

browseURL(tweets_ss$browser_url)

tweets_ss %>% 
  gs_read()
