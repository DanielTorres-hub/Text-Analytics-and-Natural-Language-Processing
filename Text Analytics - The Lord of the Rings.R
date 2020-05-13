library(textreadr)
library(tidytext)
library(ggplot2)

library(magrittr)
library(xml2)
library(rvest)

library("NLP")
library("twitteR")
library("tm")

library(magrittr)
library(dplyr)

library(tidyverse)
library(reshape2)
library(wordcloud)

setwd("~/DANIEL/2020/Hult/MODULE 2/Text Analytics and NLP/Assignment/Books")
nm <- list.files(path="~/DANIEL/2020/Hult/MODULE 2/Text Analytics and NLP/Assignment/Books")

#####################################################################################################
################################## Fellowship of the Ring ###########################################

my_data <- read_document(file=nm[1]) # creating a vector
my_data_together <- paste(my_data, collapse = " ") # Creating a concatenated vector
mydf <- data_frame(line=1, text=my_data_together)

fellowship_df <- mydf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)


fellowship_hist <- mydf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)%>%
  filter(n > 150)%>%
  mutate(word = fct_reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()


fellowship_hist

# SENTIMENT BAR PLOT

fellowship_sent <- mydf%>%
  unnest_tokens(word, text)%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

fellowship_sent

fellowship_sent %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# SENTIMENT CLOUD PLOT

#### NRC
fellowship_df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%  ########## ONLY USE ONE N
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.8, 0.8),
                   fix.asp=TRUE, 
                   title.size = 2,
                   rot.per=0.25)


#### BING
fellowship_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%  ########## ONLY USE ONE N
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.8, 0.8),
                   fix.asp=TRUE, 
                   title.size = 1,
                   rot.per=0.25)

#### AFINN
fellowship_sent %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%  ########## ONLY USE ONE N
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.5, 0.5),
                   fix.asp=TRUE, 
                   title.size = 1,
                   rot.per=0.25)

#### STANDARD CLOUD
fellowship_df %>%
  with(wordcloud(word, n, max.words = 100))

fellowship_sent %>%
  with(wordcloud(word, n, max.words = 100))


############## Bigramas and negated words ###############

fellowship_bigrams <- mydf %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)


fellowship_bigrams #We want to see the bigrams (words that appear together, "pairs")

# Using separate function to remove the stop words
fellowship_separated <- fellowship_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

negation_tokens <- c('no', 'never', 'without')

negated_fellowship <- fellowship_separated %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%  ###### SCORE???????????????
  ungroup()

negated_fellowship


#### Plot of the negations words

library(ggplot2)

negated_fellowship_plot <- function(x){
  negated_fellowship %>%
    filter(word1 == x) %>%
    mutate(contribution = n* value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by" , x))+   #######, X 
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}

negated_fellowship_plot(x="no") #this is your first negation word
negated_fellowship_plot(x="never") #this is your second negation word
negated_fellowship_plot(x="without") #this is your third negation word

######## SPIDER PLOT

#creating a new bigram, "no-stop-words":
bigram_counts_fellowship <- fellowship_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)


library(igraph)
bigram_graph_fellowship <- bigram_counts_fellowship %>%
  filter(n>10) %>%
  graph_from_data_frame()

bigram_graph_fellowship


library(ggraph)
ggraph(bigram_graph_fellowship, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)






#####################################################################################################
################################## TWO TOWERS  ###################################################### 

my_data2 <- read_document(file=nm[2]) # creating a vector
my_data_together2 <- paste(my_data2, collapse = " ") # Creating a concatenated vector
mydf2 <- data_frame(line=1, text=my_data_together2)

tower_df <- mydf2%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)


tower_hist <- mydf2%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)%>%
  filter(n > 150)%>%
  mutate(word = fct_reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()


tower_hist

# SENTIMENT BAR PLOT

tower_sent <- mydf2%>%
  unnest_tokens(word, text)%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

tower_sent

tower_sent %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# SENTIMENT CLOUD PLOT

library(reshape2)
library(wordcloud)

#### NRC
tower_df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%  ########## ONLY USE ONE N
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.5, 0.5),
                   fix.asp=TRUE, 
                   title.size = 2,
                   rot.per=0.25)


#### BING
tower_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%  ########## ONLY USE ONE N
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.8, 0.8),
                   fix.asp=TRUE, 
                   title.size = 1,
                   rot.per=0.25)

#### AFINN
tower_sent %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%  ########## ONLY USE ONE N
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.8, 0.8),
                   fix.asp=TRUE, 
                   title.size = 1,
                   rot.per=0.25)

#### STANDARD CLOUD
tower_df %>%
  with(wordcloud(word, n, max.words = 100))

tower_sent %>%
  with(wordcloud(word, n, max.words = 100))


############## Bigramas and negated words ###############

tower_bigrams <- mydf2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

tower_bigrams #We want to see the bigrams (words that appear together, "pairs")

# Using separate function to remove the stop words
tower_separated <- tower_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

negation_tokens <- c('no', 'never', 'without')

negated_tower <- tower_separated %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%  ###### SCORE???????????????
  ungroup()

negated_tower

#### Plot of the negations words

library(ggplot2)

negated_tower_plot <- function(x){
  negated_tower %>%
    filter(word1 == x) %>%
    mutate(contribution = n* value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by" , x))+   #######, X 
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}

negated_tower_plot(x="no") #this is your first negation word
negated_tower_plot(x="never") #this is your second negation word
negated_tower_plot(x="without") #this is your third negation word

######## SPIDER PLOT

#creating a new bigram, "no-stop-words":
bigram_counts_tower <- tower_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)


library(igraph)
bigram_graph_tower <- bigram_counts_tower %>%
  filter(n>10) %>%
  graph_from_data_frame()

bigram_graph_tower


library(ggraph)
ggraph(bigram_graph_tower, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)






#####################################################################################################
#################################### THE RETURN OF THE KING #########################################

my_data3 <- read_document(file=nm[3]) # creating a vector
my_data_together3 <- paste(my_data3, collapse = " ") # Creating a concatenated vector
mydf3 <- data_frame(line=1, text=my_data_together3)

king_df <- mydf3%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)


king_hist <- mydf3%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)%>%
  filter(n > 150)%>%
  mutate(word = fct_reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()


king_hist

# SENTIMENT BAR PLOT

king_sent <- mydf3%>%
  unnest_tokens(word, text)%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

king_sent

king_sent %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# SENTIMENT CLOUD PLOT

library(reshape2)
library(wordcloud)

#### NRC
king_df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%  ########## ONLY USE ONE N
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.5, 0.5),
                   fix.asp=TRUE, 
                   title.size = 2,
                   rot.per=0.25)


#### BING
king_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%  ########## ONLY USE ONE N
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.8, 0.8),
                   fix.asp=TRUE, 
                   title.size = 1,
                   rot.per=0.25)

#### AFINN
king_sent %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%  ########## ONLY USE ONE N
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.8, 0.8),
                   fix.asp=TRUE, 
                   title.size = 1,
                   rot.per=0.25)


#### STANDARD CLOUD
king_df %>%
  with(wordcloud(word, n, max.words = 100))

king_sent %>%
  with(wordcloud(word, n, max.words = 100))



############## Bigramas and negated words ###############

king_bigrams <- mydf3 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)


king_bigrams #We want to see the bigrams (words that appear together, "pairs")

# Using separate function to remove the stop words
king_separated <- king_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

negation_tokens <- c('no', 'never', 'without')

negated_king <- king_separated %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%  ###### SCORE???????????????
  ungroup()

negated_king


#### Plot of the negations words

library(ggplot2)

negated_king_plot <- function(x){
  negated_king %>%
    filter(word1 == x) %>%
    mutate(contribution = n* value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by" , x))+   #######, X 
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}

negated_king_plot(x="no") #this is your first negation word
negated_king_plot(x="never") #this is your second negation word
negated_king_plot(x="without") #this is your third negation word

######## SPIDER PLOT

#creating a new bigram, "no-stop-words":
bigram_counts_king <- king_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)


library(igraph)
bigram_graph_king <- bigram_counts_king %>%
  filter(n>10) %>%
  graph_from_data_frame()

bigram_graph_king


library(ggraph)
ggraph(bigram_graph_king, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)






##################################### ALL BOOKS ###################################################

all_books<-bind_rows(
  mutate(fellowship_df, novel='The Fellowship of the Ring'),
  mutate(tower_df,      novel='The Two Towers'),
  mutate(king_df,       novel='The Return of the King')
)


all_books

total_words_2 <- all_books %>%
  group_by(novel) %>%
  summarize(total=sum(n))


all_books_words <- left_join(all_books, total_words_2)


##### RANK FREQUENCY

rank_frequency <- all_books_words %>%
  group_by(novel) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)


rank_frequency

rank_frequency %>%
  ggplot(aes(rank, `term frequency`, color=novel))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()


all_books_words <- all_books_words %>%
  bind_tf_idf(word, novel, n) #### BOOK, WORD AND N IS WHAT YOU NEED TO BIND 

all_books_words 

all_books_words %>%
  arrange(desc(tf_idf))

#############
# looking at the graphical approach of the 3 books:
all_books_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(novel) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=novel))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~novel, ncol=2, scales="free")+
  coord_flip()


################################## CORRELOGRAMS  #######################################################

fellowship_2 <- mydf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

tower_2 <- mydf2%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

king_2 <- mydf3%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

frequency_all_books<-bind_rows(mutate(fellowship_2, novel='The Fellowship of the Ring'),
                               mutate(tower_2,      novel='The Two Towers'),
                               mutate(king_2,       novel='The Return of the King')
                               )%>%
                    mutate(word=str_extract(word, "[a-z']+")) %>%
                    count(novel, word) %>%
                    group_by(novel) %>%
                    mutate(proportion = n/sum(n))%>% #create new variable of % frequency
                    select(-n) %>%
                    spread(novel, proportion) %>%
                    gather(novel, proportion, `The Two Towers`, `The Fellowship of the Ring`)


# Correlograms vs The return of the King
ggplot(frequency_all_books, aes(x=proportion, y=`The Return of the King`, 
                       color = abs(`The Return of the King`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10()+
  scale_y_log10()+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~novel, ncol=2)+
  theme(legend.position = "none")+
  labs(y= 'The Return of the King', x=NULL)


########### Cort Test

cor.test(data=frequency_all_books[frequency_all_books$novel == "The Two Towers",],
         ~proportion + `The Return of the King`)

cor.test(data=frequency_all_books[frequency_all_books$novel == "The Fellowship of the Ring",],
         ~proportion + `The Return of the King`)


