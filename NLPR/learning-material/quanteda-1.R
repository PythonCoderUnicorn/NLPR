
# install.packages("quanteda")
library(quanteda)
# install.packages("quanteda.textmodels") 
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
# devtools::install_github("quanteda/quanteda.corpora")
library(quanteda.corpora)
# devtools::install_github("kbenoit/quanteda.dictionaries")
# install.packages("readtext")
library(readtext)

# readtext supports plain text files (.txt), JSON, csv, .tab, .tsv, .xml, pdf, .doc, .docx, etc


# load in a corpus from Quanteda pkg and save in variable
corpus = corpus(data_char_ukimmig2010)

# get a summary of corpus
summary(corpus)

# for document variables (docvars) 
# think of as "columns of text data"
docvars(corpus, "Party") = names(corpus)
summary(corpus)

# use docvars to add year to corpus
docvars(corpus, "Year") = 2010
summary(corpus)


# grab a Quanteda corpus example
as.character(data_corpus_inaugural)[2]

# get the summary
summary(data_corpus_inaugural, n= 3) # n = rows to show

# save the summary data as dataframe
txt_summary_df = summary(data_corpus_inaugural)

head(txt_summary_df)


# plot the dataframe with tokens by year
library(ggplot2)
library(tidyverse)

ggplot(
  data = txt_summary_df,
  aes(
    x= Year,
    y= Tokens,
    col= Tokens 
  )
)+
  geom_line()+
  geom_point()+
  scale_x_continuous( labels = c( seq(from= 1789, to= 2017, by= 12)),
                      breaks = seq(from= 1789, to= 2017, by= 12))+
  ggdark::dark_mode()


# get data on the 3 longest speeches, (largest tokens)
txt_summary_df %>% 
  filter(Tokens > 5e3)



# slice a corpus , then add it to another sliced corpus
corpus1 = corpus(data_corpus_inaugural)[1:5]
corpus2 = corpus(data_corpus_inaugural)[10:30]
corpus3 = corpus1 + corpus2
summary(corpus3)


# subset a corpus object 
speeches1990s =  corpus_subset(data_corpus_inaugural , Year > 1990)
summary(speeches1990s)


obama_speeches = corpus_subset(data_corpus_inaugural, President =="Obama")
summary(obama_speeches)








#  kwic (keyword in context) to search a corpus
speech_tokens =  tokens(data_corpus_inaugural)
kwic(speech_tokens, pattern = "love")

# add valuetype='regex' returns regular expressions of stem 'love'
kwic(speech_tokens, pattern = "love", valuetype = 'regex')


# phrase() for multi word expressions
kwic(
  speech_tokens,
  pattern =  phrase("soviet union")
) %>% 
  head()



head(docvars(data_corpus_inaugural))



# tokenize
inaug_tokens = tokens(data_corpus_inaugural)

# multi word expressions
# Functions for tokens objects take a character vector, a dictionary or collocations as pattern.
multiword = c('science','reason')

head( kwic(inaug_tokens, pattern = phrase(multiword)))







s

#--------- feature extracting from a corpus
# quanteda::dfm() for document feature matrix


# step 1 --- tokens
txt1 = "You made my whole day, love. Thank you for spending some time with me today and thank you for my beautiful gift. Some gifts are $400 or more but some are priceless, go to www.website.com"

# tokens()
tokens(txt1)

# further tokenization with punctuation
tokens(txt1, 
       remove_punct = F,
       remove_symbols = T,
       remove_url = T,
       remove_numbers = F,
       remove_separators = F
       )


# concat multi-word expressions 
tokens("Natural Language Processing or NLP as the hip kids say in the US") %>% 
  tokens_compound(pattern = phrase(c("NLP","US")))

# can skip step 1
# step 2 = dfm()  does tokenization, summarizes features, tolower()
speeches1990s_tokens = tokens(speeches1990s)
speech_tokens_dfm = speeches1990s_tokens %>% dfm()
speech_tokens_dfm

# english stopwords 
head( stopwords('en'), 10)


uk_2010_dfm = tokens(data_char_ukimmig2010, remove_punct = T) %>% 
  tokens_remove(stopwords('en')) %>% 
  dfm()

uk_2010_dfm %>% view()


# a list of most frequently occurring features
topfeatures(uk_2010_dfm, 10)  # top 10

# word cloud
set.seed(100)
library(quanteda.textplots)

textplot_wordcloud(uk_2010_dfm,
                   min_count = 6,
                   random_order = F,
                   random_color = T,
                   rotation = 0.25,
                   color = RColorBrewer::brewer.pal(8,'Reds'),
                   
                   )


# grouping documents by document var

inaugural_dfm = tail(data_corpus_inaugural, 20) %>% 
  tokens(remove_punct = T) %>% 
  tokens_remove(stopwords('en')) %>% 
  dfm() %>% 
  dfm_group(groups = Party)

inaugural_dfm

dfm_sort(inaugural_dfm )


# grouping words by dictionary or equivalent class
# example: 'terrorism' and 'economy' related words vary based on President
inaug_speech_1991 = corpus_subset(data_corpus_inaugural, Year > 1991)

# define a demo dictionary
dict_list = list(terror= c('terrorism','terrorists','threat','attack'), economy= c('jobs','business','grow','work'))

special_dictionary = dictionary( dict_list)

# use dictionary when using dfm
inaug_speech_1991_dict = tokens(inaug_speech_1991) %>% 
  tokens_lookup(dictionary = special_dictionary) %>% 
  dfm()

inaug_speech_1991_dict

# L = list(terror = c('one','two','three'), unicorns= c('magic','fantasy','awesome'))
# L$terror[1]
# dictionary(L)





# similarities between texts 
library(quanteda.textstats)

inaug_speech_1980_dfm = corpus_subset(data_corpus_inaugural, Year > 1980) %>% 
  tokens(remove_punct = T) %>% 
  tokens_wordstem(language = 'en') %>% 
  tokens_remove(stopwords('en')) %>% 
  dfm()

inaug_speech_1980_dfm
obama_speeches = inaug_speech_1980_dfm[ c('2009-Obama','2013-Obama'), ]

# find similar text stats 
obama_textstat = textstat_simil(inaug_speech_1980_dfm, 
               obama_speeches, 
               margin = 'documents',
               method = 'cosine'
               )

obama_textstat_list = as.list(obama_textstat)

dotchart(obama_textstat_list$`2013-Obama`, xlab = 'Cosine similarity', pch = 19)






# clustering Presidents
sotu_dfm = corpus_subset(data_corpus_sotu, Date > as.Date('1980-01-01')) %>% 
  tokens(remove_punct = T) %>% 
  tokens_wordstem(language = 'en') %>% 
  tokens_remove(stopwords('en')) %>% 
  dfm()

# trim the dfm
sotu_dfm_trim = dfm_trim(sotu_dfm, min_termfreq = 5, min_docfreq = 3)

# hierarchical clustering - get distances on normalized dfm
sotu_textstat_dist = textstat_dist( dfm_weight(sotu_dfm_trim, scheme = 'prop'))

# hierarchical clustering the distance object
president_cluster = hclust( as.dist( sotu_textstat_dist))

# label with document names
president_cluster$labels = docnames(sotu_dfm_trim)

# plot a dendrogram
plot( president_cluster, xlab = "", sub = "", main = 'Euclidian distance on normalized token frequency')


# term similarities
sotu_textstat = textstat_simil(sotu_dfm_trim, 
               sotu_dfm_trim[ , c('fair','health','terror')],
               method = 'cosine',
               margin = 'features'
               )

lapply( as.list(sotu_textstat), head, 10)
















# topic models

irish_budget = tokens(data_corpus_irishbudget2010,
       remove_punct = T,
       remove_numbers = T
       ) %>% 
  tokens_remove(stopwords('en')) %>% 
  dfm()

irish_budget_trim = dfm_trim(irish_budget, min_termfreq = 4, max_docfreq = 10)
irish_budget_trim


# fit topic model
set.seed(100)

# structured topic models (stm)
library(stm)
irish_stm_20 = stm(irish_budget_trim, K= 20, verbose=F)
plot(irish_stm_20)




















