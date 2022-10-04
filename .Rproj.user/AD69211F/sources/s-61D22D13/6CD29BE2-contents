
library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(sysfonts)
font_add_google("Noto Sans TC", "Noto Sans TC")
library(showtext)
showtext_auto()
####################################################################################
## Basic Text Analysis                                                            ##
####################################################################################

# Loading the documents
df_suncake <-  read.csv("suncake.csv")

corpus_suncake <- corpus(df_suncake, text_field = "Message")

# The followings are not necessary steps, but it is always a good idea to view a portion of your data
corpus_suncake[1:10] # print the first 10 documents
ndoc(corpus_suncake) # Number of Documents
nchar(corpus_suncake[1:10]) # Number of character for the first 10 documents
ntoken(corpus_suncake[1:10]) # Number of tokens for the first 10 documents
ntoken(corpus_suncake[1:10], remove_punct = TRUE) # Number of tokens for the first 10 documents after removing punctuation



# Meta-data
head(docvars(corpus_suncake))

library(jiebaR)
# Vanila version
tokeniser <- worker()
# Using jieba with quanteda needs a bit of hacking
raw_texts <- as.character(corpus_suncake) # getting text from the corpus
tokenised_texts <- purrr::map(gsub("\\.","",raw_texts), segment, tokeniser) # applying jieba::segment() function across rows
tokens_suncake <- tokens(tokenised_texts, # Changing it back to a tokens object
                     remove_punct = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     remove_symbols = TRUE,
                     verbose = TRUE)

dfm_suncake <- dfm(tokens_suncake)
dfm_suncake <- dfm_remove(dfm_suncake, c(stopwords('zh', source = "misc"), stopwords('english'), customstopwords))

# Inspecting the results
topfeatures(dfm_suncake, 30)

# Wordcloud
textplot_wordcloud(dfm_suncake)

###
df_shortbread <-  read.csv("shortbread.csv")
corpus_shortbread <- corpus(df_shortbread, text_field = "Message")

raw_texts <- as.character(corpus_shortbread) # getting text from the corpus
tokenised_texts <- purrr::map(gsub("\\.","",raw_texts), segment, tokeniser) # applying jieba::segment() function across rows
tokens_shortbread <- tokens(tokenised_texts, # Changing it back to a tokens object
                         remove_punct = TRUE,
                         remove_numbers = TRUE,
                         remove_url = TRUE,
                         remove_symbols = TRUE,
                         verbose = TRUE)
dfm_shortbread <- dfm(tokens_shortbread)
dfm_shortbread <- dfm_remove(dfm_shortbread, c(stopwords(language = "zh", source = "misc"), customstopwords))

topfeatures(dfm_shortbread)

####################################################################################
## Keyword Analysis                                                               ##
####################################################################################

kwds <- textstat_keyness(rbind(dfm_suncake, dfm_shortbread), target = seq_along(tokens_suncake))

head(kwds)
tail(kwds, 20)

textplot_keyness(kwds, n = 20)

kwic(corpus_shortbread,"")
