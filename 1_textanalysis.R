####################################################################################
## Setting Up                                                                     ##
####################################################################################

install.packages("tidyverse")
install.packages("quanteda")
install.packages("quanteda.textplots")
install.packages("quanteda.textstats")
install.packages("sysfonts")
install.packages("showtext")
install.packages("jiebaR")

library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)

####################################################################################
## Basic Text Analysis                                                            ##
####################################################################################

# Dataset: Tweets from https://twitter.com/Ukraine

# Loading the documents
df_ukraine <-  read.csv("ukraine.csv")
corpus_ukraine <- corpus(df_ukraine, text_field = "text")

# The followings are not necessary steps, but it is always a good idea to view a portion of your data
corpus_ukraine[1:10] # print the first 10 documents
ndoc(corpus_ukraine) # Number of Documents
nchar(corpus_ukraine[1:10]) # Number of character for the first 10 documents
ntoken(corpus_ukraine[1:10]) # Number of tokens for the first 10 documents
ntoken(corpus_ukraine[1:10], remove_punct = TRUE) # Number of tokens for the first 10 documents after removing punctuation

# Meta-data
head(docvars(corpus_ukraine))

# Creating DFM
tokens_ukraine <- tokens(corpus_ukraine,
                     remove_punct = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     verbose = TRUE)
dfm_ukraine <- dfm(tokens_ukraine)

# Inspecting the results
topfeatures(dfm_ukraine, 30)

# What do they say about "russian"?
kwic(tokens_ukraine, "russian", 3)

# Plotting a histogram
features <- topfeatures(dfm_ukraine, 100)  # Putting the top 100 words into a new object
data.frame(list(term = names(features), frequency = unname(features))) %>% # Create a data.frame for ggplot
  ggplot(aes(x = reorder(term,-frequency), y = frequency)) + # Plotting with ggplot2
  geom_point() +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Doing it again, removing stop words this time!

# Defining custom stopwords
customstopwords <- c("amp", "just", "make", "stopword")

dfm_ukraine <- dfm_remove(dfm_ukraine, c(stopwords('english'), customstopwords))

# Inspecting the results again
topfeatures(dfm_ukraine, 30)

# Top words again
features <- topfeatures(dfm_ukraine, 100)  # Putting the top 100 words into a new object
data.frame(list(term = names(features), frequency = unname(features))) %>% # Create a data.frame for ggplot
  ggplot(aes(x = reorder(term,-frequency), y = frequency)) + # Plotting with ggplot2
  geom_point() +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Wordcloud
textplot_wordcloud(dfm_ukraine)


####################################################################################
## Keyword Analysis                                                               ##
####################################################################################

# Before and After the Invasion
tstat_key <- textstat_keyness(dfm_ukraine,
                              target = docvars(dfm_ukraine, "war"))

head(tstat_key)
tail(tstat_key)

textplot_keyness(tstat_key)

####################################################################################
## Chinese Text Analysis                                                          ##
####################################################################################

# Doing text analysis can be tricky, but doing Chinese text analysis can be even trickier
# There are a few extra things we need to consider

######################### Showing Chinese Figures #########################
df_suncake <-  read.csv("suncake.csv")
corpus_suncake <- corpus(df_suncake, text_field = "Message")

# Creating DFM
tokens_suncake <- tokens(corpus_suncake,
                     remove_punct = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE,
                     remove_symbols = TRUE,
                     verbose = TRUE)
dfm_suncake <- dfm(tokens_suncake)


features <- topfeatures(dfm_suncake, 100)  # Putting the top 100 words into a new object
data.frame(list(term = names(features), frequency = unname(features))) %>% # Create a data.frame for ggplot
  ggplot(aes(x = reorder(term,-frequency), y = frequency)) + # Plotting with ggplot2
  geom_point() +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

customstopwords <- c("wwwfacebookcom", "https", "月", "日","️")
dfm_suncake <- dfm_remove(dfm_suncake, c(stopwords('zh', source = "misc"), stopwords('english'), customstopwords))

# Inspecting the results again
topfeatures(dfm_suncake, 30)

textplot_wordcloud(dfm_suncake)

# You would notice Chinese characters are not displayed correctly in the previous graph.
# This is because the default font in R doesn't support Chinese
# To fix it, we simply need to change the font
# Changing font in R can be tricky, but luckily these packages make it easier
library(sysfonts)
font_add_google("Noto Sans TC", "Noto Sans TC")
library(showtext)
showtext_auto()

textplot_wordcloud(dfm_suncake)


######################### Tokenisation #########################

# Tokenisation is the process of splitting a string into tokens (in most cases, one token is one word)
# In Latin languages, tokenisation is much easier since splitting the sentence by the space will do the trick
# Since there is no space

text <- "one two 福利 one two 福利 happy everyday one two 福利 one two 福利
福利熊 熊福利 熊福利 福利熊"

# Using the default quanteda tokens() function
print(tokens(text), max_ntoken = 100)

# It works, but not perfectly
# There is another way to do it: Jieba
# Jieba is one of the most widely used packages for tokenisation
# One advantage of using Jieba is that you can add your own words into the dictionary

library(jiebaR)

# Vanila version
tokeniser <- worker()
segment(text, tokeniser)

# Adding user defined words
new_user_word(tokeniser, c("福利熊","熊福利"))
segment(text, tokeniser)

edit_dict()

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

topfeatures(dfm_suncake)
textplot_wordcloud(dfm_suncake)

