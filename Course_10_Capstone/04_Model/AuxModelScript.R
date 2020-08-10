library(tidytext)
library(tidytext)
library(quanteda)
library(data.table)
library(purrr)

# Sample text

plan(multiprocess)

sample <-
        rawData %>%
        map(function (z, size = 0.01) {
            set.seed(123456)
            sample(z, size = (size*length(z)))})
        
        
master_vector <- c(sample$blog, sample$news, sample$twitter)
   
# Corpus creation

corp <- corpus(master_vector)

# Quick clean

master_Tokens <- tokens(
        x = tolower(corp),
        remove_punct = TRUE,
        remove_numbers = TRUE,
        split_hyphens = TRUE,
        remove_symbols = TRUE,
        remove_url = TRUE
)

# stem

stemed_words <- tokens_wordstem(master_Tokens, language = "english")

# Tokenisation

bi_gram <- tokens_ngrams(stemed_words, n = 2)

tri_gram <- tokens_ngrams(stemed_words, n = 3)

uni_DFM <- dfm(stemed_words)

bi_DFM <- dfm(bi_gram)

tri_DFM <- dfm(tri_gram)

# Trim

uni_DFM <- dfm_trim(uni_DFM, 3)

bi_DFM <- dfm_trim(bi_DFM, 3)

tri_DFM <- dfm_trim(tri_DFM, 3)

# Group and count 

# Create named vectors with counts of words 

sums_U <- colSums(uni_DFM)

sums_B <- colSums(bi_DFM)

sums_T <- colSums(tri_DFM)

# Create data tables with individual words as columns

uni_words <- data.table(word_1 = names(sums_U), count = sums_U)

bi_words <- data.table(
        word_1 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 2),
        count = sums_B)

tri_words <- data.table(
        word_1 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 2),
        word_3 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 3),
        count = sums_T)

# index

setkey(uni_words, word_1)

setkey(bi_words, word_1, word_2)

setkey(tri_words, word_1, word_2, word_3)

# Kneser Key

discount_value <- 0.75

# Finding number of bi-gram words

numOfBiGrams <- nrow(bi_words[by = .(word_1, word_2)])

# Finding probability for a word given the number of times it was second word of a bigram)

ckn2 <- bi_words[, .(Prob = ((.N) / numOfBiGrams)), by = word_2]

setkey(ckn2, word_2)

# Assigning the probabilities as second word of bigram, to unigrams

uni_words[, Prob := ckn2[word_1, Prob]]

uni_words <- uni_words[!is.na(uni_words$Prob)]

# Finding number of times word 1 occurred as word 1 of bi-grams

n1wi2 <- bi_words[, .(N = .N), by = word_1]



setkey(n1wi2, word_1)

# Assigning total times word 1 occured to bigram cn1

bi_words[, Cn1 := uni_words[word_1, count]]

# Kneser-Ney Algorithm

bi_words[, Prob := ((count - discount_value) / Cn1 + discount_value / Cn1 * n1wi[word_1, N] * uni_words[word_2, Prob])]

######## Finding Tri-Gram Probability #################

# Finding count of word1-word2 combination in bigram 

tri_words[, Cn2 := bi_words[.(word_1, word_2), count]]

# Finding count of word1-word2 combination in trigram

n1w12 <- tri_words[, .N, by = .(word_1, word_2)]

setkey(n1w12, word_1, word_2)

# Kneser Kney Algorithm

tri_words[, Prob := (count - discount_value) / Cn2 + discount_value / Cn2 * n1w12[.(word_1, word_2), N] *
                  bi_words[.(word_1, word_2), Prob]]

######## End of Finding Tri-Gram Probability #################