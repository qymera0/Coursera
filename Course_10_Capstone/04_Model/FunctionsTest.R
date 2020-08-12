# Get words function

str <- "Shall we go to"

n <- 5

tokens <-
        str %>%
        as.tibble() %>%
        bind_rows() %>%
        unnest_tokens(word, value) %>%
        mutate(word = wordStem(word, language = "english")) %>%
        arrange(-row_number()) %>%
        filter(row_number() <= 2) %>%
        arrange(-row_number())



# uni_fun

uni_fun <- function(size = 5){
        
        # The function return "size" words, weighted by the probability of 
        # happening.
        
        return(sample(unigram$word, size = size, prob = unigram$uniProb))
        
}

# bi_fun

pwords <-
        biKn %>%
        filter(word1 == as.character(tokens[2,1]))

if(dim(pwords)[1] == 0){
        
        # Return a word weight by its probability
        
        return(uni_fun())
        
}else {
        
        (pwords %>% arrange(-biProb) %>% filter(row_number() == 1))$word2
        
}

count <- nrow(pwords)

unWords <- uni_fun()[1:n-count]

return(c(pwords$word2, unWords))

# tri_fun

pwords <-
        triKn %>%
        filter(word1 == as.character(tokens[1,1])) %>%
        filter(word2 == as.character(tokens[2,1]))

if(dim(pwords)[1] == 0){
        
        print("bi_fun")

}else{
        
        (pwords %>% arrange(-triProb) %>% filter(row_number() == 1))$word3
}

count <- nrow(pwords)

bwords <- 
        


