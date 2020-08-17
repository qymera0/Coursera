# Get words function

# I love
# Shall we go to
# Cinco de

str <- "Cinco de"

n <- 5

tokens <-
        str %>%
        as_tibble() %>%
        bind_rows() %>%
        unnest_tokens(word, value) %>%
        mutate(word = wordStem(word, language = "english")) %>%
        arrange(-row_number()) %>%
        filter(row_number() <= 2) %>%
        arrange(-row_number())

tri_fun(tokens[1,1], tokens[2,1], n)

# uni_fun

uni_fun <- function(size){
        
        # The function return "size" words, weighted by the probability of 
        # happening.
        
        return(unname(stemCompletion(sample(unigram$word, 
                                            size = size,
                                            prob = unigram$uniProb), 
                                     dictionary = origWord$word, 
                                     type = "prevalent")))
              
        
}

# bi_fun

bi_fun <- function(w2, n){
        
        pwords <-
                biKn %>%
                filter(word1 == as.character(tokens[2,1]))
        
        if(dim(pwords)[1] == 0){
                
                # Return a word weight by its probability from uni-grams
                
                return(uni_fun(n))
                
        } 
        
        if(nrow(pwords) > n){
                
                # Return only words from bi-gram
                
                p <-
                        pwords %>% 
                        arrange(-biProb) %>%
                        select(word2) %>%               
                        filter(row_number() <= n)
                
                return(unname(stemCompletion(p$word2, 
                                             dictionary = origWord$word, 
                                             type = "prevalent")))
                
        }
        
        # If the number of words is smaller than n, the function return all 
        # from bi-gram than completes from uni-gram data.
        
        count <- nrow(pwords)
        
        unWords <- uni_fun((n - count))
        
        return(unname(stemCompletion(c(pwords$word2, unWords), 
                                     dictionary = origWord$word, 
                                     type = "prevalent")))
        
}

# tri_fun

tri_fun <- function(w1, w2, n = 5){
        
        require(dplyr)
        
        pwords <-
                triKn %>%
                filter(word1 == as.character(w1)) %>%
                filter(word2 == as.character(w2))
        
        if (dim(pwords)[1] == 0){
                
                # backoff to bi-grams prediction
                
                return(bi_fun(w2, n))
                #return(print("bi_fun"))
        }
        
        if (nrow(pwords) > n){
                
                # Return only words from tri-gram
                
                p <- 
                        pwords %>% 
                        arrange(-triProb) %>%
                        select(word3) %>%
                        filter(row_number() <= n)
                
                return(unname(stemCompletion(p$word3, 
                                             dictionary = origWord$word, 
                                             type = "prevalent")))
                                
        }
        
        count <- nrow(pwords)
        
        bwords <- bi_fun(w2, (n - count))
        
        return(unname(stemCompletion(c(p$word3, bwords), 
                                     dictionary = origWord$word, 
                                     type = "prevalent")))
        
}

c <- stemCompletion(paste(p$word3), dictionary = origWord$word, type = "prevalent")
