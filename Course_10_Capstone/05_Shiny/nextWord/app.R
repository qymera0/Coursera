library(shiny)
library(dplyr)
library(tidytext)
library(SnowballC)
library(tm)


# 1 - UI FOR APPLICATION THAT PREDICTS NEXT WORD --------------------------

ui <- fluidPage(

    # Application title
    titlePanel("Xt Next Word Predictor"),

    # Sidebar with a text input
    sidebarLayout(
        sidebarPanel(
            textInput("str",
                      "Type at least one word")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("word")
        )
    )
)



# 2 - SERVER TO PREDICTION APP --------------------------------------------

# load data

load("data/nextWordData.RData")

# Function to predict tri-grams

tri_fun <- function(w1, w2, n = 5){
    
    require(dplyr)
    require(tm)
    
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

# Function to predict bi-grams

bi_fun <- function(w2, n){
    
    pwords <-
        biKn %>%
        filter(word1 == as.character(w2))
    
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

# Function to predict uni-gram

uni_fun <- function(size){
    
    # The function return "size" words, weighted by the probability of 
    # happening.
    
    return(unname(stemCompletion(sample(unigram$word, 
                                        size = size,
                                        prob = unigram$uniProb), 
                                 dictionary = origWord$word, 
                                 type = "prevalent")))
    
}

# Function to bind all predictions

getWords <- function(str){
    
    require(tidytext)
    require(dplyr)
    require(SnowballC)
    
    tokens <-
        str %>%
        as_tibble() %>%
        bind_rows() %>%
        unnest_tokens(word, value) %>%
        mutate(word = wordStem(word, language = "english")) %>%
        arrange(-row_number()) %>%
        filter(row_number() <= 2) %>%
        arrange(-row_number())
    
    if(dim(tokens)[1] < 2){
        
        words <- bi_fun(tokens[1,1], 5)
        
        return(print(words))
        
    }else{
        
        words <- tri_fun(tokens[1,1], tokens[2,1], 5)
        
        return(paste(words, sep = ", "))
        
    }
    
}

# Final server function

server <- function(input, output) {

    output$word <- renderText({
        
        getWords(input$str)
        
    })
}

# 3 - RUN APPLICATION -----------------------------------------------------

shinyApp(ui = ui, server = server)
