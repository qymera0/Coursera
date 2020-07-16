
t <- str_extract(rawDataClean$blog, regex("\\b(\\S+)(?:\\s+\\1\\b)+", TRUE))

plan(multiprocess)

rawDataClean <-
        rawData %>%
        future_map(function(x) rle(x)$value)
future_map(stri_trans_general, id = "Latin-ASCII")


as_tibble() %>%
        gather(key = "source") %>%
        mutate(source = as.factor(source))