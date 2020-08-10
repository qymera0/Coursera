
t <- str_extract(rawDataClean$blog, regex("\\b(\\S+)(?:\\s+\\1\\b)+", TRUE))

plan(multiprocess)

rawDataClean <-
        rawData %>%
        future_map(function(x) rle(x)$value)
future_map(stri_trans_general, id = "Latin-ASCII")


as_tibble() %>%
        gather(key = "source") %>%
        mutate(source = as.factor(source))


map(replace_contraction) %>%
        map(replace_emoji) %>%
        map(replace_hash) %>%
        map(replace_internet_slang) %>%
        map(replace_url) %>%
        map(replace_non_ascii) %>%
        map(replace_incomplete) %>%