Xt_Presentation
========================================================
author: Samuel Bozzi Baco
date: 2020, August 17th
autosize: true

Introduction
========================================================

This presentation is related to Capstone Project from Data Science course program from Johns Hopkins University.

It was asked for the students to create an application running at Shiny that, given an initial string, would predict next words.

Data sets from Blogs, News and Twitter were supplied to the model could be created.

Initial Study
========================================================

Initially, a quick exploratory data analysis was made with the object to stay in touch with the datasets. The detailed analysis can be found at https://rpubs.com/qymera0/649880

- Instead of package *tm*, the *tidy text* package and *tidyverse* philosophy was used.

- To clean the data, the package *text clean* was choose. To allow a quicker computation, *furr* package was used to allow parallel processing from different sources.

- To remove profane words, a large dictionary was built using different sources.

- As quadi-gram presented several repeated words in a pattern (that were not possible to be removed using *text clean*), it was ignored.

Dictionary Study
========================================================

After the quick analysis, a more profound analysis comparing the sources was done. It can ve found here: https://rpubs.com/qymera0/649913

- Much of the analysis were an application form fantastic book "Text mining with R" from Julia Silge and David Robinson.

- The frequency analysis show that News and Blog sources are very similar, been twitter the most different. At least until rank 300. 

- The analysis considered the Zipf´s Law, where the importance of a word must considers it frequency in all documents.

Prediction Model
========================================================

Details about how the prediction model was developed can be found at https://rpubs.com/qymera0/649929. Most of the logic was based on Thiloshon Nagarajah work, but instead of *data.table*, *dplyr* was used to create the functions.

- 10% of all data was used to maintain the app with a size smaller than 50 Mb.

- Words from all uni-grams, bi-grams and tri-grams were weighted using Kneser-Ney smoother 

- The final app can be found here: https://samuelbozzibaco.shinyapps.io/nextWord/
