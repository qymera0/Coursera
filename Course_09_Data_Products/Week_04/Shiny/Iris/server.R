library(shiny)

# Define server logic required to draw a rpart plot and ROC Curve

shinyServer(function(input, output) {

    library(rpart)
    library(rpart.plot)
    library(multiROC)
    library(dummies)
    library(ggplot2)
    
    mdl <- reactive({
        
        rpart(Species ~., data = iris, cp = -1, maxdepth = input$Splits)
    })
        
    output$rpartPlot <- renderPlot({

        # Plot the adjusted tree
        rpart.plot(mdl(),
                   type = 1,
                   extra = 100,
                   tweak = 1.2)
        
      })
    
    output$roc <- renderPlot({
        
        # Merge labels and predicted
        
        rpartPred <- data.frame(predict(mdl(), iris, type = 'prob'))
        
        colnames(rpartPred) <- paste(colnames(rpartPred), "_pred_rpart")
        
        roc_res <- multi_roc(rpartPred, force_diag = T)
        
        true_label <- data.frame(dummy(iris$Species, sep = "."))
        
        colnames(true_label) <- paste(gsub(".*?\\.", "", colnames(true_label)), "_true")
        
        final_df <- cbind(true_label, rpartPred)
        
        # Plot
        
        roc_res <- multi_roc(final_df, force_diag = T)
        
        plot_roc_df <- plot_roc_data(roc_res)
        
        ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
            geom_path(aes(color = Group, linetype = Method), size = 1.0) +
            geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
                         colour='grey', linetype = 'dotdash') +
            theme_bw() + 
            theme(plot.title = element_text(hjust = 0.5), 
                  legend.justification = c(1, 0), 
                  legend.position = c(.95, .05),
                  legend.title = element_blank(), 
                  legend.background = element_rect(fill = NULL, 
                                                   size = 0.5, 
                                                   linetype = "solid", 
                                                   colour ="black"))
        
    })

})
