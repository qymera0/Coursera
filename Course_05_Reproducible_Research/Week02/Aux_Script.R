activity %>%
  filter(is.na(steps)) %>%
  ggplot(aes(date)) +
  geom_histogram(fill = "dodgerblue4") +
  labs(x = "Date", 
       y = "",
       title = "Counts of NA's for Step Variable") +
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "antiquewhite4"),
        axis.title = element_text(colour = "antiquewhite4"),
        axis.text = element_text(colour = "antiquewhite4"),
        plot.title = element_text(colour = "antiquewhite4"),
        plot.subtitle = element_text(colour = "antiquewhite4"),
        strip.text = element_text(colour = "antiquewhite4"))
   
stepDay <- activity %>%
  group_by(date) %>%
  summarise(total = sum(steps, na.rm = TRUE))


ggplot(stepDay, aes(total)) +
  geom_histogram(fill = "dodgerblue4") +
  geom_vline(aes(xintercept = mean(total), col = "darkred"), size = 1, linetype = 4 ) +
  geom_vline(aes(xintercept = median(total), col = "green"), size = 1, linetype = 4 ) +
  scale_color_identity(guide = "legend",
                       name = "Stats",
                       labels = c("Mean", "Median")) +
  labs(x = "Steps", 
       y = "",
       title = "Counts of Steps",
       subtitle = "Daily Basis") +
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "antiquewhite4"),
        axis.title = element_text(colour = "antiquewhite4"),
        axis.text = element_text(colour = "antiquewhite4"),
        plot.title = element_text(colour = "antiquewhite4"),
        plot.subtitle = element_text(colour = "antiquewhite4"),
        strip.text = element_text(colour = "antiquewhite4"))


stepInterval <- activity %>%
  group_by(interval) %>%
  summarise(mean = mean(steps, na.rm = TRUE))

ggplot(stepInterval, aes(x = interval, y = mean)) +
  geom_line(colour = "dodgerblue4", size = 1) +
  labs(x = "5 minute interval", 
       y = "",
       title = "Average of Steps") +
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "antiquewhite4"),
        axis.title = element_text(colour = "antiquewhite4"),
        axis.text = element_text(colour = "antiquewhite4"),
        plot.title = element_text(colour = "antiquewhite4"),
        plot.subtitle = element_text(colour = "antiquewhite4"),
        strip.text = element_text(colour = "antiquewhite4")) +
  geom_point(data = stepInterval[which.max(stepInterval$mean), ], color = "darkred", size = 3) +
  geom_text(data = stepInterval[which.max(stepInterval$mean), ], color = "darkred", size = 4,
            label = paste(round(stepInterval$mean[which.max(stepInterval$mean)]), "steps at", stepInterval$interval[which.max(stepInterval$mean)], "min", sep = " "),
            hjust = -0.1)

stepMedian <- activity %>%
  group_by(interval) %>%
  summarise(median = median(steps, na.rm = TRUE))

stepImp <- activity %>%
  left_join(stepMedian) 

stepImp$steps[is.na(stepImp$steps)] <- stepImp$median[is.na(stepImp$steps)]

stepImp$wday <- weekdays(stepImp$date)

stepImp$wknd <- as.factor(ifelse(stepImp$wday == "sábado" |
                                   stepImp$wday == "domingo", "Weekend", "Weekday"))

stepWeekend <- stepImp %>%
  group_by(interval, wknd) %>%
  summarise(mean = mean(steps, na.rm = TRUE))

stepMedian <- stepWeekend %>%
  group_by(wknd) %>%
  summarise(median = median(mean))

stepMean <- stepWeekend %>%
  group_by(wknd) %>%
  summarise(mean = mean(mean))

ggplot(stepWeekend, aes(x = interval, y = mean)) + 
  facet_grid(. ~ wknd) +
  geom_line(color =  "dodgerblue4", size = 1) +
  geom_hline(data = stepMedian, aes(yintercept = median, color = "orange"), size = 1.5) +
  geom_hline(data = stepMean, aes(yintercept = mean, color = "grey"), size = 1.5) +
  scale_color_identity(guide = "legend",
                       name = "Steps stats",
                       labels = c("Mean", "Median")) +
  labs(title = "Comparison of activity level between weekdays and weekends",
       subtitle = "Mean of steps",
       y = "",
       x = "5 minute interval") +
  theme_minimal() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "antiquewhite4"),
        axis.title = element_text(colour = "antiquewhite4"),
        axis.text = element_text(colour = "antiquewhite4"),
        plot.title = element_text(colour = "antiquewhite4"),
        plot.subtitle = element_text(colour = "antiquewhite4"),
        strip.text = element_text(colour = "antiquewhite4"),
        legend.text = element_text(colour = "antiquewhite4"),
        legend.title = element_text(colour = "antiquewhite4")
  )

  
