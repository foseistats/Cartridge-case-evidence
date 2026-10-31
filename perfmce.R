
###TWO-STAGE
# Load necessary libraries
library(dplyr)
library(gridExtra)
library(ggplot2)
library(GGally
        )

# Assuming your data is in a dataframe called df
# df <- read.csv("your_data.csv")

# Placeholder for performance metrics
sensitivity_list1 <- c()
specificity_list1 <- c()
accuracy_list1 <- c()
plot_list1<-c()
cmc1<-c()
groundtruth1<-c()
barrel1<-c()

# Get unique barrels from both refbarrel and targbarrel columns
unique_barrels <- unique(c(cmcdata2$refBarrel, cmcdata2$targBarrel))
unique_barrels<-unique_barrels[order(unique_barrels)]

# Loop over each barrel for leave-one-barrel-out cross-validation
for (barrel in unique_barrels) {
  # Split the data into training and testing sets
  test_set <- cmcdata2 %>% filter(refBarrel == barrel | targBarrel == barrel)
  train_set <- cmcdata2 %>% filter(!(refBarrel == barrel | targBarrel == barrel))
  
  # Apply the classification rule on the test set
  test_set <- test_set %>%
    mutate(prediction = ifelse(cmc > 6, "match", "non-match"),
  barrel_info = as.factor(paste("Barrel", barrel, sep = "_")))

  
  cmcv<-test_set %>% select(cmc)
  cmc1<-c(cmc1,cmcv) #all cmc
  barrelv<-test_set %>% select(barrel_info)
  barrel1<-c(barrel1,barrelv)
  groundtruthv<- test_set %>% select(MatchorNot)
  groundtruth1<-c(groundtruth1,groundtruthv) #all groundtruth

  # Calculate confusion matrix components
  true_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "match")
  true_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "non-match")
  false_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "non-match")
  false_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "match")
  
  # Calculate performance metrics
  sensitivity <- true_positives / (true_positives + false_negatives)
  specificity <- true_negatives / (true_negatives + false_positives)
  accuracy <- (true_positives + true_negatives) / nrow(test_set)
  
  # Append metrics to the lists
  sensitivity_list1 <- c(sensitivity_list1, sensitivity)
  specificity_list1 <- c(specificity_list1, specificity)
  accuracy_list1 <- c(accuracy_list1, accuracy)
  
  plot <- ggplot(test_set, aes(x = MatchorNot, y = cmc)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    ggtitle(paste("Barrel:", barrel))+xlab("Ground Truth")+theme_bw()
  
  # Append plot to the plot list
  plot_list1[[length(plot_list1) + 1]] <- plot
  
}

cmc1<-unlist(cmc1)
groundtruth1<-unlist(groundtruth1)
barrel1<-unlist(barrel1)
allmn1<-data.frame(cmc1,groundtruth1,barrel1)
ggplot(data=allmn1,aes(x=groundtruth1,y=cmc1))+geom_point(aes(color=barrel1))+
  geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 0.5) +xlab("Ground Truth")+ylab("CMC")+
  theme_bw()

combined_plot1 <- do.call(grid.arrange, c(plot_list1, ncol = 2))

# Calculate mean performance metrics
mean_sensitivity1 <- mean(sensitivity_list1, na.rm = TRUE)
mean_specificity1 <- mean(specificity_list1, na.rm = TRUE)
mean_accuracy1 <- mean(accuracy_list1, na.rm = TRUE)
two_stagemdata<-data.frame(Method=rep("Two stage approach",3),
    Metric=c("sensitvity","specificity","accuracy"),
    value=c(mean_sensitivity1,mean_specificity1,mean_accuracy1))

# Print the results
cat("Mean Sensitivity: ", mean_sensitivity1, "\n")
cat("Mean Specificity: ", mean_specificity1, "\n")
cat("Mean Accuracy: ", mean_accuracy1, "\n")


#roc curv
#roc_data1 <- data.frame(
#  specificity = specificity_list1,
#  sensitivity = sensitivity_list1
#)


#ggplot(roc_data1, aes(x = 1 - specificity, y = sensitivity)) +
 # geom_line(color = "blue") +
# geom_abline(linetype = "dashed") +
#  labs(
#    title = "ROC Curve",
#    x = "1 - Specificity",
 #   y = "Sensitivity"
 # ) +
 # theme_minimal()





### slr
sensitivity_list2 <- c()
specificity_list2 <- c()
accuracy_list2 <- c()
plot_list2<-c()
slr1<-c()
groundtruth2<-c()

# Get unique barrels from both refbarrel and targbarrel columns
unique_barrels <- unique(c(cmcdata2$refBarrel, cmcdata2$targBarrel))
unique_barrels<-unique_barrels[order(unique_barrels)]


# Loop over each barrel for leave-one-barrel-out cross-validation
for (barrel in unique_barrels) {
  # Split the data into training and testing sets
  test_set <- cmcdata2 %>% filter(refBarrel == barrel | targBarrel == barrel)
  train_set <- cmcdata2 %>% filter(!(refBarrel == barrel | targBarrel == barrel))
  
  # Apply the classification rule on the test set
  
  thetam<-aggregate(data=train_set,cmc~MatchorNot, mean)[1,2]/64
  thetan<-aggregate(data=train_set,cmc~MatchorNot, mean)[2,2]/64
  slrvalues<-log(slrfunc(test_set$cmc,64,thetam,thetan))
  
  test_set <- test_set %>%
    mutate(slrval=slrvalues,prediction = ifelse(slrvalues > 1, "match", "non-match"))
  
  slrv<-test_set %>% select(slrval)
  slr1<-c(slr1,slrv) #all slr
  groundtruthv1<-test_set %>% select(MatchorNot)
  groundtruth2<-c(groundtruth2,groundtruthv1) #all groundtruth
  
  # Calculate confusion matrix components
  true_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "match")
  true_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "non-match")
  false_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "non-match")
  false_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "match")
  
  # Calculate performance metrics
  sensitivity <- true_positives / (true_positives + false_negatives)
  specificity <- true_negatives / (true_negatives + false_positives)
  accuracy <- (true_positives + true_negatives) / nrow(test_set)
  
  # Append metrics to the lists
  sensitivity_list2 <- c(sensitivity_list2, sensitivity)
  specificity_list2 <- c(specificity_list2, specificity)
  accuracy_list2 <- c(accuracy_list2, accuracy)
  
  plot <- ggplot(test_set, aes(x = MatchorNot, y = slrval)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    ggtitle(paste("Barrel:", barrel))+ylab("log(SLR)")+xlab("Ground Truth")+
    theme_bw()
  
  # Append plot to the plot list
  plot_list2[[length(plot_list2) + 1]] <- plot
  
  
}


slr1<-unlist(slr1)
groundtruth2<-unlist(groundtruth2)
allmn2<-data.frame(slr=slr1,groundtruth2)
ggplot(data=allmn2,aes(x=groundtruth2,y=slr))+geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 0.5) +xlab("Ground Truth")+ylab("log(SLR)")+
  theme_bw()

combined_plot2 <- do.call(grid.arrange, c(plot_list2, ncol = 2))


# Calculate mean performance metrics
mean_sensitivity2 <- mean(sensitivity_list2, na.rm = TRUE)
mean_specificity2 <- mean(specificity_list2, na.rm = TRUE)
mean_accuracy2 <- mean(accuracy_list2, na.rm = TRUE)

slrmdata<-data.frame(Method=rep("SLR",3),
                           Metric=c("sensitvity","specificity","accuracy"),
                           value=c(mean_sensitivity2,mean_specificity2,mean_accuracy2))


# Print the results
cat("Mean Sensitivity: ", mean_sensitivity2, "\n")
cat("Mean Specificity: ", mean_specificity2, "\n")
cat("Mean Accuracy: ", mean_accuracy2, "\n")



### Bayesian aproach (using bayes factor)

sensitivity_list3 <- c()
specificity_list3 <- c()
accuracy_list3 <- c()
plot_list3<-c()
bfv<-c()
groundtruth3<-c()


# Get unique barrels from both refbarrel and targbarrel columns
unique_barrels <- unique(c(cmcdata2$refBarrel, cmcdata2$targBarrel))
unique_barrels<-unique_barrels[order(unique_barrels)]


# Loop over each barrel for leave-one-barrel-out cross-validation
for (barrel in unique_barrels) {
  # Split the data into training and testing sets
  test_set <- cmcdata2 %>% filter(refBarrel == barrel | targBarrel == barrel)
  train_set <- cmcdata2 %>% filter(!(refBarrel == barrel | targBarrel == barrel))
  
  # Apply the classification rule on the test set
 match<- train_set %>% select(cmc,MatchorNot) %>% filter(MatchorNot=="match")
 nonmatch<-train_set %>% select(cmc,MatchorNot) %>% filter(MatchorNot=="non-match")
  bayesfactor<-log((dbbinom(test_set$cmc,64,0.5+sum(match$cmc),0.5+sum(64-match$cmc)))/
    (dbbinom(test_set$cmc,64,0.5+sum(nonmatch$cmc),0.5+sum(64-nonmatch$cmc))))
  
  test_set <- test_set %>%
    mutate(bf=bayesfactor,prediction = ifelse(bayesfactor > 1, "match", "non-match"))
  
  bf1<-test_set %>% select(bf)
  bfv<-c(bfv,bf1) #all bf
  groundtruthv2<-test_set %>% select(MatchorNot)
  groundtruth3<-c(groundtruth3,groundtruthv2) #all groundtruth
  
  # Calculate confusion matrix components
  true_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "match")
  true_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "non-match")
  false_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "non-match")
  false_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "match")
  
  # Calculate performance metrics
  sensitivity <- true_positives / (true_positives + false_negatives)
  specificity <- true_negatives / (true_negatives + false_positives)
  accuracy <- (true_positives + true_negatives) / nrow(test_set)
  
  # Append metrics to the lists
  sensitivity_list3 <- c(sensitivity_list3, sensitivity)
  specificity_list3 <- c(specificity_list3, specificity)
  accuracy_list3 <- c(accuracy_list3, accuracy)
  
  plot <- ggplot(test_set, aes(x = MatchorNot, y = bf)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    ggtitle(paste("Barrel:", barrel))+xlab("Ground Truth")+ylab("log(BF)")+
    theme_bw()
  
  # Append plot to the plot list
  plot_list3[[length(plot_list3) + 1]] <- plot
  
}

bfv<-unlist(bfv)
groundtruth3<-unlist(groundtruth3)
allmn3<-data.frame(bfv,groundtruth3)
ggplot(data=allmn3,aes(x=groundtruth3,y=bfv))+geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 0.5) +xlab("Ground Truth")+ylab("log(BF)")+
  theme_bw()

combined_plot3 <- do.call(grid.arrange, c(plot_list3, ncol = 2))


# Calculate mean performance metrics
mean_sensitivity3 <- mean(sensitivity_list3, na.rm = TRUE)
mean_specificity3 <- mean(specificity_list3, na.rm = TRUE)
mean_accuracy3 <- mean(accuracy_list3, na.rm = TRUE)

BFmdata<-data.frame(Method=rep("Bayes Factor",3),
                     Metric=c("sensitvity","specificity","accuracy"),
                     value=c(mean_sensitivity3,mean_specificity3,mean_accuracy3))


# Print the results
cat("Mean Sensitivity: ", mean_sensitivity3, "\n")
cat("Mean Specificity: ", mean_specificity3, "\n")
cat("Mean Accuracy: ", mean_accuracy3, "\n")


##plot of metrics
metricvalues<-rbind(two_stagemdata,slrmdata,BFmdata)
ggplot(data=metricvalues,aes(x=Metric,y=value,colour = Metric))+
  geom_point()+facet_grid(~Method)+theme_bw()


ggplot(metricvalues, aes(x = Method, y = value, group =Metric , color = Metric)) +
  geom_point(shape=18,size=3) +
  geom_line() +
  labs(title = "",
       x = "Method",
       y = "Average proportion",
       color = "Metric")+theme_bw()

#ggparcoord(data = metricvalues,columns = c(3,1),groupColumn = 2)

#roc curve
#ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
 # geom_line(color = "blue") +
  #geom_abline(linetype = "dashed") +
 # labs(
#    title = "ROC Curve",
 #   x = "1 - Specificity",
  #  y = "Sensitivity"
  #) +
 # theme_minimal()

