# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming your data is in a dataframe called df
# df <- read.csv("your_data.csv")

# Placeholder for performance metrics and combined test set
sensitivity_list <- c()
specificity_list <- c()
accuracy_list <- c()
combined_test_set <- data.frame()

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
           barrel_info = as.factor(paste( barrel)))
  
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
  sensitivity_list <- c(sensitivity_list, sensitivity)
  specificity_list <- c(specificity_list, specificity)
  accuracy_list <- c(accuracy_list, accuracy)
  
  # Combine test set with the overall test set
  combined_test_set <- rbind(combined_test_set, test_set)
}

set.seed(123)
num_colors <- length(unique(combined_test_set$barrel_info))
colors1 <- rainbow(num_colors)

# Generate the combined boxplot
 ggplot(combined_test_set, aes(x = MatchorNot, y = cmc)) +
  geom_boxplot() + # Ensure that the boxplot does not include outliers as points
  geom_jitter(aes(color=barrel_info),width = 0.2, alpha = 0.5) +
  scale_color_manual(values = colors1) +
  theme(legend.position = "right") +theme_bw() +xlab("Ground Truth")+ylab("CMC")+
   labs(color="Barrel")+geom_hline(yintercept = 6)

# Calculate mean performance metrics
mean_sensitivity <- mean(sensitivity_list, na.rm = TRUE)
mean_specificity <- mean(specificity_list, na.rm = TRUE)
mean_accuracy <- mean(accuracy_list, na.rm = TRUE)

# Print the results
cat("Mean Sensitivity: ", mean_sensitivity, "\n")
cat("Mean Specificity: ", mean_specificity, "\n")
cat("Mean Accuracy: ", mean_accuracy, "\n")



#### slr

sensitivity_list2 <- c()
specificity_list2 <- c()
accuracy_list2 <- c()
combined_test_set2 <- data.frame()


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
    mutate(slrval=slrvalues,prediction = ifelse(slrvalues > 1, "match", "non-match"),
           barrel_info = as.factor(paste( barrel)))
  
  
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
  
  # Combine test set with the overall test set
  combined_test_set2 <- rbind(combined_test_set2, test_set)
  
  
}

set.seed(124)
# Generate the combined boxplot
ggplot(combined_test_set2, aes(x = MatchorNot, y = slrval)) +
  geom_boxplot() + # Ensure that the boxplot does not include outliers as points
  geom_jitter(aes(color=barrel_info),width = 0.2, alpha = 0.5) +
  scale_color_manual(values = rainbow(length(unique(combined_test_set2$barrel_info)))) +
  theme(legend.position = "right") +theme_bw() +xlab("Ground Truth")+ylab("log(SLR)")+
  labs(color="Barrel")+geom_hline(yintercept = 1)

# Calculate mean performance metrics
mean_sensitivity2 <- mean(sensitivity_list2, na.rm = TRUE)
mean_specificity2 <- mean(specificity_list2, na.rm = TRUE)
mean_accuracy2<- mean(accuracy_list2, na.rm = TRUE)

# Print the results
cat("Mean Sensitivity: ", mean_sensitivity2, "\n")
cat("Mean Specificity: ", mean_specificity2, "\n")
cat("Mean Accuracy: ", mean_accuracy2, "\n")



### Bayes factor
sensitivity_list3 <- c()
specificity_list3 <- c()
accuracy_list3 <- c()
combined_test_set3 <- data.frame()


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
    mutate(bf=bayesfactor,prediction = ifelse(bayesfactor > 1, "match", "non-match"),
           barrel_info = as.factor(paste( barrel)))
  
  
  
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
  
  # Combine test set with the overall test set
  combined_test_set3 <- rbind(combined_test_set3, test_set)
  
  
}

set.seed(128)
# Generate the combined boxplot
ggplot(combined_test_set3, aes(x = MatchorNot, y = bf)) +
  geom_boxplot() + 
  geom_jitter(aes(color=barrel_info),width = 0.2, alpha = 0.5) +
  scale_color_manual(values = rainbow(length(unique(combined_test_set3$barrel_info)))) +
  theme(legend.position = "right") +theme_bw() +xlab("Ground Truth")+ylab("log(BF)")+
  labs(color="Barrel")+geom_hline(yintercept = 1)

# Calculate mean performance metrics
mean_sensitivity3 <- mean(sensitivity_list3, na.rm = TRUE)
mean_specificity3 <- mean(specificity_list3, na.rm = TRUE)
mean_accuracy3<- mean(accuracy_list3, na.rm = TRUE)

# Print the results
cat("Mean Sensitivity: ", mean_sensitivity3, "\n")
cat("Mean Specificity: ", mean_specificity3, "\n")
cat("Mean Accuracy: ", mean_accuracy3, "\n")



