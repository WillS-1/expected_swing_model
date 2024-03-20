library(needs)
needs(baseballr, dplyr, grid, data.table, ggplot2, rpart, rpart.plot, pROC, caret)


#---Model 1-----------
model_data <- DF %>% select(PlateLocHeight,PlateLocSide,RelSpeed,InducedVertBreak,HorzBreak,Balls,Strikes,Outs,Swing) %>% mutate(Swing = ifelse(Swing == 'Swing', 1,0))

set.seed(123)

train_index_1 <- createDataPartition(model_data$Swing, p = 0.8, list = FALSE)
train_data_1 <- model_data[train_index_1, ]
test_data_1 <- model_data[-train_index_1, ]

model_1 <- glm(Swing ~ ., data = train_data_1, family = 'binomial')

#---Model 1 Predictions
predictions_1 <- predict(model_1, newdata = test_data_1, type = "response")
binary_predictions_1 <- ifelse(predictions_1 > 0.5, 1, 0)

#---Optimal Threshold Calculation for Model  
f1_scores <- data.frame(threshold = numeric(length = 101), f1 = numeric(length = 101))
  
# Calculate F1 scores for each threshold
for (i in 1:101) {
    threshold <- (i - 1) / 100
    binary_predictions <- ifelse(predictions > threshold, 1, 0)
    confusion_matrix <- table(test_data$Swing, binary_predictions)
    
    # Ensure that confusion matrix has all needed values
    if (ncol(confusion_matrix) < 2 || nrow(confusion_matrix) < 2) {
      # Not enough values in the confusion matrix, skip
      next
    }
    
    tp <- confusion_matrix[2, 2]
    fp <- confusion_matrix[1, 2]
    fn <- confusion_matrix[2, 1]
    
    f1_scores[i, "threshold"] <- threshold
    f1_scores[i, "f1"] <- calculate_f1_score(tp, fp, fn)
  }
  
# Find the threshold that maximizes the F1 score
optimal_threshold <- f1_scores$threshold[which.max(f1_scores$f1)]

# Threshold Determination Attempt 2
thresholds <- seq(0, 1, by = 0.01)
metrics <- data.frame(threshold = thresholds, sensitivity = numeric(length(thresholds)), specificity = numeric(length(thresholds)), youden_j = numeric(length(thresholds)))
  
# Calculate sensitivity, specificity, and Youden's J for each threshold
for (i in 1:length(thresholds)) {
    threshold <- thresholds[i]
    binary_predictions <- ifelse(predictions > threshold, 1, 0)
    confusion_matrix <- table(test_data$Swing, binary_predictions)
    # Ensure that confusion matrix has all needed values
    if (ncol(confusion_matrix) < 2 || nrow(confusion_matrix) < 2) {
      # Not enough values in the confusion matrix, skip
      next
    }
    # Calculate sensitivity and specificity
    sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
    specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
    
    # Calculate Youden's J statistic
    youden_j <- sensitivity + specificity - 1
    
    # Store metrics in the data frame
    metrics[i, c("sensitivity", "specificity", "youden_j")] <- c(sensitivity, specificity, youden_j)
  }
  
# Find the threshold that maximizes Youden's J statistic
optimal_threshold <- metrics$threshold[which.max(metrics$youden_j)]

#---Model 1 Predictions with new threshold of .44
predictions_1 <- predict(model_1, newdata = test_data_1, type = "response")
binary_predictions_1 <- ifelse(predictions_1 > 0.44, 1, 0)

# Correlation Testing
cor_matrix <- cor(model_data)
vif_model <- lm(Swing~., data = model_data)
vif_results <- vif(vif_model)
eigen_values <- eigen(cor_matrix)$values
tolerance <- 1 / vif_results
# Correlation Plot - Used in Correlation Heat Map in Expected_Swing_Graphs_Heatmaps
cor_melted <- melt(cor_matrix)


#---Model 2-----------(Takes out Induced Vertical Break and Outs as Predictors)
model_data <- DF %>% select(PlateLocHeight,PlateLocSide,RelSpeed,HorzBreak,Balls,Strikes,Swing) %>% mutate(Swing = ifelse(Swing == 'Swing', 1,0))

train_index_2 <- createDataPartition(model_data$Swing, p = 0.8, list = FALSE)
train_data_2 <- model_data[train_index_2, ]
test_data_2 <- model_data[-train_index_2, ]

model_2 <- glm(Swing ~ ., data = train_data_2, family = 'binomial')
#---Model 2 Predictions
predictions_2 <- predict(model_2, newdata = test_data_2, type = "response")
binary_predictions_2 <- ifelse(predictions_2 > 0.44, 1, 0)

#---Model 3-----------
###-REMATRIX is the run expectancy matrix used in the 3rd model
REMATRIX <- data.frame(Runners = c('No_No_No','Yes_No_No','No_Yes_No','Yes_Yes_No','No_No_Yes','Yes_No_Yes','No_Yes_Yes','Yes_Yes_Yes' ),
  Zero = c(.461,.831,1.068,1.373,1.426,1.798,1.920,2.282), 
  One = c(.243,.489,.644,.908,.865,1.140,1.352,1.520), 
  Two = c(.095,.214,.305,.343,.413,.471,.570,.736)) %>% reshape2::melt(., id.vars = c('Runners')) %>% 
  mutate(variable = recode(variable, 'Zero' = 0, 'One' = 1, 'Two' = 2)) %>% 
  mutate(Situation = paste0(Runners,'_', variable)) %>% select(Situation, RE = value)

###-Creates the situations so we can combine the REMATRIX with the main dataframe
model_data <- DF %>% mutate(Swing = ifelse(Swing == 'Swing', 1,0)) %>%
  mutate(Situation = paste0(ManOn1, '_', ManOn2, '_', ManOn3, '_', Outs)) %>% left_join(., REMATRIX, by = c('Situation')) %>% 
  select(PlateLocHeight,PlateLocSide,RelSpeed,HorzBreak,Balls,Strikes,RE,Swing)

train_index_3 <- createDataPartition(model_data$Swing, p = 0.8, list = FALSE)
train_data_3 <- model_data[train_index_3, ]
test_data_3 <- model_data[-train_index_3, ]

model_3 <- glm(Swing ~ ., data = train_data_3, family = 'binomial')
#---Model 3 Predictions
predictions_3 <- predict(model_3, newdata = test_data_3, type = "response")
binary_predictions_3 <- ifelse(predictions_3 > 0.44, 1, 0)

#---Individual Pitchers Swing Predictions
###--Individ is the individual pitchers data (Gerrit Cole, Dylan Cease and Dean Kremer)
Individ_Data <- Individ %>% select(PlateLocHeight,PlateLocSide,RelSpeed,InducedVertBreak,HorzBreak,Balls,Strikes,Outs,Swing) %>% mutate(Swing = ifelse(Swing == 'Swing', 1,0))
predictions_I <- predict(model_1, newdata = Individ_Data, type = "response")
binary_predictions_I <- ifelse(predictions_I > 0.44, 1, 0)


