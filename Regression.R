# Regression
library(tidyverse)
library(caret)
library(broom)
library(GGally)
library(leaps)
library(MASS)

#see data
ggpairs(df)

# Split the data into training and test set
set.seed(123)
training.samples <- df$Tetracore %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]
#another method
set.seed(12345)
Data_Train <- sample_frac(tbl = df, replace = FALSE, size = 0.80)
Data_Test <- anti_join(df, Data_Train)

#linear regression
model <- lm(Tetracore ~., data = Data_Train)
summary(model)$coef

#stepwise
set.seed(123)
train.control <- trainControl(method = "cv", number = 5)
step.model <- train(Tetracore ~., data = df,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 3:11),
                    trControl = train.control)
summary(step.model$finalModel)
step.model$results
#another method(AIC as criterion)
full.model <- lm(Sensor ~., data = df2)
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)


#To see Multicollinearity (vif)
car::vif(model1)

#Best subset (R2 and BIC)
best_subset <- regsubsets(Sensor ~ ., df, nvmax = 13)
results <- summary(best_subset)
tibble(predictors = 1:13,
       adj_R2 = results$adjr2,
       BIC = results$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic))+
  geom_line(show.legend = F) +
  geom_point(show.legend = F)+
  facet_wrap(~ statistic, scales = "free")+
  scale_color_manual(values=c('#4682B4','#008B8B'))
#see results
coef(best_subset, 10)

#list
list(model1 = broom::glance(model1), 
     model2 = broom::glance(model2),
     model3 = broom::glance(model3))

model.diag.metrics <- augment(model3)
ggplot(model.diag.metrics, aes(pH + ORP + ORP:L + ORP:Carbohydrate, Tetracore)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)

set.seed(12321)
Yacht_NN2 <- neuralnet(Tetracore ~ Fat + Cholesterol + + Sodium + Potassium + Carbohydrate + Protein + pH + ORP +  C_100Hz + C_400Hz + C_1kHz + C_10kHz + DO + L + a + b, data = Yacht_Data_Train, hidden = 2)
NN2_Train_SSE <- sum((Yacht_NN2$net.result - Yacht_Data_Train[, 17])^2)/2
Test_NN2_Output <- compute(Yacht_NN2, Yacht_Data_Test[, 1:16])$net.result
NN2_Test_SSE <- sum((Test_NN2_Output - Yacht_Data_Test[, 17])^2)/2

plot(Data_Test$Tetracore, Test_NN2_Output, pch = 18,cex = 0.7)

#Prediction
preds <- predict(Yacht_NN2, Data_Test)
data.frame(RMSE = RMSE(preds, Data_Test$Tetracore),
           R2 = R2(preds, Data_Test$Tetracore))

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(df), replace = TRUE)
cv_errors <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))


for(j in 1:k) {
  
  best_subset <- regsubsets(Tetracore ~ ., df[folds != j, ], nvmax = 13)
  
  
  for( i in 1:13) {
    pred_x <- predict.regsubsets(best_subset, df[folds == j, ], id = i)
    cv_errors[j, i] <- mean((df$Tetracore[folds == j] - pred_x)^2)
  }
}

res2 <- rcorr(as.matrix(df), type = ("spearman"))

#Penalized #0=ridge
x <- model.matrix(Sensor~., df)[,-1]
y <- df$Sensor
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 1)
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
coef(model)
#predic
x.test <- model.matrix(Sensor ~., df)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
data.frame(
  RMSE = RMSE(predictions, df$Sensor),
  Rsquare = R2(predictions, df$Sensor))
#another
set.seed(123)
model <- train(
  Sensor ~., data = df5, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
model$bestTune