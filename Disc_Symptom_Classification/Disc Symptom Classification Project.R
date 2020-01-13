library(tidyverse)
library(repr)
library(caret)
library(GGally)

setwd("C:/Users/Daniel Lee/Desktop/R Projects/vertebral_column_data")

vertebral <- read_csv("column_3C_weka.txt", col_names=FALSE, skip=12)
colnames(vertebral) <- c("Pelvic.Incidence", "Pelvic.Tilt", "Angle.of.Lumbar.Lordosis","Sacral.Slope", "Pelvic.Radius", "Spondylolisthesis.Degree", "Class")

n1 <- vertebral %>%
  group_by(Class) %>%
  summarize(total = n())

vertebral_normal <- vertebral %>%
  filter(Class == 'Normal')
vertebral_hernia <- vertebral %>%
  filter(Class == 'Hernia')
vertebral_sl <- vertebral %>%
  filter(Class == 'Spondylolisthesis')

set.seed(10)

vertebral_normal <- vertebral_normal %>%
  sample_n(60)

vertebral_sl <- vertebral_sl %>%
  sample_n(60)

vertebral_new <- rbind(vertebral_normal,vertebral_hernia, vertebral_sl)


# 1-2 ----------------------------------------------------------------------

set.seed(2000)
vertebral_new <- vertebral_new %>%
  select(-Pelvic.Tilt, -Sacral.Slope)

scaled_vertebral_new <- vertebral_new %>% select(Angle.of.Lumbar.Lordosis, Spondylolisthesis.Degree, Pelvic.Radius, Pelvic.Incidence,Class) %>%
  mutate(Angle.of.Lumbar.Lordosis = as.vector(scale(Angle.of.Lumbar.Lordosis, center = TRUE)),
         Spondylolisthesis.Degree = as.vector(scale(Spondylolisthesis.Degree, center = TRUE)),
         Pelvic.Incidence = as.vector(scale(Pelvic.Incidence, center = TRUE)),
         Pelvic.Radius = as.vector(scale(Pelvic.Radius, center = TRUE)))

head(scaled_vertebral_new)


# 1-3 -----------------------------------------------------------------------

scaled_vertebral_new <- vertebral_new %>% select(Angle.of.Lumbar.Lordosis, Spondylolisthesis.Degree, Pelvic.Radius, Pelvic.Incidence,Class) %>%
  mutate(Angle.of.Lumbar.Lordosis = as.vector(scale(Angle.of.Lumbar.Lordosis, center = TRUE)),
         Spondylolisthesis.Degree = as.vector(scale(Spondylolisthesis.Degree, center = TRUE)),
         Pelvic.Incidence = as.vector(scale(Pelvic.Incidence, center = TRUE)),
         Pelvic.Radius = as.vector(scale(Pelvic.Radius, center = TRUE)))


# 1-4 -----------------------------------------------------------------------

# Splitting the sclaed vertebral column data into training and testing sets
training_rows <- scaled_vertebral_new %>%
  select(Class) %>%
  unlist() %>%
  createDataPartition(p = 0.75, list = FALSE)

X_train <- scaled_vertebral_new %>%
  select(Pelvic.Incidence, Pelvic.Radius, Angle.of.Lumbar.Lordosis, Spondylolisthesis.Degree) %>%
  slice(training_rows) %>%
  data.frame()

Y_train <- scaled_vertebral_new %>%
  select(Class) %>%
  slice(training_rows) %>%
  unlist()

X_test <- scaled_vertebral_new %>%
  select(Pelvic.Incidence, Pelvic.Radius, Angle.of.Lumbar.Lordosis, Spondylolisthesis.Degree) %>%
  slice(-training_rows) %>%
  data.frame()

Y_test <- scaled_vertebral_new %>%
  select(Class) %>%
  slice(-training_rows) %>%
  unlist()


# 1-5 ---------------------------------------------------------------------

vertebral_new_stat <- vertebral_new %>%
  select(Pelvic.Incidence, Pelvic.Radius, Angle.of.Lumbar.Lordosis, Spondylolisthesis.Degree,Class) %>%
  slice(training_rows) %>%
  group_by(Class) %>%
  summarize(LLAngle = mean(Angle.of.Lumbar.Lordosis), SDegree = mean(Spondylolisthesis.Degree), PRadius = mean(Pelvic.Radius), PInc = mean(Pelvic.Incidence), total = n())


# 1-6 ---------------------------------------------------------------------

plot_pairs_new <- scaled_vertebral_new %>%
  slice(training_rows) %>%
  ggpairs(aes(colour = Class), ,lower = list(continuous = wrap("points", alpha = 0.4, size = 0.75)), diag = list(continuous = wrap("densityDiag", alpha = 0.4)), upper=list(continuous=wrap("cor",size=3)))+
  theme(text = element_text(size = 6))

# Two Symptoms w/ Pelvic Incidence and Angle of Lumbar Lordosis ---------------------------------------------------------------------

vertebral_db <- vertebral_new %>%
  filter(Class != "Normal")

vertebral_db <- vertebral_db %>%
  select(Pelvic.Incidence, Angle.of.Lumbar.Lordosis,Class)

scaled_vertebral_db <- vertebral_db %>%
  mutate(Angle.of.Lumbar.Lordosis = as.vector(scale(Angle.of.Lumbar.Lordosis, center = TRUE)),
         Pelvic.Incidence = as.vector(scale(Pelvic.Incidence, center = TRUE)))
scaled_vertebral_db <- scaled_vertebral_db %>%
  mutate(Class = as.factor(Class))

set.seed(1)
training_rows <- scaled_vertebral_db %>%
  select(Class) %>%
  unlist() %>%
  createDataPartition(p = 0.75, list = FALSE)

X_train_db <- scaled_vertebral_db %>%
  select(Pelvic.Incidence,Angle.of.Lumbar.Lordosis) %>%
  slice(training_rows) %>%
  data.frame()

Y_train_db <- scaled_vertebral_db %>%
  select(Class) %>%
  slice(training_rows) %>%
  unlist()

X_test_db <- scaled_vertebral_db %>%
  select(Pelvic.Incidence,Angle.of.Lumbar.Lordosis) %>%
  slice(-training_rows) %>%
  data.frame()

Y_test_db <- scaled_vertebral_db %>%
  select(Class) %>%
  slice(-training_rows) %>%
  unlist()

k <- data.frame(k = c(1:20))

train_control <- trainControl(method = 'cv', number = 10)

set.seed(1234)
knn_model_cv10_db <- train(x = X_train_db, y = Y_train_db, method = "knn", tuneGrid = k, trControl = train_control)
accuracies_db <- knn_model_cv10_db$results

accuracy_db <- ggplot(accuracies_db, aes(x = k, y = Accuracy)) +
  geom_point() +
  geom_line()

k = data.frame(k = 9)
model_knn_db <- train(x = X_train_db, y = Y_train_db, method = "knn", tuneGrid = k)
Y_validation_predicted_db <- predict(object = model_knn_db, X_test_db)

model_quality_db <- confusionMatrix(data = Y_validation_predicted_db, reference = Y_test_db)
model_quality_db


# Three Symptoms w/ Spondylolisthesis Degree, Angle of Lumbar Lordosis----------------------------------------------------------

vertebral_tp <- vertebral_new %>%
  select(Pelvic.Incidence, Angle.of.Lumbar.Lordosis,Spondylolisthesis.Degree,Class)

scaled_vertebral_tp <- vertebral_tp %>%
  mutate(Spondylolisthesis.Degree = as.vector(scale(Spondylolisthesis.Degree, center = TRUE)),
         Angle.of.Lumbar.Lordosis = as.vector(scale(Angle.of.Lumbar.Lordosis, center = TRUE)),
         Pelvic.Incidence = as.vector(scale(Pelvic.Incidence, center = TRUE)))
scaled_vertebral_tp <- scaled_vertebral_tp %>%
  mutate(Class = as.factor(Class))

set.seed(1)
training_rows <- scaled_vertebral_tp %>%
  select(Class) %>%
  unlist() %>%
  createDataPartition(p = 0.75, list = FALSE)

X_train_tp <- scaled_vertebral_tp %>%
  select(Spondylolisthesis.Degree,Angle.of.Lumbar.Lordosis) %>%
  slice(training_rows) %>%
  data.frame()

Y_train_tp <- scaled_vertebral_tp %>%
  select(Class) %>%
  slice(training_rows) %>%
  unlist()

X_test_tp <- scaled_vertebral_tp %>%
  select(Spondylolisthesis.Degree,Angle.of.Lumbar.Lordosis) %>%
  slice(-training_rows) %>%
  data.frame()

Y_test_tp <- scaled_vertebral_tp %>%
  select(Class) %>%
  slice(-training_rows) %>%
  unlist()

k <- data.frame(k = c(1:20))

train_control <- trainControl(method = 'cv', number = 10)

set.seed(1234)
knn_model_cv10_tp <- train(x = X_train_tp, y = Y_train_tp, method = "knn", tuneGrid = k, trControl = train_control)
accuracies_tp <- knn_model_cv10_tp$results

accuracy_tp <- ggplot(accuracies_tp, aes(x = k, y = Accuracy)) +
  geom_point() +
  geom_line()

k = data.frame(k = 5)
model_knn <- train(x = X_train_tp, y = Y_train_tp, method = "knn", tuneGrid = k)
Y_validation_predicted <- predict(object = model_knn, X_test_tp)

model_quality <- confusionMatrix(data = Y_validation_predicted, reference = Y_test_tp)
model_quality


