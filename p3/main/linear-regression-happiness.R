# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required packages
library(lattice)
library(data.table)
library(ggplot2)
library(caret)
library(RKEEL)

# Leer archivo con datos 
filename = "../data/2020 - 2019 world happiness.csv"
data <- read.csv(file=filename, sep=",", header = TRUE)

# Eliminar columnas que no son numericas
data$Country.or.region = NULL

# Create 10 data partitions
training_p <- 0.80

# Crear una particion de datos con 80% de entrenamiento, 20% de test
training_samples <- createDataPartition(y = data$Overall.rank, p = training_p, list = FALSE)

# Dividir la informacion de entrenamiento de la de test
training_data    <- data[training_samples, ]
test_data        <- data[-training_samples, ]

# Create Linear Model using training data. Formula = all the columns except Salary
model            <- lm(formula = training_data$Overall.rank ~., data = training_data)

# Make the prediction using the model and test data
prediction       <- predict(model, test_data)

# Calculate Mean Average Error
mean_avg_error   <- mean(abs(prediction - test_data$Overall.rank))
  
# Print Mean Absolute Error
print(paste0("- Mean average error: ", mean_avg_error))

# Print model summary
summary(model)

# Plot model
par(mfrow = c(2,2))
plot(model)