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
data <- read.csv("../data/loiu.csv")

# Eliminar columnas que no son numericas
data$fecha = NULL
data$indicativo = NULL
data$nombre = NULL
data$provincia = NULL
data$altitud = NULL
data$horatmin = NULL
data$horatmax = NULL
data$horaracha = NULL
data$horaPresMax = NULL
data$horaPresMin = NULL
data$tmax = NULL
data$tmin = NULL

# Create 10 data partitions
training_p <- 0.80

data

# Crear una particion de datos con 80% de entrenamiento, 20% de test
training_samples <- createDataPartition(y = data$tmed, p = training_p, list = FALSE)

# Dividir la informacion de entrenamiento de la de test
training_data    <- data[training_samples, ]
test_data        <- data[-training_samples, ]

# Create Linear Model using training data. Formula = all the columns except Salary
model            <- lm(formula = training_data$tmed ~., data = training_data)

# Make the prediction using the model and test data
prediction       <- predict(model, test_data)

# Calculate Mean Average Error
mean_avg_error   <- mean(abs(prediction - test_data$tmed))
  
# Print Mean Absolute Error
print(paste0("- Mean average error: ", mean_avg_error))

# Print model summary
summary(model)

# Plot model
par(mfrow = c(2,2))
plot(model)

