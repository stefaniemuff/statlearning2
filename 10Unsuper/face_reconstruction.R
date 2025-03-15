# Load necessary libraries




library("magick")
library(imager)
library(tidyverse)
library(ggmap)
library(patchwork)
library(gridExtra)

rm(list = ls())


face = load.image("~/Downloads/worldface-british-guy-white-background.jpg")

face_grey = grayscale(face) 

face_matrix = as.matrix(face_grey)
face_matrix = t(face_matrix[nrow(face_matrix):1, ])

dim(face_matrix)


ggimage(face_matrix)

# Perform PCA
pca_result <- prcomp(face_matrix, center = TRUE, scale. = FALSE)

ggimage(pca_result$x)


# Reconstruct the image using different numbers of principal components
reconstruct_face <- function(pca_result, num_components) {
  approx_matrix <- pca_result$x[, 1:num_components] %*% t(pca_result$rotation[, 1:num_components])
  approx_matrix <- sweep(approx_matrix, 2, pca_result$center, "+")  # Re-add the mean
  return(approx_matrix)
}



tot_var = sum(pca_result$sdev^2)

data.frame(var = cumsum(pca_result$sdev^2)/tot_var*100) %>%
  mutate(i = 1:3067) %>%
  filter(i<50) %>%
  ggplot() + geom_line(aes(i,var))+ geom_point(aes(i,var))


# Display original and reconstructed images

# Plot original image
p_orig = ggimage(face_matrix) + ggtitle("Original Image")

# Reconstruct using different numbers of principal components
p1 =   ggimage(reconstruct_face(pca_result, 1))
p5 =    ggimage(reconstruct_face(pca_result, 5))
p50 =    ggimage(reconstruct_face(pca_result, 10))
p200 =    ggimage(reconstruct_face(pca_result, 200))


p_orig + p1  + p5 + p50 + p200
