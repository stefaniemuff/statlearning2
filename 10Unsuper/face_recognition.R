# Load necessary libraries
library(loon.data)  # For the 'faces' dataset
library(ggplot2)    # For visualization (optional)
library(ggmap)
library(patchwork)
library(gridExtra)
library(class)

rm(list = ls())
# Load the faces dataset from loon.data
data("faces")  # The 'faces' dataset

# The faces dataset is a list of images, so we will convert it into a matrix form
# Each face is a 64X64 image, so we flatten each image into a vector of length 4096
face_matrix <- t(sapply(faces, function(x) as.vector(x)))



# Loop through the first 10 images in the 'faces' dataset

plot_list = list()
ii = 1:50
for(i in 1:length(ii))
  plot_list[[i]] = ggimage(matrix(faces[[ii[i]]], nrow = 64, ncol = 64))

grid.arrange(grobs=plot_list,ncol=10)


# set labels
labels = rep(1:40, each = 10)

# create test and training set
train_indices = sort(sample(1:400, 300))
train_data = face_matrix[train_indices,]
test_data = face_matrix[-train_indices,]


train_labels <- labels[train_indices]
test_labels <- labels[-c(train_indices)]

# Perform PCA
pca_result <- prcomp(train_data, center = T, scale. = FALSE)

# plot autofaces and mean face
plot_list = list()
ii = 1:25
for(i in 1:25)
  plot_list[[i]] = ggimage(matrix(pca_result$rotation[,i], nrow = 64, ncol = 64))
grid.arrange(grobs=plot_list,ncol=5)
ggimage(matrix(pca_result$center, nrow = 64, ncol = 64))

# scree plot
data.frame(var = pca_result$sdev^2) %>%
  mutate(i = seq_along(var)) %>%
  mutate(cum = cumsum(var)/sum(var) * 100) %>%
  ggplot() + geom_point(aes(i, cum))





# Project training data into PCA space
train_pca <- predict(pca_result, train_data)


# reconstruct face with less components
num_components = 30
ii = sample(1:300,1)
aa = train_pca[ii,,drop = F][, 1:num_components] %*% t(pca_result$rotation[, 1:num_components])
g1 = ggimage(matrix(aa + pca_result$center, 64,64))
g2 = ggimage(matrix(train_data[ii,] , 64,64))

g1 + g2


# now try to recognise faces

# Project test data into PCA space
test_pca <- predict(pca_result, test_data)

# Recognize faces using k-NN (k = 1)
accuracy = numeric(299)
preds = numeric(length(test_labels))
for(i in 1:length(accuracy))
{
  predicted_labels <- knn(train_pca[, 1:(i+1)], 
                        test_pca[, 1:(i+1)], train_labels, k = 1)

  # Calculate accuracy
  preds = preds + as.numeric(predicted_labels == test_labels)
  accuracy[i] <- sum(predicted_labels == test_labels) / length(test_labels)
}
data.frame(accuracy =accuracy,
           num_components = seq_along(accuracy)) %>%
  ggplot() + geom_point(aes(num_components, accuracy)) +
  geom_vline(xintercept = 20)


preds
which(preds==0)

# best matching face ------------------------------------------------------


num_components = 10

train_scores <- pca_result$x[, 1:num_components]
test_id = 17#sample(1:dim(test_data)[1],1)
test_image <- test_data[test_id,,drop = F]  # Change this to your actual test image

# Project the test image into the PCA space using the same principal components
test_image_pca <- predict(pca_result, newdata = (test_image))[, 1:num_components]
test_image_pca <- matrix(test_image_pca, nrow = 1)
# Compute Euclidean distances between the test image PCA scores
# and all training images PCA scores

distances <- sqrt(rowSums(t(apply(train_scores, 1, 
                                  function(x) (x-test_image_pca)^2))))

# Find the index of the closest training image
best_match_index <- which.min(distances)
# Display the best matching training image
best_match_face <- train_data[[best_match_index]]

p1 = ggimage(matrix(test_data[test_id,], nrow = 64, ncol = 64))
p2 = ggimage(matrix(train_data[best_match_index,], nrow = 64, ncol = 64))
p1 + p2






# can we try clustering?
library(cluster)
pca_result2 <- prcomp(face_matrix, center = T, scale. = FALSE)
num_components = 300
test_image_pca <- predict(pca_result2, newdata = face_matrix)[, 1:num_components]

km.res <- kmeans(test_image_pca, 40, nstart = 25)

table(km.res$cluster)

tt = which(km.res$cluster==2) 
length(tt)
list_plot = list()
for(i in 1:length(tt))
{
  list_plot[[i]] = ggimage(matrix(face_matrix[tt[i],], nrow = 64, ncol = 64))
}
grid.arrange(grobs=list_plot,ncol=5)


