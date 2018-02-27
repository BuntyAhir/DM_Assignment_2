library(tidyverse)

bc_data <- read.table("/Users/vidit/Desktop/Sem_3/MSCS 5610/Assignment_2/breast-cancer-wisconsin.csv", header = FALSE, sep = ",")
colnames(bc_data) <- c("sample_code_number", "clump_thickness", "uniformity_of_cell_size", "uniformity_of_cell_shape", "marginal_adhesion", "single_epithelial_cell_size", 
                       "bare_nuclei", "bland_chromatin", "normal_nucleoli", "mitosis", "classes")

head(bc_data)
bc_data$classes <- ifelse(bc_data$classes == "2", "benign",
                          ifelse(bc_data$classes == "4", "malignant", NA))

bc_data[bc_data == "?"] <- NA

# how many NAs are in the data
length(which(is.na(bc_data)))


install.packages("mice")
library(mice)

bc_data[,2:10] <- apply(bc_data[, 2:10], 2, function(x) as.numeric(as.character(x)))
dataset_impute <- mice(bc_data[, 2:10],  print = FALSE)
bc_data <- cbind(bc_data[, 11, drop = FALSE], mice::complete(dataset_impute, 1))

bc_data$classes <- as.factor(bc_data$classes)

# how many benign and malignant cases are there?
summary(bc_data$classes)
str(bc_data)


install.packages("party")
library(party)

# Create the input data frame.
input.dat <- bc_data[c(1:105),]

# Give the chart file a name.
png(file = "decision_tree.png")

# Create the tree.
output.tree <- ctree(
  sample_code_number ~ clump_thickness + uniformity_of_cell_size + uniformity_of_cell_shape + marginal_adhesion + single_epithelial_cell_size + bare_nuclei + bland_chromatin + normal_nucleoli + mitosis, 
  data = input.dat)

# Plot the tree.
plot(output.tree)

# Save the file.
dev.off()