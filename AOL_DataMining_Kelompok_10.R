# AOL Data Mining Kelompok 10

library(ggplot2)

data <- read.csv("E:/DataMining/survey lung cancer.csv")
head(data)

summary(data)

#Check and remove duplicate data
duplicate_count <- sum(duplicated(data))
cat("Duplicate Rows: ", duplicate_count)
data <- data[!duplicated(data), ]
duplicate_count <- sum(duplicated(data))
cat("Duplicate Rows: ", duplicate_count, "\n")

#Distribution of Age
ggplot(data, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(title = "Distribution age")

#Box Plot of Age
ggplot(data, aes(x = LUNG_CANCER, y = AGE, fill = LUNG_CANCER)) +
  geom_boxplot() +
  labs(title = "Box Plot of age")

#Plotting 
categori<- names(data)[names(data) != "AGE"]
num_plots <- length(categorical_columns)
colors <- c("cyan", "brown")

plotting <- function(var_name) {
  
  ggob <- ggplot(data, aes_string(x = categori[index], fill = "LUNG_CANCER")) +
    geom_bar(aes(fill = LUNG_CANCER), position = "dodge", color = "black") +
    labs(title = paste("Count of", var_name, "by LUNG_CANCER")) +
    geom_text(aes(label = after_stat(count), y = after_stat(count)), stat = "count") +
    scale_fill_manual(values = colors) +
    guides(fill = guide_legend(title = "LUNG_CANCER"))
  
  return(ggob)
}

for (index in 1:num_plots) {
  forit <- plotting(categori[index])
  print(forit)
}


