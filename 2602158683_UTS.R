#Yonathan Henry Christianto
library(GGally)
library(psych)

data <- read.csv("E:/DataMining/UTS/diamonds.csv")

#Check for duplicate data
duplicatecheck <- data[duplicated(data)]
duplicatecheck

#check missing data
sapply(data, function(x) sum(is.na(x)))


str(data)

#Outliner
boxplot(data$carat, main = "Box Plot")
boxplot(data$depth, main = "Box Plot")
boxplot(data$table, main = "Box Plot")
boxplot(data$price, main = "Box Plot")
boxplot(data$x, main = "Box Plot")
boxplot(data$y, main = "Box Plot")
boxplot(data$z, main = "Box Plot")

#Distribution
ggplot(data, aes(x = carat)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "carat Data", x = "carat", y = "Amount")

ggplot(data, aes(x = depth)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "depth Data", x = "depth", y = "Amount")

ggplot(data, aes(x = table)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "table Data", x = "table", y = "Amount")

ggplot(data, aes(x = price)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "price Data", x = "price", y = "Amount")

ggplot(data, aes(x = x)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "x Data", x = "x", y = "Amount")

ggplot(data, aes(x = y)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "y Data", x = "y", y = "Amount")

ggplot(data, aes(x = z)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "z Data", x = "z", y = "Amount")

#Relation
barplot(table(data$price, data$carat), 
        beside = TRUE, 
        col = c("blue", "black"), 
        main = "Diamond Price based on carat", 
        xlab = "Carat", ylab = "Price")

barplot(table(data$price, data$table), 
        beside = TRUE, 
        col = c("blue", "black"), 
        main = "Diamond Price based on table", 
        xlab = "table", ylab = "Price")

barplot(table(data$price, data$depth), 
        beside = TRUE, 
        col = c("blue", "black"), 
        main = "Diamond Price based on depth", 
        xlab = "depth", ylab = "Price")

barplot(table(data$price, data$x), 
        beside = TRUE, 
        col = c("blue", "black"), 
        main = "Diamond Price based on x", 
        xlab = "x", ylab = "Price")

barplot(table(data$price, data$y), 
        beside = TRUE, 
        col = c("blue", "black"), 
        main = "Diamond Price based on y", 
        xlab = "y", ylab = "Price")

barplot(table(data$price, data$z), 
        beside = TRUE, 
        col = c("blue", "black"), 
        main = "Diamond Price based on z", 
        xlab = "z", ylab = "Price")

#Summary
describe(data)

#Standart Deviation
sapply(data, function(data) sd(data, na.rm = TRUE))
#Variation
sapply(data, function(data) var(data, na.rm = TRUE))
#InterQuartile
sapply(data, function(data) IQR(data, na.rm = TRUE))
#Range
sapply(data, function(data) max(data, na.rm = TRUE) - min(data, na.rm = TRUE))

boxplot(data$Year, main = "Box Plot")
boxplot(data$Mo, main = "Box Plot")
boxplot(data$Dy, main = "Box Plot")
boxplot(data$HR, main = "Box Plot")
boxplot(data$Mn, main = "Box Plot")
boxplot(data$TsunamiNanCauseNanCode, main = "Box Plot")
boxplot(data$EarthquakeNanMagnitude, main = "Box Plot")
boxplot(data$Vol, main = "Box Plot")
boxplot(data$Deposits, main = "Box Plot")
boxplot(data$Deaths , main = "Box Plot")
boxplot(data$MaximumNanWaterNanHeightNan.m. , main = "Box Plot")
boxplot(data$TsunamiNanIntensity , main = "Box Plot")
boxplot(data$DamageNanDescription , main = "Box Plot")
boxplot(data$HousesNanDamaged , main = "Box Plot")
boxplot(data$TotalNanHousesNanDestroyed , main = "Box Plot")

    