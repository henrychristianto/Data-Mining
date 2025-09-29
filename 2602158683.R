#Yonathan Henry Christianto - 2602158683
library(GGally)
library(psych)
library(caret)

data <- read.csv("E:/DataMining/UAS_2602158683/tsunamis-2023-09-11_22-13-51_ 0530 (2).csv", stringsAsFactors=T)

data1 <- data0 %>%
  select(Sr.no, Country, Year, Latitude, Longitude, EarthquakeNanMagnitude, MaximumNanWaterNanHeightNan.m.) %>%
  filter(MaximumNanWaterNanHeightNan.m. > 200)

data1 <- head(data, 200)
data1


#Check for duplicate data
duplicatecheck <- data[duplicated(data)]
duplicatecheck

#check missing data
sapply(data, function(x) sum(is.na(x)))

str(data)

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


#Distribution
ggplot(data, aes(x = TsunamiNanCauseNanCode)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "Tsunami Data", x = "Tsunami", y = "Amount")

ggplot(data, aes(x = EarthquakeNanMagnitude)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "EarthQuake Data", x = "Tsunami", y = "Amount")

ggplot(data, aes(x = Vol)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "Volume Data", x = "Tsunami", y = "Amount")

ggplot(data, aes(x = Deposits)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "Deposits Data", x = "Tsunami", y = "Amount")

ggplot(data, aes(x = Deaths)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Deaths Data", x = "Tsunami", y = "Amount")

ggplot(data, aes(x = MaximumNanWaterNanHeightNan.m.)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "MAx Height Data", x = "Tsunami", y = "Amount")

ggplot(data, aes(x = DamageNanDescription)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Damage Data", x = "Tsunami", y = "Amount")

ggplot(data, aes(x = HousesNanDamaged)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "House Damage Data", x = "Tsunami", y = "Amount")

ggplot(data, aes(x = TotalNanHousesNanDestroyed)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "House Destroyed Data", x = "Tsunami", y = "Amount")

#Relation
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$Year), 
        beside = TRUE, 
        col = c("cyan", "brown"), 
        main = "Maximum Height based on year", 
        xlab = "Year", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$Mo), 
        beside = TRUE, 
        col = c("brown", "cyan"), 
        main = "Maximum Height based on Month", 
        xlab = "Month", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$Dy), 
        beside = TRUE, 
        col = c("cyan", "brown"), 
        main = "Maximum Height based on Day", 
        xlab = "Day", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$HR), 
        beside = TRUE, 
        col = c("brown", "cyan"), 
        main = "Maximum Height based on Hour", 
        xlab = "Hour", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$Mn), 
        beside = TRUE, 
        col = c("cyan", "brown"), 
        main = "Minute based on Maximum Height", 
        xlab = "Minute", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$TsunamiNanCauseNanCode), 
        beside = TRUE, 
        col = c("brown", "cyan"), 
        main = "Tsunami based on Maximum Height", 
        xlab = "Tsunami", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$EarthquakeNanMagnitude), 
        beside = TRUE, 
        col = c("cyan", "brown"), 
        main = "EarthQuake based on Maximum Height ", 
        xlab = "EarthQuake", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$Vol), 
        beside = TRUE, 
        col = c("brown", "cyan"), 
        main = "volume based on Maximum Height", 
        xlab = "volume", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$Deposits), 
        beside = TRUE, 
        col = c("cyan", "brown"), 
        main = "deposits based on Maximum Height", 
        xlab = "deposits", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$Deaths), 
        beside = TRUE, 
        col = c("brown", "cyan"), 
        main = "deaths based on Maximum Height", 
        xlab = "deaths", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$TsunamiNanIntensity), 
        beside = TRUE, 
        col = c("cyan", "brown"), 
        main = "Intensity based on Maximum Height", 
        xlab = "Intensity", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$DamageNanDescription), 
        beside = TRUE, 
        col = c("brown", "cyan"), 
        main = "damage based on Maximum Height ", 
        xlab = "damage", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$HousesNanDamaged), 
        beside = TRUE, 
        col = c("cyan", "brown"), 
        main = "house damage  based on Maximum Height", 
        xlab = "house damage", ylab = "Max Height")
barplot(table(data$MaximumNanWaterNanHeightNan.m., data$TotalNanHousesNanDestroyed), 
        beside = TRUE, 
        col = c("brown", "cyan"), 
        main = "house destroyed based on Maximum Height ", 
        xlab = "house destroyed", ylab = "Max Height")

#Summary
describe(data)


#Regression

library(ggpubr)
shapiro.test(data$MaximumNanWaterNanHeightNan.m.) # p-value < 2.2e-16

#Correlation
cor1 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$Year, method="kendall")
cor2 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$Mo, method="kendall")
cor3 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$Dy, method="kendall")
cor5 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$Mn, method="kendall")
cor6 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$TsunamiNanCauseNanCode, method="kendall")
cor7 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$EarthquakeNanMagnitude, method="kendall")
cor8 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$Vol, method="kendall")
cor9 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$Deposits, method="kendall")
cor10 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$Deaths, method="kendall")
cor11 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$TotalNanHousesNanDestroyed, method="kendall")
cor12 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$TsunamiNanIntensity, method="kendall")
cor13 <- cor.test(data$MaximumNanWaterNanHeightNan.m., data$DamageNanDescription, method="kendall")


cor1  # -0.1921617 
cor2  # 0.02215212 
cor3  # -0.01624575 
cor5  # 0.03584021 
cor6  # 0.236971 
cor7  # 0.2335713 
cor8  # -0.2704351 
cor9  # 0.280003 
cor10 # 0.3440342  
cor11 # 0.1042575 
cor12 # 0.6194474  
cor13 # 0.3608906 

# cor 12 > cor 13 > cor 10 > cor 9 > cor 6 > cor 7 > cor 11 > cor 5 > cor 2 > cor 3 > cor 1 > cor 8

# Y = MaximumNanWaterNanHeightNan.m.
# X = TsunamiNanIntensity, DamageNanDescription, Deaths

model = lm(formula = data$MaximumNanWaterNanHeightNan.m. ~ data$TsunamiNanIntensity + 
             data$DamageNanDescription + data$Deaths)
summary(model)












