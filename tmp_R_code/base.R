library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)
library(lubridate)
library(carData)
library("ggpubr")
library(caret)

########################### Input dataset ##########################

tedtalks<-read.csv("data.csv")

#Print 6 first line of dataset
head(tedtalks)

#Print columns name of dataset
colnames(tedtalks)

########################### Cleaning data ##########################

#Getting info about the missing data
colSums(is.na(tedtalks))

#There is no NA values, no cleaning needed.


######################## Data transformation ########################

#Changing "date" column to "year" column.
tedtalks$month <- substr(tedtalks$date,0, nchar(tedtalks$date)-5)
tedtalks$date = as.numeric(substr(tedtalks$date,nchar(tedtalks$date)-4, nchar(tedtalks$date)))
names(tedtalks)[names(tedtalks) == "date"] <- "year"

#Filtering the data frame to do statistic only in a twenty-year period from 2002 to 2021
tedtalks = subset(tedtalks, year >= 2002 & year <= 2021) 

#Add new column "quarter" to the data frame
tedtalks = tedtalks %>%
  mutate(quarter = case_when(
    month == "January" | month == "February" | month == "March"  ~ "Q1",
    month == "April" | month == "May" | month == "June"  ~ "Q2",
    month == "July" | month == "August" | month == "September"  ~ "Q3",
    month == "October" | month == "November" | month == "December"  ~ "Q4"
  ))

head(tedtalks,3)

######################## Some basic statistic ##########################
#Check for missing values 
colSums(is.na(tedtalks))

#Basic statistical info of the dataset.
summary(tedtalks)

#Count unique values in each column
sapply(tedtalks, n_distinct)

#### Using some tests to verify the difference in Views between quarters ####

#Data transformation

#set quarter as factor
tedtalks$quarter = as.factor(tedtalks$quarter)

head(tedtalks,3)

levels(tedtalks$quarter)

#Descriptive statistics and graph:

group_by(tedtalks, quarter) %>%
  summarise(
    count = n(),
    mean = mean(views, na.rm = TRUE),
    sd = sd(views, na.rm = TRUE)
  )

#Draw box graph for quarter and views
ggboxplot(tedtalks, x = "quarter", y = "views", 
          color = "quarter",
          order = c("Q1", "Q2", "Q3", "Q4"),
          ylab = "Views", xlab = "Quarter")

#Check the homogeneity of variance with outlier
leveneTest(views ~ quarter, data = tedtalks)

#Since the variance across groups are non-homogeneous with outlier in the dataset, we will remove outlier and check again

dim(tedtalks) #Check data frame before removing outliers

#Removing outlier using Interquartile range method:
quartiles <- quantile(tedtalks$views, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(tedtalks$views)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

tedtalks_no_outlier <- subset(tedtalks, tedtalks$views > Lower & tedtalks$views < Upper)

dim(tedtalks_no_outlier) #Check data frame after removing outliers 

#Check the homogeneity of variance without outlier
leveneTest(views ~ quarter, data = tedtalks_no_outlier)

#Draw box graph for quarter and views after removing outlier 
ggboxplot(tedtalks_no_outlier, x = "quarter", y = "views",
          add = c("mean"),
          color = "quarter",
          order = c("Q1", "Q2", "Q3", "Q4"),
          ylab = "Views", xlab = "Quarter")

ggline(tedtalks_no_outlier, x = "quarter", y = "views", 
       add = c("mean_se"), 
       order = c("Q1", "Q2", "Q3", "Q4"),
       ylab = "Views", xlab = "Quarter")

#Pre-check normality or the data frame corresponding to each quarter
qplot(sample = views, data = tedtalks_no_outlier, facets = ~ quarter)

############ ANOVA ############
# Compute the analysis of variance
res.aov <- aov(views ~ quarter, data = tedtalks_no_outlier)
# Summary of the analysis
summary(res.aov)

#Compute Tukey HSD (Tukey Honest Significant Differences, R function: TukeyHSD()) 
#for performing multiple pairwise-comparison between the means of groups
TukeyHSD(res.aov)

#Check the normality assumption
plot(res.aov, 2)

#Using Shapiro-Wilk test on the ANOVA residuals
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

#ANOVA assumptions are not met, alternatively using Kruskal-Wallis rank sum test
kruskal.test(views ~ quarter, data = tedtalks_no_outlier)

#Calculate pairwise comparisons between group levels with corrections for multiple testing
pairwise.wilcox.test(tedtalks_no_outlier$views, tedtalks_no_outlier$quarter,
                     p.adjust.method = "BH")


###### Linear regression models to predict views based on likes ######

#Linear relationship between views and likes
ggplot(tedtalks, aes(x = likes, y = views)) +
  geom_point() +
  stat_smooth()

#Split the data set into train set and test set
set.seed(123)
training.samples <- tedtalks$views %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- tedtalks[training.samples, ]
test.data <- tedtalks[-training.samples, ]

#Build the model
model <- lm(views ~ likes, data = train.data)

#Summarize the model
summary(model)
#Calculate mean of views in train data set to calculate error rate
mean(train.data$views) 

# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
# (a) Prediction error, RMSE
RMSE(predictions, test.data$views)
# (b) R-square
R2(predictions, test.data$views)
#Calculate mean of views in test data set to calculate error rate
mean(test.data$views) 
