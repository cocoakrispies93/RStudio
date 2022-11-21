
#-------------------------------
#title:  Assignment 2: R Intro
#author: Whitney May
#date:   10/9/22
#-------------------------------

#importing libraries
library(readr)
library(writexl)
library(readxl)
library(stringr)
library(ggplot2)
library(dplyr)
#library(lessR)




#-----------------------------------------------
#               Question #1
#-----------------------------------------------

# stock price in US dollars, the daily closing price for two weeks:
stocks <- c(58, 57, 55, 52, 51, 46, 48, 49, 49, 51)

# mean of stock daily closing price for two weeks
mean_stocks = mean(stocks)
mean_stocks 
# 51.6

# sorting stocks in descending order
sorted_prices <- sort(stocks, decreasing = TRUE)
sorted_prices
# [1] 58 57 55 52 51 51 49 49 48 46


#-----------------------------------------------
#               Question #2
#-----------------------------------------------

converted_stocks =  round(stocks*0.88)
# output wihtout round = [1] 51.04 50.16 48.40 45.76 44.88 40.48 42.24 43.12 43.12 44.88
# round() rounds to nearest int, because round defaults to 0 decimal places
converted_stocks
# final output = [1] 51 50 48 46 45 40 42 43 43 45
difference_stocks <- stocks - converted_stocks
difference_stocks
# [1] 7 7 7 6 6 6 6 6 6 6



#-----------------------------------------------
#               Question #3
#-----------------------------------------------

#(4 points)
#You are interested in knowing:  
#  How many values are over 50 in US dollars?
#  How many values are less than 50 in Euros?

  
over_50 = which(stocks > 50)
under_50 = which(stocks < 50)
length(over_50) #6
length(under_50) #4



#-----------------------------------------------
#               Question #4
#-----------------------------------------------

#Now we want to see how many values are the same between US dollars and Euros. 
#Which formula would you use to find this out?
#  Are there any values that are the same between US dollars and Euros?
  
same_prices = which(stocks == converted_stocks)
same_prices
# no, integer(0), because the conversion rate is 0.88 so none are the same



#-----------------------------------------------
#               Question #5
#-----------------------------------------------

#(10 points)
#Show R code and output for each of the following questions.

#What code would you use to find if any values in the US stock prices are divisible by 4?
#  How many values are there and what are these values?
#  What code would you use to find if any values in the Euro stock prices are even values?
#  How many values are there and what are these values?

#  What code would you use to find out among those Euro stock prices:
#  If ANY of them are greater than 50?
#  If ALL of them are greater than 50?

any(stocks%%4 == 0) # True, some US stocks are divisible by 4
any(converted_stocks%%2 == 0) # True, some EU stocks are even

divisible4_stocks = which(stocks%%4 == 0)
even_stocksEU = which(converted_stocks%%2 == 0)

divisible4_stocks # US stocks[4] and stocks[7], 52 and 48
even_stocksEU # EU converted_stocks where i = 2 3 4 6 7

length(divisible4_stocks) # 2 values = num of US stocks %4 == 0
i = 1
for(x in divisible4_stocks){
  cat("US stocks that are divisible by 4 are: ", stocks[divisible4_stocks[i]],"\n")
  i = i + 1
}

length(even_stocksEU) # 5 values = num of EU stocks that are even
i = 1
for(x in even_stocksEU){
  cat("EU stocks that are even are: ", converted_stocks[even_stocksEU[i]],"\n")
  i = i + 1
}

any(converted_stocks > 50) # True, at least one/some EU stock price is > 50
all(converted_stocks > 50) # False, all EU stock prices must be > 50




#-----------------------------------------------
#               Question #6
#-----------------------------------------------

#  How many rows and columns are in your dataset? 
#  What is the name of the 4th column in the dataset? 
#  Show the summary statistics for the 4th column in the dataset 
#  (min, 1st quartile, median, mean mode, 3rd quartile, max). 

getwd()
setwd("C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\Data Mining\\HW2")
getwd()

# mcdonalds menu excel sheet 
menu <- read_excel("mcds.xlsx")
dim(menu) #dimensions of dataset 
summary(menu) #brief overview
head(menu) #brief overview
menu #displays the original dataset

nrow(menu) #260 rows
ncol(menu) #24 columns

(names(menu)[4]) # Calories
summary(menu$Calories) # summary of 4th column
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   210.0   340.0   368.3   500.0  1880.0 



#-----------------------------------------------
#               Question #7
#-----------------------------------------------

#Create a Boxplot showing the range of Calories by Category. 
#Add color to the boxplot. 
#What are one or two observations you see based on the data?

cal_range = data.frame(menu$Category, menu$Calories)
cal_range

ggplot(cal_range, aes(x=menu.Category, y=menu.Calories, 
                      fill = menu.Category)) + 
  geom_boxplot(alpha=0.3, outlier.colour="red", outlier.shape=10,
               outlier.size=2)  + theme_dark()


# Breakfast and chicken and fish have the highest outliers/anomalies
# Beverages have the lowest calorie range, and desserts are surprisingly low calorie
# compared to most other food categories




#-----------------------------------------------
#               Question #8
#-----------------------------------------------

#  Filter or subset the data by those items that are greater than 800 calories. 
#  How many items are greater than 800 calories?

#  There is variable called 'Category' in the dataset, 
      #which category names are listed in this dataset where you are 
      #only looking at those items greater than 800 calories?

#  Create a pie chart with these categories represented. 
      #Add a title and change at least one of the colors. 

# Subset DataFrame by column name Item and Calories
menu[menu$Calories > 800, c('Item','Calories')]
over800 <- subset(menu, menu$Calories > 800, select = c('Item','Calories'))
nrow(over800) # 12 Items are over 800 calories

# Subset DataFrame by column name Item, Calories, and Category
menu[menu$Calories > 800, c('Item','Calories', 'Category')]
over800_Cat <- subset(menu, menu$Calories > 800, select = c('Item','Calories','Category'))
over800_Cat
dist_over800_Cat <- distinct(over800_Cat, over800_Cat$Category) #Breakfast, Chicken & Fish, Smoothies & Shakes


over800_Cat
# Simple Pie Chart
slices <- c(4, 2, 6)
lbls <- c("Breakfast", "Chicken & Fish", "Smoothies & Shakes")
pie(slices, labels = lbls, main="Categories With Highest Calories", col=heat.colors(3)) 



