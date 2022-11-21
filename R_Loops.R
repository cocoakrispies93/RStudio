
# Whitney May (I prefer going by Shane)
# 9/9/22, Updated for R lab 4: 9/23/22, submitted Oct 9th

# Must describes the purpose for each code block (5%)

# Find a company that sells products related to your field of study. 
# Select more than five different products as well as their 
# price and quantity sold in 2017. E.g. 2500 clients (quantity) 
# received the 30-year mortgage (product name) at an interest 
# rate of 5% (price) in 2017.  You can make up your data as long 
# as your data looks real. 

# Use vector(s) and loop(s). (20%)

# two vectors of prices and copies sold, rounded to millions
prices <- c(60, 70, 50, 40, 30, 35)
copies <- c(10, 17, 20, 6, 6, 6)

prices_copies_df = data.frame(prices, copies)

# game titles vector
game_titles <- c("Ghost of Tsushima", "Elden Ring", "Horizon", "Nioh", 
                 "XCOM 2", "MGSV: Phantom Pain")

# changing row names of data frame
rownames(prices_copies_df) <- c("Ghost of Tsushima", "Elden Ring", "Horizon", "Nioh", 
                                "XCOM 2", "MGSV: Phantom Pain")

# printing a data frame for presenting the table
print(prices_copies_df)

# two vectors: prices and quantity sold (copies)
prices <- array(60, 70, 50, 40, 30, 35)
copies <- array(10, 17, 20, 6, 6, 6)

# built-in R func sum() for sum of prices and copies
sum_prices = sum(prices)
sum_copies = sum(copies)

cat("Sum of all prices is: $", sum_prices)
cat("Total quantity: ", sum_copies)

# uses built in R func mean() for avg price
avg_price = mean(prices)
cat("Average price: $", avg_price)


# using a for loop
i = 1
above_avg_price = 0
total_income_high_prices = 0
for(p in prices){
  if (p > 47.5){ #checks if price is above avg ($47.50)
    cat ("Price above average: $", p, "\n")
    above_avg_price = above_avg_price + copies[i] # = price in vector
    total_income_high_prices = total_income_high_prices + (p * copies[i]) 
    i = i + 1   # ^ sum of price * quantity of copies sold
  }
}

# calculated in above for-loop
cat ("The total number of products sold above the average price of $47.50 is: ", 
     above_avg_price, "million copies") 

# calculated in above for-loop
cat ("The total income of products sold above the average price of $47.50 is: $", 
     total_income_high_prices/1000, "billion dollars")


# totals for all games
i = 1
rev = 0
total_sales = 0
for(g in game_titles){
  rev =  prices[i] * copies[i] # using for loop for each price * quantity
  cat("In millions, the individual revenue of ", 
       g, "is ---> $", rev, "\n") # g is game title vector
  i = i + 1
  total_sales = total_sales + rev # summing total sales
}

# calculated in above for-loop
cat("The total income of all games combined is: $", total_sales/1000, "billion dollars")
















