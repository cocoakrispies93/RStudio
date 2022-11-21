
% Whitney May (I prefer going by Shane)
% 9/9/22

% Must describes the purpose for each code block (5%)

%Find a company that sells products related to your field of study. 
% Select more than five different products as well as their 
% price and quantity sold in 2017. E.g. 2500 clients (quantity) 
% received the 30-year mortgage (product name) at an interest 
% rate of 5% (price) in 2017.  You can make up your data as long 
% as your data looks real. 

%Use vector(s) and loop(s). (20%)

n = newline;

game_titles = ["Ghost of Tsushima", "Elden Ring", "Horizon", "Nioh", ...
    "XCOM 2", "MGSV: Phantom Pain"];

% Price and quantity vectors, quantity by millions, so 10 is 10 million
% Rounded to the nearest int
ghost = [60, 10];
elden_ring = [70, 17];
horizon = [50, 20];
nioh = [40, 6];
xcom2 = [30, 6];
mgsv = [35, 6];

games = [ghost; elden_ring; horizon; nioh; xcom2; mgsv];

prices = [60, 70, 50, 40, 30, 35];
quantities = [10, 17, 20, 6, 6, 6];

disp("    Games                   Price   Quantity");

chart = [game_titles(1), ghost; game_titles(2), elden_ring; game_titles(3), horizon; ...
    game_titles(4), nioh; game_titles(5), xcom2; game_titles(6), mgsv];

disp(chart);


% p will be the sum of prices
p = 0;
for i = 1:length(prices); p = p + prices(i); end

% q will be the sum of quantities
q = 0;
for i = 1:length(quantities); q = q + quantities(i); end

disp("Total price: $" + p);
disp("Total quantity: " + q);

avg = round(p ./  length(prices), 2);
disp("Average price: $" + avg + "0");

i = 1;
high_prices_quantity = 0;
total_income_high_prices = 0;
while(prices(i) > 47.5); disp("Price above average: $" + prices(i)); ... 
        high_prices_quantity = high_prices_quantity + quantities(i); ... 
        total_income_high_prices = total_income_high_prices + (prices(i) .* quantities(i)); ...
        i = i + 1; end



total_sales = 0;
for i = 1:length(games); total_sales = total_sales + (prices(i) .* quantities(i)); end



disp("The total number of products sold above the average price of $47.50 is: " ...
    + high_prices_quantity + " million copies");

disp("The total income of products sold above the average price of $47.50 is: $" ...
    + (total_income_high_prices/1000) + " billion dollars");

% totals for all games
for i = 1:length(games); disp("In millions, the individual revenue of " + ...
        game_titles(i) + " is ---> $" + (prices(i) .* quantities(i))); end

disp("The total income of products combined is: $" + round((total_sales/1000), 3) + ...
    " billion dollars");




