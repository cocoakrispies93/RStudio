% Whitney May (I prefer going by Shane)
% Started 9/21/22, completed 9/29/22


% Lab 3
% Using the example distance chart these are several calculations 
% that can be solved based on the chart,  e.g. finding the 
% total distance of a person who travelled on a trip with n 
% stops; finding the total cost of all trips in the last month 

% My calculations are minimum and maximum distance based on a 3-row sample
% Converting the 3 row sample into miles from km
% Determining from the sample which flights are below 100 km, not counting
% zero values, with a hypothetical constraint of 100 miles being the max
% distance, for cost or time.

% Use matrix. (20’)
% Has a user-defined functions at the bottom, under100() and toMiles() 




%--------------------------------------------------------------------
%                Lab 3 - Matrices and Functions
%--------------------------------------------------------------------


% These lines represent the first three rows and all columns of the example
brantford_windsor = [0 234 70 149 155 144 39 39 91 112 130 70 116 186 262 53 119 161 200 108 112 302 95 277];
collingwood_windsor = [234 0 110 282 176 220 200 140 310 256 262 169 75 58 477 273 336 409 107 240 325 170 145 487];
elora_windsor = [70 110 0 208 128 149 76 30 136 177 153 94 42 121 303 114 164 239 125 131 151 224 114 317];

% dmatrix puts first three rows into a matrix
dmatrix = [brantford_windsor; collingwood_windsor; elora_windsor];

% vmatrix puts the matrix in a vertical state, vertical-matrix
vmatrix = dmatrix(:);

% zero matrix just converts the vertical matrix into a non-zero val matrix
zeromatrix = nonzeros(vmatrix);
% dmin and dmax use the built-in functions in matlab this time
% the previous lab 2 was specifically about loops so I did these manually
dmin = min(zeromatrix);
dmax = max(vmatrix);


% here I'm just adding two spaces for presentation
disp(newline + newline);

% these locations will be used for the following for-loop
locations100 = ["Brantford to Elora", "Brantford to Oakville", "Brantford to Hamilton", "Elora to Hamilton", ...
    "Collingwood to Kitchener-Waterloo", "Elora to Kitchener-Waterloo", "Brantford to London", "Elora to Brantford", ...
    "Elora to Oakville", "Collingwood to Orangeville", "Elora to Orangeville", "Collingwood to Owen Sound", ...
    "Brantford to Port Dover", "Brantford to Toronto"];




% this for loop determines if in the non-zero matrix a distance is shorter
% than 100 km, and sets d100 as a temp var just to output in a print
% statement the distance, but is replaced by the user-defined function
% called under100
% x = 1 and x = x + 1 are for loop iteration, and start at 1 instead of 0
%d100 = 0;
%x = 1;
%for i=1:length(zeromatrix);...
%    if zeromatrix(i) <= 100 
%        d100 = zeromatrix(i); 
%        disp("The distance of: " + d100 + " which is: " + locations100(x) +  ...
%            ", is less than 100 km!");
%        x = x + 1;
%    end
%end


% this for loop determines if in the non-zero matrix a distance is shorter
% than 100 km, and calls the user-defined function under100
d100 = 0;
x = 1;
for i=1:length(zeromatrix);...
    d100 = under100(zeromatrix(i));
    if d100 > 0         
        disp("The distance of: " + d100 + " which is: " + locations100(x) +  ...
            ", is less than 100 km!");
        x = x + 1;
    end
end

% displays the minimum distance in a print statement
disp(newline + "The minimum distance (besides 0 staying at the same location) is "...
    + dmin + " which is Elora to Kitchener-Wate!");

% displays the max distance in a print statement
disp(newline + "The maximum distance is: " + dmax+ " which is Collingwood to Toronto!");

% this converts all values to 0.6 of the value, converting km to miles and
% then it displays the values rounded closest integer
disp(newline + "All distances converted to rounded miles are: " + newline);
disp(toMiles(dmatrix));


%--------------------------------------------------------------------
%                       Functions section
%--------------------------------------------------------------------


% user defined function called under100, with the output of location being
% either the under 100 km distance or the location output var is set equal
% to 0
function location = under100(num)
        if num <= 100 
            location = num;
        else
            location = 0;
        end       
end



% user defined function called toMiles, converts the km matrix into a miles
% matrix by multiplying each value times 0.6
function miles_matrix = toMiles(matrix_var)
    miles_matrix = round(matrix_var*0.6);     
end



