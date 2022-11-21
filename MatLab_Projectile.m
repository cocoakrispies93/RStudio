
% Whitney May (I prefer going by Shane)
% Started 9/21/22, completed 9/29/22


% MatLab - Project One
% Find a complex problem in your major field of study and use MatLab to solve the problem. 
% Requirements:
% 1. The problem must be in your major field of study. (10%)
% 2. Use comments in the script file(s) to specify your name, major, the problem you are trying to 
% solve, and purpose for each code block. (10%)
% 3. The problem must be complex enough. Usually it means you should have more 50 lines of 
% code. (20%)
% 4. Must use user-defined functions.  (20%)
% 5. Program runs with correct output. (20%)
% 6. Must generate graphs as part of the output.  (10%)
% 7. Submitted a zipped file with all the script files. File name format is lastname_project1.zip. 
% (10%)

% My field is the general computer science/mathematics field, in the AMCS
% program at IUSB, I'll perform some Unity 3D velocity and game engine
% movement calculations


%--------------------------------------------------------------------
%                     MatLab - Project One
%--------------------------------------------------------------------

disp(newline);

% user input for the number of iterations of velocities/directions to test
% for the program 
response = input( ['How many velocities and directions would you like to' ...
    ' calculate for the game: '] ); % for the for-loop, user response


z = 1; % for matrix labels

vect1 = NaN(1,response);
vect2 = NaN(1,response);
vect3 = NaN(1,response);
vect4 = NaN(1,response);

% for loop 1 to the number given in response
for i = 1:response 

    disp(newline);
    in = input('Angle of projection in degrees: ' ); % angle of game object 
    t = input('Time in flight: ' ); % t time in seconds
    disp(newline);
    
    angle = in * pi / 180;     % convert to radians                 
    launch = 60;               % launch velocity                    
    g = 9.8;              % earth's gravity, constant  
    
    x = x_displacement(angle, t, launch);   % horizontal displacement            
    y = y_displacement(angle, t, launch, g); % vertical displacement              
    vx = x_velocity(angle, launch);                   % horizontal velocity                
    vy = y_velocity(angle, launch, g, t);          % vertical velocity   
    
    V = y_result(vx, vy);           % final velocity                 
    d = direction(vx, vy);   % direction at time t
    
    disp("Matrix #" + z);
    % A matrix of the results keeping track of the num of matrices
    results_matrix = [["horizontal displacement", vx]; ["vertical displacement",...
        vy]; ["resultant velocity", V]; ["direction at time t", d]];

    disp(results_matrix);

    % two vectors for the tables
    calculations = [vx; vy; V; d];
    labels = {'horizontal displacement'; 'vertical displacement';...
        'resultant velocity'; 'direction at time t'};
    
    % table for each iteration keeping track of the num of iterations
    disp("Table #" + z)
    tables = table(labels, calculations);
    disp(tables);
      
    %disp(newline + newline);
    
    %disp( ['horizontal displacement:  ' num2str(x)...
    %    '   vertical displacement:  ' num2str(y)] );                                  
    %disp( ['resultant velocity:  ' num2str(V)...
    %    '       direction at time t:  ' num2str(d)] ); 

    vect1(z) = V; % adding result velocities to the vect for the bar graphs
    vect2(z) = d; % adding direction to the vect for the bar graphs
    vect3(z) = t; % adding time to the vect for the bar graphs
    vect4(z) = in; % adding angles to the vect for the bar graphs

    z = z + 1; % for table/matrix labels

end

subplot(2,1,1),plot(vect);
legend('velocities');

z = z -1;
disp(newline + "Velocities: " + z);
disp(vect);

tiledlayout(2,2)
nexttile
bar(vect1,'FaceColor',[0 .5 .5],'EdgeColor',[0 .9 .9],'LineWidth',1.5)
title('Velocities')
nexttile
bar(vect2,'FaceColor',[0 .5 .5],'EdgeColor',[0 .9 .9],'LineWidth',1.5)
title('Direction')
nexttile
bar(vect3,'FaceColor',[0 .5 .5],'EdgeColor',[0 .9 .9],'LineWidth',1.5)
title('Time')
nexttile
bar(vect4,'FaceColor',[0 .5 .5],'EdgeColor',[0 .9 .9],'LineWidth',1.5)
title('Angles')



%--------------------------------------------------------------------
%                     Functions section
%--------------------------------------------------------------------


% user defined function called x_displacement, calculates the horizontal
% displacement which is the launch velocity * time * cos(angle)
function x = x_displacement(a, t, l)
    x = l * t * cos(a);     
end


% user defined function called y_displacement, calculates the vertical
% displacement: launch velocity*time*sin(angle)-half gravity*time squared
function y = y_displacement(a, t, l, g)
    y = l * t * sin(a) - 0.5 * g * t ^ 2;     
end


% user defined function called x_velocity, calculates the horizontal
% velocity: launch velocity*time*sin(angle)-half gravity*time squared
function vx = x_velocity(a, l)
    vx = l * cos(a);      
end


% user defined function called y_velocity, calculates the vertical
% velocity: launch velocity*sin(angle) - gravity*time
function vy = y_velocity(a, l, g, t)
    vy = l * sin(a) - g * t;      
end


% user defined function called y_displacement, calculates the resultant
% velocity: square root of horizontal + vertical velocities squared
function V = y_result(vx, vy)
    V = sqrt( vx^2 + vy^2 );     
end


% user defined function called y_displacement, calculates the resultant
% direction: 180/pi * atan2 of vertical and horizontal velocity
function d = direction(vx, vy)
    d = 180 / pi * atan2( vy, vx );     
end

