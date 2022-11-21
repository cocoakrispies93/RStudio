

#A survey of 500 males and 700 females showed that 132 males and 226 females 
#agreed with a particular statement. Use this information to calcualte the 
#proportions of males and females that agreed with the statement. This will 
#give you the values for p_1 and p_2. Use this to calculate q_1 and q_2. Now 
#calculate the standard error of the difference between two independent 
#proportions. Then determine the confidence interval for the difference between 
#two independent proportions for the 95 confidence level.

# .m = male and .f = female and .d means difference
# qnorm calculates 1.96 to a more precise decimal point

print("Proportion of Yes votes from the men's test:")
P.m = 132/500;  P.m
#[1] 0.264

print("Standard Error for Men:")
SE.m = sqrt(P.m*(1-P.m)/500)
SE.m

print("Confidence Interval for Men:")
CI.m = P.m + qnorm(c(.025,.975))*SE.m
CI.m
#[1] 0.225363 0.302637

print("Proportion of Yes votes from the women's test:")
P.f = 226/700; P.f
#[1] 0.3228571

print("Standard Error for Women:")
SE.f = sqrt(P.f*(1-P.f)/700);  P.f
SE.f

print("Confidence Interval for Women:")
CI.f = P.f + qnorm(c(.025,.975))*SE.f
CI.f
#[1] 0.2882198 0.3574945

print("Standard Error Difference Between Men and Women:")
SE.d = sqrt(SE.f^2 + SE.m^2);  SE.d
#[1] 0.02647495

print("Confidence Interval Difference Between Men and Women:")
#95% confidence interval for the difference
P.f-P.m + qnorm(c(.025,.975))*SE.d
#[1] 0.006967198 0.110747087






