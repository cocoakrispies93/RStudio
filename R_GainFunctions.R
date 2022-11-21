


entropy_print <- function(class1, class2) {
  cat("Entropy: ", (-class1 * log2(class1)) - (class2 * log2(class2)), "\n")
}

splits_print <- function(class_yes, class_no) {
  cat("Splits possible: ", (-class_yes * log2(class_yes)) - 
        (class_no*log2(class_no)), "\n") 
}

entropy <- function(class1, class2) {
  return(-class1*log2(class1) - class2*log2(class2))
}

splits <- function(class_yes, class_no) {
  return(-class_yes*log2(class_yes) - class_no*log2(class_no)) 
}

#  -0.5*-1   -   0.5*-1  = 0.5 -(-0.5) = 1

InfoGain <- function(e, class1, class2, 
                     split1, split2) {
  cat("Information Gain: ", e - class1*split1 - class2*split2, "\n") 
}


class1 = 3/10 #yes
class2 = 7/10 #no

Entropy:  0.971

entropy_print(class1,class2)


lottery_yes_readYes = 0/3
lottery_yes_readNo = 3/3

lottery_no_readYes = 4/7
lottery_no_readNo = 3/7

split1 <- splits(lottery_yes_readYes, lottery_yes_readNo)
print(split1)
split2 <- splits(lottery_no_readYes, lottery_no_readNo)
print(split2)

entropy1 <- entropy(class1,class2)

InfoGain(0.971, 
         class1, class2, 
         0, split2)


entropy_print(class1,class2)
print(entropy1)


#Information Gain:  0.1916312 
#> 
#  > 
#  > entropy_print(class1,class2)
#Entropy:  0.8812909 
#> print(entropy1)
#[1] 0.8812909



