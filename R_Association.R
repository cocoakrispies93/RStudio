
#--------------------------------------------
#title:  Lab 7: R Association Rules
#author: Whitney May
#date:   10/29/22
#output: Word file, will export as R markdown
#--------------------------------------------

#importing libraries
library(readr)
library(writexl)
library(readxl)
library(stringr)
library(arules)
library(arules)
library(tidyverse)
library(dplyr)
library(plyr)
library(reshape2)
library(discretization)
library(ggplot2)
library(ggpubr)

#downloading from the web data.cdc.gov with api 
covid <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD")
dim(covid) #dimensions of dataset (over 50k rows)
summary(covid) #brief overview
head(covid)
#covid #displays the original dataset


#-----------------------------------------------
#            Info on the dataset
#-----------------------------------------------

# I'm revisiting this dataset from Lab 5: datasets, this
# is realtime Covid-19 statistics coming live from aggregated
# government databases based on the CDC, WHO, and other credible
# health organizations.This association analysis is open-ended, 
# so I'm not sure what I'll find, but the data is based on states
# so I'll look for geographic associations. Instead of using aRules
# which is an association rules package, I'm using a dataset based
# on covid which may not have as much obvious material for associations, 
# since it's real raw data and not intended for learning purposes on
# association rules. 

# Key terms/abbreviations for original dataset: 
# tot = total, conf = confirmed, pnew = new probable cases

# There are two separate dataframe subsets, one for deaths and one for cases.

# Here is a snapshot of the Cases Data Frame:

#    State Total Cases Conf Cases Prob Cases New Cases Prob New Cases
#1      KS      297229     241035      56194         0              0
#2      ND      163565     135705      27860       589            220
#3      AS          11         NA         NA         0              0
#4      AL      841461     620483     220978       703            357
#5      AK      251425         NA         NA         0              0
#6     RMI           0          0          0         0              0
#7      ND         173         NA         NA        14             NA
#8      PR      173967     144788      29179       667            274
#9      PW           0         NA         NA         0              0
#10     NM      602931         NA         NA      1509              0


# Here is a snapshot of the Deaths Data Frame:

#    State Total Death Conf Death Prob Death New Death Prob New Death
#1      KS        4851         NA         NA         0              0
#2      ND        1907         NA         NA         9              0
#3      AS           0         NA         NA         0              0
#4      AL       16377      12727       3650         7              3
#5      AK        1252         NA         NA         0              0
#6     RMI           0          0          0         0              0
#7      ND           3         NA         NA         0             NA
#8      PR        2911       2482        429         8              3
#9      PW           0         NA         NA         0              0
#10     NM        8318         NA         NA         6              0


#------------------------------------------------------
#    Previous lab: Calculating Z Scores to add a column
#------------------------------------------------------

# Z score = (x - m)/sdev 
# mean() -> mean/avg of sample, in summary(iris)
# sd() -> sdev = standard deviation



#-------------------------------------------------
#     subsetting to just cases data frame
#-------------------------------------------------

covid.state <-covid$state
#covid.state

covid.tot_cases <-covid$tot_cases
#covid.tot_cases

covid.conf_cases <-covid$conf_cases
#covid.conf_cases

covid.prob_cases <-covid$prob_cases
#covid.prob_cases

covid.new_case <-covid$new_case
#covid.new_case

covid.pnew_case <-covid$pnew_case
#covid.pnew_case

cases_df = data.frame(covid.state, covid.tot_cases, covid.conf_cases, covid.prob_cases,
                      covid.new_case, covid.pnew_case)

cases_sub = data.frame(covid.state, covid.new_case)

#column titles
colnames(cases_df) <- c("State", "Total Cases", "Conf Cases", "Prob Cases",
                        "New Cases", "Prob New Cases")

#print(cases_df)




#-------------------------------------------------
#     subsetting to just deaths data frame
#-------------------------------------------------

covid.state <-covid$state
#covid.state

covid.tot_death <-covid$tot_death
#covid.tot_death

covid.conf_death <-covid$conf_death
#covid.conf_death

covid.prob_death <-covid$prob_death
#covid.prob_death

covid.new_death <-covid$new_death
#covid.new_death

covid.pnew_death <-covid$pnew_death
#covid.pnew_death

death_df = data.frame(covid.state, covid.tot_death, covid.conf_death, covid.prob_death,
                      covid.new_death, covid.pnew_death)


deaths_sub = data.frame(covid.state, covid.new_death)

#column titles
colnames(death_df) <- c("State", "Total Death", "Conf Death", "Prob Death",
                        "New Death", "Prob New Death")

#print(death_df)




#-----------------------------------------------
#         Z-Score of Total Cases
#-----------------------------------------------

caseZ <- function(cnc)  #covid new cases = cnc
{
  meanCases = mean(cnc) #mean
  casesSTD = sd(cnc) #std deviation 
  #meanCases
  
  zCases = (cnc - meanCases) / casesSTD #z score
  #zCases 
  
  return (round(zCases, digits = 3))  #rounding to 3 decimal places
}



#-----------------------------------------------
#         Z-Score of Total Deaths
#-----------------------------------------------

deathZ <- function(cnd)  #covid-new-deaths = cnd
{  
  meanDeaths = mean(cnd) #mean
  deathSTD = sd(cnd) #std deviation
  #meanDeaths
  
  zDeaths = (cnd - meanDeaths) / deathSTD  #z score
  #zDeaths
  
  return (round(zDeaths, digits = 3)) #rounding to 3 decimal places
}


cases_df$Case_Z <- caseZ(covid.new_case) #adding new columns with function
death_df$Death_Z <- deathZ(covid.new_death) #adding new columns with function
#print(cases_df)
#print(death_df)

covid$Death_Z <- deathZ(covid.new_death) #adding new columns with function

#exporting csv files, the original file is a web link to download csv
#write.csv(cases_df, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\cases_file.csv")
#write.csv(death_df, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\deaths_file.csv")
#write.csv(covid, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\covid_file.csv")








#-------------------------------------------------
#     Beginning of Lab 7: Association Rules
#-------------------------------------------------

#cases_df
#death_df

#deaths_sub = data.frame(covid.state, covid.conf_death, covid.new_death)
#cases_sub = data.frame(covid.state, covid.conf_cases,covid.new_case)



# cleaning the data into only complete observations
complete_cases <- cases_sub[complete.cases(cases_sub), ]
complete_deaths <- deaths_sub[complete.cases(deaths_sub), ]

# cleaning the data into only complete observations
subset_cases <- cases_sub[complete.cases(cases_sub), ]
subset_deaths <- deaths_sub[complete.cases(deaths_sub), ]

#column titles
colnames(subset_cases) <- c("State", "New Cases")

#column titles
colnames(subset_deaths) <- c("State", "New Death")

head(subset_cases)
head(subset_deaths)


#exporting csv files, the original file is a web link to download csv
#write.csv(complete_cases, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\cases_file.csv")
#write.csv(complete_cases, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\deaths_file.csv")
#write.csv(covid, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\covid_file.csv")

write.csv(subset_cases, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\cases_file.csv", row.names=FALSE)
write.csv(subset_deaths, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\deaths_file.csv", row.names=FALSE)


cases_trans = read.transactions("C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\cases_file.csv", 
                                #Format="Single",
                                sep=",",
                                #cols=c("State","New Cases"),
                                rm.duplicates = F
                                )
inspect(cases_trans)
# cols=c("State","Total Cases", "Conf Cases","Prob Cases", "New Cases", "Prob New Cases","Case_Z"),



deaths_trans = read.transactions("C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\deaths_file.csv", 
                                 #Format="Single",
                                 sep=",",
                                 #cols=c("State","New Death"),
                                 rm.duplicates = F
                                 )
inspect(deaths_trans)

# cols=c("State","Total Death", "Conf Death","Prob Death", "New Death", "Prob New Death","Death_Z"),


#cases_rules1 <- apriori(cases_disc, parameter = list(support = 0.01, confidence = 0.5))
cases_rules <- apriori(cases_trans, parameter = list(supp = 0.1, conf = 0.8, target="rules", minlen=1))

#deaths_rules1 <- apriori(deaths_disc, parameter = list(support = 0.01, confidence = 0.5))
deaths_rules <- apriori(deaths_trans, parameter = list(supp = 0.1, conf = 0.8, target="rules", minlen=1))










states_trans_cases <- strsplit(as.character(complete_cases$State), ',', fixed=T) # split by comma    
complete_cases_trans <- as(states_trans_cases, complete_cases$"New Cases", "transactions")
inspect(complete_cases_trans)

states_trans_deaths <- strsplit(as.character(complete_deaths$State), ',',fixed=T) # split by comma    
complete_deaths_trans <- as(states_trans_deaths, complete_deaths$"New Death", "transactions")
inspect(complete_deaths_trans)


# Read that csv back into transactions!!
complete_cases_trans <- read.transactions(
  file = "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\cases_file.csv",
  format = "single",
  sep = ",",
  cols=c("State","Conf Cases"),
  rm.duplicates = T
)
summary(complete_cases_trans)
# cols=c("State","Total Cases", "Conf Cases","Prob Cases", "New Cases", "Prob New Cases","Case_Z"),


complete_deaths_trans <- read.transactions(
  file = "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\deaths_file.csv",
  format = "single",
  sep = ",",
  cols=c("State","Conf Death"),
  rm.duplicates = T
)
summary(complete_deaths_trans)
# cols=c("State","Total Death", "Conf Death","Prob Death", "New Death", "Prob New Death","Death_Z"),


#cases_rules1 <- apriori(cases_disc, parameter = list(support = 0.01, confidence = 0.5))
cases_rules <- apriori(complete_cases_trans, parameter = list(supp = 0.3, conf = 0.3, target="rules", minlen=2))

#deaths_rules1 <- apriori(deaths_disc, parameter = list(support = 0.01, confidence = 0.5))
deaths_rules <- apriori(complete_deaths_trans, parameter = list(supp = 0.3, conf = 0.3, target="rules", minlen=2))


#head(complete_cases)
#head(complete_deaths)

# select variables v1, v2, v3
northern_states <- c("MI", "MT", "ND", "MN", "WI")
southern_states <- c("AZ", "NM", "TX", "LA", "MS")

northern_cases <- complete_cases[ which(complete_cases$State==northern_states), ]
northern_deaths <- complete_deaths[ which(complete_deaths$State==northern_states), ]
southern_cases <- complete_cases[ which(complete_cases$State==southern_states), ]
southern_deaths <- complete_deaths[ which(complete_deaths$State==southern_states), ]

#head(northern_cases,50)
WI_north_cases <- complete_cases[ which(complete_cases$State=="WI"), ]
#head(WI_north_cases, 50)
MI_north_cases <- complete_cases[ which(complete_cases$State=="MI"), ]
#head(MI_north_cases, 50)
MT_north_cases <- complete_cases[ which(complete_cases$State=="MT"), ]
#head(MT_north_cases, 50)
MN_north_cases <- complete_cases[ which(complete_cases$State=="MN"), ]
#head(MN_north_cases, 50)
ND_north_cases <- complete_cases[ which(complete_cases$State=="ND"), ]
#head(ND_north_cases, 50)


#head(northern_deaths,50)
WI_north_deaths <- complete_deaths[ which(complete_deaths$State=="WI"), ]
#head(WI_north_deaths, 50)
MI_north_deaths <- complete_deaths[ which(complete_deaths$State=="MI"), ]
#head(MI_north_deaths, 50)
MT_north_deaths <- complete_deaths[ which(complete_deaths$State=="MT"), ]
#head(MT_north_deaths, 50)
MN_north_deaths <- complete_deaths[ which(complete_deaths$State=="MN"), ]
#head(MN_north_deaths, 50)
ND_north_deaths <- complete_deaths[ which(complete_deaths$State=="ND"), ]
#head(ND_north_deaths, 50)

#head(southern_cases,50)
AZ_south_cases <- complete_cases[ which(complete_cases$State=="AZ"), ]
#head(AZ_south_cases, 50)
NM_south_cases <- complete_cases[ which(complete_cases$State=="NM"), ]
#head(NM_south_cases, 50)
TX_south_cases <- complete_cases[ which(complete_cases$State=="TX"), ]
#head(TX_south_cases, 50)
LA_south_cases <- complete_cases[ which(complete_cases$State=="LA"), ]
#head(LA_south_cases, 50)
MS_south_cases <- complete_cases[ which(complete_cases$State=="MS"), ]
#head(MS_south_cases, 50)

#head(southern_deaths,50)
AZ_south_deaths <- complete_deaths[ which(complete_deaths$State=="AZ"), ]
#head(AZ_south_deaths, 50)
NM_south_deaths <- complete_deaths[ which(complete_deaths$State=="NM"), ]
#head(NM_south_deaths, 50)
TX_south_deaths <- complete_deaths[ which(complete_deaths$State=="TX"), ]
#head(TX_south_deaths, 50)
LA_south_deaths <- complete_deaths[ which(complete_deaths$State=="LA"), ]
#head(LA_south_deaths, 50)
MS_south_deaths <- complete_deaths[ which(complete_deaths$State=="MS"), ]
#head(MS_south_deaths, 50)


#summary(northern_cases)
#summary(northern_deaths)
#summary(southern_cases)
#summary(southern_deaths)


#-------------------
#     DataFrame:
#-------------------

south_deaths_Z <- data.frame(southern_deaths$State, southern_deaths$Death_Z)
south_cases_Z <- data.frame(southern_cases$State, southern_cases$Case_Z)
north_deaths_Z <- data.frame(northern_deaths$State, northern_deaths$Death_Z)
north_cases_Z <- data.frame(northern_cases$State, northern_cases$Case_Z)

#-------------------
#     GGPlot:
#-------------------

bpSouthDeathsZ = ggplot(south_deaths_Z, aes(x=factor(southern_deaths$State), y=southern_deaths$Death_Z, 
                                            fill = southern_deaths$State)) + geom_boxplot()  

bpSouthDeathsZ 


bpSouthCasesZ = ggplot(south_cases_Z, aes(x=factor(southern_cases$State), y=southern_cases$Case_Z, 
                                            fill = southern_cases$State)) + geom_boxplot()  

bpSouthCasesZ




bpNorthDeathsZ = ggplot(north_deaths_Z, aes(x=factor(northern_deaths$State), y=northern_deaths$Death_Z, 
                                            fill = northern_deaths$State)) + geom_boxplot()  

bpNorthDeathsZ 


bpNorthCasesZ = ggplot(north_cases_Z, aes(x=factor(northern_cases$State), y=northern_cases$Case_Z, 
                                          fill = northern_cases$State)) + geom_boxplot()  

bpNorthCasesZ



#------------------------------------------------------
#            Merge the 4 Box Plots
#------------------------------------------------------


allBoxPlots <- ggarrange(bpSouthDeathsZ, bpSouthCasesZ, bpNorthDeathsZ, bpNorthCasesZ,
                         labels = c("Southern Deaths", "Southern Cases", 
                                    "Northern Deaths", "Northern Cases"),
                         ncol = 2, nrow = 2)
allBoxPlots


#---------------------------------
#   Association Rules
#---------------------------------

## choose a sample, calculate affinities
nrow(complete_cases)
nrow(complete_deaths)

cases_samp <- complete_cases[sample(nrow(complete_cases), 5000, replace = FALSE, prob = NULL),]
#cases_samp <- sample(cases_df, 500)
head(cases_samp)

deaths_samp <- complete_deaths[sample(nrow(complete_deaths), 5000, replace = FALSE, prob = NULL),]
#deaths_samp <- sample(death_df, 500)
head(deaths_samp)

#cases_disc <- discretizeDF(State ~ ., cases_samp)
#deaths_disc <- discretizeDF(State ~ ., deaths_samp)
#cases_disc <- discretizeDF(cases_samp$State ~ ., cases_samp)
#deaths_disc <- discretizeDF(deaths_samp$State ~ ., deaths_samp)

#cases_disc <- mdlp(cases_samp$State ~ ., cases_samp)
#deaths_disc <- mdlp(deaths_samp$State ~ ., deaths_samp)

#cases_disc <- mdlp(cases_samp ~ .)
#deaths_disc <- mdlp(deaths_sam ~ .)

#typeof(cases_disc)
#typeof(deaths_disc)

#head(cases_disc)
#head(deaths_disc)


groups_cases <- cases_samp %>%
  group_by(State) %>%
  dplyr::select("State", "Total Cases", "New Cases", Case_Z) %>%
  data.frame()


groups_death <- cases_samp %>%
  group_by(State) %>%
  dplyr::select("State", "Total Death", "New Death", Death_Z) %>%
  data.frame()

typeof(groups_cases)
typeof(groups_death)

head(groups_cases)
head(groups_death)

cases_trans <- as(split(groups_cases[,"Total Cases"], grouping_for_AA[,State]), "transactions")
deaths_trans <- as(split(groups_death[,"Total Death"], grouping_for_AA[,State]), "transactions")

inspect(cases_trans)
inspect(deaths_trans)

#cases_rules1 <- apriori(cases_disc, parameter = list(support = 0.01, confidence = 0.5))
cases_rules <- apriori(complete_cases_trans, parameter = list(supp = 0.3, conf = 0.3, target="rules", minlen=2))

#deaths_rules1 <- apriori(deaths_disc, parameter = list(support = 0.01, confidence = 0.5))
deaths_rules <- apriori(complete_deaths_trans, parameter = list(supp = 0.3, conf = 0.3, target="rules", minlen=2))




