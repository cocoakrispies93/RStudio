
#-------------------------------
#title:  Lab 5: R Datasets
#author: Whitney May
#date:   10/9/22
#output: File of the same format
#-------------------------------

#importing libraries
library(readr)
library(writexl)
library(readxl)
library(stringr)

#downloading from the web data.cdc.gov with api 
covid <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD")
dim(covid) #dimensions of dataset (over 50k rows)
summary(covid) #brief overview
covid #displays the original dataset


#-----------------------------------------------
#            Info on the dataset
#-----------------------------------------------

# tot = total, conf = confirmed, pnew = new probable cases
# for deaths and cases



#-----------------------------------------------
#    Calculating Z Scores to add a column
#-----------------------------------------------

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

#column titles
colnames(cases_df) <- c("State", "Total Cases", "Conf Cases", "Prob Cases",
                                "New Cases", "Prob New Cases")

print(cases_df)




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

#column titles
colnames(death_df) <- c("State", "Total Death", "Conf Death", "Prob Death",
                        "New Death", "Prob New Death")

print(death_df)




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
print(cases_df)
print(death_df)

covid$Death_Z <- deathZ(covid.new_death) #adding new columns with function

#exporting csv files, the original file is a web link to download csv
write.csv(cases_df, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\cases_file.csv")
write.csv(death_df, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\deaths_file.csv")
write.csv(covid, "C:\\Users\\penng\\OneDrive\\Documents\\IUSB\\FA22\\App Computation\\Lab 5\\covid_file.csv")




