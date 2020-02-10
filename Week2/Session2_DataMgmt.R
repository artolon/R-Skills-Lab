

'# PROLOG   ################################################################'

'# PROJECT: R SKILL LAB SP2019: DATA MANAGEMENT #'
'# PURPOSE: IMPORTING, MERGING, AND MANIPULATING DATA #'
'#          dplyr, tidyverse, magrittr #'
'# DIR:     /Users/candice_wang/Box/Courses/2019 Spring/MyClass/Session 2 #'
'# DATA:    state-geocodes-v2016.csv # for regions & state geocodes (FIPS) #'
'#          R11484269_SL050.dta #  county census data #'
'#          R11484264_SL050.csv #  county health data #'
'#          R11484264_SL050_v2.csv #  county health data variable labels #'
'# AUTHOR:  Xiaoyan Wang, TODD COMBS  #'
'# CREATED: OCT 10, 2018 #'
'# LATEST:  JAN 19, 2019 #'
'# NOTES:    #'

'# PROLOG   ###############################################################'


# Working directory --------------------------------------------
setwd("C:/Users/artol/Documents/Sp19/R Skills Lab")


# LIBRARIES ---------------------------------------------------------------

#ONLY ONLY ONLY for first loading of a package, you must install

install.packages("labelled")
install.packages("magrittr")
install.packages("readxl")

#then load packages
library(tidyverse) # for ggplot2,tibble,tidyr,readr,purrr,dplyr,stringr,forcats
library(haven) # for reading Stata files
library(labelled) # for working with variable labels
library(magrittr) # for extra %$% pipe operator
library(readxl) #for reading the excel file

# READ IN DATA ------------------------------------------------------------

census <- read_dta("R11484269_SL050.dta") #  county census data
countyh <- read_csv("R11484264_SL050.csv", n_max = 200) #  county health data 
chlabels <- read_xlsx("R11484264_SL050_v2.xlsx", n_max = 1) #  county health data variable labels


#look at the two main datasets
View(census)
View(countyh)
View(chlabels)

# DATA CLEANING: chlabels -------------------------------------------------

#create new tibble for var names & labels for reference to help with county health data
View(chlabels)
vlb <- t(chlabels)   #transpose (this is a MATRIX operation)

View(vlb)

#create tibble of variables and variable labels
vlb <- tibble(var = as.character(vlb), varlabels= row.names(vlb))
View(vlb)
#now we have an extra tibble with the labels for the county health data


# DATA CLEANING: countyh --------------------------------------------------
summary(countyh)
    #this gives you the summary of what's in the data (descriptives)

#drop unused variables using select function
ch <- countyh %>%
  select(-c(SE_NV002_001:SE_T004_003, SE_T005_001, 
            SE_NV005_001:SE_T007_001, 
            SE_T008_001:SE_T008_004, SE_T009_002:SE_T010_003))
    #the (-) makes you NOT select something
    #we dropped 19 because the comma is x "through" y

#look; compare names of new county health data to original
names(ch)
names(countyh)


#use names of new county health data to select only those we 
#need in our variable label tibble (chlab)
chlab2 <- chlab %>%
  filter(var %in% names(ch)) # %in% finds matches in right side for left side
chlab2

#rename variables to descriptive names
#rename function (dplyr)
# NEWNAME = OLDNAME (always start with new names equals old names)
#if you do "names(ch)" you can see what all the labels are in the data set
ch2 <- ch %>%
  rename(countyFIPS = Geo_FIPS, stateFIPS = Geo_STATE,
         daysPhysUH = SE_T001_001, daysMentUH = SE_T001_002,
         fairPoorH = SE_T002_001, lowBwt = SE_T003_001,
         pcpRate = SE_NV003_001, mhpRate = SE_NV003_002,
         dentRate = SE_NV003_003, uninsUnd19 = SE_T006_001,
         unins1864 = SE_T006_002, uninsUnd65 = SE_T006_003,
         ypll = SE_T007_002,
         infMort = SE_NV006_001, chiMort = SE_NV006_002,
         premAAMort = SE_NV006_003, drugMort = SE_NV006_004,
         diabAdult = SE_T009_001, teenBirth = SE_NV008_001,
         chlamRate = SE_NV008_002, hivPrevRate = SE_NV008_003,
         smokeRate = SE_T011_001, drinkRate = SE_T011_002,
         limHealthFood = SE_T012_001, accExer = SE_T012_002,
         obesity = SE_T012_003, physInact = SE_T012_004,
         freeLunch = SE_T012_005, fei = SE_T013_001)

#Look at the distribution of low birth rate
#Drop counties with the low birth rate above the third quartile using function "filter"
#filter out the cases do not meet the conditions in the parentheses.
      #Data filtering operation is $%
      #%>% means "then"
      #%$% means "within"
      #%in% means "within + match"

ch2%$%
  qplot(lowBwt)

ch2%$%
  quantile(lowBwt,0.75, na.rm=T)

ch3 <- ch2%>%
  filter(lowBwt<quantile(lowBwt,0.75, na.rm=T))

#inspect
View(ch2)
View(chlab2)

#replace varnames in label object with new ones, this works only because we haven't 
#changed the order of either dataset

chlab2 <- chlab2 %>%
  mutate(var =  names(ch2))
#this will override the variable names

#NOW WE HAVE A DATASET (TIBBLE) OF THE COUNTY HEALTH DATA (CH2)
#AND A DATASET (TIBBLE) OF VARIABLE LABELS, FOR REFERENCE
View(chlab2)

#Save for reference
write_csv(chlab2, "county_health_labels.csv")
#will automatically save in the folder that you set as the working directory 

# DATA CLEANING: census ---------------------------------------------------

View(census)
#notice variable labels 
#(these get read in from Stata, SPSS, SAS files from haven)

#if you want to look at variable labels
var_label(census) #var_label function from 'labelled' library

#inspect
str(var_label(census)) #it's a list

#if you want to create a new object (as a tibble) that contains the variable names 
#and variable labels, first we want to coerce the list to a character vector with as.character
##and then create reference tibble for variable labels & View
      #refer to this for PS2
x <- as.character(var_label(census))
x
cenlab <- tibble(var = names(census), varlabels = x) #can also replace the x here with the text
View(cenlab)

#Save for reference
write_csv(cenlab, "census_labels.csv")

#Suddenly we want to delete empty and extra geographic identifier variables
#for this, we just need to look at the dataset (above)
cen2 <- census %>% #give me a new data set based on this data set, and give me new variables
  select(FIPS, STATE, T150_001:T057_001)

names(census)
names(cen2)

#do the same with the variable labels reference tibble 
#by matching
cenlab <-   cenlab %>%  
  filter(var %in% names(cen2))

View(cenlab)

#check that each have same length 
nrow(cenlab) == ncol(cen2)
#in line above, we are asking R if the lengths are REALLY EQUAL

  
#Now that we have a clean tibble (dataset); but the variable names are still confusing
#we first renames the variables we are interested

cen3 <- cen2 %>%
  rename(pop25 = T150_001, pop25_lhs = T150_002,
         pop25_hs = T150_003, pop25_col = T150_004,
         pop25_bach = T150_005, pop25_master = T150_006,
         pop25_pro = T150_007, pop25_doc = T150_008,
         pop16=T037_001, pop16_emp=T037_002, pop16_unemp=T037_003,
         income=T057_001)

#let's first look at the education variables
view(cen3)

#recode into four categories: no HS dip., HS or some college
#bachelor's degree, grad degree

cen3 <- cen3 %>%
  mutate_at(vars(pop25_lhs:pop25_doc), .funs = funs(./pop25*100)) %>% #from "less than high school" to "doctorate" degree; the "." means "all the variables"
  rename(ed_lhs = pop25_lhs) %>%
  mutate(ed_hssc = pop25_hs-pop25_bach,
         ed_bach = pop25_bach-pop25_master,
         ed_grad = pop25_master) 
view(cen3)

#check to ensure ed categories sum to approximately 100 for each country.
cen3 %>%
  select(ed_lhs, ed_hssc, ed_bach, ed_grad) %$% #total of these four variables is 100% (each observation has a sum up to 100)
  table(rowSums(.)) #total number of observations (one row is one observation)
    #use filter function to select the one row you want to know


#look at summary statistics for the education variables
eduSums <- cen3 %>%
  select(ed_lhs,ed_hssc:ed_grad) %$% #notice NEW PIPE OPERATOR
  map(., summary) #map function from purrr

#inspect
eduSums


# education: percent with at least 4-yr degree
cen3 <- cen3 %>%
  mutate(ed_4yr = 100-ed_hssc-ed_lhs)

#look at distribution of % with 4-yr degree or more in counties
qplot(x=ed_4yr, data=cen3) #qplot (ggplot2)
#qplot takes x, y, data args and tries to pick the best
#type of graph for you--it's just for exploration

cen3%$%
  qplot(ed_4yr)

#Now let's look at the unemployment rate & income
cen3 <- cen3 %>%
  mutate(unemp = pop16_unemp/pop16*100) 

qplot(unemp,data=cen3)
qplot(income, data=cen3)

#or
cen3 %$%
  qplot(income)

#or look at quartiles of income
cen3 %$%
  quantile(income, na.rm=T) #"remove all the "NAs" for me

#create a new categorical variable for income using quartiles as the cutoff
cen3 <- cen3 %>%
  mutate(incCat = case_when(income < quantile(income,0.25, na.rm=T)~"Low income",
                            income >=quantile(income,0.25, na.rm=T) & 
                            income < quantile(income,0.5, na.rm=T)~"Low-middle income",
                            income >= quantile(income,0.5, na.rm=T) &
                            income < quantile(income,0.75, na.rm=T)~"Middle-high income",
                            income>=quantile(income,0.75, na.rm=T)~"High income"))

#check
cen3 %$%
  table(incCat, useNA = 'always') # still just one NA

#recode cats as factor
cen3 <- cen3 %>%
  mutate(incCat = factor(incCat, levels= 
                          c("Low income", "Low-middle income",
                           "Middle-high income", "High income")))


#rename variables for merging
cen3 <- cen3 %>%
  rename(countyFIPS = FIPS, stateFIPS=STATE)

View(cen3) #Now it's clean!!

# generate the file containing variables and their labels for reference
x1 <- as.character(var_label(cen3))
x1
cenlab2 <- tibble(var = names(cen3), varlabels = x1)
View(cenlab2)

cenlab2[20,2] <- "Income quartile" #[row, column]; we changed this from 16 to 20

#Save for reference
write_csv(cenlab2, "census_labels.csv")

# JOINING (MERGING) THE DATA FRAMES ---------------------------------------
# Let's merge the census data and county health data using left_join

names(ch2) #variable names
nrow(ch2) #the number of observations

names(cen3)
nrow(cen3)

county <- ch2 %>%
  left_join(cen3, by = c("countyFIPS", "stateFIPS")) #join is the merge function in r

names(county)
nrow(county)

#find out the differnce of the number of observarions when we switch the order

county2 <- cen3 %>%
  left_join(ch2, by = c("countyFIPS", "stateFIPS"))

#left_join(cen3, ch2, by=c("countyFIPS", "stateFIPS"))

names(county2)
nrow(county2)


#do full join which does not drop any obs & then inspect
ctest <- ch2 %>%
  full_join(cen3, by = "countyFIPS")
nrow(ctest) #can use names(ctest) to get names of variables


# WRITE OUT THE CLEANED/MERGED DATA & THE VARIABLE LABELS  ----------------

write_csv(county, "counties.csv")

mylabels <- chlab2 %>%
  rbind(cenlab2) 

mylabels<- unique(mylabels[,1:2]) #use this to drop the duplicates

write_csv(mylabels, "county_labels.csv")


#THEN, CLEAN UP THE WORKSPACE BY REMOVING ALL THE OBJECTS

rm(list=ls()) #"remove everything in the environment
gc()

#NOW, READ IN dataset 'county'
county <- read_csv('counties.csv')


# SORTING -----------------------------------------------------------------
hivRank <- county %>%
  arrange(hivPrevRate) %>%
  select(Geo_QNAME, stateFIPS, hivPrevRate)
hivRank

#in a descending order
hivRank <- county %>%
  arrange(desc(hivPrevRate)) %>%
  select(Geo_QNAME, hivPrevRate)
hivRank

# GROUPED SUMMARIES -------------------------------------------------------

#look at the number of counties by states

state <- county %>%
  group_by(stateFIPS) %>%
  summarise(noco=n())

state

#find average number of counties in states

state2 <- county %>%
  group_by(stateFIPS) %>% # grouping by state
  summarise(noco = n()) %>% #
  summarise(avgco = mean(noco))

state2


#find average HIV prevalence rates by state
regHIV <- county %>%
  group_by(stateFIPS) %>%
  summarise(avHIV = mean(hivPrevRate, na.rm=T)) #"na.rm=T" mean remove all of the missing values

regHIV

#find average HIV prevalence rates by states 
hivNE <- county %>%
  group_by(stateFIPS) %>%
  summarise(avHIV = mean(hivPrevRate, na.rm=T)) %>%
  arrange(desc(avHIV))

hivNE















