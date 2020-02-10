

'# PROLOG   ################################################################'

'# PROJECT: S55-5962 Spring 2019: R BASICS #'
'# PURPOSE: OBJECTS; ASSIGNMENTS; COMMON FUNCTIONS; INTRO TO GGPLOT #'
'# DIR:     /Users/candice_wang/Box/Courses/2019 Spring/R_skillLab_Combs/Day1 #'
'# DATA:     #'
'# AUTHOR:  TODD COMBS & XIAOYAN WANG  #'
'# CREATED: DEC 20, 2017 #'
'# LATEST:  DEC 21, 2018 #'
'# NOTES:   #'

'# PROLOG   ###############################################################'


# LIBRARIES ---------------------------------------------------------------

#for libraries (a.k.a. packages) you must install them THE FIRST TIME YOU USE THEM
#NO NEED to install everytime
#load them each time you need them


install.packages("tidyverse") #Quotes are required
install.packages("haven")

library(tidyverse) # for ggplot2,tibble,tidyr,readr,purrr,dplyr,stringr,forcats
library(haven) # for reading Stata, SPSS, SAS files


# WORKING DIRECTORY -------------------------------------------------------

getwd()
setwd("/Users/rosewang/Box/Courses/2019 Spring/MyClass/Session 1/") 
# To change the working directory, use setwd() with the directory path in quotes
# Use R project files and working directory is automatically set to folder of project

# CALCULATIONS ------------------------------------------------------------

#basic calculations
2 + 2

4 - 5

130*65

144/6

132+7*8-5
(132+7)*8-5

#mathematical operations functions

sqrt(1024)

#powers and logs
32^2

64^1/2
64^(1/2)

log(100)

?log

exp(4)

#wrapping functions

log(exp(4))

# VECTORIZATION -----------------------------------------------------------

#use concatenate "c()" to create a vector

c(4,5,6,7,8)

#use sequence 'seq()' to create a sequence
?seq
seq(2,100)
seq(2,100,2)

seq(from=2, to=100, by=2)

seq(2,100,length.out=5000)


seq(2,100,2)/2

round(seq(2,100,2)/seq(1,10), 1)

1:10

rep(5,3) # rep = repeat first argument N times, where N = second arg
rep(c(1,2,3),10)
sort(rep(c(1,2,3),10)) # sort will sort simple vectors from lowest to highest values

seq(100, 2,-2)
seq(100, 2,2)

sum(c(4,5,6,7,8))

# ASSIGNMENTS -------------------------------------------------------------

#vectors #shortcut key for <- is Alt - for Windows, and option - for Mac

x <- c(20,13,5,17,9,10,14)
x
x <- x[-3]
x

str(x) # str = structure (view object class, dimensions, contents)

sort(x) 
x

y <- sort(x, decreasing = FALSE)
y



# DATA TYPES & OBJECTS --------------------------------------------------------------

#vectors - numeric
x <- 1:10
str(x)
x <- c(x,"CH")
x <- seq(1,10, 0.05)
str(x)

length(x) # length tells you the length (# of elements) in x


#vectors - logical

x <- c(T,F,F,T)
str(x)

x <- c(t,f,f,t)

#vectors - string/character
x <- c("This", "is", "a", "string" , "vector")
str(x)

#error
sum(x)

x2 <- 1:5


mycolors <- c("red", "blue", "green")
str(mycolors)

#vectors - factor (like a variable with value labels)
mycol2 <- factor(mycolors)
str(mycol2)

mycol2
levels(mycol2)

mycol3 <- factor(mycolors, levels = c("red", "blue", "green"))


#matrices
x <- matrix(c(2,1,2,1,2,1,1,1,1,2,2,1))
str(x)
x

#warning message: NOT AN ERROR, just a warning
x <- matrix(1:11, nrow=2)

#fix
x <- matrix(1:12, nrow=2)

x <- matrix(1:12, nrow=2, byrow=T)

x <- matrix(1:12, nrow=2, byrow=T, ncol=3)

x <- matrix(1:12, nrow=2, byrow=T, ncol=6)

#get dimensions, rows then columns
dim(x)

#tibbles (like a dataset or data.frame)
x <- tibble(shape = c("square", "circle", "triangle", "octagon"),
                size = c(1.2,5.4,3,2.4), count = c(10,22,53,43))
x
str(x)
summary(x)
summary(x$size)
summary(x[,2])


#lists

x <- c("This", "is", "a", "string" , "vector")


x2 <- 1:5

x3 <- matrix(c(x, x2), 2, byrow=T)

cs1 <- table(1:10, rep(c(1,3),5))

mytib <- tibble(x, x2)

mylist <- list(x, x2, cs1, mytib)


x
dim(x)


#tables
x <- 1:5
y <- c(2,4,2,4,4)

table(x,y)

xy <- table(x,y)

#error
x <- c('blue', 'green', 'green', 'green', 'blue')

y <- c('blue', 'green', 'green', 'green', 'blue')

xy <- table(x,y)

#check to see all objects in the environment 
ls()

#remove objects from environment
rm(x)
ls()
x <- c("This", "is", "a", "string" , "vector")
rm(list = x)
ls()
rm(list = "x")
ls()
x <- c("This", "is", "a", "string" , "vector")
rm(list = c("x"))
ls()

#remove everything
rm(list=ls())
ls()

# INDEXING ----------------------------------------------------------------

x <- matrix(seq(1,23,2), nrow=2, byrow=T, ncol=6)


x[1,]
x[,1]

x[,1:2]

x[,1,4:6]

x[,c(1,4:6)]


x[2:3,4:6]

#for tibbles = indexing is the same as matrices

x <- tibble(shape = c("square", "circle", "triangle", "octagon"),
            size = c(1.2,5.4,3,2.4), count = c(10,22,53,43))

x[2:3,1:2]

#for lists: double brackets!

x <- c("This", "is", "a", "string" , "vector")

x2 <- 1:5

x3 <- matrix(c(x, x2), 2, byrow=T)

cs1 <- table(1:10, rep(c(1,3),5))

mytib <- tibble(x, x2)

mylist <- list(x, x2, cs1, mytib)

#get first list element
mylist[[1]]

#fourth
mylist[[4]]


#get second column (all rows) of fourth element

#error
mylist[[5]][,2]

mylist[[4]][,2]

# BASIC STATISTICS --------------------------------------------------------



x <- tibble(id = 1:10, wt = seq(45,69, length.out=10),
            ht = seq(2.5,4.3, length.out = 10),
            sex = rep(c("m","f"),5))

summary(x)

#now you realize you forgot a guy but you don't have his height
extraguy <- tibble(id=11, wt=71,ht=NA, sex='m')


#add the extraguy as the eleventh row of tibble
x <- bind_rows(x, extraguy)

x

summary(x)

#measures of central tendency & dispersion
mean(x$wt)
sd(x$wt)

se <- sd(x$wt)/sqrt(length(x$wt))

avg_wt <- c(lb = mean(x$wt) - 1.96*se, avg = mean(x$wt), ub = mean(x$wt) + 1.96*se)


quantile(x$wt)

quantile(x$wt, probs=c(0,0.1,0.3,0.5,0.7,0.9,1))

quantile(x$wt, probs=seq(0,1,0.1))


mean(x$ht)

mean(x$ht, na.rm=T)

median(x$ht, na.rm=T)

sd(x$ht)

sd(x$ht, na.rm=T)

var(x$ht, na.rm=T)

sqrt(var(x$ht, na.rm=T))

quantile(x$ht)

quantile(x$ht, na.rm=T)

#min & max
min(x$ht, na.rm=T)

max(x$ht, na.rm=T)

#simple correlation
cor(x$ht, x$wt)

cor(x$ht, x$wt, use = 'complete.obs')

# DATA EXPLORATION WITH GGPLOT2 -------------------------------------------

#read in Stata dataset of 2015 development indicators at country level
wdev <- read_dta("worldDevelopment2015.dta")

#preview dataset
wdev

#View dataset
View(wdev)

summary(wdev)

#explore life expectancy & HIV prevalence

g <- wdev %>%
  ggplot(aes(x=hivPrev, y=lifeBirth))

g <- g + geom_point(pch=3, size=12)

g + labs(x="HIV Prevalence for adults 24+ (%)",
         y="Life expectancy at birth",
         subtitle="HIV and life expectancy by country 2015")
g

# color by region
table(wdev$region5)

g <- wdev %>%
  ggplot(aes(x=hivPrev, y=lifeBirth, shape=region5, size=region5))

g <- g + geom_text(aes(label=country))

g <- g + geom_point()

g <- g + scale_color_manual(values=c(rep("grey75",4),"red"))

g


# facet by region

g <- wdev %>%
  ggplot(aes(x=hivPrev, y=lifeBirth, color=region5))

g <- g + geom_point()

g <- g + facet_wrap(~region5)

g


# explore infant mortality & life expectancy

g <- wdev %>%
  ggplot(aes(x=mortInf, y=lifeBirth))

g <- g + geom_point(size=4)
g <- g + geom_smooth(method="lm")
g <- g + labs(subtitle = expression(y=beta*x))
g
mod1 <- lm(lifeBirth~mortInf,data=wdev)

# facet by region

g <- wdev %>%
  ggplot(aes(x=mortInf, y=lifeBirth, color=region5))

g <- g + geom_point()

g <- g + facet_wrap(~region5)

g


# explore life expectancy by region

g <- wdev %>%
  ggplot(aes(y=lifeBirth, x=region5))

g <- g + geom_boxplot()

g
# add points

g <- wdev %>%
  ggplot(aes(y=lifeBirth, x=region5))

g <- g + geom_boxplot() + geom_point()

g <- g + geom_boxplot() + geom_jitter()

g

g <- wdev %>%
  ggplot(aes(y=lifeBirth, x=region5))

g <- g + geom_boxplot() + geom_jitter(width=0.1)

g
