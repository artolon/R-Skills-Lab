
'# PROLOG   ################################################################'

'# PROJECT: DAY 3 R-SKILLLAB SU18 DATA EXPLORATION IN R #'
'# PURPOSE: SUMMARIZING, DESCRIBING & VISUALIZING DATA #'
'#          ggplot2 #'
'# DIR:     /Users/candice_wang/Box/Courses/2019 Spring/RSkillLab/Session 3/#'
'# DATA:    https://raw.githubusercontent.com/tBryan13/Rlab_Sp2018/master/counties.csv #'
'#          https://raw.githubusercontent.com/tBryan13/Rlab_Sp2018/master/county_labels.csv #'
'# AUTHOR:  XIAOYAN WANG; TODD COMBS  #'
'# CREATED: OCT 16, 2017 #'
'# LATEST:  JAN 27, 2019 #'
'# NOTES:    #'

'# PROLOG   ###############################################################'


# WORKING DIRECTORY -------------------------------------------------------

setwd("C:/Users/artol/Documents/Sp19/R Skills Lab/Week3")


# LIBRARIES ---------------------------------------------------------------

#for first loading of a package, you must install via install.packages()
#then load packages
library(tidyverse) # for various packages
library(magrittr) # for extra %$% pipe

# READ IN DATA ------------------------------------------------------------

county <- read_csv("counties.csv")
countyLabs <- read_csv("county_labels.csv")


# DATA EXPLORATION: DESCRIPTION & VISUALIZATION ---------------------------
names(county) #will show you all the column names
view(county)

#look at number of counties
county %$%
  length(unique(countyFIPS))

#look at counties by state
county %>%
  group_by(state) %>%
  summarise(ncounties = n()) #n gets you # of observations in each state; "ncounties is a new variable we created

#county dataset remains unchanged. To save the result, assign it as a new object

stateCounty <- county %>%
  group_by(state) %>%
  summarise(ncounties = n())
View(stateCounty)


# HISTOGRAMS --------------------------------------------------------------

#look at histogram of average days adults feel unhealthy - physically
h <- ggplot(data = county, mapping = aes(x=daysPhysUH))
        #you do not need "data=" or "x="(see example below); r know that "county" is the data set and "daysPhysUh" is the x axis"

# save some arguments
h <- ggplot(county, aes(daysPhysUH))

#add a line where the mean is
h <- h + geom_histogram(binwidth=0.1)
h
      #geom_historgram tells you what type of graph you want to turn h into

h <- h + geom_vline(mapping=aes(xintercept = mean(daysPhysUH, na.rm=T)),
                    color="red", size=1)
h
        #geom_vline means drawing a verticle line through the data; the verticle line is the mean through the physical healthy datas and is red and size 1

#This will change the titles of the x/y axis etc.
h <- h + labs(x="Average days reported feeling unhealthy per month by adults",
              y= "Number of counties",
              title="My first histogram ever",
              subtitle="This is a subtitle",
              caption="Source: County Health Rankings & Roadmap 2016")
h

#OR we can use pipeline (since we already assigned everything from county to "h_1", we don't have to put it in the next line of code)
h_1 <- county %>%
  ggplot(aes(x=daysPhysUH)) +
  geom_histogram(binwidth=0.25) +
  geom_vline(mapping=aes(xintercept = mean(daysPhysUH, na.rm=T)),
             color="red", size=1)+
  labs(x="Average days reported feeling unhealthy per month by adults",
       y= "Number of counties",
       title="My first histogram ever",
       subtitle="This is a subtitle",
       caption="Source: County Health Rankings & Roadmap 2016")
h_1

#look at histogram of average days adults feel unhealthy - mentally
h1 <- ggplot(data = county, aes(x=daysMentUH))
h1 <- h1 + geom_histogram(color="red", fill="dark green")
h1

#add a line where the mean is
h1 <- h1 + geom_vline(aes(xintercept = mean(daysMentUH, na.rm=T)),
                    color="blue", size=3)
h1

# another type of graph to describe the continous variable if we have multiple groups

ggplot(data = county, aes(x=daysMentUH, colour = Region)) + #distribute mentally unhealthy days by regions
  geom_freqpoly(binwidth = 0.1)
      #here, we did NOT assign the graph to an object

#what if we use histogram" hard to recognize
ggplot(data = county, aes(x=daysMentUH, colour = Region)) +
  geom_histogram(binwidth = 0.1)

# SCATTERPLOTS ------------------------------------------------------------

#look at scatterplot of both
g <- county %>%
  ggplot(aes(x=daysPhysUH, y=daysMentUH, color=Region)) #you can switch "color" with "size" and "shape" as well; "alpha" assigns different transparencies
g

g <- g + 
  geom_jitter(width=0.2,height=0.2, alpha=0.4) + 
  geom_smooth(method="lm") + #using loess method (can change what "method" =, depending on what kind of line you want)
  facet_wrap(~Region)
g

#look at scatterplot of both with jittered points
g <- county %>%
  ggplot(aes(x=daysPhysUH, y=daysMentUH)) #nothin shows up here, because we did not specify the graph we wanted to make

g <- g + geom_jitter(width=0.2,height=0.2,alpha=0.4)
g

g <- g + geom_jitter(aes(color=Region))
g

#this allows us to mannually asign the colors for the various regions
g<-g + scale_color_manual(values=c("red", "blue", "purple", "grey"))
g

g <- county %>%
  ggplot(aes(x=daysPhysUH, y=daysMentUH))

g<- g + geom_jitter(aes(color=Region))
g<- g + scale_colour_brewer(palette = "Set3") #the color brewer gives you the color pallets that you can choose from (might need to install package)
g

g <- g + geom_jitter(aes(color=Region, shape=Region, size=3))
g

g <- g + scale_shape_manual(values=c("M","N","S","W")) #this will help you change the chapes manually 
g

g <- county %>%
  ggplot(aes(x=daysPhysUH, y=daysMentUH)) 

g <- g + geom_jitter(aes(color=Region, shape=Region, size=mhhi))
g

#look at scatterplots by region
g <- ggplot(data=county)

g <- g + geom_jitter(aes(x=daysPhysUH, y=daysMentUH, color=Region))

g <- g + facet_wrap(~Region)
g

#add a geom_smooth (abline + standard error area)
g <- g + geom_smooth(color="grey10", method="lm")
g
#error

#put aes in original call to ggplot and all subsequent geoms will 
#inherit from it
g <- ggplot(data=county)

g <- g + geom_jitter(aes(x=daysPhysUH, y=daysMentUH, color=Region))

g <- g + facet_wrap(~Region)
g

#add a geom_smooth (abline+standard error area)
g <- g + geom_smooth(color="grey10", method="lm")
g
#error

#put aes in original call to gglplot and all subsequent geoms will inherit from it
g <- ggplot(data=county, aes(x=daysPhysUH, y=daysMentUH, color=Region))
g

# BARPLOTS ----------------------------------------------------------------

#look at counties by region
b <- county %>%
  ggplot(aes(x=Region)) #assign data to "b"

b <- b + geom_bar() #this says, "I want my graph to be a bar chart
b

county %$% table(Region) #within this dataset, let's run the frequency for the region

#OR

table(county$Region) #dataset$variable (gives you same thing as above)

#reorder factor levels of region to go from largest to smallest
county <- county %>%
  mutate(Region = factor(Region)) %>% #this line recodes the data TYPE to be a factor variable, rather than character
  mutate(regOrd = factor(Region, levels=c("South", "Midwest", "West", "Northeast"))) #this one is creating a new variable with a different order

county %$% table(regOrd)

b <- county %>%
  ggplot(aes(x=regOrd))
b <- b + geom_bar()
b

###alternate factoring
regSorted <- county %$% sort(table(Region), decreasing=T)
regSorted
      #this allows us to run things from largest to smallest

county <- county %>%
  mutate(Region = factor(Region, levels=names(regSorted)))

b <- county %>%
  ggplot(aes(x=regOrd))

b <- b + geom_bar()
b

#Here is for the income variable
incSorted <- county %$% sort(table(incCat), decreasing=T)
incSorted

county <- county %>%
  mutate(incCat = factor(incCat, levels=names(incSorted)))
incSorted

b <- county %>%
  ggplot(aes(x=incCat))

b <- b + geom_bar()
b

#add one color to highlight midwest for any reason
b <- county %>%
  ggplot(aes(x=regOrd))

b <- b + geom_bar(fill = c("grey50", "blue", "grey50", "grey50"))
b

b <- b + geom_bar(fill = c("grey50", "blue"))
b

#search for "colors in R" on internet to find better colors

#difference between color and fill
b <- county %>%
  ggplot(aes(x=regOrd))

b <- b + geom_bar(color="red")
b

b <- b + geom_bar(fill = c("grey50", "cornflowerblue", "grey50", "grey50"), 
                  color = "red")
b

#color each bar (by region with aes)
b <- county %>%
  ggplot(aes(x=regOrd, fill=regOrd)) #"fill" means it will fill the different bars with different colors, depending on the region
b <- b + geom_bar()

b

#turn off legend as it is unnecessary here

b <- b + theme(legend.position = "none", #this will get rid of the legend
               text = element_text(color="red"), #will change color of x and y axis color
               axis.text = element_text(color="green", size=24)) #change color and size of text

b <- b + labs(title = "IS this red?")
b #will change the color of the actual title

#check out the items in theme
?theme

#set theme to minimal
theme_set(theme_minimal()) #gives a minimal background for the graph (light graph paper look)

b <- b + theme(legend.position = "top") #move the legend to the top (or whever you want)
b

#add text labels (totals) to bars

c2 <- county %>%
  group_by(regOrd) %>%
  summarise(tot = n()) %>% #total number of counties/observations in each region
  mutate(pc = round(tot/sum(tot)*100)) %>% #create a new variable that equals the percentage of counties in each region among the total regions
  mutate(lab = paste0(tot," ","(",pc,"%)")) #create another new variable; paset0 puts all the characters and numbers that you think are necessary together; here, we are pasting total, empty space, and then the percentage of the numbers in each region, then the % sign, and paranthesis to surround
view(c2)

b <- b + geom_text(data=c2, aes(x=regOrd, y=tot, label=lab)) #now, we put the above dataset into a graph; we are adding the lables to each bar
b #add any text that you want using gemo_text (such as p value, etc.)

b <- b + geom_text(data=c2, aes(x=regOrd, y=tot, label=lab), 
                   nudge_y = 50) #nudge_y moves the labels, just a little bit
b
    #To start from scratch, run this code from line 273-277, and then re-run code line 309-311

b <- county %>%
  ggplot(aes(x=regOrd, fill=regOrd))
b <- b + geom_bar()
b <- b + geom_text(data=c2, aes(x=regOrd, y=tot, label=lab), 
                   nudge_y = 50) + 
  theme(legend.position = "none") #"non" gets rid of the legend; can also move to top or bottom, etc. 
b

#remove variable name from x axis & add plot title
b <- b + labs(x="", title="Counties in US by region", y="") #put nothing between the quotes if you want to get rid of the titles
b <-  b + theme(panel.grid = element_blank(),
          axis.text.y = element_blank())
b

#center the title (this puts the title in the center of the graph)
b <- b + theme(plot.title=element_text(hjust=0.5))
b
    #0 means left aligned, 1 means right aligned, and 0.5 means in the middle!

#look at counties by income category by region
h <- county %>%
  ggplot(aes(x=Region, fill=incCat)) 

h <- h + geom_bar()
h

#drop counties with no data for income
c2 <- county %>%
  filter(!is.na(incCat)) #get rid of the data that is N/A; "pick up all values that are NOT na"; 7 cases got dropped from this variables

h <- c2 %>%
  ggplot(aes(x=Region, fill=incCat)) 


h <- h + geom_bar() + theme_bw() #for this we did not specify, but the default is position="fill"; if that's what you want, you can leave paranthesis blank. Otherwise, you have to specify
h

#put bars beside each other rather than stacked (which is the default)
h <- c2 %>%
  ggplot(aes(x=Region, fill=incCat)) 
h <- h + geom_bar(position = "dodge")
h

#Get rid of the x-axis label and legend title (x-axis title will disappear)
h <- h + labs(fill="", x="") 

h

c2 <- c2 %>%
  mutate(incCat = factor(incCat)) 
c2 %$% levels(incCat)

c2 <- c2 %>%
  mutate(incCat = factor(incCat, levels=c("Low income","Low-middle income","Middle-high income","High income"))) 

h <- c2 %>%
  ggplot(aes(x=regOrd, fill=incCat)) 
h <- h + geom_bar(position = "dodge")
h <- h + labs(fill="", x="") + theme(legend.position = 'top') 
h

# BOXPLOTS ----------------------------------------------------------------

#look at boxplots of income by region

h <- c2 %>%
  ggplot(aes(x=regOrd, y=mhhi))

h <- h + geom_boxplot()

h

# flip it 90°
h <- h + coord_flip() #this will basically flip the x and y axis
h

# add points
#h <- h + geom_jitter()
h <- h + geom_jitter(width = 0.2, alpha=0.3)
h
    #this will put ALL the points on the graphs

# STATISTICAL SUMMARY PLOTS -----------------------------------------------

g <- county %>%
  ggplot(aes(x=Region, y=mhhi))

g <- g + stat_summary(fun.ymin=min, fun.y=median, fun.ymax = max)

g
      #use stat_summary function to get data that you want (ex: min, med, and max)



# STANDARD ERROR BAR PLOTS ---------------------------------------------------------

mod1 <- lm(obesity~smokeRate + freeLunch + white, data=county) #"lm" is linear modeling; obesity is dep. variable; ind. var are all the others!
summary(mod1) #this will show you a summary of all the data you want
str(summary(mod1)) 
# get coefficients
mycoefs <- summary(mod1)$coefficients[-1,1:2] #we are extracting the coefficients from the model summary; we only want 2nd row to 4th row, so get rid of 1st row (row, column); delete first row, give me column 1 and 2
mycoefs <- tibble(var= c("Smoking rate(%)","Free lunch (%)", "White pop. (%)"),
                  est=mycoefs[,1], se=mycoefs[,2]) #this will create a dataset
view(mycoefs)

# calculate CIs; #we are generating two new variables (upper and lower bound)

mycoefs <- mycoefs %>%
  mutate(lb=est-se*1.96,ub=est+se*1.96)
  
g <- mycoefs %>%
  ggplot(aes(x=est, xmin=lb, xmax=ub, y=var))

g <- g + geom_errorbarh() + geom_point()

g

# clean it up a bit
g <- mycoefs %>%
  ggplot(aes(x=est, xmin=lb, xmax=ub, y=reorder(var,-est)))

g <- g + geom_errorbarh(height=0.2) + geom_point()

g <- g + geom_vline(aes(xintercept=0), color="red", linetype=2)
g <- g + labs(x="Estimates with 95% confidence intervals", y="", 
              title = "Dependent variable: Obesity rates (%)")
g

