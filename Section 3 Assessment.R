
setwd ("C:/Users/Iwara Arikpo/Dropbox/MyDocs/Capacity Development/Harvard edX R Course/R Professional Certificate Program/COURSES/R Basics/Lab/Code")

#install.packages("dslabs")

library(dslabs)


#load the murders dataset from the dslabs package
data("murders")


#US Murder rate
murder_rate <- (murders$total/murders$population)*100000

#Using logicals to index vectors
ind <- murder_rate < 0.71

#To see which states meet this logical condition above
murders$state[ind]

#To get number of states that meet this condition use sum function on the vector
sum(ind)


#Create a logical vector showing which murder rates are lower than 1 and which are not name it low
low <- murder_rate < 1


#Assessment 3.2 Basic Data Wrangling
#Using the dplyr package
library(dplyr)

#creating columns with mutate function of dplyr
murders <- mutate(murders, rate = total/population *100000)


#To rank murder rate from highest to lowest and putting into a new column rank
murders <- mutate(murders, rank(-rate))

#show the state names and abbreviations only using select function
select(murders, state, abb)


#Use filter to show the top 5 states with the highest murder rates in descending order. 
#Use rank to do this
filter(murders, rank(-rate) <=5)

#Create a new data frame called no_south that removes states from the South region.
no_south <- filter(murders, region!= "South")


#How many states are in this category? You can use the function nrow for this.
nrow(no_south)


#Create a new data frame called murders_nw with only the states from the Northeast and the West.
murders_nw <- filter(murders, region %in% c("Northeast", "West"))

#How many states are in this category?
nrow(murders_nw)


#filtering by two conditions
#Add a murder rate column and a rank column as done before
murders <- mutate(murders, rate = total/population *100000, rank=rank(-rate))


#Create a table, call it my_states, that satisfies both the conditions: 
#it is in the Northeast or West and the murder rate is less than 1.

my_states <- filter(murders, region %in% c("Northeast", "West") & rate < 1)


#Use select to show only the state name, the rate and the rank
select(my_states, state, rate, rank)


#Using the pipe %>%
#For example we could have mutated and selected in the same line like this:
mutate(murders, rate =  total / population * 100000, rank = (-rate)) %>% 
  select(state, rate, rank)


#Repeat the previous exercise, but now instead of creating a new object, show the result and only 
#include the state, rate, and rank columns in that order.
filter(murders, region %in% c("Northeast", "West") & rate < 1) %>% 
  select(state, rate, rank)


#Introducing a second level pipe to further extract those with rates <= 0.5
filter(murders, region %in% c("Northeast", "West") & rate < 1) %>% 
  select(state, rate, rank) %>% filter(rate <= 0.5)




#mutate, filter and select
#Now we will reset murders to the original table by using data(murders)

data(murders)

# Use one line of code to create a new data frame, called my_states, that has murder rate and rank 
# columns (with the rank ordered from highest to lowest), considers only states in the 
# Northeast or West which have a murder rate lower than 1, and contain only the state, rate, and rank 
# columns. The line should have four components separated by three %>% operators:

my_states <- mutate(murders, rate =  total / population * 100000) %>% 
  mutate(rank = rank(-rate)) %>% 
  filter(region %in% c("Northeast", "West") & rate < 1) %>%
  select(state, rate, rank)



#####################Scatterplots
#We made a plot of total murders versus population and noted a strong relationship: 
#not surprisingly states with larger populations had more murders. You can run the code in the 
#console to get the plot.

library(dslabs)
data(murders)

population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total

plot(population_in_millions, total_gun_murders)


#Note that many states have populations below 5 million and are bunched up in the plot. 
#We may gain further insights from making this plot in the log scale.

#Transform the variables using the log base 10 transformation

# Transform population (not population in millions) using the log10 transformation and 
#save to object log10_population
log10_population <- log10(murders$population)

# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
log10_total_gun_murders <- log10(murders$total)

#Plot the log transformed total murders versus population
plot(log10_population, log10_total_gun_murders)



#################Histograms
#Compute the population in millions and save it to the object population_in_millions
population_in_millions <- murders$population/10^6

#Create a histogram of the state populations using the function hist
hist(population_in_millions)



###################Boxplots
#In one line of code:
#Stratify the state populations by region.
#Generate boxplots for the strata.

#Note that you can achieve this using this population~region inside boxplot to generate the 
#strata and specify the dataset with the data argument.

boxplot(population~region, data = murders)




#Section 3 Assessment
#For questions 1-8, load the dslabs dataset heights:
library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers


#First, determine the average height in this dataset. Then create a logical vector ind with the 
#indices for those individuals who are above average height.

#How many individuals in the dataset are above average height?
avg_height <- mean(heights$height)
ind <- heights$height > avg_height

sum(ind)


#How many individuals in the dataset are above average height and are female?
heights$sex[ind]

female_abv_avg <- filter(heights, sex %in% c("Female") & height > avg_height)



#If you use mean() on a logical (TRUE/FALSE) vector, it returns the proportion of observations 
#that are TRUE.
ind_fmale <- heights$sex == "Female"

#What proportion of individuals in the dataset are female?
mean(ind_fmale)




#This question takes you through three steps to determine the sex of the individual with 
#the minimum height.

#(a) Determine the minimum height in the heights dataset.
min_height <- min(heights$height)


#(b) Use the match() function to determine the index of the first individual with the minimum height.
match(min_height, heights$height)


#(c) Subset the sex column of the dataset by the index in 4b to determine the individual's sex.
ind_min_height <- match(min_height, heights$height)
heights$sex[ind_min_height]



#This question takes you through three steps to determine how many of the integer height values 
#between the minimum and maximum heights are not actual heights of individuals in the heights 
#dataset.


#(a) Determine the maximum height.
max_height <- max(heights$height)


#(b) Which integer values are between the maximum and minimum heights? For example, 
# if the minimum height is 10.2 and the maximum height is 20.8, your answer should be x <- 11:20 
# to capture the integers in between those values. (If either the maximum or minimum height are 
# integers, include those values too.)
# 
# Write code to create a vector x that includes the integers between the minimum and maximum heights 
# (as numbers).
# There are multiple ways to solve this problem, but the grader expects you to use the format in the 
# problem description. Your answer should have numbers and a colon (:), and it should not use other 
# functions.

x <- 50:82


#(c) How many of the integers in x are NOT heights in the dataset?
#Use the sum() and %in% functions in addition to the ! operator.

sum(!(x %in% heights$height))


# Using the heights dataset, create a new column of heights in centimeters named ht_cm. 
# Recall that 1 inch = 2.54 centimeters. Save the resulting dataset as heights2.

heights2 <- mutate(heights, ht_cm = height * 2.54)


#(a) What is the height in centimeters of the 18th individual (index 18)
heights2$ht_cm[18]


#(b) What is the mean height in centimeters?
mean(heights2$ht_cm)


#(a) Create a data frame females by filtering the heights2 data to contain only female individuals
females <- filter(heights2, sex=="Female")


#(b) What is the mean height of the females in centimeters?
mean(females$ht_cm)



#Question 8
# The olive dataset in dslabs contains composition in percentage of eight fatty acids found in the lipid fraction of 572 Italian 
# olive oils:

library(dslabs)
data(olive)
head(olive)

#Plot the percent palmitic acid versus palmitoleic acid in a scatterplot. 
#What relationship do you see?

plot(olive$palmitic, olive$palmitoleic)  


#Question 9
#Create a histogram of the percentage of eicosenoic acid in olive.

hist(olive$eicosenoic)


#Question 10
#Make a boxplot of palmitic acid percentage in olive with separate distributions for each region.

boxplot(palmitic~region, data=olive)

