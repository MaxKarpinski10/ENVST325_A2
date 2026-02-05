#Homework 2: Intro to data wrangling 
#Max Karpinski 
#ENVST 325: Environmental Data Science, Hamilton College

#install.packages(c('dplyr','lubridate'))
library(dplyr)
library(lubridate)


#In-class prompts---- 
#Prompt 1---- 
#Follow the steps in the tutorial to join streamH and siteInfo into a data frame called Floods. 
#Check if the type of join makes a difference in the outcome. 

streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

#Prompt 2----
#Parse the date for the Floods data frame. 

streamH$dateF <- ymd_hm(streamH$datetime)
streamH$year <- year(streamH$dateF)

#Other In-class work----
peaceH = streamH %>%
  filter(siteID == 2295637)

plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date", ylab="Stage Height (ft)" )

floods = full_join(streamH, siteInfo, by="siteID")

height.ave = floods %>%
  group_by(names) %>%
  summarise(mean.height = mean(gheight.ft))

floods$doy = yday(floods$dateF)

height.day = floods %>%
  group_by(names, doy) %>%
  summarise(mean.height = mean(gheight.ft))

max.cat = floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(n.major = n())

#Prompt 3----
#What was the earliest date that each river reached the flood stage?
flood_cat = floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))
View(flood_cat)

#Looking at the table we find that the rivers reach the flood stage on the following days:
#WITHLACOOCHEE RIVER AT US 301 AT TRILBY: 2017-09-11 /doy 254
#FISHEATING CREEK AT PALMDALE: 2017-09-11 /doy 254
#PEACE RIVER AT US 17 AT ZOLFO SPRINGS: 2017-09-08 /doy 251
#SANTA FE RIVER NEAR FORT WHITE: 
#Never reaches the flood stage as the stream stage doesn't exceed it's designated flood height of 23ft

#Homework----
# Question 1----
# Make a separate plot of the stream stage data for each river. 
# In 3-4 sentences compare general patterns in the stream stage between sites around Hurricane Irma.


# Question 2----
# What was the earliest date of occurrence for each flood category in each river? 
# How quickly did changes in flood category occur for each river? 
# Do you think there was enough time for advanced warning before a flood category changed?


# Question 3----
# Which river had the highest stream stage above its listed height in the major flood category?


# Question 4----
# Learning to read the R documentation and online resources is a major part of coding. 
# Use Google or run help or ? in R to find the official R documentation for the following functions: 
# select in the dplyr package, ifelse , and hist . 
# Write a brief plain language explanation of what the function does, 
# describe the key arguments needed to run the function, 
# and provide an example use case using the flood data.


# Question 5---- 
# Copy the url for your R script from GitHub and paste it here.





