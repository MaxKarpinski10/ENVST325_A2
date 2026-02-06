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

#PEACE RIVER AT US 17 AT ZOLFO SPRINGS
peaceH = streamH %>%
filter(siteID == 2295637)
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19,main ="PEACE RIVER AT US 17 AT ZOLFO SPRINGS", xlab="Date", ylab="Stage Height (ft)" )
#FISHEATING CREEK AT PALMDALE
Fisheating = streamH %>%
  filter(siteID == 2256500)
plot(Fisheating$dateF, Fisheating$gheight.ft, type="b", pch=19, main ="FISHEATING CREEK AT PALMDALE",xlab="Date", ylab="Stage Height (ft)" )
#SANTA FE RIVER NEAR FORT WHITE
SantaFe = streamH %>%
  filter(siteID == 2322500)
plot(SantaFe$dateF, SantaFe$gheight.ft, type="b", pch=19, main ="SANTA FE RIVER NEAR FORT WHITE",xlab="Date", ylab="Stage Height (ft)" )
"WITHLACOOCHEE RIVER AT US 301 AT TRILBY"
Withlacoochee = streamH %>%
  filter(siteID == 2312000)
plot(Withlacoochee$dateF, Withlacoochee$gheight.ft, type="b", pch=19, main ="WITHLACOOCHEE RIVER AT US 301 AT TRILBY",xlab="Date", ylab="Stage Height (ft)" )

#I noticed that the stream stage height for all of the rivers quickly spikes up around September 11th. 
#It then takes a day or two for the stream stage to reach its max height
#After which the stage height dwindles down closer to original/non-flood levels over a span of about 2 weeks
#Withachoochee river stage height behaves a little differently where it does have the original spike, 
#but it continues to rise for about another week, until it starts to slowly decrease in height

# Question 2----
# What was the earliest date of occurrence for each flood category in each river?
#Flood
flood_cat1 = floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))
#Moderate
flood_cat2 = floods %>%
  filter(gheight.ft >= moderate.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))
#Major
flood_cat3 = floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

#Flood
#WITHLACOOCHEE RIVER AT US 301 AT TRILBY: 2017-09-11 /doy 254
#FISHEATING CREEK AT PALMDALE: 2017-09-11 /doy 254
#PEACE RIVER AT US 17 AT ZOLFO SPRINGS: 2017-09-08 /doy 251
#SANTA FE RIVER NEAR FORT WHITE: 
#Never reaches the flood stage as the stream stage doesn't exceed it's designated flood height of 23ft

#Moderate Flood
#WITHLACOOCHEE RIVER AT US 301 AT TRILBY: 2017-09-12 /doy 255
#FISHEATING CREEK AT PALMDALE: 2017-09-11 /doy 254
#PEACE RIVER AT US 17 AT ZOLFO SPRINGS: 2017-09-10 /doy 253
#SANTA FE RIVER NEAR FORT WHITE: 
#Never reaches the flood stage as the stream stage doesn't exceed it's designated flood height of 23ft

#Major Flood
#WITHLACOOCHEE RIVER AT US 301 AT TRILBY: 2017-09-16 /doy 259
#FISHEATING CREEK AT PALMDALE: 2017-09-12 /doy 255
#PEACE RIVER AT US 17 AT ZOLFO SPRINGS: 2017-09-10 /doy 253
#SANTA FE RIVER NEAR FORT WHITE: 
#Never reaches the flood stage as the stream stage doesn't exceed it's designated flood height of 23ft

# How quickly did changes in flood category occur for each river? 

#WITHLACOOCHEE RIVER AT US 301 AT TRILBY: Reached moderate flood from normal flood in 1 day then major flood from moderate flood in 4 days 
#FISHEATING CREEK AT PALMDALE: Reached moderate flood from normal flood within the same day then major flood from moderate flood in 1 day
#PEACE RIVER AT US 17 AT ZOLFO SPRINGS: Reached moderate flood from normal flood in 2 days then major flood from moderate flood within the same day.
#SANTA FE RIVER NEAR FORT WHITE: 
#Never reaches the flood stage as the stream stage doesn't exceed it's designated flood height of 23ft

# Do you think there was enough time for advanced warning before a flood category changed?
#There definitely wasn't enough time for advanced warning before a flood category changed given
#many of the flood categories changed within the same day or within a day which is little to no time to react.

# Question 3----
# Which river had the highest stream stage above its listed height in the major flood category?
highestdiff = floods %>%
  group_by(names) %>%
  summarize(max_diff = max(gheight.ft - major.ft))
 
#Peace River had the highest stream stage above its listed height in the major flood category with a value of 7.85ft.

# Question 4----
# Learning to read the R documentation and online resources is a major part of coding. 
# Use Google or run help or ? in R to find the official R documentation for the following functions: 
# select in the dplyr package, ifelse , and hist . 
# Write a brief plain language explanation of what the function does, 
# describe the key arguments needed to run the function, 
# and provide an example use case using the flood data.

#Used google

#dplyr package - select()
#Explanation: The function allows you to pick specific columns you want to keep and throws away the rest. 
#For example it is useful when you have a dataset with a lot of columns but only need to analyze a few of them.
#Key arguments: 
#.data: The data table (usually implied if you use the pipe %>%).
# ... : The names of the columns you want to keep. You can also use minus signs (-column) to specifically drop columns
#Example:
# Keep only the 'names' and 'gheight.ft' columns
selectedflooddata <- floods %>%
  select(names, gheight.ft)

#ifelse package - ifelse()
#Explanation: The function asks a Yes/No question for every single row in a column. 
#It creates a new list of values: one value if the answer is "Yes" (True), and a different value if the answer is "No" (False). 
#It is the standard way to create labels based on data.
#Key arguments: 
#test: The condition to check (e.g., is x > 5?).
#yes: The value to return if the condition is True.
#no: The value to return if the condition is False. 
#Example:
#Create a new column called Safety that labels a river as "Dangerous" if it is above flood stage, and "Safe" if it isn't.
floods <- floods %>%
  mutate(Safety = ifelse(gheight.ft > flood.ft, "Dangerous", "Safe"))

#hist package - hist()
#Explanation: This function creates a Histogram. It takes a single column of numbers and draws a bar chart showing the frequency distribution. 
#It groups the numbers into "bins"/groups to show if the data is clumped together or spread out.
#Key arguments:
#x: The vector of numbers you want to plot (usually data$column).
#main: (Optional) The title of the chart.
#xlab: (Optional) The label for the x-axis.
#breaks: (Optional) Controls how many bars/bins are drawn.
#Example:
#Visualize the distribution of all river heights to see if most rivers are low or high over time
river <- floods %>%
  filter(siteID == 2256500)
hist(river$gheight.ft, 
     main = "Distribution of River Heights", 
     xlab = "Height in Feet",
     col = "blue")

# Question 5---- 
# Copy the url for your R script from GitHub and paste it here.
# https://github.com/MaxKarpinski10/ENVST325_A2.git




