#### Weight and Diet ###
#### Frank Goeddeke ####
#### December 2020 #####

#### Personal Data from MyFitnessPal smartphone app #####
#### This program takes export files from MyFitnessPal app
#### and creates and exports three useful jpg charts in your working directory
#### Note, this does not handle missing data well, if you enter all your food and weight every day
#### this should work fairly well
#### Note, not sure how this will work with January 2021 data, it might need tweaking on x-axes

#### Load Packages
library(tidyverse)      # data manipulation and visualization
library(lubridate)      # easily work with dates and times
library(ggplot2)        # plot results
library(tidyquant)      # plot moving averages
library(dplyr)          # wrangle data

#### LOAD WEIGHT AND NUTRITION AND EXERCISE DATA FILES FROM LOCAL DRIVE
#### Customize this to match your file name and local drive
#### Load Weight Data from local drive
weight.df <- read.csv("~/Magic Briefcase/Diet/Measurement-Summary-2020-08-31-to-2020-12-25.csv")
#### Load Nutrient Data from local drive
nutrition.df <- read.csv("~/Magic Briefcase/Diet/Nutrition-Summary-2020-08-31-to-2020-12-25.csv")
#### Load Exercise Data from local drive
exercise.df <- read.csv("~/Magic Briefcase/Diet/Exercise-Summary-2020-08-31-to-2020-12-25.csv")

#### WEIGHT PLOT
#### Convert date column to dates
weight.df$date <- ymd(weight.df$ï..Date)
weight.df$ï..Date <- NULL

####  Plot Weight and export jpg file
jpeg("Weight.jpg", width = 700, height = 700)
weight.df %>%
  ggplot(aes(x = date, y = Weight)) +
  geom_line(linetype = "dotted", size = 1) +                         # Plot weight
  geom_ma(ma_fun = SMA, n = 7, linetype = "solid", color = "red", size = 1)  +       # Plot 7-day SMA
  labs(title = "Weight", subtitle = "Solid Line is 7 Day Simple Moving Average") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
dev.off()

#### NUTRITION PLOT
#### Convert date column to dates
nutrition.df$date <- ymd(nutrition.df$ï..Date)
nutrition.df$ï..Date <- NULL
nutrition.df$Meal <- NULL

# get the mean of each day
nutrition.df2 <- aggregate(nutrition.df[, 0:16], list(nutrition.df$date), sum)

#  Get months
nutrition.df2$Month <- months(nutrition.df2$Group.1)

#  Get years
nutrition.df2$Year <- format(nutrition.df2$Group.1,format="%y")

#aggregate all columns by mean
nutrition.df3 <- aggregate(. ~ Month + Year, nutrition.df2, mean)

# combine dates
nutrition.df3$date <- as.Date(nutrition.df3$Group.1)
format(nutrition.df3$date, format = "%m%Y")

#  rename columns
names(nutrition.df3)[names(nutrition.df3)=="Fat..g."] <- "Fat"
names(nutrition.df3)[names(nutrition.df3)=="Carbohydrates..g."] <- "Carbohydrates"
names(nutrition.df3)[names(nutrition.df3)=="Protein..g."] <- "Protein"

# plot gram  and export chart
nutrition.tb <- nutrition.df3 %>% select(date, Fat, Saturated.Fat, Polyunsaturated.Fat, Monounsaturated.Fat, Trans.Fat, Carbohydrates, Fiber, Sugar, Protein) %>% gather(key = "Variable", value = "Grams", -date)
head(nutrition.tb)

jpeg("nutrition.jpg", width = 700, height = 700)
ggplot(nutrition.tb, aes(x=date, y=Grams)) + 
  geom_line(aes(color = Variable, linetype = Variable)) + 
  geom_point(aes(color = Variable, shape = Variable)) + 
  labs(title = "Average Daily Nutrition Intake Each Month") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks=seq(0, 200, 20))
dev.off()

#### CALORIE AND EXERCISE PLOT
#### Convert date column to dates
exercise.df$date <- ymd(exercise.df$ï..Date)
exercise.df$ï..Date <- NULL

#### Aggregate exercise calories by month
#  Get months
exercise.df$Month <- months(exercise.df$date)

#  Get years
exercise.df$Year <- format(exercise.df$date,format="%y")

#aggregate all columns by mean
exercise.df2 <- aggregate(exercise.df$Exercise.Calories ~ Month + Year, exercise.df, mean)

# combine dates
exercise.df2$date <- ymd(paste(exercise.df2$Year, exercise.df2$Month, "15", sep="-"))

# rename column
names(exercise.df2)[names(exercise.df2)=="exercise.df$Exercise.Calories"] <- "Exercise.Calories"

# round calories to zero decimal places
exercise.df2$Exercise.Calories <- round(exercise.df2$Exercise.Calories, digits = 0)
nutrition.df3$Calories <- round(nutrition.df3$Calories, digits = 0)

#### create and export plot
jpeg("calories.jpg", width = 700, height = 700)
ggplot() +
  geom_line(data = nutrition.df3, aes(x=date, y=Calories), size = 2, color = "green") +  
  geom_point(data = nutrition.df3, aes(date, y=Calories), size = 4, shape = "square", color = "green") +
  geom_text(data = nutrition.df3, aes(label = Calories, x = date, y = Calories + 0.2), position = position_dodge(0.9), vjust = -1) +
  geom_line(data = exercise.df2, aes(x=date, y=Exercise.Calories), size = 2, color = "red") +
  geom_point(data = exercise.df2, aes(date, y=Exercise.Calories), size = 4, shape = "circle", color = "red") +
  geom_text(data = exercise.df2, aes(label = Exercise.Calories, x = date, y = Exercise.Calories + 0.2), position = position_dodge(0.9), vjust = -1) +
  labs(title = "Average Daily Calorie Intake and Exercise Each Month", subtitle = "Green = Calorie Intake, Red = Exercise Calories") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks=seq(0, 3500, 250))
dev.off()
  
