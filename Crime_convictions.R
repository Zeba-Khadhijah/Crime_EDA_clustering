#Analysis of the Convictions data of the Crown Prosecution Service

getwd()

#load required packages 
library(readr)
library(tidyverse)
library(plyr)
library(dplyr)
library(psych)
library(ggplot2)
library(Hmisc)
library(stats)
library(corrplot)
library(factoextra)

#listing files
myfiles = list.files(path= c("Dataset/2014", 
                             "Dataset/2015",
                             "Dataset/2016"), 
                     pattern="*.csv",full.names=TRUE)
myfiles

#get data frame for 2014-2016
df = ldply(myfiles, read_csv)

#DATA PRE-PROCESSING

#add column for year
df$Year[990:1419] <- rep("2016",430)
df$Year[517:989] <- rep("2015", 473)
df$Year[1:516] <- rep("2014", 516)

#add column for month
df$Month[1:43] <- rep("April", 43)
df$Month[44:86] <- rep("August", 43)
df$Month[87:129] <- rep("December",43)
df$Month[130:172] <- rep("February",43)
df$Month[173:215] <- rep("January", 43)
df$Month[216:258] <- rep("July", 43)
df$Month[259:301] <- rep("June",43)
df$Month[302:344] <- rep("March",43)
df$Month[345:387] <- rep("May",43)
df$Month[388:430] <- rep("November",43)
df$Month[431:473] <- rep("October",43)
df$Month[474:516] <- rep("September",43)

df$Month[517:559]<- rep("April",43)
df$Month[560:602] <- rep("August", 43)
df$Month[603:645] <- rep("December",43)
df$Month[646:688] <- rep("February",43)
df$Month[689:731] <- rep("January", 43)
df$Month[732:774] <- rep("July", 43)
df$Month[775:817] <- rep("June",43)
df$Month[818:860] <- rep("March",43)
df$Month[861:903] <- rep("May",43)
df$Month[904:946] <- rep("October",43)
df$Month[947:989] <- rep("September",43)


df$Month[990:1032]<- rep("April",43)
df$Month[1033:1075] <- rep("August", 43)
df$Month[1076:1118] <- rep("December",43)
df$Month[1119:1161] <- rep("January", 43)
df$Month[1162:1204] <- rep("July", 43)
df$Month[1205:1247] <- rep("June",43)
df$Month[1248:1290] <- rep("May",43)
df$Month[1291:1333] <- rep("November",43)
df$Month[1334:1376] <- rep("October",43)
df$Month[1377:1419] <- rep("September",43)


#get col names
colnames(df)

#get num_con_df
num_con_df <- df[c(1,2,6,10,14,18,22,26,30,34,38,42,46,50,52,53)]

#simplify column names
names(num_con_df) <- c("Area","Homicide","Offence_against_person",
                       "Sexual_offence","Burglary","Robbery",
                       "Theft_handling","Fraud_forgery","Criminal_damage",
                       "Drugs","Public_order","Other",
                       "Motor_offence","Admin_unsuccessful",
                       "Year","Month")

#remove national data
num_con_df = subset(num_con_df, num_con_df$Area!="National")


#Exploratory Data Analysis

#descriptive stats
summary(num_con_df)
lapply(num_con_df, sd)

#histograms

#Function to plot histograms for all numeric columns
plot_histograms <- function(num_con_df) {
  # Select only numeric columns
  numeric_df <- num_con_df %>% select_if(is.numeric)
  
  # Plot histogram for each numeric column
  numeric_df %>% gather() %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30) +
    facet_wrap(~ key, scales = "free") +
    labs(title = "Histograms of Convictions (2014-16)")+
    theme(plot.title = element_text(hjust = 0.5))
}

# Use the function to plot histograms
plot_histograms(num_con_df)

#Box Plots

# Function to plot boxplots for all numeric columns
plot_boxplots <- function(num_con_df) {
  # Select only numeric columns
  numeric_df <- num_con_df %>% select_if(is.numeric)
  
  # Plot boxplot for each numeric column
  numeric_df %>% gather() %>%
    ggplot(aes(x = key, y = value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Boxplot: Convictions (2014-16)")+
    theme(plot.title = element_text(hjust = 0.5))
}

# Use the function to plot boxplots
plot_boxplots(num_con_df)

#pair plots - to show relationship between variables

#data frame with top 5 crimes
top5crime_df <- num_con_df %>% select("Offence_against_person",
                                      "Drugs","Theft_handling",
                                      "Public_order","Motor_offence")

#getting pairs plot
pairs(top5crime_df)

#Exploratory Data Analysis for Conviction Rates Data

#get con_rate_df
con_rate_df <- df[c(1,3,7,11,15,19,23,27,31,35,39,43,47,52,53)]

#simplify column names
names(con_rate_df) <- c("Area","Homicide","Offence_against_person",
                        "Sexual_offence","Burglary","Robbery","Theft_handling",
                        "Fraud_forgery","Criminal_damage","Drugs",
                        "Public_order","Other","Motor_offence","Year","Month")

#remove national data (independent obs)
con_rate_df = subset(con_rate_df, con_rate_df$Area!="National")

#remove % and change columns to numeric
for (i in seq(2,13)) {
  con_rate_df[, i] <- as.numeric(gsub("%", "", con_rate_df[, i]))
}

#confirming data type is numeric
str(con_rate_df)

#descriptive stats
summary(con_rate_df)

#histograms

#Function to plot histograms for all numeric columns
plot_histograms <- function(con_rate_df) {
  # Select only numeric columns
  numeric_df <- con_rate_df %>% select_if(is.numeric)
  
  # Plot histogram for each numeric column
  numeric_df %>% gather() %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30) +
    facet_wrap(~ key, scales = "free") +
    labs(title = "Histograms of Conviction Rates (2014-16)")+
    theme(plot.title = element_text(hjust = 0.5))
}

# Use the function to plot histograms
plot_histograms(con_rate_df)


#box plots

# Function to plot boxplots for all numeric columns
plot_boxplots <- function(con_rate_df) {
  # Select only numeric columns
  numeric_df <- con_rate_df %>% select_if(is.numeric)
  
  # Plot boxplot for each numeric column
  numeric_df %>% gather() %>%
    ggplot(aes(x = key, y = value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Boxplot: Conviction Rates (2014-16)")+
    theme(plot.title = element_text(hjust = 0.5))
}

# Use the function to plot boxplots
plot_boxplots(con_rate_df)

#total convictions by area

# Calculate the totals for areas using group_by() and summarise()
totals <- num_con_df %>%
  group_by(Area) %>%
  summarise_if(is.numeric, sum) %>%
  as.data.frame()

#make new column with total of all crimes
totals <- totals %>% mutate(Total_Convictions = 
                              Homicide + Offence_against_person+ Sexual_offence+
                              Burglary + Robbery+ Theft_handling+ Fraud_forgery+
                              Criminal_damage+ Drugs+Public_order+ Other+
                              Motor_offence)
#plot
ggplot(totals, aes(x = Total_Convictions, y = Area)) + geom_col() + 
  ggtitle("Total Convictions By Area (2014-16)") + xlab("No. of Convictions") + 
  ylab("Area") +theme(axis.text.y = element_text(size = 6))


#YEAR WISE TRENDS OF NO. OF CONVICTIONS

#get data frame with all years from 2014-2018
#listing files
myfiles_2 = list.files(path= c("Dataset/2014", 
                             "Dataset/2015",
                             "Dataset/2016",
                             "Dataset/2017", 
                             "Dataset/2018"), 
                     pattern="*.csv",full.names=TRUE)
myfiles_2

#get data frame for 2014-2018
all_years_df = ldply(myfiles_2, read_csv)

#add year column
all_years_df$Year[1807:2193] <- rep("2018", 387)
all_years_df$Year[1420:1806] <- rep("2017", 387)
all_years_df$Year[990:1419] <- rep("2016",430)
all_years_df$Year[517:989] <- rep("2015", 473)
all_years_df$Year[1:516] <- rep("2014", 516)

#colnames
colnames(all_years_df)

#get con rates data for all years
r_all_years_df <- all_years_df[c(1,3,7,11,15,19,23,27,31,35,39,43,47,52)]

#simplify column names
names(r_all_years_df) <- c("Area","Homicide","Offence_against_person",
                           "Sexual_offence","Burglary","Robbery",
                           "Theft_handling","Fraud_forgery","Criminal_damage",
                           "Drugs","Public_order", "Other",
                           "Motor_offence", "Year")


#keep only national data
r_all_years_df = subset(r_all_years_df, r_all_years_df$Area=="National")

#remove % and change columns to numeric
for (i in seq(2,13)) {
  r_all_years_df[, i] <- as.numeric(gsub("%", "", r_all_years_df[, i]))
}

# Calculate the mean conviction rates for years
mean_con_df <- r_all_years_df %>%
  group_by(Year) %>%
  summarise_if(is.numeric, mean) %>%
  as.data.frame()

# year wise trend for each crime
#homicide
ggplot(mean_con_df, aes(Year, Homicide, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Homicide Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")

#offences against the person
ggplot(mean_con_df, aes(Year, Offence_against_person, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Offences against the person Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")

#Sexual Offences
ggplot(mean_con_df, aes(Year, Sexual_offence, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Sexual Offences Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")

#burglary
ggplot(mean_con_df, aes(Year, Burglary, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Burglary Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")

#Robbery
ggplot(mean_con_df, aes(Year, Other, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Robbery Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")

#Theft
ggplot(mean_con_df, aes(Year, Theft_handling, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Theft and Handling Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")


#Fraud
ggplot(mean_con_df, aes(Year, Fraud_forgery, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Fraud and Forgery Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")


#Criminal damage
ggplot(mean_con_df, aes(Year, Criminal_damage, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Criminal Damage Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")


#drugs
ggplot(mean_con_df, aes(Year, Drugs, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Drug Offences Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")

#public order
ggplot(mean_con_df, aes(Year, Public_order, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Public Order Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")

#other
ggplot(mean_con_df, aes(Year, Other, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Other Offences Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")

#motor offences
ggplot(mean_con_df, aes(Year, Motor_offence, group = 1)) +
  geom_point() + geom_path()+
  ggtitle("Year Wise Trends of Motor Offences Convicition Rate") +
  xlab("Year") +
  ylab("Conviction Rate")



#CLUSTERING MODELS

#get cluster df
cluster_df <- df[c(1,2,6,10,14,18,22,26,30,34,38,46)]

#simplify column names
names(cluster_df) <- c("Area","Homicide","Offence_against_person",
                       "Sexual_offence","Burglary","Robbery",
                       "Theft_handling","Fraud_forgery","Criminal_damage",
                       "Drugs","Public_order","Motor_offence")


#remove national data
cluster_df = subset(cluster_df, cluster_df$Area!="National")

# Calculate the totals for areas
cluster_df <- cluster_df %>%
  group_by(Area) %>%
  summarise_if(is.numeric, sum) %>%
  as.data.frame()

#transpose
clust2_df <- data.frame(t(cluster_df[-1]))
colnames(clust2_df) <- cluster_df[, 1]


#replicability
set.seed(123)

#K-means clustering

# Compute k-means with k = 3
res.km <- kmeans(scale(clust2_df[-c(1)]), 3, nstart = 25)

# K-means clusters showing the group of each individuals
res.km$cluster

#plot
fviz_cluster(res.km, data = clust2_df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())


