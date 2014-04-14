# Homework 1 for Data Science 350.

# By Josh Lohrenz (UW NetID lohrej)

###########################################################################
# 1) Abalone data


rm(list=ls()) #clear the workspace
x <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=FALSE) #import data into dataframe

# name the columns
names(x)<-c('Sex','Length', 'Diameter', 'Height', 'Whole.weight', 'Shucked.weight', 'Viscera.weight', 'Shell.weight', 'Rings')

#explanation of data is here: http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.names
summary(x)

x$Age= x$Rings+ 1.5 #create an age variable, based on the fact that age = rings+1.5

#   a) Use the command "quantile" to find the deciles (10 groups) for height from the complete data set. Hint: You may find the command "seq" helpful.
#parameter for what kind of q-quantile we wish to make. q=10 for deciles.
q = quantile(x$Height,seq(0,1,0.1))
q
#   b) Use the command "cut" to assign each height value to the corresponding decile 
#      (e.g. the smallest values are assigned to the first decile and get mapped to the value, 1). 
#      Hint: use 'as.numeric' to get integer values instead of ranges.
x$Height.dec= as.numeric(cut(x$Height, breaks=q))


#   c) Now create a table of age vs height decile. Examine the table and describe what you observe:
Age.Height.table = table(x$Age, x$Height.dec)
Age.Height.table

#   Generally, there appears to be a positive relationship between age and height (as would be expected). 
#   However, among larger abalones this relationship is less distinct, as there is a wider variety of ages represented. 
#   For example, among the smallest decile of abalones, the age range represented is 2.5 to 14.5 (range of 13), whereas among the
#   largest decile the age range is 9.5 to 28.5 (range of 20).


#   d) Another way to aggregate data is averaging. Lets compute the average whole weight of abalone as a function of age and plot the relationship.

#       i) Use the commands "unique" and "sort" to find the unique values of age and store the values in ascending order to a variable named "ua"
ua= sort(unique(x$Age))

#       ii) Use the command sapply to apply a function to each value in "ua". The function should return the mean whole weight of all abalone of a given age.
mw= sapply(ua, function(age) mean(x[x$Age==age,]$Whole.weight))

#         writing out the complete function below
#         mean_weight <- function(age){
#         return(mean(data[data$age==age,]$whole_weight))
#           }
#         mean_weights <- sapply(ua,mean_weight)

#       iii) Finally, use the plot command to plot mean weight vs age. Describe the relationship revealed by the plot.
#            Include an explanation for the behavior seen in the abalone of the 25-30 year age group.
plot(mw ~ ua, t='b')

#           The plot reveals that abalone's grow in mass as they age until around the age of 12, after which point growth plateaus (with some small variations).
#           After the Ablone turns 25 they can increase in size substantially, but it very rarely happens that they live that long. 
#           (7 out of 4177 total were older than 25).



#############################################################################################################################################

#   2) UW Weather Data, 200
weather.data <- read.csv("weather_data_2000_2014.csv", header=TRUE)

#       a) Using summary statistics, histograms, boxplots, or other means identify and describe at least one data quality issue in the dataset.
summary(weather.data)
#boxplot(weather.data) #commented out because of how long it takes to execute.

#       There are many outlier values that are obviously incorrect, so this dataset needs extensive cleaning before analyses can begin.
#       For example, negative temperature, absurdly high/low wind speeds, directions not between 0 and 360.

#       b) Filter the data to remove the questionable data you identified in part a. 
#          How much of the data is affected? Hint: some of the functions "length", "nrow", "ncol", "is.na", and "which" may be helpful.

original.nrow <- nrow(weather.data)


weather.data <- weather.data[weather.data$RHum >= 0 & weather.data$RHum <= 100,] #relative humidity between 0 and 100.
weather.data <- weather.data[abs(weather.data$Temp-median(weather.data$Temp))<100,] #temp within 100 degrees of median (stdev was funky so didn't use it)
weather.data <- weather.data[weather.data$Wind.Direct >= 0 & weather.data$Wind.Direct <= 360,] #wind direction between 0 and 360
weather.data <- weather.data[weather.data$Speed >= 0 & weather.data$Speed <= 150,] #wind speed between 0 and 150 (to get rid of absurdly high speeds)
weather.data <- weather.data[weather.data$Gust >= 0 & weather.data$Gust <= 150,] # ", but with gusts
weather.data <- weather.data[weather.data$Rain >= 0 & weather.data$Rain <= 10,] #rainfall to somewhat realistic levels
weather.data <- weather.data[weather.data$Radiation >= 0 & weather.data$Radiation <= 1500 ,] #gets rid of outlier.

number.bad.records <- original.nrow - nrow(weather.data) #number of bad records we got rid of

#       c) Look for and describe a monthly trend in the data.
table(weather.data$RHum,weather.data$Month)
table(weather.data$Temp,weather.data$Month)

#           You can see the monthly variations in temperature and humidity