#Reading the Tmperatures by Country Dataset into a data frame
country <- read.csv("GlobalLandTemperaturesByCountry.csv")

df_country <- data.frame(country)

dim(df_country)
names(df_country)
str(df_country)
attributes(df_country)

sapply(df_country, function(x) sum(is.na(x)))

#Creating a subset of the actual data with data from year 1990 to 2013.
country_year <- subset(df_country,as.Date(df_country$dt) > '1989-12-31')

country_year

sapply(country_year, function(x) sum(is.na(x)))

year <-substring(country_year$dt,1,4)

year

#Add a new column year to the data frame
country_year$year=substring(country_year$dt,1,4)

country_year

# Subset of country_year without Antarctica

country_temp <- subset(country_year,country_year$Country != 'Antarctica')

country_temp

#x1990 <- (country_temp$AverageTemperature[country_temp$year == "1990"])

#mean(x1990)

typeof(country_temp$year)


u <- unique(as.numeric(country_temp$year))
u

 sapply(country_temp, function(x) sum(is.na(x)))

 #install.packages("mice")
 
library(mice)
 
md.pattern(country_temp)
 
library(VIM)
aggr_plot <- aggr(country_temp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(country_temp), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# As the percentage of missing values is only 0.32%, we can ignore the missing values.

#Creating a subset of country_temp that includes the data only for United States

country_usa <- subset(country_temp,country_temp$Country == 'United States')

country_usa

#Average yearly temperature for United States
us_avg_temp <- vector(mode = "double", length = 24)
year <- vector(mode = "double", length = 24)
j<-1

for(i in 1990:2013){
  
  demosub <- subset(country_usa, year == i)
  demoAvg  <- mean(demosub$AverageTemperature, na.rm = TRUE)
  us_avg_temp[j] <- demoAvg
  year[j] <- i
  j <- j+1
  print(demoAvg)
}

print(year)
print(us_avg_temp)

df_us_temp <- data.frame(year, us_avg_temp)
df_us_temp

#Plotting the Yearly Average Temperature for the United States and the World
library(ggplot2)
ggplot(df_us_temp, aes(x=df_us_temp$year, y=df_us_temp$us_avg_temp)) + geom_line()+
  scale_x_continuous(breaks=seq(1990,2013,by=1), limits=c(1990,2013)) 



world_avg_temp <- vector(mode = "double", length = 24)
year <- vector(mode = "double", length = 24)
j<-1

for(i in 1990:2013){
  
  demosub <- subset(country_temp, year == i)
  demoAvg  <- mean(demosub$AverageTemperature, na.rm = TRUE)
  world_avg_temp[j] <- demoAvg
  year[j] <- i
  j <- j+1
  #print(demoAvg)
}

print(year)
print(world_avg_temp)

df_world_temp <- data.frame(year, world_avg_temp)
#df_world_temp

library(ggplot2)
ggplot(df_world_temp, aes(x=df_world_temp$year, y=df_world_temp$world_avg_temp)) + geom_line()+
  scale_x_continuous(breaks=seq(1990,2013,by=1), limits=c(1990,2013)) 

merged_df <- merge(df_us_temp,df_world_temp,by="year")
merged_df


plot(df_us_temp,type = "o",col = "red", xlab = "Year", ylab = "Temp", 
     main = "Avg Temp")

lines(df_world_temp, type = "o", col = "blue")

#Comparing the Yearly Average Temperatures of the World with US
library(ggplot2)
ggplot(merged_df, aes(x=merged_df$year)) + geom_line(aes(y = merged_df$us_avg_temp), color = "red")+geom_line(aes(y = merged_df$world_avg_temp), color = "blue")+
  scale_x_continuous(breaks=seq(1990,2013,by=1), limits=c(1990,2013)) +
    scale_y_continuous(breaks=seq(7,21,by=1), limits=c(7,21))
  

sd_us_temp <- sd(merged_df$us_avg_temp)*sqrt((length(merged_df$us_avg_temp)-1)/(length(merged_df$us_avg_temp)))
mean_us_temp <- mean(merged_df$us_avg_temp)
sd_us_temp
mean_us_temp
zscore.us <- (merged_df$us_avg_temp - mean_us_temp) / sd_us_temp
zscore.us
hist(zscore.us, breaks=11,xlim=c(-5,5), main="histogram of Z-score",
     xlab="Temp",ylab="Counts")
box(which = "plot", lty="solid", col="black")

sd_world_temp <- sd(merged_df$world_avg_temp)*sqrt((length(merged_df$world_avg_temp)-1)/(length(merged_df$world_avg_temp)))
mean_world_temp <- mean(merged_df$world_avg_temp)
sd_world_temp
mean_world_temp
zscore.world <- (merged_df$world_avg_temp - mean_world_temp) / sd_world_temp
zscore.world

df_zscore_world <- data.frame(year, zscore.world)
df_zscore_us <- data.frame(year, zscore.us)

merged_df_zscore <- merge(df_zscore_world,df_zscore_us,by="year")
merged_df_zscore

#Comparing the Yearly Average Temperatures of the World with US based on zscore
library(ggplot2)
ggplot(df_zscore, aes(x=merged_df_zscore$year)) + geom_line(aes(y = merged_df_zscore$zscore.world), color = "red")+geom_line(aes(y = merged_df_zscore$zscore.us), color = "blue")+
  scale_x_continuous(breaks=seq(1990,2013,by=1), limits=c(1990,2013)) +
  scale_y_continuous(breaks=seq(-5,5,by=1), limits=c(-5,5))

boxplot(df_us_temp$us_avg_temp, ylab = "Average Temperature", main = "Boxplot for Average Temperatures of US")
boxplot(df_world_temp$world_avg_temp, ylab = "Average Temperature", main = "Boxplot for Average Temperatures of the world")

        
##################################################################################################################################
#Reading the Temperatures by States of the United States into a data frame.
stateRelated <- read.csv(file="E:/MS docs/UNCC Docs/KDD/kdd project/Usa_states.csv", stringsAsFactors=TRUE)
View(stateRelated)

df_StateRelated <- data.frame(stateRelated)

names(df_StateRelated)
sapply(df_StateRelated, function(x) sum(is.na(x)))

library(mice)

md.pattern(df_StateRelated)

library(VIM)
aggr_plot <- aggr(df_StateRelated, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(country_temp), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#The percentage of missing values is negligible. Hence we can ignore it.

u <- unique(df_StateRelated$State)
u

length(u)

#Calculating the 24 Years Average Temperature, Statewise

u <- unique(df_StateRelated$State)
u

length(u)

mylist <- list()
k <- 1
sum_avg <-0
state_vector <- vector(mode = "numeric", length = 24)


for(j in u){
  
  for(i in 1990:2013){
    
    
    demoST <- subset(df_StateRelated, Year == i & df_StateRelated$State == j )
    demoAvgST  <- mean(demoST$AverageTemperature, na.rm = TRUE)
    sum_avg <- sum_avg + demoAvgST
    
    
  }
  avg_states <- (sum_avg/24)
  df_us_temp_ST <- data.frame(j, avg_states)
  mylist[[k]] <- df_us_temp_ST
  k <- k+1
  sum_avg<-0
  
  
}

#mylist[[51]]

df_avg_temp <- data.frame(j, avg_states)

temp <- do.call("rbind", mylist)
temp

class(temp)

#Plotting the 24 Years Average Temperature, Statewise

library(ggplot2)
#qplot(x=temp$j, y=temp$avg_states)
#ggplot(temp, aes(x=temp$j, y=temp$avg_states)) + geom_point()

ggplot(temp,aes(x=temp$j, y=temp$avg_states)) + geom_bar(stat = "identity", width = 0.5, space = 0) +
  scale_y_continuous(breaks=seq(-5,30,by=5), limits=c(-5,30))

boxplot(temp$avg_states, main = "24 Years Average Temperature, Statewise")
