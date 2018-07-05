emission <- read.csv("E:/MS docs/UNCC Docs/KDD/kdd project/CAIT_US_GAS_Emissions.csv", skip = 3)

df_emission <- data.frame(emission)

names(df_emission)

emission_usa <- subset(df_emission, df_emission$State == "United States")

library(ggplot2)

ggplot(emission_usa, aes(x=emission_usa$Year, y=emission_usa$Total.GHG.Emissions.Excluding.LUCF..MtCO2e.)) + geom_bar(stat = "identity", width = 0.5) +
  scale_x_continuous(breaks=seq(1989,2012,by=1), limits=c(1989,2012)) +
  ggtitle("GHG Emissions per Year for the US") +
  labs(x="Year",y="GHG Emissions")

library(ggplot2)
ggplot(emission_usa, aes(x=emission_usa$Year,y=emission_usa$Total.GHG.Emissions.Excluding.LUCF..MtCO2e.)) + geom_line()+
  scale_x_continuous(breaks=seq(1989,2012,by=1), limits=c(1989,2012)) +
  ggtitle("GHG Emissions per Year for the US") +
  labs(x="Year",y="GHG Emissions")



library(ggplot2)
ggplot(emission_usa, aes(x=emission_usa$Year)) + geom_line(aes(y = emission_usa$Total.CH4..MtCO2e.), color = "red")+geom_line(aes(y = emission_usa$Total.CO2..excluding.LUCF...MtCO2e.), color = "blue")+geom_line(aes(y = emission_usa$Total.N2O..MtCO2e.), color = "green")+geom_line(aes(y = emission_usa$Total.F.Gas..MtCO2e.), color = "black") +
  scale_x_continuous(breaks=seq(1990,2013,by=1), limits=c(1990,2013)) +
    ggtitle("GHG Emissions for each gas per Year in the US") +
    labs(x="Year",y="GHG Emissions")

sd_CH4 <- sd(emission_usa$Total.CH4..MtCO2e.)*sqrt((length(emission_usa$Total.CH4..MtCO2e.)-1)/(length(emission_usa$Total.CH4..MtCO2e.)))
mean_CH4 <- mean(emission_usa$Total.CH4..MtCO2e.)
sd_CH4
mean_CH4
zscore.CH4 <- (emission_usa$Total.CH4..MtCO2e. - mean_CH4) / sd_CH4
zscore.CH4
hist(zscore.CH4, breaks=11,xlim=c(-5,5), main="histogram of Z-score (ch4)",
     xlab="Emission",ylab="Counts")
box(which = "plot", lty="solid", col="black")

  
sd_CO2 <- sd(emission_usa$Total.CO2..excluding.LUCF...MtCO2e.)*sqrt((length(emission_usa$Total.CO2..excluding.LUCF...MtCO2e.)-1)/(length(emission_usa$Total.CO2..excluding.LUCF...MtCO2e.)))
mean_CO2 <- mean(emission_usa$Total.CO2..excluding.LUCF...MtCO2e.)
sd_CO2
mean_CO2
zscore.CO2 <- (emission_usa$Total.CO2..excluding.LUCF...MtCO2e. - mean_CO2) / sd_CO2

zscore.CO2
hist(zscore.CO2, breaks=11,xlim=c(-5,5), main="histogram of Z-score (CO2)",
     xlab="Emission",ylab="Counts")
box(which = "plot", lty="solid", col="black")

sd_N2O <- sd(emission_usa$Total.N2O..MtCO2e.)*sqrt((length(emission_usa$Total.N2O..MtCO2e.)-1)/(length(emission_usa$Total.N2O..MtCO2e.)))
mean_N2O <- mean(emission_usa$Total.N2O..MtCO2e.)
sd_N2O
mean_N2O
zscore.N2O <- (emission_usa$Total.N2O..MtCO2e. - mean_N2O)/ sd_N2O
zscore.N2O
hist(zscore.N2O, breaks=11,xlim=c(-5,5), main="histogram of Z-score (N2O)",
     xlab="Emission",ylab="Counts")
box(which = "plot", lty="solid", col="black")

sd_F <- sd(emission_usa$Total.F.Gas..MtCO2e.)*sqrt((length(emission_usa$Total.F.Gas..MtCO2e.)-1)/(length(emission_usa$Total.F.Gas..MtCO2e.)))
mean_F <- mean(emission_usa$Total.F.Gas..MtCO2e.)
sd_F
mean_F
zscore.F <- (emission_usa$Total.F.Gas..MtCO2e. - mean_F) / sd_F
zscore.F
hist(zscore.F, breaks=11,xlim=c(-5,5), main="histogram of Z-score (F)",
     xlab="Emission",ylab="Counts")
box(which = "plot", lty="solid", col="black")

df <- data.frame(emission_usa$Year, zscore.CH4, zscore.CO2, zscore.N2O, zscore.F)

df$emission_usa.Year <- as.character(df$emission_usa.Year)

class(df$emission_usa.Year)

library(reshape2)
dd <- melt(df, id = "emission_usa.Year")

dd$emission_usa.Year = as.numeric(dd$emission_usa.Year)
library(ggplot2)
ggplot(dd, aes(x=dd$emission_usa.Year)) + geom_line(aes(y = value, color = variable))+ #geom_line(aes(y = df$zscore.CO2), color = "zscore.CO2")+geom_line(aes(y = df$zscore.N2O), color = "zscore.N2O") + geom_line(aes(y = df$zscore.F), color = "zscore.F") + 
  scale_colour_manual(values = c("red","blue","green","black")) +
  scale_x_continuous(breaks=seq(1990,2013,by=1), limits=c(1990,2013)) +
  scale_y_continuous(breaks=seq(-5,5,by=1), limits=c(-5,5)) +
  ggtitle("GHG Emissions(Standardized) per Year for the US") +
  labs(x="Year",y="GHG Emissions") 


boxplot(emission_usa$Total.GHG.Emissions.Excluding.LUCF..MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for GHG Emissions per year for the US")
boxplot(emission_usa$Total.CO2..excluding.LUCF...MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for CO2 Emissions per year for the US")
boxplot(emission_usa$Total.CH4..MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for CH4 Emissions per year for the US")
boxplot(emission_usa$Total.N2O..MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for N2O Emissions per year for the US")
boxplot(emission_usa$Total.F.Gas..MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for F Emissions per year for the US")
boxplot(emission_usa$Energy..MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for Energy Emissions per year for the US")
boxplot(emission_usa$Industrial.Processes..MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for Industrial Emissions per year for the US")
boxplot(emission_usa$Agriculture..MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for Agricultural Emissions per year for the US")
boxplot(emission_usa$Waste..MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for Waste Emissions per year for the US")
boxplot(emission_usa$Land.Use.and.Forestry..MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for Land Emissions per year for the US")
boxplot(emission_usa$Bunker.Fuels..MtCO2e., ylab = "GHG Emission Values", main = "Boxplot for Bunker Fuel Emissions per year for the US")

sd_energy <- sd(emission_usa$Energy..MtCO2e.)*sqrt((length(emission_usa$Energy..MtCO2e.)-1)/(length(emission_usa$Energy..MtCO2e.)))
mean_energy <- mean(emission_usa$Energy..MtCO2e.)
sd_energy
mean_energy
zscore.energy <- (emission_usa$Energy..MtCO2e. - mean_energy) / sd_energy
zscore.energy
hist(zscore.energy, breaks=11,xlim=c(-5,5), main="Histogram of Z-score(Energy)",
     xlab="Emission",ylab="Counts")
box(which = "plot", lty="solid", col="black")

sd_ind <- sd(emission_usa$Industrial.Processes..MtCO2e.)*sqrt((length(emission_usa$Industrial.Processes..MtCO2e.)-1)/(length(emission_usa$Industrial.Processes..MtCO2e.)))
mean_ind <- mean(emission_usa$Industrial.Processes..MtCO2e.)
sd_ind
mean_ind
zscore.ind <- (emission_usa$Industrial.Processes..MtCO2e. - mean_ind) / sd_ind
zscore.ind
hist(zscore.ind, breaks=11,xlim=c(-5,5), main="Histogram of Z-score(Industrial)",
     xlab="Emission",ylab="Counts")
box(which = "plot", lty="solid", col="black")

sd_agr <- sd(emission_usa$Agriculture..MtCO2e.)*sqrt((length(emission_usa$Agriculture..MtCO2e.)-1)/(length(emission_usa$Agriculture..MtCO2e.)))
mean_agr <- mean(emission_usa$Agriculture..MtCO2e.)
sd_agr
mean_agr
zscore.agr <- (emission_usa$Agriculture..MtCO2e. - mean_agr) / sd_agr
zscore.agr
hist(zscore.agr, breaks=11,xlim=c(-5,5), main="Histogram of Z-score(Agriculture)",
     xlab="Emission",ylab="Counts")
box(which = "plot", lty="solid", col="black")

sd_waste <- sd(emission_usa$Waste..MtCO2e.)*sqrt((length(emission_usa$Waste..MtCO2e.)-1)/(length(emission_usa$Waste..MtCO2e.)))
mean_waste <- mean(emission_usa$Waste..MtCO2e.)
sd_waste
mean_waste
zscore.waste <- (emission_usa$Waste..MtCO2e. - mean_waste) / sd_waste
zscore.waste
hist(zscore.waste, breaks=11,xlim=c(-5,5), main="Histogram of Z-score(Waste)",
     xlab="Emission",ylab="Counts")
box(which = "plot", lty="solid", col="black")

sd_land <- sd(emission_usa$Land.Use.and.Forestry..MtCO2e.)*sqrt((length(emission_usa$Land.Use.and.Forestry..MtCO2e.)-1)/(length(emission_usa$Land.Use.and.Forestry..MtCO2e.)))
mean_land <- mean(emission_usa$Land.Use.and.Forestry..MtCO2e.)
sd_land
mean_land
zscore.land <- (emission_usa$Land.Use.and.Forestry..MtCO2e. - mean_land) / sd_land
zscore.land
hist(zscore.land, breaks=11,xlim=c(-5,5), main="Histogram of Z-score(Land)",
     xlab="Emission",ylab="Counts")
box(which = "plot", lty="solid", col="black")

sd_bunk <- sd(emission_usa$Bunker.Fuels..MtCO2e.)*sqrt((length(emission_usa$Bunker.Fuels..MtCO2e.)-1)/(length(emission_usa$Bunker.Fuels..MtCO2e.)))
mean_bunk <- mean(emission_usa$Bunker.Fuels..MtCO2e.)
sd_bunk
mean_bunk
zscore.bunk <- (emission_usa$Bunker.Fuels..MtCO2e. - mean_bunk) / sd_bunk
zscore.bunk
hist(zscore.bunk, breaks=11,xlim=c(-5,5), main="Histogram of Z-score(Bunker Fuel)",
     xlab="Emission",ylab="Counts")
box(which = "plot", lty="solid", col="black")


df1 <- data.frame(emission_usa$Year, zscore.energy, zscore.ind, zscore.agr, zscore.waste, zscore.land, zscore.bunk)

df1$emission_usa.Year <- as.character(df1$emission_usa.Year)
library(reshape2)
dd1 <- melt(df1, id = "emission_usa.Year")

dd1$emission_usa.Year = as.numeric(dd1$emission_usa.Year)
library(ggplot2)
ggplot(dd1, aes(x=dd1$emission_usa.Year)) + geom_line(aes(y = value, color = variable))+ #geom_line(aes(y = df$zscore.CO2), color = "zscore.CO2")+geom_line(aes(y = df$zscore.N2O), color = "zscore.N2O") + geom_line(aes(y = df$zscore.F), color = "zscore.F") + 
  scale_colour_manual(values = c("red","blue","green","black","yellow", "orange")) +
  scale_x_continuous(breaks=seq(1990,2013,by=1), limits=c(1990,2013)) +
  scale_y_continuous(breaks=seq(-5,5,by=1), limits=c(-5,5)) +
  ggtitle("GHG Emissions(Standardized) per Year for the US") +
  labs(x="Year",y="GHG Emissions") 

invsqrt_GHG <- 1/sqrt(emission_usa$Total.GHG.Emissions.Excluding.LUCF..MtCO2e.)
par(mfrow = c(1,1))
qqnorm(invsqrt_GHG,
       datax = TRUE,
       col = "red",
       ylim = c(0.011, 0.0150),
       main = "Normal Q-Q Plot of Inverse
       Square root of GHG Emission")
qqline(invsqrt_GHG,
       col = "blue",
       datax = TRUE)

qqnorm(emission_usa$Total.GHG.Emissions.Excluding.LUCF..MtCO2e.)
qqline(emission_usa$Total.GHG.Emissions.Excluding.LUCF..MtCO2e.)

invsqrt_CO2 <- 1/sqrt(emission_usa$Total.CO2..excluding.LUCF...MtCO2e.)
par(mfrow = c(1,1))
qqnorm(invsqrt_CO2,
       datax = TRUE,
       col = "red",
       ylim = c(0.011, 0.0150),
       main = "Normal Q-Q Plot of Inverse
       Square root of CO2 Emission")
qqline(invsqrt_CO2,
       col = "blue",
       datax = TRUE)

invsqrt_CH4 <- 1/sqrt(emission_usa$Total.CH4..MtCO2e.)
par(mfrow = c(1,1))
qqnorm(invsqrt_CH4,
       datax = TRUE,
       col = "red",
       ylim = c(0.035, 0.050),
       main = "Normal Q-Q Plot of Inverse
       Square root of CH4 Emission")
qqline(invsqrt_CH4,
       col = "blue",
       datax = TRUE)

invsqrt_N2O <- 1/sqrt(emission_usa$Total.N2O..MtCO2e.)
par(mfrow = c(1,1))
qqnorm(invsqrt_N2O,
       datax = TRUE,
       col = "red",
       ylim = c(0.050, 0.060),
       main = "Normal Q-Q Plot of Inverse
       Square root of N2O Emission")
qqline(invsqrt_N2O,
       col = "blue",
       datax = TRUE)

invsqrt_F <- 1/sqrt(emission_usa$Total.F.Gas..MtCO2e.)
par(mfrow = c(1,1))
qqnorm(invsqrt_F,
       datax = TRUE,
       col = "red",
       ylim = c(0.08, 0.20),
       main = "Normal Q-Q Plot of Inverse
       Square root of F Emission")
qqline(invsqrt_F,
       col = "blue",
       datax = TRUE)

invsqrt_energy <- 1/sqrt(emission_usa$Energy..MtCO2e.)
par(mfrow = c(1,1))
qqnorm(invsqrt_energy,
       datax = TRUE,
       col = "red",
       ylim = c(0.0110, 0.015),
       main = "Normal Q-Q Plot of Inverse
       Square root of Energy Emission")
qqline(invsqrt_energy,
       col = "blue",
       datax = TRUE)

plot(density(emission_usa$Total.GHG.Emissions.Excluding.LUCF..MtCO2e.))

u <- unique(df_emission$State)
u

mylist <- list()
k <- 1
sum_avg <-0

for(j in u){
  
  for(i in 1990:2011){
    
    
    demoST <- subset(df_emission, df_emission$Year == i & df_emission$State == j )
    demoAvgST  <- mean(demoST$Total.GHG.Emissions.Excluding.LUCF..MtCO2e., na.rm = TRUE)
    sum_avg <- sum_avg + demoAvgST
    
  }
  avg_states <- (sum_avg/22)
  df_us_emission_ST <- data.frame(j, avg_states)
  mylist[[k]] <- df_us_emission_ST
  k <- k+1
  sum_avg<-0
}

temp <- do.call("rbind", mylist)
temp

temp_all <- subset(temp, temp$j != "United States")

write.csv(temp_all, file = "E:/MS docs/UNCC Docs/KDD/kdd project/avg_state_emission.csv")


library(ggplot2)
#qplot(x=temp$j, y=temp$avg_states)
#ggplot(temp, aes(x=temp$j, y=temp$avg_states)) + geom_point()

ggplot(temp_all,aes(x=temp_all$j, y=temp_all$avg_states)) + geom_bar(stat = "identity", width = 0.5) 

boxplot(temp_all$avg_states, main = "22 Years Average Emission, Statewise")

new_temp_all <- subset(temp_all, temp_all$j != "Alaska" & temp_all$j != "Hawaii")
new_temp_all

new_temp_all$region <- new_temp_all$j
new_temp_all

all_states <- map_data("state")
all_states

merged_df <- merge(x = all_states, y = new_temp_all, by = NULL)

install.packages("sqldf")

library(sqldf)

df4 <- sqldf("SELECT all_states.region, long, lat, new_temp_all.avg_states 
              FROM all_states
              LEFT JOIN new_temp_all USING(region)")

write.csv(df4, file = "E:/MS docs/UNCC Docs/KDD/kdd project/map.csv")

map1 <- read.csv("E:/MS docs/UNCC Docs/KDD/kdd project/avgstates.csv")
map2_df <- data.frame(map1)

p <- ggplot()
p <- p + geom_polygon(data=map2_df, aes(x=long, y=lat, fill=map2_df$avg_states),colour="white"
) + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Black to White Incarceration Rates \n Weighted by Relative Population" 
                             ,title = "State Incarceration Rates by Race, 2010", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())

library(ggmap)
map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')

#install.packages("ggmap")

ggmap(map) + geom_point(
  aes(x=long, y=lat, show_guide = TRUE, colour=map2_df$avg_states), 
  data=map2_df, alpha=.5, na.rm = T)  + 
  scale_color_gradient(low="beige", high="blue")


