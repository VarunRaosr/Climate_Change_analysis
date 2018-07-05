merged_data1 <- read.csv(file="E:/MS docs/UNCC Docs/KDD/kdd project/integrated.csv", stringsAsFactors = FALSE)
merged_data1_df <- data.frame(merged_data1)

install.packages("discretization")
library(discretization)
install.packages("arules")
library(arules)
temp_discretized <- data.frame(discretize(merged_data1_df$Annlual_Temp, "cluster", categories=3))
temp_discretized$temp_category <- factor(discretize(merged_data1_df$Annlual_Temp, "cluster", categories=3, labels = c("low", "medium", "high")))
temp_discretized$GHG_desc <- discretize(merged_data1_df$Total_CHG_exc_LUCF, "cluster", categories=3)
temp_discretized$GHG_category <- factor(discretize(merged_data1_df$Total_CHG_exc_LUCF, "cluster", categories=3, labels = c("low", "medium", "high")))
temp_discretized$CO2_desc <- discretize(merged_data1_df$Total_C02, "cluster", categories=3)
temp_discretized$CO2_category <- factor(discretize(merged_data1_df$Total_C02, "cluster", categories=3, labels = c("low", "medium", "high")))
temp_discretized$CH4_desc <- discretize(merged_data1_df$Total_ch4, "cluster", categories=3)
temp_discretized$CH4_category <- factor(discretize(merged_data1_df$Total_ch4, "cluster", categories=3, labels = c("low", "medium", "high")))
temp_discretized$F_desc <- discretize(merged_data1_df$Total_Fgas, "cluster", categories=3)
temp_discretized$F_category <- factor(discretize(merged_data1_df$Total_Fgas, "cluster", categories=3, labels = c("low", "medium", "high")))
temp_discretized$Energy_desc <- discretize(merged_data1_df$Energy, "cluster", categories=3)
temp_discretized$Energy_category <- factor(discretize(merged_data1_df$Energy, "cluster", categories=3, labels = c("low", "medium", "high")))
temp_discretized$Industrial_desc <- discretize(merged_data1_df$Industrial_Processes, "cluster", categories=3)
temp_discretized$Industrial_category <- factor(discretize(merged_data1_df$Industrial_Processes, "cluster", categories=3, labels = c("low", "medium", "high")))
temp_discretized$Agri_desc <- discretize(merged_data1_df$Agriculture, "cluster", categories=3)
temp_discretized$Agri_category <- factor(discretize(merged_data1_df$Agriculture, "cluster", categories=3, labels = c("low", "medium", "high")))
temp_discretized$Waste_desc <- discretize(merged_data1_df$Waste, "cluster", categories=3)
temp_discretized$Waste_category <- factor(discretize(merged_data1_df$Waste, "cluster", categories=3, labels = c("low", "medium", "high")))
temp_discretized$Land_desc <- discretize(merged_data1_df$Land_use_and_Forestry, "cluster", categories=2)
temp_discretized$Land_category <- factor(discretize(merged_data1_df$Land_use_and_Forestry, "cluster", categories=2, labels = c("low", "high")))

temp_discretized1 <- data.frame(discretize(merged_data1_df$Total_N20, "cluster", categories=3))
temp_discretized1$N2O_category <- factor(discretize(merged_data1_df$Total_N20, "cluster", categories=3, labels = c("low", "medium", "high")))
write.csv(temp_discretized, file = "E:/MS docs/UNCC Docs/KDD/kdd project/discretization.csv")
merged_data2 <- read.csv(file="E:/MS docs/UNCC Docs/KDD/kdd project/discretization.csv", stringsAsFactors = FALSE)
merged_data2_df <- merged_data2
merged_data2_df$N2O_desc <- temp_discretized1$discretize.merged_data1_df.Total_N20...cluster...categories...3.
merged_data2_df$N2O_category <- temp_discretized1$N2O_category
write.csv(merged_data2_df, file = "E:/MS docs/UNCC Docs/KDD/kdd project/discretization1.csv")



