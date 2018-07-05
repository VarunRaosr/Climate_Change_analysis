merged_data <- read.csv(file="integrated.csv", stringsAsFactors = FALSE)
merged_data_df <- data.frame(merged_data)
bound <- floor((nrow(merged_data_df)/4)*3)         #define % of training and test set

sampled_merged_data <- merged_data_df[sample(nrow(merged_data_df)), ]           #sample rows 
merged_data.train <- sampled_merged_data[1:bound, ]              #get training set
merged_data.test <- sampled_merged_data[(bound+1):nrow(merged_data_df), ]    #get test set

total_co2 <- merged_data.train$Total_C02
total_ch4 <- merged_data.train$Total_ch4
total_n2o <- merged_data.train$Total_N20
total_f <- merged_data.train$Total_Fgas
total_state <- merged_data.train$State

avg_temp <- merged_data.train$Annlual_Temp
# Create regression line
mreg.out <- lm(avg_temp~total_co2 + total_ch4 + total_n2o + total_f + total_state)
summary(mreg.out)

mreg.int <- predict(mreg.out,
                    data.frame(total_co2 = 10.881645,
                               total_ch4 = 0.39696328,
                               total_n2o = 0.3215282,
                               total_f = 0.08639798,
                               total_state = 'Rhode Island'),
                    interval = "prediction")
mreg.int
