total <- read.csv(file="E:/MS docs/UNCC Docs/KDD/kdd project/final_merged_file.csv", stringsAsFactors = FALSE)
total_df <- data.frame(total)

dtree_df <- data.frame(total_df$Region, total_df$temp_category, total_df$GHG_category, total_df$Energy_category, total_df$Industrial_category, total_df$Agri_category, total_df$Waste_category, total_df$Land_category)

bound <- floor((nrow(dtree_df)/4)*3)         #define % of training and test set

sampled_merged_data1 <- dtree_df[sample(nrow(dtree_df)), ]           #sample rows 
merged_data.train1 <- sampled_merged_data1[1:bound, ]              #get training set
merged_data.test1 <- sampled_merged_data1[(bound+1):nrow(dtree_df), ]    #get test set



install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
dtm <- rpart(merged_data.train1$total_df.temp_category ~ ., merged_data.train1, method = "class")
dtm
plot(dtm)
text(dtm)

p <- predict(dtm, merged_data.test1, method = "class")
p





