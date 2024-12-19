install.packages(c("ggplot2", "dplyr", "caret", "data.table", "lubridate",
                   "caret", "randomForest", "terra", "nnet"))
library(ggplot2)
library(dplyr)
library(caret)
library(data.table)
library(lubridate)
library(caret)
library(randomForest)
library(terra)
library(nnet)

#-----------------------------------------------------

##STEP 1: Collect all buoy (Lake Superior water data) into one dataframe

#setup site 45001:

site45001yr2006 <- read.table("Data/45001-2006.txt", header = TRUE)
site45001yr2007 <- read.table("Data/45001-2007.txt", header = TRUE)
site45001yr2012 <- read.table("Data/45001-2012.txt", header = TRUE)
site45001yr2017 <- read.table("Data/45001-2017.txt", header = TRUE)

#make all column names the same
colnames(site45001yr2007) <- colnames(site45001yr2006)
colnames(site45001yr2012) <- colnames(site45001yr2007)
colnames(site45001yr2017) <- colnames(site45001yr2007)

#group
site45001 <- rbind(site45001yr2007, site45001yr2012,
                   site45001yr2017)

#setup site 45006:

site45006yr2006 <- read.table("Data/45006-2006.txt", header = TRUE)
site45006yr2007 <- read.table("Data/45006-2007.txt", header = TRUE)
site45006yr2012 <- read.table("Data/45006-2012.txt", header = TRUE)
site45006yr2017 <- read.table("Data/45006-2017.txt", header = TRUE)

colnames(site45006yr2007) <- colnames(site45006yr2006)
colnames(site45006yr2012) <- colnames(site45006yr2007)
colnames(site45006yr2017) <- colnames(site45006yr2007)

site45006 <- rbind(site45006yr2007, site45006yr2012,
                   site45006yr2017)

#Combine into one data frame
all_buoys <- rbind(site45001, site45006)

#Neaten the variable names
colnames(all_buoys) <- c("year", "month", "day", "hour", "minute", 
                         "wind_dir", "wind_speed", "GST", "wave_height", 
                         "dom_period", "avg_period", "MPD", "pressure", "air_temp", 
                         "surface_temp", "DEWP", "VIS", "TIDE")

#Filter buoy data and average by month
all_buoys <- all_buoys %>%
  filter(year == 2007 & month == 7 | year == 2007 & month == 8
         | year == 2012 & month == 7 | year == 2012 & month == 8
         | year == 2017 & month == 6 | year == 2017 & month == 7
         | year == 2017 & month == 8) %>%
  group_by(year, month) %>%
  summarise(wind_dir = mean(wind_dir), wind_speed = mean(wind_speed), 
            wave_height = mean(wave_height), dom_period = mean(dom_period), 
            avg_period = mean(avg_period), 
            pressure = mean(pressure), air_temp = mean(air_temp), 
            surface_temp = mean(surface_temp), .groups = "drop")

#-----------------------------------------------------

##STEP 2: Collect all data from surrounding sites and combine into one data frame

algae_data_1 <- read.csv("Data/nla2007_phytoplankton_softalgaecount_20091023.csv")
algae_data_2 <- read.csv("Data/nla2012_wide_phytoplankton_count_02122014.csv")
algae_data_3 <- read.csv("Data/nla_2017_phytoplankton_count-data.csv")

#Neaten up dates using lubridate
algae_data_1$parsed_date <- mdy(algae_data_1$DATE_COL)
algae_data_1$month <- month(algae_data_1$parsed_date)
algae_data_1$year <- year(algae_data_1$parsed_date)

algae_data_2$parsed_date <- mdy(algae_data_2$DATE_COL)
algae_data_2$month <- month(algae_data_2$parsed_date)
algae_data_2$year <- year(algae_data_2$parsed_date)

algae_data_3$parsed_date <- mdy(algae_data_3$DATE_COL)
algae_data_3$month <- month(algae_data_3$parsed_date)
algae_data_3$year <- year(algae_data_3$parsed_date)

#extract the sites we want to use for 2007 based on the map
algae_sites_2007 <- algae_data_1 %>%
  filter(SITE_ID == "NLA06608-1274" | SITE_ID == "NLA06608-0890" 
         | SITE_ID == "NLA06608-0782" | SITE_ID == "NLA06608-0526"
         | SITE_ID == "NLA06608-1102" | SITE_ID == "NLA06608-1342"
         | SITE_ID == "NLA06608-0318" | SITE_ID == "NLA06608-1150"
         | SITE_ID == "NLA06608-1758" | SITE_ID == "NLA06608-0734"
         | SITE_ID == "NLA06608-1742" | SITE_ID == "NLA06608-0974"
         | SITE_ID == "NLA06608-0630" | SITE_ID == "NLA06608-1654"
         | SITE_ID == "NLA06608-1018" | SITE_ID == "NLA06608-1038"
         | SITE_ID == "NLA06608-0926" | SITE_ID == "NLA06608-0606"
         | SITE_ID == "NLA06608-ELS:2B2-008" | SITE_ID == "NLA06608-1998"
         | SITE_ID == "NLA06608-1358" | SITE_ID == "NLA06608-2078"
         | SITE_ID == "NLA06608-1398" | SITE_ID == "NLA06608-1206"
         | SITE_ID == "NLA06608-0286")

#get rid of rows where there are no measurements of biovolume or abundance
algae_sites_2007 <- algae_sites_2007 %>%
  filter(BIOVOLUME != "NA" & ABUND != "NA")

#extract the sites we want to use for 2012 based on the map
algae_sites_2012 <- algae_data_2 %>%
  filter(SITE_ID == "NLA12_MN-R14" | SITE_ID == "NLA12_MN-147"
         | SITE_ID == "NLA12_MI-169" | SITE_ID == "NLA12_MI-162"
         | SITE_ID == "NLA12_MI-118" | SITE_ID == "NLA12_WI-135" 
         | SITE_ID == "NLA12_WI-163" | SITE_ID == "NLA12_WI-147" 
         | SITE_ID == "NLA12_MI-116" | SITE_ID == "NLA12_MI-125" 
         | SITE_ID == "NLA12_MI-141" | SITE_ID == "NLA12_MI-137" 
         | SITE_ID == "NLA12_MI-164" | SITE_ID == "NLA12_MI-150")

#get rid of rows where there are no measurements of biovolume or abundance
algae_sites_2012 <- algae_sites_2012 %>%
  filter(BIOVOLUME != "NA" & ABUNDANCE != "NA")

#extract the sites we want to use for 2017 based on the map
algae_sites_2017 <- algae_data_3 %>%
  filter(SITE_ID == "NLA17_WI-10008" | SITE_ID == "NLA17_WI-10080"
         | SITE_ID == "NLA17_WI-10069" | SITE_ID == "NLA17_MI-10018"
         | SITE_ID == "NLA17_MI-10014" | SITE_ID == "NLA17_MI-10089"
         | SITE_ID == "NLA17_MN-10159" | SITE_ID == "NLA17_MN-10005"
         | SITE_ID == "NLA17_MN-HP003" | SITE_ID == "NLA17_MN-10038"
         | SITE_ID == "NLA17_MI-10117" | SITE_ID == "NLA17_MI-10121"
         | SITE_ID == "NLA17_MI-10028" | SITE_ID == "NLA17_MI-10008"
         | SITE_ID == "NLA17_MN-10008" | SITE_ID == "NLA17_MN-10018"
         | SITE_ID == "NLA17_MN-10028" | SITE_ID == "NLA17_WI-HP003"
         | SITE_ID == "NLA17_WI-HP001" | SITE_ID == "NLA17_WI-10112"
         | SITE_ID == "NLA17_WI-10085" | SITE_ID == "NLA17_MI-10073"
         | SITE_ID == "NLA17_MI-10035")

#get rid of rows where there are no measurements of biovolume
algae_sites_2017 <- algae_sites_2017 %>%
  filter(BIOVOLUME != "NA" & ABUNDANCE != "NA")

#Extract only the variables of interest
algae_sites_2007 <- select(algae_sites_2007, SITE_ID, month, year, 
                           BIOVOLUME, ABUND)

algae_sites_2012 <- select(algae_sites_2012, SITE_ID, month, year, 
                           BIOVOLUME, ABUNDANCE)

algae_sites_2017 <- select(algae_sites_2017, SITE_ID, month, year, 
                           BIOVOLUME, ABUNDANCE)

#Fix variable names for 2007 so they match the other two years
colnames(algae_sites_2007) <- colnames(algae_sites_2012)

#Group into one data frame
algae_final <- rbind(algae_sites_2007, algae_sites_2012, 
                     algae_sites_2017)

#Note: We could have done this in fewer lines of code by combining all the
#algae data into one data frame initially and then filtering out the sites we
#used and removing the invalid rows, but this way makes it clearer to someone
#less familiar with the project which sites are coming from which year from the 
#beginning, and avoids one massive command where all 50+ sites are being looked for
#simultaneously.

#Combine all data into one data frame, grouped by month and year
final_data <- merge(all_buoys, algae_final, by = c("month", "year"))

final_data$date <- make_date(final_data$year, final_data$month, day = 1)

#-----------------------------------------------------

##STEP 3: Create graphs and models

#plot 1:
ggplot(final_data, aes(x = date, y = BIOVOLUME)) + geom_point()
ggplot(final_data, aes(x = date, y = ABUNDANCE)) + geom_point()

##Training models

#Standard regression
bio_model <- glm(BIOVOLUME ~ wind_dir + wind_speed + 
                   pressure + air_temp + surface_temp,
                 final_data,
                 family = gaussian)

abund_model <- glm(ABUNDANCE ~ wind_dir + wind_speed + 
                     pressure + air_temp + surface_temp,
                   final_data,
                   family = gaussian)

#Summarize models
summary(bio_model)
summary(abund_model)

#Neural network

#Set seed so that results are consistent
set.seed(100)

#Create variableto separate training and validation sets
validation <- rbinom(nrow(final_data), 1, prob = 0.15)
final_data$training <- ifelse(validation == 0, "train", "validation")

#Make training data frame
training_set <- final_data %>%
  filter(training == "train")

#Make validation data frame
validation_set <- final_data %>%
  filter(training == "validation")

#scale before training
training_proc <- preProcess(training_set, method = c("center", "scale"))
training_scale <- predict(training_proc, training_set)
validation_proc <- preProcess(validation_set, method = c("center", "scale"))
validation_scale <- predict(validation_proc, validation_set)

#Train neural network to predict biovolume
bio_nn_model <- nnet(
  BIOVOLUME ~ wind_dir + wind_speed + pressure + air_temp + 
    surface_temp, 
  training_scale, 
  size = 3, 
  linout = TRUE, 
  maxit = 100, 
  decay = 0.01 
)

#Train neural network to predict abundance
abund_nn_model <- nnet(
  ABUNDANCE ~ wind_dir + wind_speed + pressure + air_temp + 
    surface_temp, 
  training_scale, 
  size = 3, 
  linout = TRUE, 
  maxit = 100, 
  decay = 0.01 
)

#summarize models
summary(bio_nn_model)
summary(abund_nn_model)

#Test on training data
bio_train_predictions <- predict(bio_nn_model, training_scale)
abundance_train_predictions <- predict(abund_nn_model, training_scale)

#Calculate RMSE on training to compare with validation
bio_error_t <- (mean((training_scale$BIOVOLUME - bio_train_predictions)^2))^0.5
cat("RMSE:", bio_error_t)

#Calculate RMSE on training to compare with validation
abundance_error_t <- (mean((training_scale$BIOVOLUME - abundance_train_predictions)^2))^0.5
cat("RMSE:", abundance_error_t)

#Make predictions
bio_valid_predictions <- predict(bio_nn_model, validation_scale)
abundance_valid_predictions <- predict(abund_nn_model, validation_scale)

#Cite: http://tutorial.math.trinity.edu/content/display-text-r#:~:text=To%20display%20(%20or%20print)%20a,print)%20to%20see%20the%20difference.
#Looked up how to output text using cat

#Calculate RMSE of biovolume predictions
bio_error_v <- (mean((validation_scale$BIOVOLUME - bio_valid_predictions)^2))^0.5
cat("RMSE:", bio_error_v)

#Calculate RMSE of abundance predictions
abundance_error_v <- (mean((validation_scale$ABUNDANCE - abundance_valid_predictions)^2))^0.5
cat("RMSE:", abundance_error_v)

#remove scientific notation
options(scipen=999)

#Graph predictions vs. actual values of biovolume
plot(validation_scale$BIOVOLUME, bio_predictions, main = "Predicted vs Actual Biovolume", 
     xlab = "Actual Biovolume z-score", ylab = "Predicted Biovolume z-score")

#predictions along this line are correct
abline(0, 1, col = "blue")

#Graph predictions vs. actual values of abundance
plot(validation_scale$ABUNDANCE, abundance_predictions, main = "Predicted vs Actual Abundance", 
     xlab = "Actual Abundance z-score", ylab = "Predicted Abundance z-score")

#predictions along this line are correct
abline(0, 1, col = "blue")

