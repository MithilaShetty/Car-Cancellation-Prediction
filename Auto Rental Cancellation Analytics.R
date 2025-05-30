library(tidyr)
library(mice)
library(dplyr)  
library(ggplot2)
library(ggcorrplot)
library(zoo)
library(lubridate)
library(rpart)
library(rpart.plot)
library(gridExtra)
library(caret)
library(DMwR2)
library(pROC)
library(e1071)
library(knitr)
library(Boruta)
library(ROSE)
library(ROCR)
library(rlang)



######################### DATA COLLECTION #########################
# Importing the SAR dataset and understanding dataset by checking dimensions of the dataset and 
# displaying the first 6 rows in a kable 
# inspected the structure of the dataset, checking data types and variable formats.

file_path <- "/Users/mithilapapishetty/Downloads/SAR_data.csv"
sar_data <- utils::read.csv(file_path, stringsAsFactors = FALSE)
dim(sar_data)
knitr::kable(head(sar_data[ ,1:10]), caption = "First 6 Rows of SAR Dataset")
knitr::kable(head(sar_data[ ,11:19]), caption = "First 6 Rows of SAR Dataset")
str(sar_data)

################## DATA EXPLORATION AND PREPROCESSING ###################
# Column names are checked to understand the structure of the dataset, 
# and only 'Car_Cancellation' is renamed to 'car_cancellation' to maintain naming consistency. 
# After that, the dataset is checked for duplicate records to ensure there are no repeated entries.
# Once the data is clean, the data types are converted to appropriate formats:
# identifiers and categorical fields are converted to factors, 
# timestamps are converted to date type using the correct format,
# and geographic coordinates (latitude and longitude) are converted to numeric values 
# to prepare the data for further analysis and modeling.

colnames(sar_data)
colnames(sar_data)[colnames(sar_data) == "Car_Cancellation"] <- "car_cancellation"

######## DUPLICATES CHECK
duplicate_records <- sum(duplicated(sar_data))  
duplicate_records

#######DATA TYPE CONVERSION
sar_data$user_id <- as.factor(sar_data$user_id)
sar_data$vehicle_model_id <- as.factor(sar_data$vehicle_model_id)
sar_data$package_id <- as.factor(sar_data$package_id)
sar_data$travel_type_id <- as.factor(sar_data$travel_type_id)
sar_data$from_area_id <- as.factor(sar_data$from_area_id)
sar_data$to_area_id <- as.factor(sar_data$to_area_id)
sar_data$from_city_id <- as.factor(sar_data$from_city_id)
sar_data$to_city_id <- as.factor(sar_data$to_city_id)
sar_data$online_booking <- as.factor(sar_data$online_booking)
sar_data$mobile_site_booking <- as.factor(sar_data$mobile_site_booking)
sar_data$car_cancellation <- as.factor(sar_data$car_cancellation)


str(sar_data)
sar_data$from_date <- as.Date(sar_data$from_date, format="%m/%d/%Y")
sar_data$to_date <- as.Date(sar_data$to_date, format="%m/%d/%Y")
sar_data$booking_created <- as.Date(sar_data$booking_created, format="%m/%d/%Y")


sar_data$from_lat <- as.numeric(sar_data$from_lat)
sar_data$from_long <- as.numeric(sar_data$from_long)
sar_data$to_lat <- as.numeric(sar_data$to_lat)
sar_data$to_long <- as.numeric(sar_data$to_long)

############## MISISNG AND ZERO VALUES DETECTION AND HANDLING

# Missing and zero values are detected across all variables by calculating their respective counts, 
# and the results are organized neatly into a table for easy visualization.
# Columns with more than 50% missing values are dropped to avoid losing too much information.
# For the remaining important columns, rows with missing values are filtered out to ensure a complete dataset.

######### MISISNG AND ZERO VALUES COUNT
combined_table <- sar_data %>%
  summarise(across(everything(), list(
    Missing = ~sum(is.na(.)),
    Zero = ~sum(. == 0, na.rm = TRUE)
  ))) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Type"),
    names_pattern = "^(.*)_(Missing|Zero)$"
  ) %>%
  pivot_wider(names_from = Type, values_from = value)

knitr::kable(combined_table, caption = "Missing and Zero Value Count per Variable")


############ MISSING VALUE HANDLING
sar_data <- sar_data %>%
  dplyr::filter(!is.na(from_area_id) & 
           !is.na(to_area_id) & 
           !is.na(from_lat) & 
           !is.na(to_lat) & 
           !is.na(from_long) & 
           !is.na(to_long))


str(sar_data)
sar_data <- sar_data %>%
  dplyr::select(-package_id, -from_city_id, -to_city_id, -to_date)

############################## VARIABLE DISTRIBUTION #########################
# Distributions of numeric variables such as 'from_lat', 'from_long', 'to_lat', and 'to_long' are explored 
# using histograms to understand the spread and concentration of location data points.
# Categorical variables like 'vehicle_model_id', 'travel_type_id', 'online_booking', 'mobile_site_booking', 
# and 'car_cancellation' are visualized through bar plots to examine their frequency distribution.
# Additionally, the top 10 most frequent 'from_area_id' and 'to_area_id' are identified and visualized 
# using bar charts with flipped coordinates for better readability and comparison.

############# NUMERIC VARIABLES

p1 <- ggplot(sar_data, aes(x = from_lat)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of from_lat") +
  theme_minimal()

p2 <- ggplot(sar_data, aes(x = from_long)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of from_long") +
  theme_minimal()

p3 <- ggplot(sar_data, aes(x = to_lat)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of to_lat") +
  theme_minimal()

p4 <- ggplot(sar_data, aes(x = to_long)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of to_long") +
  theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol = 2)

############### CATEGORICAL VARIABLES 

p1 <- ggplot(sar_data, aes(x = vehicle_model_id)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Vehicle Model ID") +
  theme_minimal()

p2 <- ggplot(sar_data, aes(x = travel_type_id)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Travel Type ID") +
  theme_minimal()

p3 <- ggplot(sar_data, aes(x = online_booking)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Online Booking") +
  theme_minimal()

p4 <- ggplot(sar_data, aes(x = mobile_site_booking)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Mobile Site Booking") +
  theme_minimal()

p5 <- ggplot(sar_data, aes(x = car_cancellation)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Car Cancellation") +
  theme_minimal()

grid.arrange(p3, p4, p5, p2, ncol = 2)
grid.arrange(p1, ncol = 1)

########### area ids
top_from_area <- sar_data %>%
  count(from_area_id, sort = TRUE) %>%
  top_n(10, n)

p1 <- ggplot(top_from_area, aes(x = reorder(from_area_id, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  coord_flip() +
  labs(title = "Top 10 From Area IDs", x = "From Area ID", y = "Count") +
  theme_minimal()
top_to_area <- sar_data %>%
  count(to_area_id, sort = TRUE) %>%
  top_n(10, n)

p2 <- ggplot(top_to_area, aes(x = reorder(to_area_id, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  coord_flip() +
  labs(title = "Top 10 To Area IDs", x = "To Area ID", y = "Count") +
  theme_minimal()
grid.arrange(p1, p2, ncol = 2)


################### FEATURE ENGINEERING AND TRANSFORMATION ####################
# Calculated trip distance using Haversine formula (GPS coordinates).
# Created 'phone_booking' flag where bookings were neither online nor mobile.
# Extracted weekday and month from trip start date and booking date.
# These new features help capture user behavior, trip patterns, and booking channels.

GPSDist <- function(lat1, lon1, lat2, lon2) {
  # Radius of the Earth in kilometers
  R <- 6371.0
  # Convert degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Differences in coordinates
  delta_lat <- lat2 - lat1
  delta_lon <- lon2 - lon1
  
  # Haversine formula
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Distance
  distance <- R * c
  return(distance)
}
trip_length <- GPSDist(sar_data$from_lat, sar_data$from_long, sar_data$to_lat, sar_data$to_long)
sar_data$trip_length <- mapply(GPSDist, 
                               lat1 = sar_data$from_lat, 
                               lon1 = sar_data$from_long, 
                               lat2 = sar_data$to_lat, 
                               lon2 = sar_data$to_long)

sar_data$phone_booking <- ifelse(sar_data$online_booking == 0 & sar_data$mobile_site_booking == 0, 1, 0)
sar_data$from_weekday <- weekdays(sar_data$from_date)
sar_data$from_month <- format(sar_data$from_date, "%b")   

sar_data$booking_weekday <- weekdays(sar_data$booking_created)
sar_data$booking_month <- format(sar_data$booking_created, "%b")
sar_data$phone_booking <- as.factor(sar_data$phone_booking)
sar_data$from_weekday <- as.factor(sar_data$from_weekday)
sar_data$from_month <- as.factor(sar_data$from_month)
sar_data$booking_weekday <- as.factor(sar_data$booking_weekday)
sar_data$booking_month <- as.factor(sar_data$booking_month)

################### BINNING NUMERIC VARIABLE
sar_data$trip_length_group <- cut(sar_data$trip_length, 
                                  breaks = c(-Inf, 10, 25, Inf), 
                                  labels = c("Short", "Medium", "Long"))
str(sar_data)


################ DISTRIBUITON OF NEW VARIABLES


# Trip Length distribution (numeric)
p1 <- ggplot(sar_data, aes(x = trip_length)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Trip Length", x = "Trip Length (km)", y = "Frequency") +
  theme_minimal()

# Phone Booking (binary 0/1)
p2 <- ggplot(sar_data, aes(x = as.factor(phone_booking))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Phone Booking Distribution", x = "Phone Booking (0=No, 1=Yes)", y = "Count") +
  theme_minimal()

# From Weekday
p3 <- ggplot(sar_data, aes(x = from_weekday)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Trips by Weekday (Trip Start)", x = "Weekday", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# From Month
p4 <- ggplot(sar_data, aes(x = from_month)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Trips by Month (Trip Start)", x = "Month", y = "Count") +
  theme_minimal()

# Booking Weekday
p5 <- ggplot(sar_data, aes(x = booking_weekday)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Bookings by Weekday", x = "Booking Weekday", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Booking Month
p6 <- ggplot(sar_data, aes(x = booking_month)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Bookings by Month", x = "Booking Month", y = "Count") +
  theme_minimal()
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

################checking for structure and missing values count  after transformation
str(sar_data)
colSums(is.na(sar_data))

######################### PREDICTOR RELEVANCY #########################################
# The relationship between the numeric predictor 'trip_length' and the target variable 'car_cancellation' 
# is explored using a boxplot, followed by a t-test to check for any statistically significant differences.
# For categorical predictors, Chi-square tests or Fisher's exact tests are performed depending on the data distribution,
# systematically looping through all major categorical variables to evaluate their association with car cancellation.
# Variables with insufficient variation are skipped automatically to ensure valid statistical testing.

########### NUMERIC PREDICTORS VS CAR CANCELLATION
# Boxplot of trip_length by car_cancellation

ggplot(sar_data, aes(x = car_cancellation, y = trip_length)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Trip Length vs Car Cancellation", x = "Car Cancellation", y = "Trip Length (km)") +
  theme_minimal()
t.test(trip_length ~ car_cancellation, data = sar_data)

########### CATEGORICAL PREDICTORS VS CAR CANCELLATION

important_vars <- c("online_booking", "mobile_site_booking", "phone_booking",
                    "from_weekday", "from_month", "booking_weekday", "booking_month")

plot_list_simple <- list()

for (var in important_vars) {
  p <- ggplot(sar_data, aes(x = .data[[var]], fill = car_cancellation)) +
    geom_bar(position = position_dodge(width = 0.8)) +
    geom_text(stat = "count",
              aes(label = ..count..),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(title = paste("Car Cancellation vs", var),
         x = var, y = "Count") +
    scale_fill_manual(values = c("steelblue", "navy"),
                      name   = "Car Cancellation",
                      labels = c("Not Cancelled", "Cancelled")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_list_simple[[var]] <- p
}


grid.arrange(grobs = plot_list_simple, ncol = 2)


################################# DIMENSION REDUCTION ###############################
# Dropping location-related columns and row identifier after creating necessary features
#to reduce redundancy and simplify the dataset

sar_data <- sar_data[, !(names(sar_data) %in% c("user_id", "travel_type_id", "from_area_id", "to_area_id", "from_lat", "from_long", "to_lat", "to_long", "row.", "trip_length", "booking_created", "from_date"))]
str(sar_data)

########################### FEATURE SELECTION  #################################
# Feature selection is performed using the Boruta algorithm to identify the most relevant predictors for car cancellation.
# Tentative features are resolved using TentativeRoughFix to finalize the important variables.
# The importance history and final attribute statistics are reviewed to understand feature relevancy.
# A new dataset is created by selecting only the confirmed important variables along with the target variable 
# for building predictive models, and its structure is checked for confirmation.

set.seed(123)  
boruta_result <- Boruta(car_cancellation ~ ., data = sar_data, doTrace = 2, maxRuns = 20)
boruta_result
Boruta::getSelectedAttributes(boruta_result)

final_boruta <- TentativeRoughFix(boruta_result)
final_boruta

final_boruta$ImpHistory
Boruta::attStats(final_boruta)


selected_vars <- c("vehicle_model_id", 
                   "trip_length_group", "phone_booking", "online_booking",
                   "from_weekday", "from_month", "booking_weekday", "booking_month",
                   "car_cancellation") 
model_data <- sar_data[, selected_vars]
str(model_data)


#################### DATA PARTITIONING ############################
# The dataset is partitioned into training, validation, and test sets.
# Initially, 70% of the data is separated for training, while the remaining 30% is split equally 
# into validation and test sets to support model tuning and final evaluation.

set.seed(42)
trainIndex <- caret::createDataPartition(sar_data$car_cancellation, p = 0.7, list = FALSE)
train_data <- sar_data[trainIndex, ]
temp_data <- sar_data[-trainIndex, ]

valIndex <- caret::createDataPartition(temp_data$car_cancellation, p = 0.5, list = FALSE)
validation_data <- temp_data[valIndex, ]
test_data <- temp_data[-valIndex, ]


cat("Training Set Records:", nrow(train_data), "\n")
cat("Validation Set Records:", nrow(validation_data), "\n")
cat("Test Set Records:", nrow(test_data), "\n")

########################### CLASS BALANCING WITH OVER SAMPLING 
# The training data is balanced using the ROSE package by applying both oversampling and undersampling, 
# ensuring an even distribution of the car cancellation classes to improve model training.
# A table is generated to verify the class distribution after balancing.

prop.table(table(sar_data$car_cancellation))
train_data_balanced <- ROSE::ovun.sample(car_cancellation ~ ., 
                                   data = train_data, 
                                   method = "both", 
                                   N = 2 * nrow(train_data))$data

prop.table(table(train_data_balanced$car_cancellation))

##################### MODEL FITTING #################################
# A Naive Bayes model is fitted on the balanced training data and evaluated on both validation and test sets using confusion matrices.
# The model is then re-fitted with Laplace smoothing to handle zero probabilities, 
# and performance is again assessed on the validation and test datasets to check for any improvement.

nb_model <- e1071::naiveBayes(car_cancellation ~ ., 
                       data = train_data_balanced)
nb_model

val_pred_class <- stats::predict(nb_model, newdata = validation_data, type = "class")
caret::confusionMatrix(val_pred_class, validation_data$car_cancellation, positive ='1')

test_pred_class <- stats::predict(nb_model, newdata = test_data, type = "class")
caret::confusionMatrix(test_pred_class, test_data$car_cancellation, positive = '1')


######## NAIVE BAYES  WITH LAPALCE SMOOHTING 

nb_model_tune <- e1071::naiveBayes(car_cancellation ~ ., 
                       data = train_data_balanced,
                       laplace = 1)   

val_pred_class_tune <- stats::predict(nb_model_tune, newdata = validation_data, type = "class")
caret::confusionMatrix(val_pred_class_tune, validation_data$car_cancellation, positive ='1')

test_pred_class_tune <- stats::predict(nb_model_tune, newdata = test_data, type = "class")
caret::confusionMatrix(test_pred_class_tune, test_data$car_cancellation, positive = '1')




##################### MODEL EVALUATION SUMMARY #####################
# Collated confusion matrices for validation and test sets before and after Laplace smoothing
# Extracted accuracy, sensitivity, and specificity from each confusion matrix
# Compiled these metrics into separate data frames for each scenario
# Prepared a consolidated model evaluation summary across all datasets

val_conf_before <- confusionMatrix(val_pred_class, validation_data$car_cancellation, positive = "1")
val_metrics_before <- data.frame(
  Dataset = "Validation (Before Laplace)",
  Accuracy = val_conf_before$overall["Accuracy"],
  Sensitivity = val_conf_before$byClass["Sensitivity"],
  Specificity = val_conf_before$byClass["Specificity"]
)


val_conf_after <- confusionMatrix(val_pred_class_tune, validation_data$car_cancellation, positive = "1")
val_metrics_after <- data.frame(
  Dataset = "Validation (After Laplace)",
  Accuracy = val_conf_after$overall["Accuracy"],
  Sensitivity = val_conf_after$byClass["Sensitivity"],
  Specificity = val_conf_after$byClass["Specificity"]
)

test_conf_before <- caret::confusionMatrix(test_pred_class, test_data$car_cancellation, positive = "1")
test_metrics_before <- data.frame(
  Dataset = "Test (Before Laplace)",
  Accuracy = test_conf_before$overall["Accuracy"],
  Sensitivity = test_conf_before$byClass["Sensitivity"],
  Specificity = test_conf_before$byClass["Specificity"]
)


test_conf_after <- caret::confusionMatrix(test_pred_class_tune, test_data$car_cancellation, positive = "1")
test_metrics_after <- data.frame(
  Dataset = "Test (After Laplace)",
  Accuracy = test_conf_after$overall["Accuracy"],
  Sensitivity = test_conf_after$byClass["Sensitivity"],
  Specificity = test_conf_after$byClass["Specificity"]
)
####### AUC CALCULATION 
# Generated predicted probabilities from the original and tuned Naive Bayes models
# Computed AUC values for both validation and test datasets before and after Laplace smoothing
# Appended the AUC metrics to the existing performance tables
# Combined all results into one summary table and rendered it with kable

val_probs_before <- stats::predict(nb_model, newdata = validation_data, type = "raw")[,2]
val_probs_after  <- stats::predict(nb_model_tune, newdata = validation_data, type = "raw")[,2]
test_probs_before <- stats::predict(nb_model, newdata = test_data, type = "raw")[,2]
test_probs_after  <- stats::predict(nb_model_tune, newdata = test_data, type = "raw")[,2]


auc_val_before <- pROC::auc(validation_data$car_cancellation, val_probs_before)
auc_val_after <- pROC::auc(validation_data$car_cancellation, val_probs_after)
auc_test_before <- pROC::auc(test_data$car_cancellation, test_probs_before)
auc_test_after <- pROC::auc(test_data$car_cancellation, test_probs_after)

val_metrics_before$AUC <- auc_val_before
val_metrics_after$AUC <- auc_val_after
test_metrics_before$AUC <- auc_test_before
test_metrics_after$AUC <- auc_test_after

final_results <- base::rbind(val_metrics_before, val_metrics_after,
                             test_metrics_before, test_metrics_after)

kable(final_results, digits = 3, caption = "Performance Metrics for Naive Bayes Models (Before and After Laplace Smoothing)")


######################## ROC CURVES PLOTTING
# Computed ROC curves for validation and test datasets before and after Laplace smoothing.
# Plotted the validation ROC curves to compare model discrimination pre- and post-smoothing.
# Plotted the test ROC curves to assess performance on unseen data post-smoothing.

roc_val_before <- roc(validation_data$car_cancellation, val_probs_before)
roc_val_after <- roc(validation_data$car_cancellation, val_probs_after)

roc_test_before <- roc(test_data$car_cancellation, test_probs_before)
roc_test_after <- roc(test_data$car_cancellation, test_probs_after)


plot(roc_val_before, col = "blue", lwd = 2,
     main = "Validation ROC Curve (Before and After Laplace)")
lines(roc_val_after, col = "red", lwd = 2)
legend("bottomright", legend = c("Before Laplace", "After Laplace"),
       col = c("blue", "red"), lwd = 2)


 plot(roc_test_before, col = "blue", lwd = 2,
     main = "Test ROC Curve (Before and After Laplace)")
lines(roc_test_after, col = "red", lwd = 2)
legend("bottomright", legend = c("Before Laplace", "After Laplace"),
       col = c("blue", "red"), lwd = 2)

