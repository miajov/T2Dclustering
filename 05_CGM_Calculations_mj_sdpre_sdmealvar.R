# CGM Data #####################################
# Calculation of all the 24h CGM measures and overall postprandial values 
# And nutrition Data
############################################

# Libraries-----------------------------------------
library(lubridate)
library(dplyr)
library(iglu)
library(slider)
library(tidyr)


# Read Data-----------------------------------------
data <- read.csv("Downloads/Data_total_04.csv")

# Put Data into usable format -----------------------------------------
## Transform time variable in correct format
data$Date <- ymd_hms(data$Date)

##Add time variables 
data$Date <- as.POSIXct(data$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Extract day, time, and month
data$day <- format(data$Date, "%d")
data$time <- format(data$Date, "%H:%M:%S")
data$month <- format(data$Date, "%m")

## Add new variable
#### Indicate Min Date of each ID -----------------------
data <- data %>%
  group_by(ID) %>%
  dplyr::mutate(min_Date = min(Date)) %>%
  ungroup() 

## Clean and process------------------------------------------------------------
data <- data[!with(data,is.na(Date)),] #Remove those values without indication of time 

## CGM Features---------------
### First 24 h-------
#Filter the first 24h
data_24 <- data %>%
  group_by(ID)  %>%
  filter(Date <= (min_Date + (24*60*60))) %>%
  ungroup()
# 
# ## Most important features based on https://drc.bmj.com/content/9/1/e001869.abstract#supplementary-materials
# # - MSG-mean sensor glucose; 
# # - MPSG-mean postprandial sensor glucose; 
# # - TIR-percentages of values within the target range ([3.9-10] mmol/L); 
# # - TOR-percentages of values out of the target range (<3.9 or >10 mmol/L); 
# # - high blood glucose indices
# # - J-Index
# # - M value
# 
#### MSG --------------
MSG <- data_24 %>% dplyr::group_by(ID) %>%
  dplyr::summarize(MSG_24h = mean(CGM..mg...dl.)) %>% ungroup()

#### TIR --------------
### Convert mmol/L in mg/dl range
# 3.9*18.018 = 70.2702
# 10*18.018 = 180.18
data_24 <- data_24 %>% group_by(ID) %>% mutate(TIR_bin =
                                                 ifelse(CGM..mg...dl. >= 70.2702
                                                        & CGM..mg...dl. <= 180.18, 1, 0))
TIR <- data_24 %>%
  group_by(ID) %>%
  dplyr::summarize(Percentage_TIR_24h = mean(TIR_bin) * 100) %>%
  ungroup()

df_24 <- merge(MSG, TIR, by="ID")

#### TOR --------------
### Convert mmol/L in mg/dl range
# 3.9*18.018 = 70.2702
# 10*18.018 = 180.18
SD <- data_24 %>%
  group_by(ID) %>%
  dplyr::summarize(SD_CGM_24h = sd(CGM..mg...dl., na.rm = TRUE)) %>%
  ungroup()

df_24 <- merge(df_24, SD, by="ID")

#### HBGI -------------
HBGI_df <- data_24 %>% select(ID, Date, CGM..mg...dl.)
names(HBGI_df) <- c("id", "time", "gl")
HBGI <- hbgi(HBGI_df)
names(HBGI) <- c("ID", "HBGI_24h")


df_24 <- merge(df_24, HBGI, by = "ID")

#### J-Index ---------------
J_Index <- j_index(HBGI_df)
names(J_Index) <- c("ID", "J_index_24h")
df_24 <- merge(df_24, J_Index, by = "ID")

#### M-value --------------
# The ideal blood glucose level in the M value function was set as 5.8 mmol/L or 104 mg/dl
M_value <- m_value(HBGI_df, r=104)
names(M_value) <- c("ID", "M_value_24h")
df_24 <- merge(df_24, M_value, by = "ID")

### Baseline Meal - Premeal interval -------------------------------
data <- data %>% mutate(Meal_time = case_when(Meal==1 ~ Date)) #postprandial start/meal consumption
data$Meal_time <- ymd_hms(data$Meal_time) 

##new SD
#data$SD_Meal_time_24 = sd(data$Meal_time)

## 1h Pre-Meal Interval Start
data$Pre_Meal_start<- format(with(data, Meal_time - 60*60))
data$Pre_Meal_start <- ymd_hms(data$Pre_Meal_start)#check again right format

## 1h Premeal interval 
data$Pre_meal_inter <- interval(data$Pre_Meal_start, data$Meal_time)

#fill up time interval till next meal 
data <- data %>% 
  dplyr::group_by(ID) %>% 
  arrange(Date) %>% 
  do(fill(., Pre_meal_inter, .direction = 'up')) %>% 
  dplyr::ungroup()

data$premeal_glu <-  ifelse(data$Date %within% data$Pre_meal_inter ==TRUE, 
                                          data$CGM..mg...dl., "No_PreMeal") 

 data <- data %>%
   dplyr::group_by(ID, Pre_meal_inter) %>%
   #filter(premeal_glu != "No_PreMeal") %>% 
   dplyr::mutate(premeal_1h_mean_glu = mean(CGM..mg...dl., na.rm = TRUE)) %>%   ungroup() 
 
 ##########
 
 #### Mean postprandial and other values----------------------
 premeal_24 <-  data %>%
   dplyr::group_by(ID) %>%
   filter(Date <= (min_Date + (24*60*60))) %>%
   ungroup()
 
 #######SD 
 premeal_24 <-  premeal_24  %>%
   dplyr::group_by(ID) %>%
   dplyr::mutate(premeal_1h_mean_glu_mean_24 =  mean(premeal_1h_mean_glu, na.rm = TRUE),
                 premeal_1h_mean_glu_sd_24 =  sd(premeal_1h_mean_glu, na.rm = TRUE)) %>%   ungroup() 
 
 premeal_24 = premeal_24 %>%  select(premeal_1h_mean_glu_mean_24, 
                                     premeal_1h_mean_glu_sd_24,
                                     ID)
 
 write.csv(premeal_24, "premeal_24.csv")
 

#Remove helping columns
data$Meal_time <- NULL
data$Pre_Meal_start <- NULL
data$Pre_meal_inter <- NULL


### Postprandial time interval-------------------------------------------------
## Prepare Postprandial calculation 
### Choose all meals even those where data not available
data <- data %>% mutate(postpranidal = case_when(Meal==1 ~ Date)) #postprandial start/meal consumption
data$postpranidal <- ymd_hms(data$postpranidal) #check that it's in the right format

## 2h postprandial interval end
data$postpranidal_end <- format(with(data, postpranidal + 120*60))
data$postpranidal_end <- ymd_hms(data$postpranidal_end)#check again right format

## 2h postprandial interval 
data$time_inter <- interval(data$postpranidal, data$postpranidal_end)

#fill up time interval till next meal 
data <- data %>% 
  dplyr::group_by(ID) %>% 
  arrange(Date) %>% 
  do(fill(., time_inter, .direction = 'down')) %>% 
  dplyr::ungroup()


## Check if time stamp of the glucose measurement is within the postprandial time period
data$postprandial_glu <-  ifelse(data$Date %within% data$time_inter ==TRUE, 
                                       data$CGM..mg...dl., "No_postpran") 

#Remove those observations which are not postprandial
data_postprandial <- data %>% filter(postprandial_glu != "No_postpran")

### Overall Postprandial Calculation-----------------------
#### Calculate Postprandial Mean -----------------------
data_postprandial <- data_postprandial %>% group_by(ID, time_inter) %>% 
  dplyr::mutate(mean_postprandial = mean(CGM..mg...dl.),
         sd_postprandial = sd(CGM..mg...dl.)) %>% ungroup()

## Shrink Dataset to Postprandial Dataset ---------------
### As we only need one row per ID per meal 
data_post_meal <- data_postprandial %>% filter(Meal == 1)

#### Calculate rolling 24h postprandial mean----------------
# Ensure data is sorted by ID and time_inter
data_post_meal <- data_post_meal %>%
  arrange(ID, Date)

#Rolling Postprandial Mean 
## 24h
data_post_meal <- data_post_meal %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(
    mean_postprandial_previous_24h = slider::slide_index_dbl(
      .x = mean_postprandial,
      .i = Date,
      .f = ~ mean(head(.x, -1), na.rm = TRUE),
      .before = ~.x - hours(24),
      .complete = TRUE
    )
  ) %>%
  ungroup()

data_post_meal <- data_post_meal %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(
    mean_postprandial_all_previous = slider::slide_index_dbl(
      .x = mean_postprandial,
      .i = row_number(),
      .f = ~ mean(.x[-length(.x)]),  # Exclude the current value
      .before = Inf,
      .complete = TRUE
    )
  ) %>%
  ungroup()

### First 24h -------------------
#### Mean postprandial and other values----------------------
postpran_24 <- data_post_meal %>%
  dplyr::group_by(ID) %>%
  filter(Date <= (min_Date + (24*60*60))) %>%
  ungroup()

postpran_24= postpran_24 %>%
  dplyr::group_by(ID) %>% 
  dplyr::mutate(mean_postprandial_all_first_24 = mean(mean_postprandial),
                sd_postprandial_all_first_24 = sd(mean_postprandial))

postpran_24 = postpran_24 %>%  select(mean_postprandial_all_first_24,
                                      sd_postprandial_all_first_24, ID)
write.csv(postpran_24, "postpran_24.csv")

##merge

cgm_clust = merge(postpran_24,premeal_24, by="ID")
write.csv(cgm_clust,"cgm_clust.csv")

## Meal count -------------------------------
# Filtering meals
meal_count <- postpran_24 %>%
  filter(Meal == 1)



# Count the number of meals per ID
meal_count <- meal_count %>%
  group_by(ID) %>%
  summarize(meals_consumed = sum(!is.na(Meal)), .groups = "drop")

# Combine the meal count with the 24-hour summary
df_24 <- df_24 %>%
  left_join(meal_count, by = "ID")

# Define columns to compute mean & sum separately
columns_to_mean <- c("mean_postprandial")  # Only the postprandial column should be averaged
columns_to_sum <- postpran_24 %>%
  dplyr::select(Staples_g:Other_Foods_bin, Mixed_g:Meals_Foodgroups) %>%
  colnames()

# Compute the 24-hour aggregated values
postpran_24_summary <- postpran_24 %>%
  group_by(ID) %>%
  summarize(across(all_of(columns_to_mean), ~ mean(.x, na.rm = TRUE), .names = "{.col}_frist_24h"),
         across(all_of(columns_to_sum), ~ sum(.x, na.rm = TRUE), .names = "{.col}_first_24h")) %>%
  ungroup()

# # Filter only meals
# postpran_24 <- postpran_24 %>%
#   filter(Meal == 1)
# 
# # Summarize within 24 hours
# postpran_24_summary <- postpran_24 %>%
#   group_by(ID) %>%
#   summarize(across(starts_with("mean_24h_"), ~ mean(.x, na.rm = TRUE), .names = "{.col}_24h"),
#             across(starts_with("sum_24h_"), ~ sum(.x, na.rm = TRUE), .names = "{.col}_24h")) %>%
#   ungroup()

# Merge with the main dataset
df_24 <- merge(df_24, postpran_24_summary, by = "ID")

###
## Exclude Values
## HERE PROBABLY NOT NECESSARY TO EXCLUDE MEALS THAT ARE CLOSE TO EACH OTHER 
## Time difference between meals and or snacks
# Meals_time_dif <- data_postprandial %>% filter(Meal==1) %>%
#   arrange(ID, Date) %>%
#   group_by(ID) %>%
#   mutate(diff = Date - lag(Date),
#          diff_mins = as.numeric(diff, units = 'mins'))
# 
# ## Time between meals and or snacks needs to be more than 60 minutes appart
# Meals_time_dif <- Meals_time_dif %>% filter(diff_mins > 60)
# Meals_time_dif <- Meals_time_dif %>% dplyr::select(ID, Date, diff_mins)
# 
# data_postprandial <- merge(data_postprandial, Meals_time_dif, by=c("ID", "Date"), all.x = T, all.y = F)
# data_postprandial <- data_postprandial %>% group_by(ID, time_inter) %>% filter(any(!is.na(diff_mins))) %>%
#   ungroup() #remove those which do not have a meal

### Overall Postprandial after 24h-----------------------
#### Calculate Postprandial Mean -----------------------
# data_postprandial <- data_postprandial %>% group_by(ID, time_inter) %>% 
#   mutate(mean_postprandial = mean(CGM..mg...dl.)) %>% ungroup()
# 
# ## Shrink Dataset to Postprandial Dataset ---------------
# ### As we only need one row per ID 
# data_post_meal <- data_postprandial %>% filter(Meal == 1)
# 
# #### Calculate rolling 24h postprandial mean----------------
# # Ensure data is sorted by ID and time_inter
# data_post_meal <- data_post_meal %>%
#   arrange(ID, Date)
# 
# # Calculate mean postprandial values for the previous 24 hours for each observation
# data_post_meal <- data_post_meal %>%
#   dplyr::group_by(ID) %>%
#   dplyr::mutate(mean_postprandial_previous_24h = slider::slide_index_dbl(
#     .x = mean_postprandial,
#     .i = Date,
#     .f = mean,
#     .before = ~.x - hours(24))) %>%
#   ungroup()
# 
# #CHECK
# data_post_meal <- data_post_meal %>%
#   dplyr::group_by(ID) %>%
#   dplyr::mutate(
#     mean_postprandial_previous_24h = slider::slide_index_dbl(
#       .x = mean_postprandial,
#       .i = Date,
#       .f = ~ if(length(.x) > 1) mean(head(.x, -1), na.rm = TRUE) else NA,
#       .before = ~.x - hours(24),
#       .complete = TRUE)) %>% 
#   ungroup()
# 
# 



## iAUC Calculation ------------------------------------
## Not needed
# iauc_fn <- function(x, y) {
#   # Initialize auc and seg.type vectors
#   auc <- numeric(length(x) - 1)
#   seg.type <- integer(length(x) - 1)
#   
#   # Check if there are sufficient points to calculate AUC
#   if (length(x) < 2) {
#     return(list(auc = NA, segments = auc, seg.type = seg.type)[[1]])
#   }
#   
#   # Calculate the initial segment
#   auc[1] <- ifelse(!is.na(y[2]) & !is.na(y[1]) & y[2] > y[1], (y[2] - y[1]) * (x[2] - x[1]) / 2, 0)
#   seg.type[1] <- ifelse(!is.na(y[2]) & !is.na(y[1]) & y[2] > y[1], 1, 0)
#   
#   # Loop through remaining points
#   for (i in 3:length(x)) {
#     if (is.na(y[i]) | is.na(y[i-1])) {
#       auc[i-1] <- NA
#       seg.type[i-1] <- NA
#     } else if (y[i] >= y[1] & y[i-1] >= y[1]) {
#       auc[i-1] <- (((y[i] - y[1]) / 2) + (y[i-1] - y[1]) / 2) * (x[i] - x[i-1]) / 2
#       seg.type[i-1] <- 1
#     } else if (y[i] >= y[1] & y[i-1] < y[1]) {
#       auc[i-1] <- ((y[i] - y[1])^2 / (y[i] - y[i-1])) * (x[i] - x[i-1]) / 2
#       seg.type[i-1] <- 2
#     } else if (y[i] < y[1] & y[i-1] >= y[1]) {
#       auc[i-1] <- ((y[i-1] - y[1])^2 / (y[i-1] - y[i])) * (x[i] - x[i-1]) / 2
#       seg.type[i-1] <- 3
#     } else if (y[i] < y[1] & y[i-1] < y[1]) {
#       auc[i-1] <- 0
#       seg.type[i-1] <- 4
#     } else {
#       stop(paste("Error at i:", i))
#     }
#   }
#   
#   return(list(auc = sum(auc, na.rm = TRUE), segments = auc, seg.type = seg.type)[[1]])
# }
# data_postprandial$postprandial_glu <- as.numeric(data_postprandial$CGM..mg...dl.)
# 
# 
# AUC_match <- data_postprandial %>% #implement function
#   group_by(ID, time_inter) %>%
#   do(data.frame(iAUC=iauc_fn(.$Date, .$postprandial_glu)))
# 
# data_post_meal <- merge(data_post_meal, AUC_match, by=c("ID", "time_inter"), #merge iAUC values and data frame
#                     all.x = T, all.y = F)



## Rolling Meal Intake --------------------------------------------------
Rolling_mean <- function(data, new_colname, colname, time_hours){
  # Convert string inputs to symbols
  col_sym = rlang::sym(colname)
  new_col_sym = rlang::sym(new_colname)
  
  data %>%
    group_by(ID) %>%
    mutate(!!new_col_sym := slide_index_dbl(
      .x = !!col_sym, 
      .i = Date, 
      .f = ~ if(length(.x) > 1) sum(.x[-length(.x)], na.rm = TRUE) else NA, 
      .before = ~.x - hours(time_hours), 
      .complete = TRUE
    )) %>%
    ungroup()
}

### Staples 
data_post_meal <- Rolling_mean(data_post_meal,"Staples_g_8h", "Staples_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Staples_g_24h", "Staples_g", 24)

### Vegetables
data_post_meal <- Rolling_mean(data_post_meal,"Vegetables_g_8h", "Vegetables_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Vegetables_g_24h", "Vegetables_g", 24)

### Fruits
data_post_meal <- Rolling_mean(data_post_meal,"Fruits_g_8h", "Fruits_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Fruits_g_24h", "Fruits_g", 24)

### Animal Protein
data_post_meal <- Rolling_mean(data_post_meal,"Animal_foods_g_8h", "Animal_foods_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Animal_foods_g_24h", "Animal_foods_g", 24)

### Dairy
data_post_meal <- Rolling_mean(data_post_meal,"Dairy_Products_g_8h", "Dairy_Products_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Dairy_Products_g_24h", "Dairy_Products_g", 24)

### Legumes & Nuts
data_post_meal <- Rolling_mean(data_post_meal,"Legumes_Nuts_g_8h", "Legumes_Nuts_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Legumes_Nuts_g_24h", "Legumes_Nuts_g", 24)

### Sweets
data_post_meal <- Rolling_mean(data_post_meal,"Sweets_g_8h", "Sweets_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Sweets_g_24h", "Sweets_g", 24)

### Other_Foods_g
data_post_meal <- Rolling_mean(data_post_meal,"Other_Foods_g_8h", "Other_Foods_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Other_Foods_g_24h", "Other_Foods_g", 24)

### Mixed_g
data_post_meal <- Rolling_mean(data_post_meal,"Mixed_g_8h", "Mixed_g", 8)

data_post_meal <- Rolling_mean(data_post_meal,"Mixed_g_24h", "Mixed_g", 24)

### Total_g 
data_post_meal <- Rolling_mean(data_post_meal,"Total_g_8h", "total_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Total_g_24h", "total_g", 24)

### Rolling presence 
### For binary variables indicate if they consumed the FG in the previous 8h or 24h
Rolling_presence <- function(data, new_colname, colname, time_hours) {
  # Convert string inputs to symbols
  col_sym <- rlang::sym(colname)
  new_col_sym <- rlang::sym(new_colname)
  
  data %>%
    group_by(ID) %>%
    mutate(!!new_col_sym := slide_index_dbl(
      .x = !!col_sym, 
      .i = Date, 
      .f = ~ if(any(head(.x, -1) == 1)) 1 else 0, 
      .before = ~.x - hours(time_hours),
      .complete = TRUE
    )) %>%
    ungroup()
}

### Staples 
data_post_meal <- Rolling_presence(data_post_meal,"Staples_bin_8h", "Staples_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Staples_bin_24h", "Staples_bin", 24)

### Vegetables
data_post_meal <- Rolling_presence(data_post_meal,"Vegetables_bin_8h", "Vegetables_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Vegetables_bin_24h", "Vegetables_bin", 24)

### Fruits
data_post_meal <- Rolling_presence(data_post_meal,"Fruits_bin_8h", "Fruits_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Fruits_bin_24h", "Fruits_bin", 24)

### Animal Protein
data_post_meal <- Rolling_presence(data_post_meal,"Animal_foods_bin_8h", "Animal_foods_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Animal_foods_bin_24h", "Animal_foods_bin", 24)

### Dairy
data_post_meal <- Rolling_presence(data_post_meal,"Dairy_Products_bin_8h", "Dairy_Products_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Dairy_Products_bin_24h", "Dairy_Products_bin", 24)

### Legumes & Nuts
data_post_meal <- Rolling_presence(data_post_meal,"Legumes_Nuts_bin_8h", "Legumes_Nuts_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Legumes_Nuts_bin_24h", "Legumes_Nuts_bin", 24)

### Sweets
data_post_meal <- Rolling_presence(data_post_meal,"Sweets_bin_8h", "Sweets_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Sweets_bin_24h", "Sweets_bin", 24)

### Meal yes or no 
data_post_meal <- Rolling_presence(data_post_meal,"Meal_bin_8h", "Meal", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Meal_bin_24h", "Meal", 24)

## Add further variables---------------
data_post_meal$Date <- as.POSIXct(data_post_meal$Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

## Add further variables----------------------
# Extract hours and categorize
data_post_meal <- data_post_meal %>% 
  dplyr::mutate(
    Hour = as.integer(format(Date, "%H")),
    Meal_time = case_when(
      Hour >= 5 & Hour < 10 ~ 1, #Morning
      Hour >= 10 & Hour < 14 ~ 2, #Lunch
      Hour >= 14 & Hour < 18 ~ 3,#Afternoon
      Hour >= 18 & Hour < 21 ~ 4,#Dinner
      TRUE ~ 5  # Else, Midnight
    )
  )

#### Remove First 24h-------------------
data_post <- data_post_meal %>%
  group_by(ID) %>%
  filter(Date > min_Date + (24*60*60)) %>%
  ungroup()


#### Average Mean Postprandial Overall -------------------
data_post_meal <- data_post %>% group_by(ID) %>% 
  mutate(mean_postprandial_ID = mean(mean_postprandial)) %>% ungroup()

#### Binary Postprandial Indication -------------------
##### Constant over overall postprandial mean per ID -----------------
data_post_meal <- data_post_meal %>% group_by(ID) %>% 
  mutate(postprandial_constant = 
           ifelse(mean_postprandial>mean_postprandial_ID, 1, 0)) %>% ungroup()

##### Rolling 24h mean postprandial per ID -----------------
data_post_meal <- data_post_meal %>% group_by(ID) %>% 
  mutate(postprandial_rolling = 
           ifelse(mean_postprandial>mean_postprandial_previous_24h, 1, 0)) %>% ungroup()

data_post_meal <- data_post_meal %>% group_by(ID) %>% 
  mutate(postprandial_rolling_all = 
           ifelse(mean_postprandial>mean_postprandial_all_previous, 1, 0)) %>% ungroup()


### Merge df_24 and postprandial data---------------
postprandial_df <- left_join(data_post_meal, df_24, by = "ID")


### Remove all additional columns which aren't needed anymore
postprandial_df <- postprandial_df %>% 
  dplyr::select(-c(postpranidal, postpranidal_end, time_inter, postprandial_glu, min_Date, CGM..mg...dl.))

### Calculate time difference
colnames(postprandial_df)
postprandial_df$Date <- as.POSIXct(postprandial_df$Date, format = "%Y-%m-%d %H:%M:%S")

Meal_diff <- postprandial_df %>%
  arrange(ID, Meal_time, Date) %>%
  group_by(ID, Meal_time) %>%
  mutate(Meal_time_diff = difftime(Date, lag(Date), units = "hours")) %>%
  ungroup()

Meal_diff$Meal_time_diff <- Meal_diff$Meal_time_diff-24 
hist(as.numeric(Meal_diff$Meal_time_diff))

average_meal_time_diff <- Meal_diff %>%
  group_by(ID) %>%
  summarize(Average_Meal_time_diff = mean(Meal_time_diff, na.rm = TRUE))

hist(as.numeric(average_meal_time_diff$Average_Meal_time_diff))

write.csv(average_meal_time_diff, "DF_Average_Meal_Time_Diff.csv")

columns_g <- grep("_g$", names(postprandial_df), value = TRUE)


# Calculate the standard deviation for each selected column grouped by ID
sd_per_group <- postprandial_df %>%
  group_by(ID) %>%
  summarize(across(all_of(columns_g), sd, na.rm = TRUE, .names = "sd_{col}"))

write.csv(sd_per_group, "DF_Average_Food_Group_sd.csv")
write.csv(postprandial_df, "postprandial_df.csv")


## Remove Data Not Available Meals ---------------------
#n=101, obs. = 3142
postprandial_df <- postprandial_df %>% filter(Dietary.intake != "data not available" &
                                                     Dietary.intake != "Data not available" & 
                                                     !is.na(Dietary.intake)) #n=97, obs. = 2522

# ## Remove those Patients with less than 3 Observations---------------------
postprandial_df <- postprandial_df %>% group_by(ID) %>% filter(n() > 10) #n=81, obs. = 2411

# ## Remove those observations where the postprandial rolling value is NA------
postprandial_df <- postprandial_df %>% filter(!is.na(postprandial_rolling)) #n=81, obs. = 2388

write.csv(postprandial_df, "Data_Analysis/Data/Postprandial_05.csv", row.names = FALSE)





library(dplyr)
library(lubridate)
library(purrr)


library(dplyr)
library(lubridate)
library(purrr)
library(dplyr)
library(lubridate)

library(dplyr)
library(lubridate)

# Step 1: Get the unique meal times for each person
library(dplyr)
library(lubridate)

# Step 1: Get the unique meal times for each person
meal_times_df <- premeal_24 %>%
  filter(Meal == 1) %>%
  select(ID, Meal_time = Date) %>%  # Use Date as Meal_time for each person
  distinct()


# Load necessary library
library(dplyr)
library(lubridate)

# Assuming premeal_24 is already loaded

# Step 1: Get the unique meal times for each person
meal_times_df <- premeal_24 %>%
  filter(Meal == 1) %>%
  select(ID, Meal_time = Date) %>%  # Use Date as Meal_time for each person
  distinct() %>%
  mutate(
    Meal_type = case_when(
      hour(Meal_time) >= 6 & hour(Meal_time) < 10 ~ "Breakfast",  # 6am - 9:59am
      hour(Meal_time) >= 10 & hour(Meal_time) < 15 ~ "Lunch",     # 10am - 2:59pm
      hour(Meal_time) >= 15 & hour(Meal_time) < 21 ~ "Dinner",    # 3pm - 8:59pm
      TRUE ~ "Other"  # For any other time, default to "Other"
    )
  )

# View the result
head(meal_times_df)

# Step 2: Calculate the standard deviation of Meal_time for each person and meal type
meal_variability <- meal_times_df %>%
  dplyr::group_by(ID, Meal_type)

meal_times_df <- meal_times_df %>%
  mutate(Meal_time = ymd_hms(Meal_time))

meal_times_df$hour <- sub(".* (\\d{2}:\\d{2}):.*", "\\1", meal_times_df$Meal_time)

mdy_hms(meal_times_df$hour)

# Step 1: Calculate the standard deviation of the Meal_time (breakfast) for each person
meal_variability <- meal_times_df %>%
  dplyr::group_by(ID) %>%
  dplyr::filter(Meal_type == "Breakfast") %>% 
  dplyr::mutate(
    time_sd = sd(hm(hour), na.rm = TRUE) / (60 * 60)) %>% 
  ungroup()

meal_variability_first24 <- meal_times_df %>%
  dplyr::group_by(ID) %>%
  #dplyr::filter(Meal_type == "Breakfast") %>% 
  dplyr::summarise(
    time_sd_first24 = sd(hm(hour), na.rm = TRUE)/ (60 * 60)) %>% 
  ungroup()

write.csv(meal_variability_first24, 'meal_variability_first24.csv')

###

#### Mean postprandial and other values----------------------
data_after_24 <- data %>%
  dplyr::group_by(ID) %>%
  filter(Date > (min_Date + (24*60*60))) %>%  # Keeps data after the first 24 hours
  ungroup()
    

meal_times_df <- data_after_24  %>%
  filter(Meal == 1) %>%
  select(ID, Meal_time = Date) %>%  # Use Date as Meal_time for each person
  distinct() %>%
  mutate(
    Meal_type = case_when(
      hour(Meal_time) >= 6 & hour(Meal_time) < 10 ~ "Breakfast",  # 6am - 9:59am
      hour(Meal_time) >= 10 & hour(Meal_time) < 15 ~ "Lunch",     # 10am - 2:59pm
      hour(Meal_time) >= 15 & hour(Meal_time) < 21 ~ "Dinner",    # 3pm - 8:59pm
      TRUE ~ "Other"  # For any other time, default to "Other"
    )
  )

# View the result
head(meal_times_df)

# Step 2: Calculate the standard deviation of Meal_time for each person and meal type
meal_variability <- meal_times_df %>%
  dplyr::group_by(ID, Meal_type)

meal_times_df <- meal_times_df %>%
  mutate(Meal_time = ymd_hms(Meal_time))

meal_times_df$hour <- sub(".* (\\d{2}:\\d{2}):.*", "\\1", meal_times_df$Meal_time)

mdy_hms(meal_times_df$hour)

# Step 1: Calculate the standard deviation of the Meal_time (breakfast) for each person
meal_variability <- meal_times_df %>%
  dplyr::group_by(ID) %>%
  dplyr::filter(Meal_type == "Breakfast") %>% 
  dplyr::mutate(
    time_sd = sd(hm(hour), na.rm = TRUE) / (60 * 60)) %>% 
  ungroup()

meal_variability_last24 <- meal_times_df %>%
  dplyr::group_by(ID) %>%
  #dplyr::filter(Meal_type == "Breakfast") %>% 
  dplyr::summarise(
    time_sd_last24 = sd(hm(hour), na.rm = TRUE)/ (60 * 60)) %>% 
  ungroup()

write.csv(meal_variability_last24, 'meal_variability_reaminingdf.csv')

