##########################################
### MADRID HOUSING PROJECT
############################
############################
source("DataAnalyticsFunctions.R")
#read data into R
data<-read_excel("houses.xlsx",col_names = TRUE)

installpkg('readxl')
installpkg('dplyr')
installpkg('tidyr')
installpkg('ggplot2')
installpkg('onehot')
installpkg('MASS')
installpkg('e1071')
installpkg('caTools')
installpkg('reshape2')

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2) 
library(onehot)  
library(MASS)
library(e1071) 
library(caTools)
library(reshape2)

ncol(data)
#We have a total of 57 variables

#To become familiar with the data, I will investigate each variable
#one by one

### EXPLORATORY ANALYSI
## VARIABLE 1: ID
#This is just an Identifier ID, I don't think it will be helpful for my modeling
#so I will remove it

##VARIABLE 2: TITLE
#This is the title from listing
summary(data$title)
unique(data$title)
#This is only a descriptive point
#I was thinking of creating a new variable that includes the house
#type, but it would be the same as the "house_type_id" 

## I will remove this varaible for further analysis

####################################################################
####################################################################
##VARIABLE 3: SUBTITLE
#This contains the neighborhood and city
#I think this is very interesting to see the effect of the neighborhood
#on the price of the household since we know that cities have more expensive 
#areas
summary(data$subtitle)
length(unique(data$subtitle)) #we have 146 neighborhoods 
table(data$subtitle)

#I want to remove the ", Madrid" since all of our houses are in Madrid, and therefore
#it is redundant to include it
data$subtitle <- gsub(", Madrid", "", data$subtitle)
table(data$subtitle)

#I want to know the neighborhood with most houses 
neighborhood_counts <- table(data$subtitle)
neighborhood_counts_df <- as.data.frame(neighborhood_counts)
colnames(neighborhood_counts_df) <- c("Neighborhood", "Count")
top_5_neighborhoods <- neighborhood_counts_df[order(-neighborhood_counts_df$Count), ][1:5, ]
print(top_5_neighborhoods)
#We see how most houses are in Chamartín and Moncloa

#Lastly, I will factor 
data$subtitle<- as.factor(data$subtitle)
####################################################################
####################################################################


##VARIABLE 4:SQ_MT_BUILT
#Square meter built
summary(data$sq_mt_built)
sum(is.na(data$sq_mt_built)) #I have 126 NA values 
data$sq_mt_built<-as.numeric(data$sq_mt_built) #I need to transform it to a numeric variable
summary(data$sq_mt_built)
#I will rename this variable to sqmt so there is no confusion
names(data)[names(data) == "sq_mt_built"] <- "sqmt"


##VARIABLE 5: SQ_MT_USEFUL
#Square meter useful
summary(data$sq_mt_useful)
data$sq_mt_useful<-as.numeric(data$sq_mt_useful) #I need to transform it to a numeric variable
summary(data$sq_mt_useful) #I have so many NA's
sum(is.na(data$sq_mt_useful))/(length(data$sq_mt_useful))*100 #I have 62% of my data as NAs

##VARIABLE 6: N_ROOMS
#This represents the number of rooms
# data$n_rooms<-as.factor(data$n_rooms)
# Do not treat n_rooms as factor
summary(data$n_rooms) #Most of the houses have 3 or 2 rooms, 
#an interesting fact is that there are houses with no rooms
data[data$n_rooms == 0, ]

##VARIABLE 7: N_BATHROOMS
summary(data$n_bathrooms)
sum(is.na((data$n_bathrooms))) #I have 16 NA's
# data$n_bathrooms[is.na(data$n_bathrooms)] <- "no information"
# data$n_bathrooms<-as.factor(data$n_bathrooms)
# Do not treat n_bathrooms as factor
# I will remove rows with no information for bathroom at the end of this section


##VARIABLE 8: N_FLOORS
summary(data$n_floors)
sum(is.na((data$n_floors))) #most of my data is NA so I will remove this vairable 
#since it won't be relevant for my analysis 
length(data$n_floors) 

##VARIABLE 9: SQ_MT_ALLOTMENT
#Square meter allotment
summary(data$sq_mt_allotment)
sum(is.na((data$sq_mt_allotment))) #lots of NAs so I won't use it for my analysis

##VARIABLE 10: LATITUDE
sum(is.na((data$latitude))) #lots of NAs so I won't use it for my analysis

##VARIABLE 11: LONGITUDE
#This variable will be useless if we don't use latitude
#Additionally, we have enough information about the neighborhood


##VARIBLE 12:RAW ADDRESS
summary(data$raw_address)
sum(is.na(data$raw_address)) #I have some NA's that I will convert to no info
data$raw_address[is.na(data$raw_address)] <- "no information"
data$raw_address<-as.factor(data$raw_address)
#I am unsure if I will use this variable since I have neighborhood information
#and I think it conveys a similar type of information

##VARIABLE 13: IS_EXACT_ADDRESS_HIDEN
#Boolean values
#I don't think I will use this variable since it doesn't give
#me any type of valuable info
summary(data$is_exact_address_hidden)
sum(is.na(data$is_exact_address_hidden))
data$is_exact_address_hidden<-as.factor(data$is_exact_address_hidden)

##VARIABLE 14: STREET NAME
summary(data$street_name)
sum(is.na(data$street_name)) # I have a couple of NAs
#I will replace them with unknown
data$street_name[is.na(data$street_name)] <- "Unknown"
data$street_name<-as.factor(data$street_name)

## We are not going to narrow down the house price prediction to street level
## street-level is often too specific and may lead to overfitting
## Will remove this variable for further analysis


##VARIABLE 15: STREET NUMBER
#I don't think the street number will be relevant for our
#analysis so I will remove it 

##VARIABLE 16: PORTAL
#This is an irrelevant variable I will remove

##VARIABLE 17: FLOOR
#this is the floor the house is on
sum(is.na(data$floor)) #I have a couple of NAs I will replace
data$floor[is.na(data$floor)] <- "Unknown"
data$floor<-as.factor(data$floor)
summary(data$floor)
#Here's what each floor means:
## Floors 1-9 correspond to the respective levels of the building.
## Bajo refers to the ground floor, 
## and Entreplanta refers to a mezzanine level (an intermediate floor in a building which is open to the floor below)
## which can be either external or internal
## Semi-sótano indicates a semi-basement level, with variations for external and internal.
## Sótano is the basement level.

##VARIABLE 18: IS_FLOOR_UNDER
#I am unsure of the meaning of this variable
summary(data$is_floor_under)
sum(is.na(data$is_floor_under)) #We do have NAs
data$is_floor_under[is.na(data$is_floor_under)] <- "Unknown"
data$is_floor_under<-as.factor(data$is_floor_under)

contingency_table <- table(data$floor, data$is_floor_under)
print(contingency_table)
#Based on what I see here it appears to indicate whether the
#specified floor is below ground level
#Floors labeled 1 to 9 are not associated with True, 
#indicating that these floors are all above ground.

#I don't think this variable will be useful but I won't remove it as for now

##VARIABLE 19: DOOR
#I am unsure of what this variable means and I don't think a "door"
#variable has a meaning for this so I will drop it
summary(data$door)


#############################################################################
#############################################################################

##VARIABLE 20: NEIGHBORHOOD_ID
#Based on my data exploration, this variable categorizes real estate properties
#by neighborhood in Madrid, including details  their 
#average price per square meter (€/m²)
summary(data$neighborhood_id)
sum(is.na(data$neighborhood_id)) #No NAs, YAY!
data$neighborhood_id<-as.factor(data$neighborhood_id)
summary(data$neighborhood_id)
#This is another variable related to neighborhood, I will keep it in 
#the dataset for now, but later on I will decide what variable to use
#for determining the neighborhood price since as I have described above
#I have a couple of those

## I will take out the mean price per sqmt by neighborhood and make them a new column
data$avg_price_by_area_by_neighborhood <- as.numeric(gsub(".*\\(([^ ]+) €/m2\\).*", "\\1", data$neighborhood_id))
summary(data$avg_price_by_area_by_neighborhood)
## There are 82 NA, 0.3% of the total obs
## I will just remove these 82 rows for further analysis

## Take neighborhood name out of NEIGHBORHOOD_ID and make a new column
data$neighborhood_name <- gsub(".*Neighborhood [0-9]+: ([^\\(]+) \\(.*", "\\1", data$neighborhood_id)
summary(data$neighborhood_name)
## No NAs
data$neighborhood_name <- as.factor(data$neighborhood_name)
summary(data$neighborhood_name)
length(unique(data$neighborhood_name))

#############################################################################
#############################################################################

## VARIABLE 21: OPERATION
summary(data$operation)
sum(is.na(data$operation))
data$operation<-as.factor(data$operation)
summary(data$operation) #All of our records are sale so I don't think it will be
#relevant to keep this variable since we only have one category

## VARIABLE 22: RENT_PRICE
sum(is.na(data$rent_price))
summary(data$rent_price) #I am suprised I see negative values and a negative mean
#I think this dataset is not meant for rent, but to look at sale prices since this is 
#the only category we have

negative_count <- sum(data$rent_price < 0)  # Count negative values
total_count <- nrow(data)                     # Count total values

# Calculate the percentage of negative values
percentage_negative <- (negative_count / total_count) * 100

# Print results
cat("Number of negative values:", negative_count, "\n")
cat("Total values:", total_count, "\n")
cat("Percentage of negative values:", percentage_negative, "%\n")

## VARIABLE 23: RENT_RPICE_BY_AREA
summary(data$rent_price_by_area) #Lots of NA's I don't think this variable will be relevant
length(data$rent_price_by_area)

## VARIABLE 24: IS_RENT_PRICE_KNOWN
summary(data$is_rent_price_known)
sum(is.na(data$rent_price_by_area)) #Again, lots of NAs, I will remove this variable

## VARIABLE 25: BUY_PRICE
#This is my target variable
summary(data$buy_price)
options(scipen = 999)
hist(data$buy_price, 
     main = "Distribution of Buy Prices", 
     xlab = "Buy Price", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")

## VARIABLE 26: BUY PRICE PER AREA
summary(data$buy_price_by_area)
sum(is.na(data$buy_price_by_area))
## This variable is simply just buy_price / sqmt
## We will keep buy_price and sqmt, remove buy_price_by_area


## VARIABLE 27: IS BUY PRICE KNOWN
sum(is.na(data$is_buy_price_known))
summary(data$is_buy_price_known)
data$is_buy_price_known<-as.factor(data$is_buy_price_known)
summary(data$is_buy_price_known) #I only get TRUE, which is good because
#this means we have enough data but at the same time it won't be relevant to
#include this specific variable in our model

## VARIABLE 28: HOUSE TYPE ID
sum(is.na(data$house_type_id)) #I have 391 NAs that I interpret as Unknowns
data$house_type_id[is.na(data$house_type_id)] <- "Unknown"
data$house_type_id<-as.factor(data$house_type_id)
summary(data$house_type_id)
#The translations are
#HouseType 1: Pisos - Apartments
#HouseType 2: Casa o chalet - House 
#HouseType 4: Dúplex - Duplex
#HouseType 5: Áticos - Penthouses
#Unknown - Unknown
#most of our data points are apartments

## VARIABLE 29: IS RENEWAL NEEDED
sum(is.na(data$is_renewal_needed)) #No NAs
summary(data$is_renewal_needed)
data$is_renewal_needed<-as.factor(data$is_renewal_needed)
summary(data$is_renewal_needed) 
#Most of our houses don't need a renewal

## VARIABLE 30: IS NEW DEVELOPMENT
sum(is.na(data$is_new_development)) #We have a couple of NAs 
data$is_new_development[is.na(data$is_new_development)] <- "Unknown"
data$is_new_development<-as.factor(data$is_new_development)
summary(data$is_new_development)
#Most of the houses are not a new development
#I will keep the variable for now, but I don't know how relevant it will be

## VARIABLE 31: BUILT_YEAR
sum(is.na(data$built_year)) #we don't have data for 11742
sum(is.na(data$built_year))/length(data$built_year) #that's a good amount of datapoints without info
data$built_year[is.na(data$built_year)] <-"Unknown"
data$built_year<-as.factor(data$built_year)
summary(data$built_year)
#More than 50% is Unknown
#I will remove it for further analysis


## VARIABLE 32: HAS_CENTRAL_HEATING
sum(is.na(data$has_central_heating)) #No info for a couple
data$has_central_heating[is.na(data$has_central_heating)] <-"Unknown"
data$has_central_heating<-as.factor(data$has_central_heating)
summary(data$has_central_heating)
#We have a good amount of Unknown so I don't think this will be 
#relevant for our analysis

## VARIABLE 33: ARE_PETS_ALLOWED
sum(is.na(data$are_pets_allowed)) #ALL IS NAs
length(data$are_pets_allowed)


## VARIABLE 34: HAS_AC
sum(is.na(data$has_ac)) #Significant NAs
data$has_ac<-as.factor(data$has_ac)
summary(data$has_ac) #The categories are pretty similar, 
#so I don't think it's very impactful because the categories are the same

## VARIABLE 35: HAS_FITTED_WARDROBES
sum(is.na(data$has_fitted_wardrobes)) #We have 8343 NAs
summary(data$has_fitted_wardrobes)
data$has_fitted_wardrobes[is.na(data$has_fitted_wardrobes)] <-"Unknown"
data$has_fitted_wardrobes<-as.factor(data$has_fitted_wardrobes)

## VARIABLE 36: HAS_LIFT
sum(is.na(data$has_lift))
data$has_lift[is.na(data$has_lift)] <-"Unknown"
data$has_lift<-as.factor(data$has_lift)
summary(data$has_lift)


## VARIABLE 37: IS_EXTERIOR
sum(is.na(data$is_exterior))
data$is_exterior[is.na(data$is_exterior)] <-"Unknown"
data$is_exterior<-as.factor(data$is_exterior)
summary(data$is_exterior)


## VARIABLE 38: HAS_GARDEN
sum(is.na(data$has_garden)) #Lots of NAs
data$has_garden[is.na(data$has_garden)] <-"False"
data$has_garden<-as.factor(data$has_garden)
summary(data$has_garden)

## VARIABLE 39: HAS_POOL
sum(is.na(data$has_pool)) #Lots of NAs I interpret those as False
data$has_pool[is.na(data$has_pool)] <-"False"
data$has_pool<-as.factor(data$has_pool)
summary(data$has_pool)

## VARIABLE 40: HAS_TERRACE
sum(is.na(data$has_terrace))
data$has_terrace[is.na(data$has_terrace)] <-"False"
data$has_terrace<-as.factor(data$has_terrace)
summary(data$has_terrace)

## VARIABLE 41: HAS_BALCONY
sum(is.na(data$has_balcony)) 
data$has_balcony[is.na(data$has_balcony)] <-"False"
data$has_balcony<-as.factor(data$has_balcony)
summary(data$has_balcony)


## VARIABLE 42: HAS_STORAGE_ROOM
sum(is.na(data$has_storage_room)) #I have 14044 NAs
data$has_storage_room[is.na(data$has_storage_room)] <-"False"
data$has_storage_room<-as.factor(data$has_storage_room)
summary(data$has_storage_room)

## VARIABLE 43: IS FURNISHED
sum(is.na(data$is_furnished)) #I have 21742 NAs
#Lost of NAs so I will exclude this from dataset 


## VARIABLE 44: IS_KITCHEN_EQUIPED
sum(is.na(data$is_kitchen_equipped))#I have 21742 NAs
#Lost of NAs so I will exclude this from dataset 


## VARIABLE 45: IS_ACCESSIBLE
sum(is.na(data$is_accessible)) #I have a lot  17668NAs


## VARIABLE 46: HAS_GREEN_ZONES
sum(is.na(data$has_green_zones))#I have a lot 17685 NAs

## VARIABLE 47: ENERGY_CERTIFICATE
sum(is.na(data$energy_certificate)) #No NA's YAY
summary(data$energy_certificate)
data$energy_certificate<-as.factor(data$energy_certificate)
summary(data$energy_certificate)
#The meaning is: 
#A, B, C, D, E, F, G: These represent the different categories of 
#energy certificates, where A is likely the most energy-efficient 
#rating and G the least efficient.
#inmueble extento:"property exempt," indicating properties that are exempt from requiring an energy certificate.
#no indicado: no info
#en trámite: in the process of getting the certificate


## VARIABLE 48: HAS_PARKING
sum(is.na(data$has_parking))
summary(data$has_parking)
data$has_parking<-as.factor(data$has_parking)

## VARIABLE 49: HAS_PRIVATE_PARKING
sum(is.na(data$has_private_parking))
data$has_private_parking<-as.factor(data$has_private_parking)
summary(data$has_private_parking) #ALL NA's so I will remove it 

## VARIABLE 50: HAS_PUBLIC PARKING
sum(is.na(data$has_public_parking)) #Again a lot of NAs so I will remove it 


## VARIABLE 51: IS_PARKING_INCLUDED_IN_PRICE
sum(is.na(data$is_parking_included_in_price))
data$is_parking_included_in_price[is.na(data$is_parking_included_in_price)] <-"Unknown"
data$is_parking_included_in_price<-as.factor(data$is_parking_included_in_price)
summary(data$is_parking_included_in_price)

## More than half obs, 13999, is unknown. 13999 is equal to false in has_parking
## This variable is only valuable for analyzing house with parking
## I will remove is_parking_included_in_price


## VARIABLE 52: PARKING_PRICE
sum(is.na(data$parking_price)) #We have the same number of NA's as the parking included in price
data$parking_price[is.na(data$parking_price)] <-"No Parking Available"
# Problem here, I do not think NA necessarily means "No Parking Available"
data$parking_price<-as.factor(data$parking_price)
summary(data$parking_price)
# More than half of the data is NA, we should remove this column for further analysis
# And the price range is very large, apparently some parking spaces are for sale
# While some others are for rent
# We should not confuse selling price and rent

## VARIABLE 53: IS_ORIENTATION_NORTH
sum(is.na(data$is_orientation_north))
data$is_orientation_north[is.na(data$is_orientation_north)] <-"Unkown"
data$is_orientation_north<-as.factor(data$is_orientation_north)
summary(data$is_orientation_north)

## VARIABLE 54: IS_ORIENTATION_WEST
sum(is.na(data$is_orientation_west))
data$is_orientation_west[is.na(data$is_orientation_west)] <-"Unkown"
data$is_orientation_west<-as.factor(data$is_orientation_west)
summary(data$is_orientation_west)

## VARIABLE 55: IS_ORIENTATION_SOUTH
sum(is.na(data$is_orientation_south))
data$is_orientation_south[is.na(data$is_orientation_south)] <-"Unkown"
data$is_orientation_south<-as.factor(data$is_orientation_south)
summary(data$is_orientation_south)

## VARIABLE 56: IS_ORIENTATION_EAST
sum(is.na(data$is_orientation_east))
data$is_orientation_east[is.na(data$is_orientation_east)] <-"Unkown"
data$is_orientation_east<-as.factor(data$is_orientation_east)
summary(data$is_orientation_east)

## Out of 21616 total obs, 10331 obs have no data for orientation, nearly half.
## The high percentage of missing values makes it difficult to rely on them for
## predictive modeling.
## I will remove above 4 orientation variables from further analysis


## NEW VARIABLE: price_based_on_area
## I checked Kaggle, and this variable here is simply just buy price/sqmt
## I do not think it is necessary to have, I will remove it

data$price_based_on_area <- data$buy_price_by_area * data$sqmt
summary(data$price_based_on_area)
#I will go ahead and remove rows where the data is NAs
data <- data[!is.na(data$price_based_on_area), ]

summary(data$price_based_on_area)


####################################################################
## Remove 82 rows having NA for avg_price_by_area_by_neighborhood
## Remove rows having no information for n_bathrooms

data <- data[!is.na(data$avg_price_by_area_by_neighborhood), ]
data <- data[!is.na(data$n_bathrooms), ]

## Left with 21518 obs

sort(table(data$floor))
## There are four floor types have less than 10 obs
## These floor types are unlikely to be representative for the analysis
## I will rows with these floor types
data <- data[!data$floor %in% c("Semi-sótano", "Entreplanta", "Sótano exterior", "Sótano"), ]
data$floor <- droplevels(data$floor)
sort(table(data$floor))

## Left with 21505 obs

## Remove Unknwon in housetype_id
data <- data[!data$house_type_id %in% c("Unknown"), ]
data$house_type_id <- droplevels(data$house_type_id)
summary(data$house_type_id)

# convert n_bathrooms from char to num
data$n_bathrooms <- as.numeric(data$n_bathrooms)
str(data$n_bathrooms)
summary(data$n_bathrooms)
table(data$n_bathrooms)
####################################################################


## Below is a check I did to see the diff between buy_price and buy_price_by_area
data_check <- data[, c("sqmt", "buy_price", "buy_price_by_area", "price_based_on_area")]
data_check$by_area_no_rounding <- data_check$buy_price / data_check$sqmt
data_check$rounding_diff <- (data_check$buy_price_by_area - data_check$by_area_no_rounding) * data_check$sqmt
data_check$buy_price_price_based_on_area_diff <- data_check$price_based_on_area - data_check$buy_price


########################################
#############VISUALIZATIONS#############
########################################

##To know the houses that are the cheapest and the most expensive
#El Viso is a very prominent neighborhood
top_expensive <- data %>%
  arrange(desc(buy_price)) %>%
  head(5)

top_cheap <- data %>%
  arrange(buy_price) %>%
  head(5)

top_houses <- bind_rows(
  mutate(top_expensive, Price_Category = "Most Expensive"),
  mutate(top_cheap, Price_Category = "Cheapest")
)

ggplot(top_houses, aes(x = reorder(title, buy_price), y = buy_price, fill = Price_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Top 5 Most Expensive and Cheapest Houses",
       x = "House Title",
       y = "Buy Price") +
  theme_minimal() +
  scale_fill_manual(values = c("Most Expensive" = "darkblue", "Cheapest" = "lightgreen"))


##################This visualization I think is not very good. Hard to see diff.
##################Need some work on


#Variables to remove: ID, SQ_MT_USEFUL, N_FLOORS, SQ_MT_ALLOTMENT
#LATITUDE, LONGITUDE, RAW_ADDRESS, IS_EXACT_ADDRESS_HIDEN, STREET NUMBER
#PORTAL, DOOR, OPERATION, RENT_PRICE, RENT_RPICE_BY_AREA, IS_RENT_PRICE_KNOWN, 
#IS_BUY_PRICE_KNOWN, ARE_PETS_ALLOWED, HAS AC, HAS FITTED WARDROBES,
#IS FURNISHED,  IS_KITCHEN_EQUIPED, IS_ACCESSIBLE, HAS_PRIVATE_PARKING, 
#HAS_PUBLIC PARKING, IS_ORIENTATION_NORTH, IS_ORIENTATION_WEST, 
#IS_ORIENTATION_SOUTH, IS_ORIENTATION_EAST

#Now I will create a data set with the variables that are relevant
# Create a new data set by removing the specified columns
# Create a new dataset by removing the specified columns
new_data <- data[, !(names(data) %in% c("id", "sq_mt_useful", "n_floors", "sq_mt_allotment", 
                                        "latitude", "longitude", "raw_address", 
                                        "is_exact_address_hidden", "street_number", 
                                        "portal", "door", "operation", "rent_price", 
                                        "rent_price_by_area", "is_rent_price_known", 
                                        "is_buy_price_known", "are_pets_allowed", 
                                        "has_ac", "has_fitted_wardrobes", "is_furnished", 
                                        "is_kitchen_equipped", "is_accessible", 
                                        "has_private_parking", "has_public_parking", "has_individual_heating", "has_green_zones",
                                        "is_orientation_north", "is_orientation_west", "is_orientation_south", 
                                        "is_orientation_east","price_based_on_area", "is_parking_included_in_price",
                                        "subtitle", "neighborhood_id", "title", "street_name", "buy_price_by_area",
                                        "built_year", "parking_price"))]

## Drop Unused Levels from Factors
summary(new_data$neighborhood_name)
new_data$neighborhood_name <- droplevels(new_data$neighborhood_name)
sort(table(new_data$neighborhood_name), decreasing = FALSE)
n_distinct(new_data$neighborhood_name)

# Check the structure of the new dataset
summary(new_data)
str(new_data)
colSums(is.na(new_data))
nrow(new_data)
colnames(new_data)

########################################
#############VISUALIZATIONS#############
########################################

#Correlation between price and sqmt, the biggest the house the more expensive
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mai = c(1.02, 0.82, 0.82, 0.42))
plot(new_data$sqmt, new_data$buy_price, 
     xlab = "Square Meters", 
     ylab = "Buy Price", 
     main = "Scatter Plot of Square Meters vs. Buy Price",
     col = "blue", pch = 19)
fit <- lm(buy_price ~ sqmt, data = new_data)
abline(fit, col = "red", lwd = 2)

#Price vs N rooms
ggplot(new_data, aes(x = as.factor(n_rooms), y = buy_price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Buy Price by Number of Rooms",
       x = "Number of Rooms",
       y = "Buy Price") +
  theme_minimal()

#Price vs N bathrooms
ggplot(new_data, aes(x = as.factor(n_bathrooms), y = buy_price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Buy Price by Number of Bathrooms",
       x = "Number of Bathrooms",
       y = "Buy Price") +
  theme_minimal()

# Histogram for Buy Price
ggplot(new_data, aes(x = buy_price)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Property Prices", 
       x = "Buy Price (€)", 
       y = "Frequency") +
  theme_minimal()

# Scatter plot for Property Size vs. Buy Price
ggplot(new_data, aes(x = sqmt, y = buy_price)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Property Size vs. Buy Price", 
       x = "Square Meters Built", 
       y = "Buy Price (€)") +
  geom_smooth(method = "lm", col = "red") +  # Adding linear regression line

# Bar Chart for Number of Rooms
  ggplot(new_data, aes(x = factor(n_rooms))) +
  geom_bar(fill = "orange") +
  labs(title = "Distribution of Properties by Number of Rooms", 
       x = "Number of Rooms", 
       y = "Count") +
  theme_minimal()

# Distribution of Energy Cert
ggplot(new_data, aes(x = energy_certificate, y = buy_price)) +
  geom_boxplot(fill = "green") +
  labs(title = "Buy Price by Energy Certificate Rating", 
       x = "Energy Certificate", 
       y = "Buy Price (€)") +
  theme_minimal()

# Multivariate Correlation Heatmap
installpkg('reshape2')
library(reshape2)
num_vars <- new_data[, c("buy_price", "sqmt", "n_rooms", "n_bathrooms", "price_per_sqm")]
corr_matrix <- cor(num_vars, use="complete.obs")

corr_melt <- melt(corr_matrix)

ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 0) +
  labs(title = "Correlation Heatmap of Key Variables",
       x = "Variable", 
       y = "Variable") +
  theme_minimal()

# Boxplot of Buy Price by House Type
ggplot(new_data, aes(x = house_type_id, y = buy_price)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Buy Price by House Type", 
       x = "House Type", 
       y = "Buy Price (€)") +
  theme_minimal()


############################################
## Modeling
############################################

installpkg('caret')
library(caret)

## Set random seed
set.seed(123)

## Ensure that the training set contains at least one data point from each neighborhood_name category
train_index <- createDataPartition(new_data$neighborhood_name, p = 0.8, list = FALSE)
new_train <- new_data[train_index, ]
new_test <- new_data[-train_index, ]

n_distinct(new_data$neighborhood_name)
n_distinct(new_train$neighborhood_name)
n_distinct(new_test$neighborhood_name)


## Create fold memberships for cross validation on the training set
n <- nrow(new_train)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]


nrow(new_train) 
nrow(new_test)

############

PerformanceMeasure <- function(prediction, actual, threshold) {
  R2(y=actual, pred=prediction)
}

############

## NULL MODEL
# My <- new_train$buy_price
prednull <- mean(new_train$buy_price) * rep(1, length(new_train$buy_price))
mean( (new_train$buy_price - prednull )^2) ## MSE
sqrt(mean( (new_train$buy_price - prednull )^2)) ## RMSE
PerformanceMeasure(prednull , new_train$buy_price, 0.02 ) ##R2

## Linear Model
model.lm <- glm(buy_price~.-neighborhood_name, data=new_train)
pred.lm <- predict(model.lm, newdata=new_train, type="response")
mean( (new_train$buy_price - pred.lm )^2) ## MSE
sqrt(mean( (new_train$buy_price - pred.lm )^2)) ## RMSE
PerformanceMeasure(pred.lm, new_train$buy_price, .02) ##R2

## Lasso
installpkg('glmnet')
library('glmnet')

Mx <- model.matrix(buy_price ~. -neighborhood_name, data=new_train)[,-1]
My <- new_train$buy_price
Mx_test <- model.matrix(buy_price ~. -neighborhood_name, data=new_test)[,-1]
My_test <- new_test$buy_price

lasso <- glmnet(Mx, My)
lassoCV <- cv.glmnet(Mx, My)

par(mar = c(4, 4, 4, 1))
par(mai = c(1, 1, 1, 0.5))
plot(lassoCV, main="Fitting Graph for CV Lasso", xlab = expression(paste("log(",lambda,")")))

lassomin <- glmnet(Mx, My, lambda=lassoCV$lambda.min)
predlassomin <- predict(lassomin, newx=Mx, type="response")
mean((new_train$buy_price - predlassomin)^2) ## MSE
sqrt(mean((new_train$buy_price - predlassomin)^2)) ## RMSE
PerformanceMeasure(predlassomin, new_train$buy_price, 0.02) ##R2

## Post Lasso
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
data.min <- data.frame(Mx[,features.min],My)
pl <- glm(My ~ ., data = data.min)
predpostlasso <- predict(pl, newdata = data.min, type = "response")
mean((new_train$buy_price - predpostlasso)^2)  ## MSE
sqrt(mean((new_train$buy_price - predpostlasso)^2))  ## RMSE
PerformanceMeasure(predpostlasso, new_train$buy_price, 0.02) ##R2

## Decision Tree
installpkg('rpart')
installpkg('rpart.plot')
library(rpart)
library(rpart.plot)

model.tree <- rpart(buy_price ~ .-neighborhood_name, data=new_train, cp=0.008)
rpart.plot(model.tree)
pred.tree <- predict(model.tree, newdata=new_train)
mean((new_train$buy_price - pred.tree)^2) ## MSE
sqrt(mean((new_train$buy_price - pred.tree)^2)) ## RMSE
PerformanceMeasure(pred.tree, new_train$buy_price, 0.02) ##R2

## Random Forest
installpkg("ranger")
library(ranger)

model.rf <- ranger(buy_price ~ .-neighborhood_name, data = new_train, importance = 'impurity')
importance_ranger <- importance(model.rf)
importance_df <- as.data.frame(importance_ranger)
importance_df$Feature <- rownames(importance_df)
ggplot(importance_df, aes(x=reorder(Feature, importance_ranger), y=importance_ranger)) +
  geom_bar(stat='identity', fill="darkblue") +
  coord_flip() +
  labs(title="Variable Importance Plot", x="Features", y="Importance")

pred.rf <- predict(model.rf, data = new_train)$predictions
mean((new_train$buy_price - pred.rf)^2) ## MSE
sqrt(mean((new_train$buy_price - pred.rf)^2)) ## RMSE
PerformanceMeasure(pred.rf, new_train$buy_price, 0.02) ##R2

## Cross Validation for OOSR2
OOSR2 <- data.frame(lm=rep(NA,nfold), l=rep(NA,nfold), 
                             pl=rep(NA,nfold), rf=rep(NA,nfold), 
                             tree=rep(NA,nfold)) 

for(k in 1:nfold){ 
  train <- which(foldid != k)
  
  ### Post-Lasso CV
  pl <- glm(My ~ ., data = data.min, subset = train)
  predmin <- predict(pl, newdata = data.min[-train, ], type = "response")
  OOSR2$pl[k] <- PerformanceMeasure(predmin, My[-train], .02)
  
  ### Lasso CV
  lassomin <- glmnet(Mx[train, ], My[train], lambda = lassoCV$lambda.min)
  predlassomin <- predict(lassomin, newx = Mx[-train, ], type = "response")
  OOSR2$l[k] <- PerformanceMeasure(predlassomin, My[-train], .02)
  
  ### Linear Model
  model.lm <- glm(buy_price ~ .-neighborhood_name, data = new_train, subset = train)
  pred.lm <- predict(model.lm, newdata = new_train[-train, ], type = "response")
  OOSR2$lm[k] <- PerformanceMeasure(pred.lm, My[-train], .02)
  
  ### Random Forest
  model.rf <- ranger(buy_price ~ .-neighborhood_name, data = new_train[train, ])
  pred.rf <- predict(model.rf, data = new_train[-train, ])$predictions
  OOSR2$rf[k] <- PerformanceMeasure(pred.rf, My[-train], .02)
  
  ### Decision Tree
  model.tree <- rpart(buy_price ~ .-neighborhood_name, data = new_train[train, ], cp = 0.008)
  pred.tree <- predict(model.tree, newdata = new_train[-train, ])
  OOSR2$tree[k] <- PerformanceMeasure(pred.tree, My[-train], .02)
  
  print(paste("Iteration", k, "of", nfold, "completed"))
}

res <- colMeans(OOSR2)
names(res) <- c("Linear Model", "Lasso", "Post-Lasso", "Random Forest", "Decision Tree")

par(mar = c(7, 7, 5, 5))
barplot(res, col = "darkblue", las = 2, xpd = FALSE, 
        xlab = "", ylab = bquote("Average OOS R2"), ylim = c(0, 1))


## Cross Validation for OOS TMAPE
PerformanceMeasure_TMAPE <- function(prediction, actual, threshold) {
    mean( abs( prediction - actual )/ ifelse(abs(actual)>threshold, abs(actual),threshold) )  
}

OOSTMAPE <- data.frame(lm=rep(NA,nfold), l=rep(NA,nfold), 
                    pl=rep(NA,nfold), rf=rep(NA,nfold), 
                    tree=rep(NA,nfold)) 

for(k in 1:nfold){ 
  train <- which(foldid != k)
  
  ### Post-Lasso CV
  pl <- glm(My ~ ., data = data.min, subset = train)
  predmin <- predict(pl, newdata = data.min[-train, ], type = "response")
  OOSTMAPE$pl[k] <- PerformanceMeasure_TMAPE(predmin, My[-train], .02)
  
  ### Lasso CV
  lassomin <- glmnet(Mx[train, ], My[train], lambda = lassoCV$lambda.min)
  predlassomin <- predict(lassomin, newx = Mx[-train, ], type = "response")
  OOSTMAPE$l[k] <- PerformanceMeasure_TMAPE(predlassomin, My[-train], .02)
  
  ### Linear Model
  model.lm <- glm(buy_price ~ .-neighborhood_name, data = new_train, subset = train)
  pred.lm <- predict(model.lm, newdata = new_train[-train, ], type = "response")
  OOSTMAPE$lm[k] <- PerformanceMeasure_TMAPE(pred.lm, My[-train], .02)
  
  ### Random Forest
  model.rf <- ranger(buy_price ~ .-neighborhood_name, data = new_train[train, ])
  pred.rf <- predict(model.rf, data = new_train[-train, ])$predictions
  OOSTMAPE$rf[k] <- PerformanceMeasure_TMAPE(pred.rf, My[-train], .02)
  
  ### Decision Tree
  model.tree <- rpart(buy_price ~ .-neighborhood_name, data = new_train[train, ], cp = 0.008)
  pred.tree <- predict(model.tree, newdata = new_train[-train, ])
  OOSTMAPE$tree[k] <- PerformanceMeasure_TMAPE(pred.tree, My[-train], .02)
  
  print(paste("Iteration", k, "of", nfold, "completed"))
}

res_TMAPE <- colMeans(OOSTMAPE)
names(res_TMAPE) <- c("Linear Model", "Lasso", "Post-Lasso", "Random Forest", "Decision Tree")

par(mar = c(7, 7, 5, 5))
barplot(res_TMAPE, col = "darkblue", las = 2, xpd = FALSE, 
        xlab = "", ylab = bquote("Average OOS TMAPE"), ylim = c(0, 0.4))

### Random Forest is better on both OOS TMAPE and OOS R2

pred.rf <- predict(model.rf, data = new_test)$predictions
mean((new_test$buy_price - pred.rf)^2) ## MSE
sqrt(mean((new_test$buy_price - pred.rf)^2)) ## RMSE
PerformanceMeasure(pred.rf, new_test$buy_price, 0.02) ##R2

summary(new_data$buy_price)

## R2 is good, but the RMSE is a bit big

histogram(pred.rf)
min(pred.rf)
max(pred.rf)

## Distribution of Residuals
ggplot(new_test, aes(x = residuals)) +
  geom_histogram(binwidth = 50000, fill = "lightblue", color = "black", alpha = 0.8) +
  labs(title = "Distribution of Residuals (Test Data)",
       x = "Residuals (Actual - Predicted)",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

## Actual vs. Predicted
ggplot(new_test, aes(x = predicted_price, y = buy_price)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "solid", size = 1.2) +
  labs(title = "Actual vs. Predicted Prices (Test Data)",
       x = "Predicted Price (€)",
       y = "Actual Price (€)") +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


################################################
############ Madrid Map Visualization ##########
################################################
installpkg('sf')
library(sf)

madrid_shape <- st_read("Barrios.shp")

neighborhood_names <- unique(new_data$neighborhood_name)
shape_names <- unique(madrid_shape$NOMBRE)
missing_names <- setdiff(neighborhood_names, shape_names)
print(missing_names)

new_data_modified <- new_data %>%
  mutate(neighborhood_name = recode(neighborhood_name,
                                    "Los Ángeles" = "Ángeles",
                                    "San Andrés" = "Villaverde Alto - Casco Histórico de Villaverde",
                                    "Valdebernardo - Valderribas" = "Valdebernardo",
                                    "El Cañaveral - Los Berrocales" = "El Cañaveral",
                                    "Ensanche de Vallecas - La Gavia" = "Ensanche de Vallecas",
                                    "12 de Octubre-Orcasur" = "Orcasur",
                                    "Cuzco-Castillejos" = "Castillejos",
                                    "Ventilla-Almenara" = "Almenara",
                                    "Jerónimos" = "Los Jerónimos",
                                    "Palomeras sureste" = "Palomeras Sureste",
                                    "Conde Orgaz-Piovera" = "Piovera",
                                    "Valdebebas - Valdefuentes" = "Valdefuentes",
                                    "Nuevos Ministerios-Ríos Rosas" = "Ríos Rosas",
                                    "Concepción" = "La Concepción",
                                    "Bernabéu-Hispanoamérica" = "Hispanoamérica",
                                    "Lavapiés-Embajadores" = "Embajadores",
                                    "Buena Vista" = "Buenavista",
                                    "Huertas-Cortes" = "Cortes",
                                    "Chueca-Justicia" = "Justicia",
                                    "Palos de Moguer" = "Palos de la Frontera",
                                    "Campo de las Naciones-Corralejos" = "Corralejos",
                                    "Malasaña-Universidad" = "Universidad",
                                    "Ambroz" = "Casco Histórico de Vallecas",
                                    "Las Tablas" = "Valverde",
                                    "Montecarmelo" = "El Pardo",
                                    "Tres Olivos - Valverde" = "Valverde",
                                    "Arroyo del Fresno" = "El Pardo"
                                    
  ))

neighborhood_names_modified <- unique(new_data_modified$neighborhood_name)
missing_names <- setdiff(neighborhood_names_modified, shape_names)
print(missing_names)

setdiff(shape_names, neighborhood_names_modified)

merged_data <- merge(madrid_shape, new_data, by.x = "NOMBRE", by.y = "neighborhood_name")

plot(st_geometry(madrid_shape))

installpkg("ggrepel")
library(ggrepel)

summary(merged_data$buy_price)

## Median Price
merged_data_summary <- merged_data %>%
  group_by(NOMBRE) %>%
  summarize(median_buy_price = median(buy_price))

ggplot(data = merged_data_summary) +
  geom_sf(aes(fill = median_buy_price)) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Median Buy Price") + 
  labs(title = "Madrid Neighborhood House Prices (Median)") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

## Mean Price
merged_data_summary <- merged_data %>%
  group_by(NOMBRE) %>%
  summarize(mean_buy_price = mean(buy_price))

ggplot(data = merged_data_summary) +
  geom_sf(aes(fill = mean_buy_price)) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Mean Price") + 
  labs(title = "Madrid Neighborhood Residential Property Prices (Mean)") +
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

## Mean Price With Label
merged_data_summary <- merged_data %>%
  group_by(NOMBRE) %>%
  summarize(mean_buy_price = mean(buy_price))

merged_data_summary <- merged_data_summary %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1], y = st_coordinates(centroid)[,2])

ggplot(data = merged_data_summary) +
  geom_sf(aes(fill = mean_buy_price)) +
  geom_text_repel(aes(x = x, y = y, label = NOMBRE), 
                  size = 2.5, 
                  color = "black", 
                  point.padding = 1,
                  segment.color = "grey50",
                  segment.size = 0.5,
                  nudge_y = 0.3,
                  nudge_x = 0.3) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Mean Price") + 
  labs(title = "Madrid Neighborhood Residential Property Prices (Mean) With Labels") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

## House Type Map

house_type_counts <- new_data %>%
  group_by(neighborhood_name, house_type_id) %>%
  summarize(house_count = n()) %>%
  ungroup()

merged_house_type_1 <- merge(madrid_shape, house_type_counts %>% filter(house_type_id == "HouseType 1: Pisos"), 
                             by.x = "NOMBRE", by.y = "neighborhood_name")

merged_house_type_2 <- merge(madrid_shape, house_type_counts %>% filter(house_type_id == "HouseType 2: Casa o chalet"), 
                             by.x = "NOMBRE", by.y = "neighborhood_name")

# HouseType 1: Pisos 
ggplot(data = merged_house_type_1) +
  geom_sf(aes(fill = house_count)) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "HouseType 1: Pisos Count") +
  labs(title = "Number of HouseType 1: Pisos in Each Madrid Neighborhood") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# HouseType 2: Casa o chalet 
ggplot(data = merged_house_type_2) +
  geom_sf(aes(fill = house_count)) +
  scale_fill_gradient(low = "pink", high = "red", name = "HouseType 2: Casa o chalet Count") +
  labs(title = "Number of HouseType 2: Casa o chalet in Each Madrid Neighborhood") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


######################################
## Some Clustering
######################################
x <- model.matrix(~ buy_price, data=merged_data)[,-1]
x.scaled <- scale(x)

five.clusters <- kmeans(x.scaled, 5, nstart=10)

merged_data$cluster <- five.clusters$cluster

ggplot(data = merged_data) +
  geom_sf(aes(fill = factor(cluster), geometry = geometry)) +
  scale_fill_manual(values = c("red", "orange", "yellow", "lightblue", "blue"), name = "Cluster") +
  labs(title = "Madrid Neighborhood Clusters") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

## buy_price = expected revenue
## 
## Limitation: we are only focus on average, 


## Decision Making

decision <- new_test

predicted_buy_price <- predict(model.rf, data = decision)$predictions

calculate_profit <- function(actual_price, predicted_price) {
  # Buying cost
  property_tax <- 0.06
  notary_land_registry_fees <- 0.005
  community_fees <- 200
  
  expected_cost <- actual_price * (1 + property_tax + notary_land_registry_fees) + community_fees
  
  # Selling cost
  capital_gains_tax <- 0.18
  agency_fees <- 0.03
  
  # Expected Annual growth rate
  r <- 0.05
  
  # Expected Profit
  expected_profit <- (predicted_price - expected_cost) * (1 - capital_gains_tax - agency_fees)
  
  return(expected_profit)
}

decision$predicted_buy_price <- predicted_buy_price
decision$expected_profit <- mapply(calculate_profit, decision$buy_price, predicted_buy_price)

decision$investment_decision <- ifelse(decision$expected_profit > 0, "Yes", "No")

summary(decision$investment_decision)

table(decision$investment_decision)
hist(decision$expected_profit)



