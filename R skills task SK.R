#Task Solutions - SK

install.packages("tidyverse")

#Load data
library(tidyverse)
read_csv("C:/Users/shani/Documents/R skills Shanice Kilaire/Input/ice.cream.data.csv")

#Create data frame
ice.cream.data<-read_csv("C:/Users/shani/Documents/R skills Shanice Kilaire/Input/ice.cream.data.csv")
df<-data.frame(ice.cream.data)
head(df)

#remove the 'X' prefixes
colnames(df) <- gsub("^X", "", colnames(df))
head(df)

#Task 1.1
#Subset the data based on Retail Volume in 
#tonnes and Retail value in USD

#Create a data subset by filtering
#by the ID number  that corresponds 
#to the retail volume in tonnes and 
#retail value in USD.

df1<-filter(df, DataTypeID %in% c(410,911))
head(df1$Data.Type)

#Task 1.2
#Remove columns. 
#Columns to keep: Region, Country, Data Type, 
#Unit, Unit Multiplier and years from 2000 to 2019

#Create data frame by selecting necessary columns
df2 <- df1 %>%
select('Region','Country','Data.Type', 'Unit', 'Unit.Multiplier', '2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019' )
head(df2)

#Task 1.3
#Convert volume data to kilograms and value data to 
#US dollars. Then remove Unit and Unit Multiplier columns. 

#First Convert the Year data into a single column
df3 <- df2 |> 
pivot_longer(cols = c('2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019'),
               names_to = 'year',
               values_to = 'value')
head(df3)


#Multiply the values by the corresponding 
#unit multiplier

df4<-df3 %>%  mutate(across(c('value'), 
                            ~.* df3$Unit.Multiplier))

head(df4)
View(df4)

#remove unit and unit multiplier columns
df5 <- df4 %>%
select(-c('Unit','Unit.Multiplier'))
head(df5)
View(df5)

#Task 1.4
#Convert data into a more user friendly format
#Split the data type columns so that the value and volume data are each in independent columns
df6<-df5 %>% pivot_wider(names_from = Data.Type)
head(df6)
View(df6)

#Task 1.5
#Rename variables to ice cream value and ice 
#cream volume
colnames(df6)[4] <- "Ice Cream Volume"
colnames(df6)[5] <- "Ice Cream Value"
head(df6)
View(df6)

#Task 1.6
#Calculate the ice cream price
#price is calculated by dividing the value by the 
#volume to give the price per kg
df7 <- df6 %>%
mutate('Ice Cream Price' = df6$`Ice Cream Value` / df6$`Ice Cream Volume`)

head(df7)
View(df7)

#Task 1.7
#For each year, calculate total volume by Region. 
#Which is the second largest region by ice cream 
#volumes in 2018?

# First, calculate the total volume per region per year
regions<-df7 %>%
group_by(Region,year) %>%
summarize('Total Volume' = sum(`Ice Cream Volume`)) 
head(regions)

#filter by the year 2018
volume_2018<- regions %>% filter(year==2018)
print(volume_2018)

#arrange in desc order
volume_2018<-volume_2018 %>% arrange(desc(`Total Volume`))
print(volume_2018)

#return the second largest region by ice cream volume
second_largest<-volume_2018[2,]
print(second_largest)

#Task 1.8
#Try writing a function, which applies step I.7.
#Make it as general as possible.

#General function code with explanations

# The function needs to output a value for a 
#specific rank with regards to interchangeable metric
#parameters and filtering by year, country and region.
#needs to be applied to the df7 data set 

#general code for ranking function framework explained

ranking_funtion <- df7 %>%
#filter(year=='') # filter function applied if needed
group_by('country or region') %>% # parameter for
#country or region
summarize('Total measure' = sum('ice cream measure') %>% #summarise by desired measure
arrange(desc('measure'))) %>%
slice('rank') %>% # provide the desired rank
pull('') # pull the country or region relative to 
#the desired rank

#Task 1.8a)
#What is the third largest Region by Ice Cream Value?

third_largest_region <- df7 %>%
group_by(Region) %>%
summarize('Total Ice Cream Value'= sum(`Ice Cream Value`)) %>%
arrange(desc('Total Ice Cream Value')) %>%
slice(3) %>% 
pull(Region) 
print(paste("The third largest region by ice cream value is:", third_largest_region))

#Task 1.8b)
#What is the tenth largest country by Ice Cream Volume?
tenth_largest_country <- df7 %>%
group_by(Country) %>%
summarize('Total Ice Cream Volume'= sum(`Ice Cream Volume`)) %>%
arrange(desc('Total Ice Cream Volume')) %>%
slice(10) %>% 
pull(Country) 
print(paste("The tenth largest country by ice cream volume is:", tenth_largest_country))

#Task 1.8c) 
#Which country had the second largest Ice 
#Cream Price in 2009?
second_largest_price <- df7 %>%
  filter(year==2009) %>%
  group_by(Country) %>%
  summarize('Total Ice Cream Price'= sum(`Ice Cream Price`)) %>%
  arrange(desc('Total Ice Cream Price')) %>%
  slice(2) %>% 
  pull(Country)

print(paste("The country with the second largest ice cream price in 2009 is:", second_largest_price))

#Task 2: Convert macro data into easier format
#load data
library(tidyverse)
read_csv("C:/Users/shani/Documents/R skills Shanice Kilaire/Input/macro.data.csv")
# create data frame
macro.data<-read_csv("C:/Users/shani/Documents/R skills Shanice Kilaire/Input/macro.data.csv")
df_1<-data.frame(macro.data)
head(df_1)

#check columns
column_names <- colnames(df_1)
print(column_names)

#remove the 'X' prefixes
colnames(df_1) <- gsub("^X", "", colnames(df_1))

#remove unnecessary columns
df_1<- df_1 %>% select(-c("CountryID", "Industry", 
"Edition", "Category","ParentID","Data.Type","Lowest.Level",
"Modelled","ProductID", "DataTypeID", "Hierarchy.Level",
"Current.Constant", "Currency.Conversion"))

head(df_1)

#Convert the years to a single column
df_2<- df_1 |> 
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to="Year",
               values_to="Value")
head(df_2)
View(df_2)

#multiply the values by the corresponding unit multiplier
df_3 <- df_2 %>%
  mutate(Value = Value * Unit.Multiplier)
View(df_3)

#Remove Unit and Unit multiplier columns
df_4 <- df_3 %>%
  select(-c('Unit','Unit.Multiplier'))
head(df_4)
View(df_4)

#Segregate the subcategory data into individual columns
df_5 <- df_4 %>% pivot_wider(names_from =Subcategory,
                             values_from = Value)
View(df_5)

#rename the columns
colnames(df_5)[4] <- "GDP"
colnames(df_5)[6] <- "Population"
head(df_5)
View(df_5)

#Task 3.1 and 3.2
#Join both data frames df7 (Ice cream data)
#and df_5( population data)

#There are many ways of joining the two data frames,
#the first 3 methods will join the data based on
#matching information for the 'Region', 'Country' 
#and 'Year' variables and will require no subsetting
#for the years variable

#rename year column
colnames(df7)[3] <- "Year"

#These two join functions give the same results

#inner join - merge data based on matching values in specific columns
merged_df <- inner_join(df7, df_5, by = c("Region", "Country", "Year"))
View(merged_df)

#left joined df, keeping rows of df7 and matching to rows of df_5
left_joined_df <- left_join(df7, df_5, by = c("Region", "Country", "Year"))
View(left_joined_df)

#The third alternative method is to do a right join
#Right joined df - keeping rows of df_5 and matching rows of df7
right_joined_df <- right_join(df_5, df7, by = c("Region", "Country", "Year"))
View(right_joined_df)

#Using any of the inner, left and right join methods
#will not require to subset the data later on, the
#matching data for the years are assigned upfront 
#with no need to remove or subset values relevant 
#to only one data frame

#Methods that would require data subsetting

#If the data frames are joined by one matching column
#data, in this case the region and country, the years
#will not match and further adjustments will need to 
#be made

#Merging by region
df_joined_1 = merge(x = df7, y = df_5, by = "Region")

#Merging by country
df_joined_2 = merge(x = df7, y = df_5, by ="Country")

View(df_joined_1)
View(df_joined_2)

#These two methods will require subsetting the 
#years which can be done as follows:

#sub-setting the data by year
df_joined_1_sub <- df_joined_1 %>%
filter(Year.x >= 2000, Year.x <= 2019, Year.y >= 2000,
       Year.y <= 2019)
View(df_joined_1_sub)

#This is still not optimal as further transformations
#are needed such as removing repeating columns

#Alternatively, the data frames can be joined by the
#'Year' variable
df_joined = merge(x = df7, y = df_5, by = "Year")
View(df_joined)

#But again this is not optimal as it joins all data
#per year and is harder to interpret and will need 
#extra transformations to arrange the years, and 
#delete repeating columns for country and region.

#so to avoid data sub-setting on the years, it's
#best to use inner joins on the data frames upfront

#Task 3.3
#Calculate per capita variables where appropriate.

#1) Calculate GDP per capita

merged_df1 <- merged_df %>%
mutate('GDP per Capita' = merged_df$`GDP`
         / merged_df$`Population`)

#2) Ice cream volume per capita

merged_df2 <- merged_df1 %>%
mutate('Ice Cream Volume per Capita' =
merged_df1$`Ice Cream Volume` / merged_df1$`Population`)


#3) Ice cream value per capita

merged_df3 <- merged_df2 %>%
mutate('Ice Cream Value per Capita' =
merged_df2$`Ice Cream Value` / merged_df2$`Population`)

View(merged_df3)

#Task 3.4
#Visualize the relationship between Ice Cream 
#Volume per Capita and GDP per Capita. 

#Lets take a look at the range of values
summary(merged_df3$`GDP per Capita`)
summary(merged_df3$`Ice Cream Volume per Capita`)

#The graph axis may potentially benefit from scaling

#Scatter plot for the raw data
library(ggplot2)

# Create the scatter plot with regression line
ggplot(merged_df3, aes(x = `GDP per Capita`, y = `Ice Cream Volume per Capita`)) +
  geom_point(color = "black", alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  
  #labels and titles
  labs(x = "GDP per Capita (USD)", y = "Ice Cream Volume per Capita (kg) ", 
       title = "GDP per Capita vs Ice Cream Volume per Capita") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_x_continuous(n.breaks = 10) +  
  scale_y_continuous(n.breaks = 10) 

#The graph shows a positive linear relationship -
#as the GDP per capita increases, so does the volume 
#of ice cream per capita. There is a range of GDP per 
#capita values: 4000-6000 USD which constitutes to the
#highest proportion of values for ice cream volume per capita. 
#In this range, the largest values for volume of ice
#cream per capita was observed. 

#Check to see if scaling the data produces
#a better output graph

# Scatter plot using the log scale for the data
#log transform data
library(dplyr)
merged_df3_log <- merged_df3 %>%
mutate(
    log_GDP_per_Capita = log10(`GDP per Capita`),
    log_Ice_Cream_Volume_per_Capita = log10(`Ice Cream Volume per Capita`))

#scatter plot
ggplot(merged_df3_log, aes(x = `GDP per Capita`, y = `Ice Cream Volume per Capita`)) +
  geom_point(color = "black", alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  # Labels and titles
  labs(x = "GDP per Capita (USD)", y = "Ice Cream Volume per Capita (kg)", 
       title = "GDP per Capita vs Ice Cream Volume per Capita") +
  theme(plot.title = element_text(hjust = 0.5)) +
  
  # Log scale for x and y axes
  scale_x_log10(n.breaks = 10) +  
  scale_y_log10(n.breaks = 10)

#log transform doesn't seem suitable for this data
#set as the Ice cream volume scale per capita is quite
#small.The same positive linear relationship is given but
#it's less easier to interpret

#Check if min/max scaling produces a better result

#First scale the data
merged_df3_scaled <- merged_df3 %>%
  mutate(
    scaled_GDP_per_Capita = rescale(`GDP per Capita`),
    scaled_Ice_Cream_Volume_per_Capita = rescale(`Ice Cream Volume per Capita`)
  )

# Create the scatter plot with regression line using scaled data
ggplot(merged_df3_scaled, aes(x = scaled_GDP_per_Capita, y = scaled_Ice_Cream_Volume_per_Capita)) +
  geom_point(color = "black", alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  
  # Labels and titles
  labs(x = "Scaled GDP per Capita", y = "Scaled Ice Cream Volume per Capita", 
       title = "Scaled GDP per Capita vs Scaled Ice Cream Volume per Capita") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(n.breaks = 10) +  
  scale_y_continuous(n.breaks = 10)

#The output is similar to the raw data graph,
#a positive linear relationship is observed - the
#raw data graph suffices for the visualization of the
#relationship and is better for identifying notable 
#ranges of values for each variable

#save the graph as 'my_plot' to the output folder

#First name the graph as p1 
p1<-# Create the scatter plot with regression line
  ggplot(merged_df3, aes(x = `GDP per Capita`, y = `Ice Cream Volume per Capita`)) +
  geom_point(color = "black", alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  
  #labels and titles
  labs(x = "GDP per Capita (USD)", y = "Ice Cream Volume per Capita (kg) ", 
       title = "GDP per Capita vs Ice Cream Volume per Capita") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_x_continuous(n.breaks = 10) +  
  scale_y_continuous(n.breaks = 10) 

ggsave(filename ="C:/Users/shani/Documents/R skills Shanice Kilaire/Output/my_plot.png", plot = p1, width = 8, height = 6)


#Task 3.5
#Write a function, which calculates percent
#of missing data for each Country and each Indicator.

#First check the number of missing values in the
#data and the number of distinct countries

#Calculate the total number of missing values
total_missing_values <- sum(is.na(merged_df3))

# Print the total number of missing values
print(paste("The total number of missing values is:",total_missing_values))

#Calculate the total number of countries
total_countries <- merged_df3 %>% summarise(total_countries = n_distinct(Country))

# Print the total number of unique countries
print(paste("The total number of countries is:",total_countries$total_countries))

#Check the indicators that have these missing values
colSums(!is.na(merged_df3))

#There are 80 countries, 177 total missing values
#and the indicators for the missing values are:'Ice
#Cream Value per Capita', 'Ice Cream Price' and 'Ice 
#cream value'. We can use this to check that the 
#function outputs the correct number of missing values
#for each country. 

#Function which calculates percent of missing data for 
#each Country and each Indicator. 

# Define the function to calculate the percentage
#of missing data for each country and each indicator

calculate_missing_percent <- function(data) {
data %>%
gather(key = "Indicator", value = "Value",
-Country, -Region, -Year) %>%#Excluding country,
#region and year #gather the remaining variables in 
#single indicator column
    
group_by(Country, Indicator) %>% # group the rows
#by the country and indicator variables
    
summarise( # Formula for calculating missing data as
#a percentage
total_count = n(),
missing_count = sum(is.na(Value)),
missing_percent = (missing_count / total_count) * 100
) %>%
arrange(desc(missing_percent)) # put in descending order
}

#apply to data 
result <- calculate_missing_percent(merged_df3)

# Print the result
print(result)
View(result)

#Check the resulting table has the correct number
#of missing values for all 80 countries

# Check that total number of missing values has been correctly identified for 80 countries
total_sum <- sum(result$missing_count, na.rm = TRUE)

# Print the total sum of the missing values
print(paste("The total number of missing values is:", total_sum))

#count total number of distinct countries
distinct_countries <- unique(result$Country)

# Print the number of distinct countries
print(paste("The total number of distinct countries is:", length(distinct_countries)))

#The results table values for the missing data are
#correct. 

#saving the table as a csv to output folder
write.csv(result, "C:/Users/shani/Documents/R skills Shanice Kilaire/Output/percent_missing_data_table.csv", row.names = FALSE)
