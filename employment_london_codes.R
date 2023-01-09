####################### Loading Packages #####################
library("sf")
library("tmap")
library("tidyverse")
library("janitor")
library("spdep")
library("spatialreg")

####################### Data Loading #########################

# Read in shape file data - wards boundary for London
london_wards_shp <- st_read(
  "employment_london_data/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")

qtm(london_wards_shp) # Check the data


# Read in csv file - some attribute data for wards profile
london_wards_profiles <- read_csv("employment_london_data/ward-profiles-excel-version.csv", 
                               na = c("", "NA", "n/a"), 
                               locale = locale(encoding = 'Latin1'), 
                               col_names = TRUE)

Datatypelist <- london_wards_profiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist # Check all columns were read in correctly by inspecting the variable type

####################### Data Wrangling ###################

# 1. Selecting the columns to be used in this study
employment_data<- london_wards_profiles %>%
  clean_names() %>%
  dplyr::select(ward_name, 
                new_code,
                percent_bame_2011, # Independent Variable X1
                percent_with_level_4_qualifications_and_above_2011, # Independent Variable X2
                employment_rate_16_64_2011) # Dependent Variable Y

# 2. Check if there are missing values in the selected attribute data
sum(is.na(employment_data)) # No missing values were found in this case

# 3. Merge employment_data to london_wards_shp uniquely by using the common column
spatial_employment_data <- merge(london_wards_shp, 
                                 employment_data, 
                                 by.x = "GSS_CODE", 
                                 by.y = "new_code")

##################### Exploratory Data Analysis ############

# 1. Histograms of variables 
# Frequency Distribution of Dependent Variable Y
ggplot(spatial_employment_data, 
       aes(x=`employment_rate_16_64_2011`)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 1) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1) +
  ggtitle("Frequency Distribution of Dependent Variable Y: \n Employment rate (16-64) - 2011") +
  theme(plot.title = element_text(hjust = 0.5)) + # Center the Title
  xlab("Employment rate (16-64) - 2011")

# Frequency Distribution of Independent Variable X1
ggplot(spatial_employment_data, 
       aes(x=`percent_bame_2011`)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1) +
  ggtitle("Frequency Distribution of Independent Variable X1: \n % BAME - 2011") +
  theme(plot.title = element_text(hjust = 0.5)) + # Center the Title
  xlab("% BAME - 2011")

# Frequency Distribution of Independent Variable X2
ggplot(spatial_employment_data, 
       aes(x=`percent_with_level_4_qualifications_and_above_2011`)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1) +
  ggtitle("Frequency Distribution of Independent Variable X2: \n % with Level 4 qualifications and above - 2011") +
  theme(plot.title = element_text(hjust = 0.5)) + # Center the Title
  xlab("% with Level 4 qualifications and above - 2011")

# 2. Statistical Description
# Mean
round(mean(spatial_employment_data$employment_rate_16_64_2011),2) # Dependent Variable Y
round(mean(spatial_employment_data$percent_bame_2011),2) # Independent Variable X1
round(mean(spatial_employment_data$percent_with_level_4_qualifications_and_above_2011),2) # Independent Variable X2

# Standard Deviation
round(sd(spatial_employment_data$employment_rate_16_64_2011),2) # Dependent Variable Y
round(sd(spatial_employment_data$percent_bame_2011),2) # Independent Variable X1
round(sd(spatial_employment_data$percent_with_level_4_qualifications_and_above_2011),2) # Independent Variable X2

# 3. Spatial Distribution of Variables

# Create an image object for Y
plot_Y <- tm_shape(spatial_employment_data) + 
  tm_fill("employment_rate_16_64_2011", title = "Employment rate (16-64) - 2011", 
          style = "quantile", n = 5, palette = "Purples") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 0.8)

# Plot the image object
plot_Y

# Create an image object for X1
plot_X1 <- tm_shape(spatial_employment_data) + 
  tm_fill("percent_bame_2011", title = "% BAME - 2011", 
          style = "quantile", n = 5, palette = "Oranges") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 0.8)

# Plot the image object
plot_X1

# Create an image object for X2
plot_X2 <- tm_shape(spatial_employment_data) + 
  tm_fill("percent_with_level_4_qualifications_and_above_2011", 
          title = "% with Level 4 qualifications and above - 2011", 
          style = "quantile", n = 5, palette = "Blues") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 0.8)

# Plot the image object
plot_X2

#################### Non-spatial Multivariable Linear Regression ############

# 1. Fitting a non-spatial multivariable Linear Regression on the selected variables
model_MLR <- lm(employment_rate_16_64_2011 ~ percent_bame_2011 + percent_with_level_4_qualifications_and_above_2011, 
                data = spatial_employment_data)

options(scipen = 7) # remove those annoying scientific notations
summary(model_MLR) # report the output of model_MLR

# 2. Extract the residuals and make the new column
spatial_employment_data$RESIDUALS <- model_MLR$residuals
summary(spatial_employment_data$RESIDUALS) # Reporting the basic summary of residuals

# 3. Create a map to examine if these residuals show patterns of spatial autocorrelation
tm_shape(spatial_employment_data) + 
  tm_fill("RESIDUALS", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 0.8)

# 4. Use the Moran’s I test to check the presence of spatial autocorrelation
# generate unique number for each row
spatial_employment_data$ROWNUM <- 1:nrow(spatial_employment_data)
# transform the sf object into a new sp object
spatial_employment_data_sp <- as(spatial_employment_data, "Spatial")

# Create spatial weights matrix for areas
Weights <- poly2nb(spatial_employment_data_sp, 
                   row.names = spatial_employment_data_sp$ROWNUM)
WeightsMatrix <- nb2mat(Weights, style='B')
Residual_WeightMatrix <- mat2listw(WeightsMatrix , style='W')

# Run the test on the regression model output object
lm.morantest(model_MLR, Residual_WeightMatrix, alternative="two.sided")

################ Spatial Regression #####################

# 1. Spatial Lag Model lagged on the dependent variable

# Fit model
model_SLY <- lagsarlm(employment_rate_16_64_2011 ~ percent_bame_2011 + percent_with_level_4_qualifications_and_above_2011, 
                      data = spatial_employment_data_sp, 
                      Residual_WeightMatrix)
# Report results
summary(model_SLY)

# Extract the residuals and dump back to original sf object
spatial_employment_data$RESID_SLY <- model_SLY$residuals

# Moran's I test
moran.mc(spatial_employment_data$RESID_SLY, 
         Residual_WeightMatrix, 1000, zero.policy = T)

# Generate the map
tm_shape(spatial_employment_data) + 
  tm_fill("RESID_SLY", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 0.8)

# Interpretation of results using impacts
Weights_2.0 <- as(Residual_WeightMatrix, "CsparseMatrix")
trMC <- trW(Weights_2.0, type="MC")
summary(impacts(model_SLY, tr = trMC, R=100), zstats=TRUE)


# 2. Spatial Error Model
# Fit model
model_SER <- errorsarlm(employment_rate_16_64_2011 ~ percent_bame_2011 + percent_with_level_4_qualifications_and_above_2011, 
                        data = spatial_employment_data_sp,
                        Residual_WeightMatrix)

# Report results
summary(model_SER)

# Extract the residuals and dump back to original sf object
spatial_employment_data$RESID_SER <- model_SER$residuals

# Moran's I test
moran.mc(spatial_employment_data$RESID_SER, 
         Residual_WeightMatrix, 1000, zero.policy = T)

# Generate the map
tm_shape(spatial_employment_data) + 
  tm_fill("RESID_SER", style = "cont", midpoint = 0, palette = "-RdBu") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 0.8)

########### Model Comparison #############

AIC(model_MLR) # non-spatial multivariable Linear Regression model
AIC(model_SLY) # Spatial Lag Model lagged on the dependent variable
AIC(model_SER) # Spatial Error Model




