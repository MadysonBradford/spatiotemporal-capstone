library(ggplot2)

#Reading in the CSVs and removing the Null Row
precipCSV_m <- read.csv("./files/Lane_Annual_Precip_Meters.csv")
precipCSV_m <- precipCSV_m[-c(14),]   # remove row 14


# All precipitation means and things in meters
precipMean_m = mean(precipCSV_m$Precipitation_m)
precipStdev_m = sd(precipCSV_m$Precipitation_m)
precipUpperBound_m = precipMean_m+2*precipStdev_m
precipLowerBound_m = precipMean_m-2*precipStdev_m

# Initialize For Records at end (outside of iterations)
year <- c()
water_table_height_yearly <- c()
population_yearly <- c()
shelter_yearly <- c()
precipitation_yearly <- c()
water_index_yearly <- c()
pop_index_yearly <- c()
sustainability_yearly <- c()

# Creating Empty Dataframe for Results
Results <- data.frame(matrix(ncol=8,nrow=50))


a#########################################
### Begin Characterization of Inputs ####
#########################################

time = 1 #Will change for each iteration, initialized outside of loop

while (time <= 50) {
  
# Water tables and unconfined aquifer base calculations, From DEM in ArcGIS Pro
z_min = -17.9302 #m
z_max = 2792.48 #m
# calculation for starting water table altitude
water_table_altitude = z_min + 5.0
# calculation for base of unconfined aquifer 
unconfined_aq_base = water_table_altitude - 1000.0


# Calculation for the Base water volume in the unconfined aquifer
study_area_m = 12229368882.799999 #m^2
study_area_km = 12229.368883 #km^2
porosity  = 0.3 # of Willamete Silt g^cm^3
#Base water volume = Lane County Area*(z_w-z_b)*porosity
base_v_water = study_area_m*(water_table_altitude-unconfined_aq_base)*0.3 #m^3


# Calculations for INPUT of water in system over a year through PRECIPITATION
#v_w_t = LaneCountyRaster*P(t)[m^3]
#P(t)= P0 + Bt
# Linear regression for Precipitation to get beta-coefficient
model <- lm(precipCSV_m$Precipitation_m ~ precipCSV_m$Year, data = precipCSV_m)
precip_beta = -0.01621 #from model
Precip_time = precipMean_m + precip_beta*time #m/yr
#total precipitation water volume over the geographic area caused by precipitation at year x
precip_v_water =  study_area_m*Precip_time #m^3


# Calculation for the height or depth of the precipitation across the entire study area
height_precip_v_water = precip_v_water/study_area_m #m
# Calculation for the porosity-corrected height or depth of the water given precipitation
height_precip_v_water_corrected = height_precip_v_water/0.3 #m


#Water table modeled as a function of time at year x with corrected precipitation added
#Z_table_t = z_w_t + h_p_t
water_table_tfunction = water_table_altitude + height_precip_v_water_corrected*time ####Weird 

#########################################
### Begin Characterization of Outputs ###
#########################################

# Forcing trajectories for Population Growth
# p(t)=p0*exp(kt)
population = 382067*exp(0.01*time)


# Calculation for Population Shelter Requirements
# R = Abr / As(t + 1) <- Residential requirement
# Np(t + 1) = N0p / R <- number of people that can be accommodated by increase in area
Residential_Requirement = 93 #m^2
People_Per_Residence = 8
Shelter_Increase = 1*time #m^2
R = Residential_Requirement/Shelter_Increase
People_Accomodated_byIncr = People_Per_Residence/R #people


# Calculation for People-water consumption requirements
# Qpw(t) = p(t)*Wp*365.0
People_water_consumption = (population*5*365)/1000 #m^3


# Characterization of Water Well Properties
# Qw(t) = sum(Qwell(t)*Wi)
Number_wells = (0.8)*(250000/36) #wells
# 1 GPM, 1440 gallons, 6.54637 m^3, 365 days a year is 2389.43 m^3 (water well stats)
W_coef = 1
water_wells_output = Number_wells*(2389.43*time*W_coef)


# Decrease Water Table as water is pumped out of well
# Hp = Qw(t) / Area
# H0p = Hp / porosity
height_well_output = water_wells_output / study_area_m
depth_well_output = height_well_output / 0.3
#Updating the water table to now account for water pumped OUT by wells (already accounts for precipitation)
water_table_tfunction = water_table_tfunction-(depth_well_output*time)


##############################
### Sustainability Indices ###
##############################

# Water Sustainability Index
#sw(t)=V(t)-V0w/V(t)+V01
#V(t) = V(t-1)+Vw(t)-Q_wt
changed_water_volume = study_area_m*(water_table_tfunction-unconfined_aq_base)*0.3
year_total_water_v = changed_water_volume+(study_area_m*(Precip_time+1))-water_wells_output
water_index = (year_total_water_v-base_v_water)/(year_total_water_v+base_v_water)

# Shelter and Population Sustainability Index
#sp(t)=As(t)-A'(t)/As(t)+A'(t)
#A'(t)=Abr/[N0p/Npt]
land_cover_shelter = (population/People_Per_Residence)*Residential_Requirement
land_cover_shelter_required = (population/People_Per_Residence)*((Residential_Requirement/(People_Per_Residence/People_Accomodated_byIncr)))
pop_index = (land_cover_shelter-land_cover_shelter_required)/(land_cover_shelter+land_cover_shelter_required)


#############################################
### OVERALL WEIGHTED SUSTAINABILITY INDEX ###
#############################################

# Overall Sustainability = (w1*s_wt+w2*s_pt)/w1+w2
w1 = 2
w2 = 1
sustainability = ((w1*water_index)+(w2*pop_index))/(w1+w2)


##########################
### Recording each Run ###
##########################

#water table height, population growth, shelter area, precipitation, volume of water

year <- append(year, time)
water_table_height_yearly <- append(water_table_height_yearly,water_table_tfunction)
population_yearly <- append(population_yearly,population)
shelter_yearly <- append(shelter_yearly,(land_cover_shelter/1000))
precipitation_yearly <- append(precipitation_yearly,Precip_time)
water_index_yearly <- append(water_index_yearly, water_index)
pop_index_yearly <- append(pop_index_yearly, pop_index)
sustainability_yearly <- append(sustainability_yearly,sustainability)


########################################
### Adding a Year for Each Iteration ###
########################################

time = time + 1

}

# Summary to Make Sure it Worked for all 50 iterations
year
water_table_height_yearly
population_yearly
shelter_yearly
precipitation_yearly

water_index_yearly
pop_index_yearly
sustainability_yearly

# Adds All Results to One Table
Results <- cbind(year,water_table_height_yearly,population_yearly,shelter_yearly,precipitation_yearly,water_index_yearly,pop_index_yearly,sustainability_yearly)
write.csv(Results,"./files/Results.csv", row.names = TRUE)

########################
### GRAPHING RESULTS ###
########################

ResultCSV <- read.csv("./files/Results.csv")

# WATER TABLE OVER 50 YEAR PERIOD
ggplot(ResultCSV, aes(x=year, y=water_table_height_yearly))+ geom_line(size = 1, color="blue")+
  theme_bw()+
  labs(title="Water Table Over 50 Year Period",subtitle ="Lane County, OR. [Management Scenario 2]", x = "Year", y = "Water Table Height [m]")+
  theme(plot.title = element_text("serif", "bold", "black", "12"),
        plot.subtitle = element_text(family="serif",size="10"),
        axis.title.x = element_text(family="serif",size="10"),
        axis.title.y = element_text(family="serif",size="10"))

# POPULATION OVER 50 YEAR PERIOD
ggplot(ResultCSV, aes(x=year, y=population_yearly))+ geom_line(size = 1, color="purple")+
  theme_bw()+
  labs(title="Population Over 50 Year Period",subtitle ="Lane County, OR. [Management Scenario 2]", x = "Year", y = "Total Population")+
  theme(plot.title = element_text("serif", "bold", "black", "12"),
        plot.subtitle = element_text(family="serif",size="10"),
        axis.title.x = element_text(family="serif",size="10"),
        axis.title.y = element_text(family="serif",size="10"))

# POPULATION SHELTER OVER 50 YEAR PERIOD
ggplot(ResultCSV, aes(x=year, y=shelter_yearly))+ geom_line(size = 1, color="orange")+
  theme_bw()+
  labs(title="Population Shelter Over 50 Year Period",subtitle ="Lane County, OR. [Management Scenario 2]", x = "Year", y = "Total Shelter Area [km^2]")+
  theme(plot.title = element_text("serif", "bold", "black", "12"),
        plot.subtitle = element_text(family="serif",size="10"),
        axis.title.x = element_text(family="serif",size="10"),
        axis.title.y = element_text(family="serif",size="10"))


# PRECIPITATION OVER 50 YEAR PERIOD
ggplot(ResultCSV, aes(x=year, y=precipitation_yearly))+ geom_line(size = 1, color="blue")+
  theme_bw()+
  labs(title="Precipitation Over 50 Year Period",subtitle ="Lane County, OR. [Management Scenario 2]", x = "Year", y = "Total Precipitation [m]")+
  theme(plot.title = element_text("serif", "bold", "black", "12"),
        plot.subtitle = element_text(family="serif",size="10"),
        axis.title.x = element_text(family="serif",size="10"),
        axis.title.y = element_text(family="serif",size="10"))


# WATER INDEX OVER 50 YEAR PERIOD
ggplot(ResultCSV, aes(x=year, y=water_index_yearly))+ geom_line(size = 1, color="blue")+
  theme_bw()+
  labs(title="Water SI Over 50 Year Period",subtitle ="Lane County, OR. [Management Scenario 2]", x = "Year", y = "Water Sustainability Index")+
  theme(plot.title = element_text("serif", "bold", "black", "12"),
        plot.subtitle = element_text(family="serif",size="10"),
        axis.title.x = element_text(family="serif",size="10"),
        axis.title.y = element_text(family="serif",size="10"))


# POPULATION/SHELTER INDEX OVER 50 YEAR PERIOD
ggplot(ResultCSV, aes(x=year, y=pop_index_yearly))+ geom_line(size = 1, color="red")+
  theme_bw()+
  labs(title="Population/Shelter SI Over 50 Year Period",subtitle ="Lane County, OR. [Management Scenario 2]", x = "Year", y = "Population-Shelter Index")+
  theme(plot.title = element_text("serif", "bold", "black", "12"),
        plot.subtitle = element_text(family="serif",size="10"),
        axis.title.x = element_text(family="serif",size="10"),
        axis.title.y = element_text(family="serif",size="10"))


# OVERALL SUSTAINABILITY INDEX OVER 50 YEAR PERIOD
ggplot(ResultCSV, aes(x=year, y=sustainability_yearly))+ geom_line(size = 1, color="green")+
  theme_bw()+
  labs(title="Overall Sustainability Index Over 50 Year Period",subtitle ="Lane County, OR. [Management Scenario 2]", x = "Year", y = "Combined Sustainability Index")+
  theme(plot.title = element_text("serif", "bold", "black", "12"),
        plot.subtitle = element_text(family="serif",size="10"),
        axis.title.x = element_text(family="serif",size="10"),
        axis.title.y = element_text(family="serif",size="10"))

# PRECIPITATION OVER PREVIOUS YEARS
envelope <- ggplot(precipCSV_m, aes(x=Year, y=Precipitation_m))+ geom_line(size = 1, color="blue")+
  theme_bw()+
  labs(title="Average Annual Precipitation Envelope",subtitle ="Lane County, OR. Station # OR-LA-112 (Eugene 2.7)", x = "Year", y = "Average Annual Precipitation [m]")+
  theme(plot.title = element_text("serif", "bold", "black", "12"),
        plot.subtitle = element_text(family="serif",size="10"),
        axis.title.x = element_text(family="serif",size="10"),
        axis.title.y = element_text(family="serif",size="10"))+
  scale_x_continuous(breaks = round(seq(min(precipCSV_m$Year), max(precipCSV_m$Year), by = 2),1))
  #scale_y_continuous(breaks = round(seq(min(precipCSV_m$Precipitation_m), max(precipCSV_m$Precipitation_m), by = 2),1))

  envelope + geom_hline(yintercept = c(precipMean_m,precipUpperBound_m,precipLowerBound_m), color=c("red","green","orange"), size=1)