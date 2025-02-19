#-------------------------------------------------------------------------------
# clear environment
rm(list = ls())

# install and load packages 
install.packages("tidyverse")  # Install the tidyverse package
install.packages("tidycensus")  # Install the tidycensus package
install.packages("scales") # Install the scales package for plot labels

library(tidyverse)  # Load the tidyverse package
library(tidycensus)  # Load the tidycensus package
library(scales) # Load the scales package
library(ggplot2) # Load the ggpolt2 package
census_api_key("63217a192c5803bfc72aab537fe4bf19f6058326", overwrite= TRUE, install=TRUE)
# ==============================================================================
# Part 1: Look at CDC Data and merge with ACS
# ==============================================================================

# load CDC Data
# I downloaded the following dataset from: https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/swc5-untb/about_data 
# This dataset contains county-level estimates for 40 healthcare related measures 
# Data sources used to generate these model-based estimates are Behavioral Risk Factor Surveillance System (BRFSS) 2022 or 2021 data

CA_CDC_Data <- read_csv("~/Desktop/Desktop - Jerra’s MacBook Pro/MaCSS/CompSS224a/data/CDC_CA_Data.csv")
view(CA_CDC_Data) # look at data 

# I specifically want to look at SF County and LA County
CDC_filtered <- CA_CDC_Data %>% 
  filter(LocationName == "San Francisco" | LocationName == "Los Angeles", # filter for SF County only
         Data_Value_Type == "Age-adjusted prevalence") %>% # filter for age-adjusted prevalence instead of crude comparison for fairer comparison
  select(LocationName, Category, Measure, Data_Value, Low_Confidence_Limit, High_Confidence_Limit, LocationID) # only keep relevant columns

view(CDC_filtered) # look at data 

# Look at unique categories to identify which variables I may want to drive into
CDC_filtered %>% distinct(Category)

# Now I need to get 2022 data on SF and LA Counties from the ACS 5-year survey
# load acs5 table to look for specific variables
vars_2022 <- load_variables(2022, dataset="acs5")
view(vars_2022)

# get acs data
acs_5 <- get_acs(
  geography = "county",
  variables = c(median_home_value = "B25077_001", 
                median_rent_percent = "B25071_001"
               ),
  state = "CA",
  county = c("San Francisco", "Los Angeles"),
  year = 2022,
  survey = "acs5")

# load race vars 
race_vars <- c(total_pop = "B02001_001",
               white_pop = "B02001_002",
               black_pop = "B02001_003",
               am_indian_pop = "B02001_004",
               asian_pop = "B02001_005",
               hawaiian_pop = "B02001_006",
               other_pop = "B02001_007")
# load race data 
race_data <- get_acs(
  geography = "county",
  variables = race_vars,
  state = "CA",
  county = c("San Francisco", "Los Angeles"),
  year = 2022,
  survey ="acs5") 


# add columns with low and high confidence levels to match CDC data before merging
acs_5_new_columns <- acs_5 %>% mutate(low_confidence_limit = estimate - moe, # calculate low limit of moe 
                 high_confidence_limit = estimate + moe) %>% # calculate high limit of moe
  separate(NAME, into = c("county","state"), sep="County,") %>% # separate column into county and state 
  select(GEOID, county, variable, estimate, low_confidence_limit, high_confidence_limit) # drop moe and state columns 
view(acs_5_new_columns) # look at data 

# calculate race proportions in each county
race_props <- race_data %>%
  group_by(NAME) %>% # group by county
  select(-moe) %>% # drop margin of error
  pivot_wider(names_from = variable, # pivot wider
              values_from = estimate) %>%
  mutate(
    "White" = white_pop / total_pop,
    "Black" = black_pop / total_pop,
    "American Indian" = am_indian_pop / total_pop,
    "Asian" = asian_pop / total_pop,
    "Native Hawaiian" = hawaiian_pop / total_pop,
    "Other" = other_pop / total_pop
  )

view(race_props)

# plot race proportion by county for demographic information reference
long_race_df <- race_props %>%
  select(NAME, # select relevant columns
         "White",
         "Black",
         "American Indian",
         "Asian",
         "Native Hawaiian",
         "Other") %>%
  mutate(county = str_remove(NAME, " County.*")) %>% # rename counties
  pivot_longer(cols=!c(county, NAME), names_to="race", values_to ="percent") # pivot longer to plot grouped bar chart

view(long_race_df) # look at data

# plot grouped bar chart of racial groups by county
long_race_df %>%
  ggplot(aes(x=percent, y=fct_reorder(race, percent), fill=county)) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) + # set labels to percents
  scale_fill_manual(values = c("Los Angeles" = "darkgreen", "San Francisco" = "royalblue")) + # change default colors
  labs(
    title = "Race Distribution by County", # main insight
    subtitle = "Racial group as a percent of total county population", # info about the graph
    caption = "Source: ACS 5-year Data Profile" # data source 
  )+
  theme_minimal(base_size=12)+ # set theme and font size
  theme(
    legend.position ="top",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"),
    legend.justification = "left", 
    legend.direction = "horizontal",
    legend.title = element_blank(), # remove legend title
    panel.grid.minor.x = element_blank(), # remove minor grid lines along x-axis
    panel.grid.minor.y = element_blank(), # remove minor grid lines along y-axis
    plot.title = element_text(size=16, face="bold"), # sets the title to font size 16 and bolds it
    plot.caption = element_text(size=10, colour="grey50", hjust = 0, lineheight = 1.2, margin=margin(t = 14, r = 0, b = 0, l = 0, unit = "pt")),
    # style the plot caption; set font to 10, color to gray, and adjust position with left-align (hjust) and line spacing (linehieght)
    # margin() add 14 points of top margin for spacing above the caption
    plot.margin = margin(t=1, r=3, b=1, l=1, unit="cm"), # margin adjusts the margin around the entire plot
    axis.title.x = element_blank(), # remove x axis title
    axis.title.y = element_blank() # remove y axis title 
  )+
  coord_cartesian(clip="off") # make sure no plot elements are cut off

# Save plot to plots file
ggsave(filename = "race_by_county_plot.png",
       path = ("/Users/jerramclaughlin/Desktop/Desktop - Jerra’s MacBook Pro/MaCSS/CompSS224a/plots"),
       width = 10,
       height = 6)    

# merge datasets on geoid 
merged_df <- CDC_filtered %>% 
  full_join(acs_5_new_columns, by = c("LocationID" = "GEOID", 
                                      "Measure" = "variable", 
                                      "Data_Value" = "estimate", 
                                      "LocationName" = "county", 
                                      "Low_Confidence_Limit" = "low_confidence_limit", 
                                      "High_Confidence_Limit" = "high_confidence_limit")) # full_join keeps all observations in both data sets
view(merged_df) # look at merged data 

new_merged <- merged_df %>%
  mutate(across(everything(), ~ replace_na(.x, "Housing"))) # replace null values in the category column

view(new_merged) # look at changes

# plot dot plot of health outcomes by county
merged_df %>% filter(Category == "Health Outcomes") %>% # filter for health outcomes
  mutate(Measure = str_remove(Measure, "among adults.*")) %>% # make x axis labels shorter 
  ggplot(aes(x=Data_Value, y=fct_reorder(Measure, Data_Value), color=LocationName, fill=LocationName)) + # set axis values and order by prevalence
  geom_point(size =3) + # set size of points 
  scale_x_continuous(labels = scales::label_percent(scale = 1)) + # set labels to percents
  scale_color_manual(values = c("Los Angeles" = "darkgreen", "San Francisco" = "royalblue")) + # change default colors
  labs(
    title = "LA county out-ranks SF for most poor health indicators", # main insight
    subtitle = "Percent of adults with poor health outcomes", # info about the graph
    caption = "Source: Centers for Disease Control and Prevention" # data source 
  )+
  theme_minimal(base_size=12)+ # set theme and font size
  theme(
    legend.position ="top",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"),
    legend.justification = "left", 
    legend.direction = "horizontal",
    legend.title = element_blank(), # remove legend title
    panel.grid.minor.x = element_blank(), # remove minor grid lines along x-axis
    panel.grid.minor.y = element_blank(), # remove minor grid lines along y-axis
    plot.title = element_text(size=16, face="bold"), # sets the title to font size 16 and bolds it
    plot.caption = element_text(size=10, colour="grey50", hjust = 0, lineheight = 1.2, margin=margin(t = 14, r = 0, b = 0, l = 0, unit = "pt")),
    # style the plot caption; set font to 10, color to gray, and adjust position with left-align (hjust) and line spacing (linehieght)
    # margin() add 14 points of top margin for spacing above the caption
    plot.margin = margin(t=1, r=3, b=1, l=1, unit="cm"), # margin adjusts the margin around the entire plot; 
    # t = 1: 1 cm top margin
    # r = 3: 3 cm right margin
    # b = 1: 1 cm bottom margin
    # l = 1: 1 cm left margin
    axis.title.x = element_blank(), # remove x axis title
    axis.title.y = element_blank() # remove y axis title 
  )+
  coord_cartesian(clip="off") # make sure no plot elements are cut off

# Save plot to plots file
ggsave(filename = "health_outcomes_plot.png",
       path = ("/Users/jerramclaughlin/Desktop/Desktop - Jerra’s MacBook Pro/MaCSS/CompSS224a/plots"),
       width = 10,
       height = 6)  


# plot median rent as a proportion of income  
rent_prop <- new_merged %>% filter(Measure == "median_rent_percent") %>%
  pivot_wider(names_from = Measure, # pivot wider 
              values_from = Data_Value) 

ggplot(rent_prop, aes(x=LocationName, y=median_rent_percent)) +
  geom_col(aes(fill=LocationName)) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) + # set y axis to have percentage signs
 # scale_color_manual(values = c("Los Angeles " = "darkgreen", "San Francisco " = "royalblue")) + # change color of bar outlines
  scale_fill_manual(values = c("Los Angeles " = "darkgreen", "San Francisco " = "royalblue")) +
  labs(
    title = "LA renters are more burdened than SF renters",
    subtitle = "Median Gross Rent as a Percentage of Household Income by county",
    caption = "Source: ACS 5-year Data Profile variable B25071_001 via the tidycensus R package" 
  )+ 
  theme_minimal(base_size=12)+ # set theme and font size
  theme(
    legend.position = "none", # remove legend 
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size=16, face="bold"), # sets the title to font size 16 and bolds it
    plot.caption = element_text(size=10, colour="grey50", hjust = 0, lineheight = 1.2, margin=margin(t = 14, r = 0, b = 0, l = 0, unit = "pt")),
    # style the plot caption; set font to 10, color to gray, and adjust position with left-align (hjust) and line spacing (linehieght)
    # margin() add 14 points of top margin for spacing above the caption
    plot.margin = margin(t=1, r=3, b=1, l=1, unit="cm"), # margin adjusts the margin around the entire plot; 
    axis.title.x = element_blank(), # remove x axis title
    axis.title.y = element_blank() # remove y axis title 
  )+
  coord_cartesian(clip="off") # make sure no plot elements are cut off
    
# Save plot to plots file
ggsave(filename = "LA_SF_rent_plot.png",
       path = ("/Users/jerramclaughlin/Desktop/Desktop - Jerra’s MacBook Pro/MaCSS/CompSS224a/plots"),
       width = 10,
       height = 6)  

# ===============================================================================================
# Part 2: compare groups of renters, homeowners, racial groups, seniors and children in LA vs. SF
# ===============================================================================================

# -----------------------------------------------------------------------------------------------
# Proportion of rent-burdened renters in LA compared to SF
# ----------------------------------------------------------------------------------------------- 

# variables:
# B25070_001 gives total number of households in the subgroups
# B25070_002:B25070_010 gives total household income that goes toward paying rent by percentage group
# note: these variables give number of households that belong to each subgroup

# to calculate the proportion of rent burdened individuals, I will find the number of renters 
# who pay more than 30% of their income to rent 

# specify variables
rent_groups <- c(
  "B25070_001", # Estimate!!Total:
  "B25070_002", # Estimate!!Total:!!Less than 10.0 percent
  "B25070_003", # Estimate!!Total:!!10 to 14.9 percent
  "B25070_004", # Estimate!!Total:!!15 to 19.9 percent
  "B25070_005", # Estimate!!Total:!!20 to 24.9 percent
  "B25070_006", # Estimate!!Total:!!25 to 29.9 percent
  "B25070_007", # Estimate!!Total:!!30 to 34.9 percent
  "B25070_008", # Estimate!!Total:!!35 to 39.9 percent
  "B25070_009", # Estimate!!Total:!!40 to 49.9 percent
  "B25070_010" # Estimate!!Total:!!50 percent or more
)

# load data
acs_rb <- get_acs(
  geography = "county",
  variables = rent_groups,
  state = "CA",
  county = c("San Francisco", "Los Angeles"),
  year = 2022,
  survey = "acs5")

view(acs_rb) # look at data

# get the number of rent-burdened people
rb_count <- acs_rb %>% 
  group_by(NAME) %>% # group by NAME
  select(-moe) %>% # drop margin of error column for readability
  pivot_wider( # pivot wider 
    names_from = variable,
    values_from = estimate
  ) %>%
  reframe(
    rb_count = sum( # sum number of households in each subgroups from 30% to 50% 
      B25070_007, # Estimate!!Total:!!30 to 34.9 percent
      B25070_008, # Estimate!!Total:!!35 to 39.9 percent
      B25070_009, # Estimate!!Total:!!40 to 49.9 percent
      B25070_010 # Estimate!!Total:!!50 percent or more
    ),
    total_count =  B25070_001, # Estimate!!Total:
  ) %>%
  mutate(
    p_rb = rb_count / total_count # calculate percent
  )

# this outputs a data frame called rb_count with the following columns:
# NAME: county
# rb_count: number of rent burdened housholds
# total_count: total number of households 
# p_rb: percent of rent burdened households
view(rb_count) # look at data 
# Finding: 54% of renters in LA are rent burdened, compared to only 35% of renters in SF

# -----------------------------------------------------------------------------------------------
# Proportion of homeowners with a mortgage rate and housing expenses over $3,000
# -----------------------------------------------------------------------------------------------

mortgage_vars <- c(
  "B25087_001", # Estimate!!Total: 
  "B25087_002", # Estimate!!Total:Housing units with a mortgage
  "B25087_003", # Estimate!!Total:Housing units with a mortgage:!!Less than $200
  "B25087_004", # Estimate!!Total:Housing units with a mortgage:!!$200 to $299
  "B25087_005", # Estimate!!Total:Housing units with a mortgage:!!$300 to $399
  "B25087_006", # Estimate!!Total:Housing units with a mortgage:!!$400 to $499
  "B25087_007", # Estimate!!Total:Housing units with a mortgage:!!$500 to $599
  "B25087_008", # Estimate!!Total:Housing units with a mortgage:!!$600 to $699
  "B25087_009", # Estimate!!Total:Housing units with a mortgage:!!$700 to $799
  "B25087_010", # Estimate!!Total:Housing units with a mortgage:!!$800 to $999
  "B25087_011", # Estimate!!Total:Housing units with a mortgage:!!$900 to $999
  "B25087_012", # Estimate!!Total:Housing units with a mortgage:!!$1000 to $1249
  "B25087_013", # Estimate!!Total:Housing units with a mortgage:!!$1250 to $1499
  "B25087_014", # Estimate!!Total:Housing units with a mortgage:!!$1500 to $1999
  "B25087_015", # Estimate!!Total:Housing units with a mortgage:!!$2000 to $2499
  "B25087_016", # Estimate!!Total:Housing units with a mortgage:!!$2500 to $2999
  "B25087_017", # Estimate!!Total:Housing units with a mortgage:!!$3000 to $3499
  "B25087_018", # Estimate!!Total:Housing units with a mortgage:!!$3500 to $3999
  "B25087_019" # Estimate!!Total:Housing units with a mortgage:!!$4000 or more
)

# load data
acs_mortgage <- get_acs(
  geography = "county",
  variables = mortgage_vars,
  state = "CA",
  county = c("San Francisco", "Los Angeles"),
  year = 2022,
  survey = "acs5")

# get the number of housing units with costs over $3,000/month
mortgage_count <- acs_mortgage %>% 
  group_by(NAME) %>% # group by NAME
  select(-moe) %>% # drop margin of error column for readability
  pivot_wider( # pivot wider 
    names_from = variable,
    values_from = estimate
  ) %>%
  reframe(
    high_mortgage_count = sum( # sum number of households in each subgroup of owners with housing costs over $3000/month
      B25087_017, # Estimate!!Total:Housing units with a mortgage:!!$3000 to $3499
      B25087_018, # Estimate!!Total:Housing units with a mortgage:!!$3500 to $3999
      B25087_019  # Estimate!!Total:Housing units with a mortgage:!!$4000 or more
    ),
    total_units_with_mort =  B25087_002, # Estimate!!Total:Housing units with a mortgage
    total_units = B25087_001 # total housing units (all homeowners with and without a mortgage)
  ) %>%
  mutate(
    p_mortgage_high = high_mortgage_count / total_units_with_mort, # calculate percent
    p_mortgage_total = total_units_with_mort / total_units # total housing units with a mortgage / total housing units 
    
  )
view(mortgage_count) # look at data 
# Findings: LA has a higher proportion of homeowners with a mortgage at 69% compared to SF's 64%
# However, 75% of SF homeowners have monthly housing costs over $3000 compared to LA's 48%

# -----------------------------------------------------------------------------------------------
# Proportion of SNAP benefit recipients by racial group
# -----------------------------------------------------------------------------------------------

snap_vars <- c(
  "B22005B_001", # Estimate!!Total: Receipt of Food Stamps/SNAP in the Past 12 Months by Race of Householder  (Black or African American Alone)
  "B22005B_002", # Estimate!!Total:!!Household received Food Stamps/SNAP in the past 12 months (Black or African American Alone)
  "B22005B_003", # Estimate!!Total:!!Household did not receive Food Stamps/SNAP in the past 12 months (Black or African American Alone)
  "B22005C_001", # Estimate!!Total: Receipt of Food Stamps/SNAP in the Past 12 Months by Race of Householder  (American Indian and Alaska Native Alone)
  "B22005C_002", # Estimate!!Total:!!Household received Food Stamps/SNAP in the past 12 months (American Indian and Alaska Native Alone)
  "B22005C_003", # Estimate!!Total:!!Household did not receive Food Stamps/SNAP in the past 12 months (American Indian and Alaska Native Alone)
  "B22005D_001", # Estimate!!Total: Receipt of Food Stamps/SNAP in the Past 12 Months by Race of Householder (Asian Alone)
  "B22005D_002", # Estimate!!Total:!!Household received Food Stamps/SNAP in the past 12 months (Asian Alone)
  "B22005D_003", # Estimate!!Total:!!Household did not receive Food Stamps/SNAP in the past 12 months (Asian Alone)
  "B22005E_001", # Estimate!!Total: Receipt of Food Stamps/SNAP in the Past 12 Months by Race of Householder (Native Hawaiian and Other Pacific Islander Alone)
  "B22005E_002", # Estimate!!Total:!!Household received Food Stamps/SNAP in the past 12 months (Native Hawaiian and Other Pacific Islander Alone)
  "B22005E_003", # Estimate!!Total:!!Household did not receive Food Stamps/SNAP in the past 12 months (Native Hawaiian and Other Pacific Islander Alone)
  "B22005F_001", # Estimate!!Total: Receipt of Food Stamps/SNAP in the Past 12 Months by Race of Householder (Some Other Race Alone)
  "B22005F_002", # Estimate!!Total:!!Household received Food Stamps/SNAP in the past 12 months (Some Other Race Alone)
  "B22005F_003", # Estimate!!Total:!!Household did not receive Food Stamps/SNAP in the past 12 months (Some Other Race Alone)
  "B22005H_001", # Estimate!!Total: Receipt of Food Stamps/SNAP in the Past 12 Months by Race of Householder (White Alone, Not Hispanic or Latino)
  "B22005H_002", # Estimate!!Total:!!Household received Food Stamps/SNAP in the past 12 months (White Alone, Not Hispanic or Latino)
  "B22005H_003", # Estimate!!Total:!!Household did not receive Food Stamps/SNAP in the past 12 months (White Alone, Not Hispanic or Latino)
  "B22005I_001", # Estimate!!Total: Receipt of Food Stamps/SNAP in the Past 12 Months by Race of Householder (Hispanic or Latino)
  "B22005I_002", # Estimate!!Total:!!Household received Food Stamps/SNAP in the past 12 months (Hispanic or Latino)
  "B22005I_003" # Estimate!!Total:!!Household did not receive Food Stamps/SNAP in the past 12 months (Hispanic or Latino)
)
# load data
acs_snap_by_race <- get_acs(
  geography = "county",
  variables = snap_vars,
  state = "CA",
  county = c("San Francisco", "Los Angeles"),
  year = 2022,
  survey = "acs5")

view(acs_snap_by_race) # look at data 

# get snap recipient by race 
snap_by_race_count <- acs_snap_by_race %>% 
  group_by(NAME) %>% # group by NAME
  select(-moe) %>% # drop margin of error column for readability
  pivot_wider( # pivot wider 
    names_from = variable,
    values_from = estimate
  ) %>%
  reframe( 
    total_snap_count = sum( # sum number of people across all racial groups 
      B22005B_002, # Black or African American Alone SNAP recipients
      B22005C_002, # American Indian and Alaska Native Alone SNAP recipients 
      B22005D_002, # Asian Alone SNAP recipients
      B22005E_002, # Native Hawaiian and Other Pacific Islander Alone SNAP recipient
      B22005F_002, # Some Other Race Alone SNAP recipient
      B22005H_002, # White Alone, Not Hispanic or Latino SNAP recipient
      B22005I_002 # Hispanic or Latino SNAP recipient 
    ),
    snap_black = B22005B_002,
    snap_am_indian = B22005C_002,
    snap_asian = B22005D_002,
    snap_hawaiian = B22005E_002,
    snap_other = B22005F_002,
    snap_white = B22005H_002,
    snap_hispanic = B22005I_002
  ) %>%
  mutate( # calculate proportion of snap recipiants by race 
    "Black" = snap_black / total_snap_count,
    "American Indian" = snap_am_indian / total_snap_count,
    "Asian" = snap_asian / total_snap_count,
    "Native Hawaiian" = snap_hawaiian / total_snap_count,
    "Other" = snap_other / total_snap_count,
    "White" = snap_white / total_snap_count,
    "Hispanic" = snap_hispanic / total_snap_count
  )
view(snap_by_race_count)

# need to plot findings
# plot race proportion by county for demographic information reference
long_snap_df <- snap_by_race_count %>%
  select(NAME, # select relevant columns
         "Black",
         "American Indian",
         "Asian",
         "Native Hawaiian",
         "Other",
         "White",
         "Hispanic") %>%
  mutate(county = str_remove(NAME, " County.*")) %>% # rename counties
  pivot_longer(cols=!c(county, NAME), names_to="race", values_to ="percent") # pivot longer to plot grouped bar chart

view(long_snap_df) # look at data

# plot grouped bar chart of racial groups by county
long_snap_df %>%
  ggplot(aes(x=percent, y=fct_reorder(race, percent), fill=county)) +
  geom_col(position = "dodge", width = 0.85) +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) + # set labels to percents
  scale_fill_manual(values = c("Los Angeles" = "darkgreen", "San Francisco" = "royalblue")) + # change default colors
  labs(
    title = "Distribution of SNAP Recipients by Race and County", # main insight
    subtitle = "Racial group as a percent of total SNAP Recipients in County", # info about the graph
    caption = "Source: ACS 5-year Data Profile" # data source 
  )+
  theme_minimal(base_size=12)+ # set theme and font size
  theme(
    legend.position ="top",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"),
    legend.justification = "left", 
    legend.direction = "horizontal",
    legend.title = element_blank(), # remove legend title
    panel.grid.minor.x = element_blank(), # remove minor grid lines along x-axis
    panel.grid.minor.y = element_blank(), # remove minor grid lines along y-axis
    plot.title = element_text(size=16, face="bold"), # sets the title to font size 16 and bolds it
    plot.caption = element_text(size=10, colour="grey50", hjust = 0, lineheight = 1.2, margin=margin(t = 14, r = 0, b = 0, l = 0, unit = "pt")),
    # style the plot caption; set font to 10, color to gray, and adjust position with left-align (hjust) and line spacing (linehieght)
    # margin() add 14 points of top margin for spacing above the caption
    plot.margin = margin(t=1, r=3, b=1, l=1, unit="cm"), # margin adjusts the margin around the entire plot
    axis.title.x = element_blank(), # remove x axis title
    axis.title.y = element_blank() # remove y axis title 
  )+
  coord_cartesian(clip="off") # make sure no plot elements are cut off

# Save plot to plots file
ggsave(filename = "snao_by_race_plot.png",
       path = ("/Users/jerramclaughlin/Desktop/Desktop - Jerra’s MacBook Pro/MaCSS/CompSS224a/plots"),
       width = 10,
       height = 6)    


# -----------------------------------------------------------------------------------------------
# Proportion of seniors without health insurance 
# -----------------------------------------------------------------------------------------------
senior_insurance_vars <- c(
  "B27001_024", # Estimate!!Total:!!Male:!!65 to 74 years:
  "B27001_025", # Estimate!!Total:!!Male:!!65 to 74 years:With health insurance coverage
  "B27001_026", # Estimate!!Total:!!Male:!!65 to 74 years:No health insurance coverage
  "B27001_052", # Estimate!!Total:!!Female:!!65 to 74 years:
  "B27001_053", # Estimate!!Total:!!Female:!!65 to 74 years:With health insurance coverage
  "B27001_054" # Estimate!!Total:!!Female:!!65 to 74 years:No health insurance coverage
)

# load data
acs_senior_insurance <- get_acs(
  geography = "county",
  variables = senior_insurance_vars,
  state = "CA",
  county = c("Los Angeles", "San Francisco"),
  year = 2022,
  survey = "acs5"
)

senior_insurance_counts <- acs_senior_insurance %>%
  group_by(NAME) %>% # group by county
  select(-moe) %>% # drop margin of error
  pivot_wider( # pivot wider
    names_from = variable,
    values_from = estimate
  ) %>% 
  reframe(
    total_seniors = sum(
      B27001_024, # senior men
      B27001_052  # senior women
    ),
    uninsured = B27001_026 + B27001_054 # add uninsured senior men and women 
  ) %>%
  mutate(
    p_uninsured = uninsured / total_seniors # calculate proportion of uninsured seniors 
  )

view(senior_insurance_counts) # look at data 

# plot data

# -----------------------------------------------------------------------------------------------
# Proportion of Children and family type below the poverty line
# -----------------------------------------------------------------------------------------------
children_vars <- c(
  "B17006_001", # Estimate!!Total: All households 
  "B17006_002", # Estimate!!Total: All households below the poverty level
  "B17006_004", # Estimate!!Total: households below the poverty level, married-couple w/ kids under 5
  "B17006_005", # Estimate!!Total: households below the poverty level, married-couple w/ kids 5 years old 
  "B17006_006", # Estimate!!Total: households below the poverty level, married-couple w/ kids aged 6-17
  "B17006_009", # Estimate!!Total: single dad households below the poverty level w/ kids under 5
  "B17006_010", # Estimate!!Total: single dad households below the poverty level w/ kids 5 tears old
  "B17006_011", # Estimate!!Total: single dad households below the poverty level w/ kids aged 6-17
  "B17006_013", # Estimate!!Total: single mom households below the poverty level w/ kids under 5
  "B17006_014", # Estimate!!Total: single mom households below the poverty level w/ kids aged 5
  "B17006_015" # Estimate!!Total: single mom households below the poverty level w/ kids aged 6-17
)
# load data 
acs_children <- get_acs(
  geography = "county",
  variables = children_vars,
  state = "CA",
  county = c("Los Angeles", "San Francisco"),
  year = 2022,
  survey = "acs5"
)

children_counts <- acs_children %>%
  group_by(NAME) %>% # group by county
  select(-moe) %>% # drop margin of error 
  pivot_wider( # pivot wider
    names_from = variable,
    values_from = estimate
  ) %>% 
  reframe(
    kids_under_6 = sum( # combine under 5 and aged 5
      B17006_004, # two-parent household (kids under 5)
      B17006_009, # single dad household (kids under 5)
      B17006_013, # single mom household (kids under 5)
      B17006_005, # two-parent household (kids aged 5)
      B17006_010, # single dad household (kids aged 5)
      B17006_014  # single mom household (kids aged 5)
    ),
    kids_6_to_17 = sum(
      B17006_006, # two-parent household
      B17006_011, # single dad household
      B17006_015 # single mom household 
    ),
    total_below_poverty = B17006_002,
    total_households = B17006_001,
  ) %>%
  mutate(
    p_below_poverty = total_below_poverty / total_households, # proportion of households below the poverty line with kids 
    p_kids_under_6 = kids_under_6 / total_households, # proportion of households with kids under 6 living below the poverty line 
    p_kids_6_to_17 = kids_6_to_17 / total_households # proportion of households with kids aged 6 to 17 living below the poverty line 
  )
view(children_counts) # look at data to determine which county has more children living below the poverty level
