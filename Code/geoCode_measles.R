library(tidyverse)
library(tidycensus)
library(readxl)
library(tigris)
library(cowplot)

setwd("~/Documents/GitHub/Measles_in_Texas/")
source("../SVICalculation/functionsForSVI.R")

socioEcoData<-getVariables("county","TX",2022)

svi_counties_TX<-rankingAndSvi(socioEcoData)

population_county_US <- get_acs(
  geography = "county",  # ZIP Code Tabulation Area
  variables = "B01003_001",  # Total Population
  year = 2022,
  survey = "acs5",
  geometry = T
)

population_county_TX<-population_county_US %>% separate(NAME,c("County","State"),sep=", ") %>%
  mutate(County=County %>% str_remove_all(.," County")) %>%
  filter(State=="Texas") 

#This gets the map using tigris
# options(tigris_use_cache = TRUE)  # Cache the data to avoid re-downloading
# texas_counties <- counties(state = "TX", year = 2022, class = "sf")  # Get counties for Texas  

cases<-read_excel("Data/measles_TX_02_28_2025.xlsx")

counties_with_cases<-population_county_TX %>% filter(County %in% cases$County)

cases_map<-ggplot() + theme_void() +
  geom_sf(data=population_county_TX) +
  geom_sf(data = counties_with_cases,fill="red")

svi_map<-population_county_TX %>% left_join(svi_counties_TX %>% select(GEOID=Zip,SVI)) %>%
  ggplot(aes(fill=SVI)) + theme_void() +
  geom_sf()


### RUCA values ZCTA level ---------
#From this link: https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes

ruca_zcta<-read_csv("Data/RUCA2010zipcode_data.csv")
ruca_county<-read_csv("Data/ruca2010revised_county.csv")

ruca_county_agg<-ruca_county %>% select(GEOID=1,State=2,County=3,RUCA=5) %>%
  filter(State=="TX") %>% group_by(GEOID,State,County) %>%
  filter(RUCA!=99) %>% summarise_each(list(RUCA_mean=mean,RUCA_sd=sd))

rucaMean_map<-population_county_TX %>% left_join(ruca_county_agg %>% ungroup() %>% select(-County,-State)) %>%
  ggplot(aes(fill=RUCA_mean)) + theme_void() +
  geom_sf()

rucaSd_map<-population_county_TX %>% left_join(ruca_county_agg %>% ungroup() %>% select(-County,-State)) %>%
  ggplot(aes(fill=RUCA_sd)) + theme_void() +
  geom_sf()

plot_grid(cases_map,svi_map,rucaMean_map,rucaSd_map)

#Household distribution and age groups in TX
library(tidycensus)
library(stringr)

#housegholds in all US
load("~/Documents/GitHub/Mpox_2024/Data/house_from_cdc.RData")
#age groups in all US
load("~/Documents/GitHub/Mpox_2024/Data/age_groups_by_zipcode.RData")

#I need zip codes that are in TX
population_zcta_TX <- get_acs(
  geography = "zcta",  # ZIP Code Tabulation Area
  variables = "B01003_001",  # Total Population
  state="TX",
  year = 2019,
  survey = "acs5"
)

population_zcta_TX

houses_in_TX<-house_from_cdc %>%
  filter(GEOID %in% population_zcta_TX$GEOID)

total_number_homes<-houses_in_TX %>% select(type=variable,totalHomes=estimate) %>%
  mutate(type=type %>% str_remove_all("_nonFam")) %>%
  mutate(type=type %>% str_remove_all("_Fam")) %>%
  group_by(type) %>% summarise_each(sum) %>% 
  mutate(probHomes=totalHomes/sum(totalHomes))

#Total population
total_number_homes %>% slice(3,7,6,2,1,5,4) %>% 
  mutate(people_in_house=seq(1,nrow(.)),
         total_people_type=5000*probHomes*people_in_house) %>%
  pull(total_people_type) %>% sum()

total_number_homes %>% slice(3,7,6,2,1,5,4) %>% pull(probHomes)

#houses_all_US<-
dist_households<-total_number_homes %>%
  #  mutate(type_1=type %>% str_replace_all("_"," ")) %>%
  mutate(type=type %>% fct_relevel(c("one_person","two_person","three_person",
                                     "four_person","five_person","six_person","seven_plus"))) %>%
  #  mutate(type_1=type_1 %>% fct_relevel(c("one person","two person","three person",
  #                                     "four person","five person","six person","seven plus"))) %>%
  ggplot(aes(x=type,y=probHomes)) + geom_col() + theme_bw() +
  xlab("Number of people in household") + ylab("Proportion of households") +
  theme(text=element_text(size=18))

dist_ages<-age_groups_by_zipcode %>% 
  filter(GEOID %in% population_zcta_TX$GEOID) %>% ungroup() %>% select(-GEOID) %>%
  group_by(age_group) %>% summarise_each(sum) %>%
  mutate(total=sum(estimate)) %>%
  mutate(age_group=age_group %>% fct_relevel(c("0-4","5-17","18+"))) %>%
  ggplot(aes(x=age_group,y=estimate/total)) + theme_bw() +
  geom_col() + xlab("Age groups") + ylab("Fraction of population") +
  theme(text=element_text(size=18))

age_groups_by_zipcode %>% 
  filter(GEOID %in% population_zcta_TX$GEOID) %>% ungroup() %>% select(-GEOID) %>%
  group_by(age_group) %>% summarise_each(sum) %>% mutate(total=sum(estimate),proporAges=estimate/total) %>%
  slice(1,3,2) %>% pull(proporAges)

# Processing results using household and age distribution for TX

load("Data/simulation_agents_MeaslesResults.RData")
load("Data/simulation_timeSeries_MeaslesResults.RData")

results_agents

merged_timeSeries<-bind_rows(results_counts %>% bind_rows() %>% mutate(Simulation=1),
          results_counts_1 %>% bind_rows() %>% mutate(Simulation=2),
          results_counts_2 %>% bind_rows() %>% mutate(Simulation=3),
          results_counts_3 %>% bind_rows() %>% mutate(Simulation=4),
          results_counts_4 %>% bind_rows() %>% mutate(Simulation=5),
          results_counts_5 %>% bind_rows() %>% mutate(Simulation=6),
          results_counts_6 %>% bind_rows() %>% mutate(Simulation=7),
          results_counts_7 %>% bind_rows() %>% mutate(Simulation=8),
          results_counts_8 %>% bind_rows() %>% mutate(Simulation=9),
          results_counts_9 %>% bind_rows() %>% mutate(Simulation=10),
          results_counts_10 %>% bind_rows() %>% mutate(Simulation=11))

fig1<-merged_timeSeries %>% filter(Vacc_perc>0.35,day<50) %>%
  mutate(perct_popu_vacc=paste0("Frac. popu vacc. ",Vacc_perc))%>%
  ggplot(aes(x=day,y=I,color=as.factor(Simulation),group=as.factor(Simulation))) +
  geom_line() + facet_wrap(~perct_popu_vacc,scales = "free_y") +theme_bw()+
  ylab("Incidence") + theme(legend.position = "none",text = element_text(size=18))

fig2<-merged_timeSeries %>% filter(Vacc_perc>0.35,day<50) %>%
  mutate(perct_popu_vacc=paste0("Frac. popu vacc. ",Vacc_perc))%>%
  ggplot(aes(x=day,y=R,color=as.factor(Simulation),group=as.factor(Simulation))) +
  geom_line() + facet_wrap(~perct_popu_vacc,scales = "free_y") + theme_bw()+
  ylab("Total infected") + theme(legend.position = "none",text = element_text(size=18))

archivos<-c("results_agents","results_agents_1","results_agents_2","results_agents_3",
            "results_agents_4","results_agents_5","results_agents_6","results_agents_7",
            "results_agents_8","results_agents_9","results_agents_10")

s=1

results_endSimulation<-NULL
results_households_endSimulation<-NULL

for(s in 1:length(archivos)){
  test<-get(archivos[s]) %>% bind_rows() %>% mutate(Simulation=s)
  #total population
  total_population<-test %>% select(Vacc_perc,Simulation) %>% 
    group_by(Vacc_perc,Simulation) %>% count()
  #Agents initially susceptible
  initially_suscep<-test %>% filter(state != "V") %>% 
    group_by(Vacc_perc,Simulation) %>% count() %>%
    rename_at("n",~"init_suscep")
  #Agents in either R or S state at the end
  uno<-test %>% filter(state != "V") %>% select(state,Vacc_perc,Simulation) %>% 
    group_by(Vacc_perc,state,Simulation) %>% count() %>%
    left_join(initially_suscep)
  results_endSimulation<-rbind(results_endSimulation,uno)
  
  #Total households with children
  households_with_children<-get(archivos[s]) %>% bind_rows() %>% mutate(Simulation=s) %>% 
    filter(age_group %in% c("0-4","5-17")) %>%
    select(Simulation,Vacc_perc,household_id,size_household) %>% unique() %>% 
    group_by(Simulation,Vacc_perc) %>% count() %>% rename_at("n",~"household_with_children")
  #Households with at least one infected
  dos<-get(archivos[s]) %>% bind_rows() %>% mutate(Simulation=s) %>% 
    filter(age_group %in% c("0-4","5-17"),state=="R") %>%
    select(Simulation,Vacc_perc,household_id,state) %>% unique() %>% 
    group_by(Simulation,Vacc_perc) %>% count() %>% left_join(households_with_children)
  results_households_endSimulation<-rbind(results_households_endSimulation,dos)
}

res1<-results_endSimulation %>% filter(state=="R") %>%filter(Vacc_perc>0.35) %>%
  ggplot(aes(x=as.factor(Vacc_perc),y=n/init_suscep,group=Vacc_perc))+
  geom_boxplot(position=position_dodge()) + theme_bw() +
  ylab("Faction of susceptible that got infected") + xlab("Fraction of population vaccinated")+
  theme(text=element_text(size=18))

res2<-results_households_endSimulation %>% filter(Vacc_perc>0.35) %>%
  ggplot(aes(x=as.factor(Vacc_perc),y=n/1000,group=Vacc_perc))+
  geom_boxplot() + theme_bw() + theme(text=element_text(size=18)) +
  xlab("Fraction of population vaccinated")+ ylab("Fraction of household with at least one infected")


dists<-plot_grid(dist_households,dist_ages)
ggsave(dists,file="dists.png",height = 7,width = 19)
ggsave(fig1,file="timeSeries.png",height = 10,width = 19)
ggsave(fig2,file="timeSeries2.png",height = 10,width = 19)


boxes<-plot_grid(res1,res2)
ggsave(boxes,file="boxes.png",height = 7,width = 13)

### For analysis at the census tract level or census block
#I need zip codes that are in TX

# population_tract_TX <- get_acs(
#   geography = "tract",  # ZIP Code Tabulation Area
#   variables = "B01003_001",  # Total Population
#   state="TX",
#   year = 2022,
#   survey = "acs5"
# )

house_cdc_vars<-c(
  two_person_Fam="B11016_003",
  three_person_Fam="B11016_004",
  four_person_Fam="B11016_005",
  five_person_Fam="B11016_006",
  six_person_Fam="B11016_007",
  seven_plus_Fam="B11016_008",
  one_person_nonFam="B11016_010",
  two_person_nonFam="B11016_011",
  three_person_nonFam="B11016_012",
  four_person_nonFam="B11016_013",
  five_person_nonFam="B11016_014",
  six_person_nonFam="B11016_015",
  seven_plus_nonFam="B11016_016"
)

load_variables(2022, "acs5", cache = TRUE) %>%
  filter(str_detect(name,"B11016")) %>% print(n=20)

#Extracting data used by the CDC to get household distributions
house_tracts <- get_acs(
  geography = "tract",
  state = "TX",
  variables = house_cdc_vars,
  #  summary_var = "B11016_002",
  year = 2022
)

#cases$County

#Tracts
distributions_Counties_households_tracts<-house_tracts %>% 
  filter(str_detect(NAME,"Dallam|Dawson|Ector|Gaines|Lubbock|Lynn|Martin|Terry|Yoakum|Harris|Travis|Dallas")) %>%
  select(NAME,type=variable,totalHomes=estimate) %>%
  separate(NAME,c("Tract","County","State"),sep = "; ") %>%
  select(County,type,totalHomes) %>% mutate(County=str_remove_all(County," County")) %>%
  mutate(type=type %>% str_remove_all("_nonFam")) %>%
  mutate(type=type %>% str_remove_all("_Fam")) %>%
  group_by(County,type) %>% summarise_each(sum) %>% 
  mutate(probHomes=totalHomes/sum(totalHomes)) 

distributions_Counties_households_tracts %>%
  mutate(type=type %>% fct_relevel(c("one_person","two_person","three_person",
                                     "four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=type,y=totalHomes,fill=County)) +
  geom_col(position = position_dodge())

distributions_Counties_households_tracts %>%
  mutate(type=type %>% fct_relevel(c("one_person","two_person","three_person",
                                     "four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=type,y=probHomes,fill=County)) +
  geom_col(position = position_dodge())

#Age distributions

vars <- c(
  "B01001_003",  # Male Under 5 years
  "B01001_004",  # Male 5 to 9 years
  "B01001_005",  # Male 10 to 14 years
  "B01001_006",  # Male 15 to 17 years  
  "B01001_007",  # Male 18-19 years
  "B01001_008",  # Male 20 years
  "B01001_009",  # Male 21 years
  "B01001_010",  # Male 22-24 years
  "B01001_011",  # Male 25-30 years
  "B01001_012",  # Male 30-34 years
  "B01001_013",  # Male 35-39 years
  "B01001_014",  # Male 40-44 years
  "B01001_015",  # Male 45-49 years
  "B01001_016",  # Male 50-54 years
  "B01001_017",  # Male 55-59 years
  "B01001_018",  # Male 60-61 years
  "B01001_019",  # Male 62-64 years
  "B01001_020",  # Male 65-66 years
  "B01001_021",  # Male 67-69 years
  "B01001_022",  # Male 70-74 years
  "B01001_023",  # Male 75-79 years
  "B01001_024",  # Male 80-84 years
  "B01001_025",  # Male 85+ years
  
  "B01001_027",  # Female Under 5 years
  "B01001_028",  # Female 5 to 9 years
  "B01001_029",  # Female 10 to 14 years
  "B01001_030",  # Female 15 to 17 years
  "B01001_031",  # Female 18-19 years
  "B01001_032",  # Female 20 years
  "B01001_033",  # Female 21 years
  "B01001_034",  # Female 22-24 years
  "B01001_035",  # Female 25-29 years
  "B01001_036",  # Female 30-34 years
  "B01001_037",  # Female 35-39 years
  "B01001_038",  # Female 40-44 years
  "B01001_039",  # Female 45-49 years
  "B01001_040",  # Female 50-54 years
  "B01001_041",  # Female 55-59 years
  "B01001_042",  # Female 60-61 years
  "B01001_043",  # Female 62-64 years
  "B01001_044",  # Female 65-66 years
  "B01001_045",  # Female 67-69 years
  "B01001_046",  # Female 70-74 years
  "B01001_047",  # Female 75-79 years
  "B01001_048",  # Female 80-84 years
  "B01001_049"   # Female 85+ years
)

# Get census data by ZIP code
census_data_tracts <- get_acs(
  geography = "tract", # ZIP Code Tabulation Area
  variables = vars,
  year = 2022,
  state="TX",
  survey = "acs5"
)

age_groups_counties_tracts<-census_data_tracts %>% 
  filter(str_detect(NAME,"Dallam|Dawson|Ector|Gaines|Lubbock|Lynn|Martin|Terry|Yoakum|Harris|Travis|Dallas")) %>%
  separate(NAME,c("Tract","County","State"),sep="; ") %>% mutate(County=County %>% str_remove_all(.," County")) %>%
  select(County,variable,estimate) %>%
  mutate(age_group = case_when(
  variable == "B01001_003" ~ "0-4",
  variable <= "B01001_006" ~ "5-17",
  variable <= "B01001_025" ~ "18+",
  variable == "B01001_027" ~ "0-4",
  variable <= "B01001_030" ~ "5-17",
  variable >= "B01001_031" ~ "18+")) %>% 
  select(County,age_group,estimate) %>% group_by(County,age_group) %>% summarise_each(sum) %>%
  mutate(total_in_county=sum(estimate),prop_age_group=estimate/total_in_county)

age_groups_counties_tracts %>%
  mutate(age_group= age_group %>% fct_relevel(c("0-4","5-17","18+"))) %>%
  ggplot(aes(x=age_group,y=prop_age_group,fill=County))+
  geom_col(position = position_dodge())

##### Data to get distribution of people in house using the
##### crowd index of the SVI
vars_crowding<-c("B25024_007E","B25024_008E","B25024_009E","B25024_001E", #Percent of Housing Units with 10+ Units in Structure
                 "B25033_006E","B25033_007E","B25033_012E","B25033_013E","B25033_001E",#Percent of Population Living in Mobile Homes
                 #Percent of Population Living In Accommodations with Less Than 1 Room Per Person/Crowding - At Household Level, 
                 #Occupied Housing Units, More People Than Rooms Estimate
                 "B25014_005E","B25014_006E","B25014_007E","B25014_011E","B25014_012E","B25014_013E","B25014_001E",
                 "B25044_003E","B25044_010E","B25044_001E", #Percent of Population with No Vehicle Available
                 "B26001_001E","B01003_001E") #Percent of Population Living in Group Quarters
for_crowding_index <- get_acs(
  geography = "zcta",
  #  state = "TX",
  variables = vars_crowding,
  #  summary_var = "B11016_001",
  year = 2022
)

data_crowding<-for_crowding_index %>% select(Zip=GEOID,variable,estimate) %>%
  mutate(variable=paste0(variable,"E")) %>%
  pivot_wider(names_from = "variable",values_from = "estimate") %>%
  mutate(MUNIT=(B25024_007E+B25024_008E+B25024_009E)/B25024_001E, #PER_MULTI_DWELL
         MOBILE=(B25033_006E+B25033_007E+B25033_012E+B25033_013E)/B25033_001E, #PER_MOBILE_DWEL
         CROWD=(B25014_005E+B25014_006E+B25014_007E+B25014_011E+B25014_012E+B25014_013E)/B25014_001E, #PER_CROWD_DWELL
         NOVEH=(B25044_003E+B25044_010E)/B25044_001E, #PER_NO_VEH_AVAIL
         GROUPQ=B26001_001E/B01003_001E) #PER_GROUP_DWELL
j <- data_crowding$RNKMUNIT <- rank(x = -data_crowding$MUNIT, na.last = "keep", ties.method = "max")
k <- data_crowding$RNKMOBILE <- rank(x = -data_crowding$MOBILE, na.last = "keep", ties.method = "max")
l <- data_crowding$RNKCROWD <- rank(x = -data_crowding$CROWD, na.last = "keep", ties.method = "max")
m <- data_crowding$RNKNOVEH <- rank(x = -data_crowding$NOVEH, na.last = "keep", ties.method = "max")
n <- data_crowding$RNKGROUPQ <- rank(x = -data_crowding$GROUPQ, na.last = "keep", ties.method = "max")
#Sum The Ranks
data_crowding$SUMRANK <- j+k+l+m+n
data_crowding$SUMRANK <- j+k+l+m+n
#Derive the Adaptive Capacity Index
data_crowding$CROWD_INDEX <- dplyr::percent_rank(data_crowding$SUMRANK)
save(data_crowding,file="~/Documents/GitHub/Mpox_2024/Data/data_crowding.RData")

## Age distribution in population / we will assume that household distribution 
## by age group is the same as the total population by age group

load_variables(2022, "acs5", cache = TRUE) %>%
  filter(str_detect(name,"B01001")) %>% filter(str_detect(name,"B01001_")) %>% print(n=300)
# load_variables(2020, "acs5", cache = TRUE) %>%
#   filter(str_detect(name,"B09001")) %>% print(n=20)


save(age_groups_by_zipcode,file="~/Documents/GitHub/Mpox_2024/Data/age_groups_by_zipcode.RData")
age_groups_by_zipcode

age_groups_by_zipcode %>% ungroup() %>% select(-GEOID) %>%
  group_by(age_group) %>% summarise_each(sum) %>%
  mutate(total=sum(estimate)) %>%
  mutate(age_group=age_group %>% fct_relevel(c("0-4","5-17","18+"))) %>%
  ggplot(aes(x=age_group,y=estimate/total)) + geom_col()

age_distribution_all_US <- age_groups_by_zipcode %>% ungroup() %>% select(-GEOID) %>%
  group_by(age_group) %>% summarise_each(sum) %>% mutate(total=sum(estimate),proporAges=estimate/total) %>% slice(1,3,2)
save(age_distribution_all_US,file="~/Documents/GitHub/Mpox_2024/Data/age_distribution_all_US.RData")

