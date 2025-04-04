library(tidyverse)
library(tidycensus)
library(readxl)
library(sf)

setwd("~/Documents/GitHub/Measles_in_Texas/")

#I want to see if there is some similarities among counties with cases
cases<-read_excel("Data/measles_TX_02_28_2025.xlsx",sheet = 2)
cases %>% arrange(desc(Cases))

cases<-read_excel("Data/measles_TX_02_28_2025.xlsx",sheet = 2)

schools2023<-read_sf("Data/Schools_2023_to_2024/Schools_2023_to_2024.shp")

schools2023 %>% glimpse()

schools2023$USER_Sch_1 %>% unique() %>% sort() %>% head(30)

schools_in_counties_with_cases<- schools2023 %>% 
  select(County=Subregion,Grade_range=USER_Grade,School_enroll_2023=USER_Sch15,School_status=USER_Sch16,
         School_Type=School_Typ,District_ISD=USER_Dis_1,District_enroll_2023=USER_Dis16,
         School_number=USER_Schoo,School_name=USER_Sch_1) %>%
  filter(School_enroll_2023>0) %>%
  mutate(County=County %>% str_remove_all(" County")) %>%
  filter(County %in% cases$County) %>%
  as_tibble() %>% select(County,Grade_range,School_enroll_2023,District_ISD)

schools_in_counties_with_cases %>%
  select(County) %>% group_by(County) %>% count() %>% arrange(desc(n))

schools_in_counties_with_cases %>% 
  select(-District_ISD) %>% group_by(County,Grade_range) %>%
  summarise_each(sum) %>%
  ggplot(aes(x=Grade_range,y=School_enroll_2023,fill=County)) +
  geom_col(position = position_dodge()) +
  theme(axis.text.x=element_text(angle = 90,hjust=0)) +
  facet_wrap(~County,scales = "free")

#total population in counties
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

population_county_TX %>% as_tibble() %>% filter(County %in% cases$County) %>%
  select(County,population=estimate) %>% arrange(desc(population))

counties_with_cases<-population_county_TX %>% filter(County %in% cases$County) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2])

ggplot() + theme_void() +
  geom_sf(data=population_county_TX) +
  geom_sf(data = counties_with_cases,fill="red") +
  geom_text(data = counties_with_cases, aes(x = lon, y = lat, label = County),
            color = "yellow", fontface = "bold", size = 3) +
  ggtitle("Counties with reported measles cases as of March 25, 2025")

ggsave(last_plot(),file="Figures/county_cases_texas.png",bg="white",width = 27,height = 13)

#Household distribution in affected counties

#Below, I was using a different variable from the CDC study for household size
#No variable for Family household with 1-person
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

# load_variables(2022, "acs5", cache = TRUE) %>%
#   filter(str_detect(name,"B11016")) %>% print(n=20)
# 
#Extracting data used by the CDC to get household distributions
house_from_cdc <- get_acs(
  geography = "county",
  state = "TX",
  variables = house_cdc_vars,
  #  summary_var = "B11016_002",
  year = 2022
)

household_size_dist_Texas<-house_from_cdc %>% 
  select(County=NAME,type=variable,totalHomes=estimate) %>%
  mutate(County=County %>% str_remove_all(" County, Texas")) %>%
  mutate(type=type %>% str_remove_all("_nonFam")) %>%
  mutate(type=type %>% str_remove_all("_Fam")) %>%
  group_by(County,type) %>% summarise_each(sum) %>% 
  mutate(probHomes=totalHomes/sum(totalHomes))

household_size_dist_Texas %>% filter(County %in% cases$County) %>%
  mutate(type=type %>% 
           fct_relevel(c("one_person","two_person","three_person","four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=type,y=totalHomes,fill=County))+
  geom_col(position = position_dodge()) + facet_wrap(~County,scales = "free_y")

household_size_dist_Texas %>% filter(County %in% cases$County) %>%
  mutate(type=type %>% 
           fct_relevel(c("one_person","two_person","three_person","four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=type,y=probHomes,fill=County))+
  geom_col(position = position_dodge()) #+ facet_wrap(~County,scales = "free_y")

#Population by age group

# load_variables(2022, "acs5", cache = TRUE) %>%
#   filter(str_detect(name,"B01001")) %>% filter(str_detect(name,"B01001_")) %>% print(n=300)

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
population_by_age_Texas <- get_acs(
  geography = "county", # ZIP Code Tabulation Area
  state="TX",
  variables = vars,
  year = 2022,
  survey = "acs5"
)

age_groups_by_county_Texas<-population_by_age_Texas %>% mutate(age_group = case_when(
  variable == "B01001_003" ~ "0-4",
  variable <= "B01001_006" ~ "5-17",
  variable <= "B01001_025" ~ "18+",
  variable == "B01001_027" ~ "0-4",
  variable <= "B01001_030" ~ "5-17",
  variable >= "B01001_031" ~ "18+")) %>% #print(n=50)
  select(County=NAME,age_group,estimate) %>% mutate(County=County %>% str_remove_all(" County, Texas")) %>%
  group_by(County,age_group) %>% summarise_each(sum)

age_groups_by_county_Texas %>% filter(County %in% cases$County) %>%
  mutate(total_popu=sum(estimate)) %>% mutate(prop_in_group=estimate/total_popu) %>%
  mutate(age_group=age_group %>% fct_relevel(c("0-4","5-17","18+"))) %>%
  ggplot(aes(x=age_group,y=estimate,fill=County)) +
  geom_col(position = position_dodge()) + facet_wrap(~County,scales = "free_y")

age_groups_by_county_Texas %>% filter(County %in% cases$County) %>%
  mutate(total_popu=sum(estimate)) %>% mutate(prop_in_group=estimate/total_popu) %>%
  mutate(age_group=age_group %>% fct_relevel(c("0-4","5-17","18+"))) %>%
  ggplot(aes(x=age_group,y=prop_in_group,fill=County)) +
  geom_col(position = position_dodge()) 

# Since transmission is mostly happening in children (0-17) years of age,
# I will see which counties have the highest proportion of people in these
# age groups.

population_county_TX %>% select(GEOID,County) %>%
  left_join(age_groups_by_county_Texas) %>% group_by(County) %>%
  mutate(total_popu=sum(estimate)) %>%
  filter(age_group!="18+") %>%
  ggplot(aes(fill=estimate/total_popu)) + geom_sf() + theme_void() +
  scale_fill_gradient(low="lightyellow", high="slateblue4") +
  facet_wrap(~age_group) #+
  scale_fill_gradient(trans = "log",
               #       breaks = my_breaks, labels = my_breaks,
                      low = "white", #"#075AFF",
                      high = "#0072B2",
                      name="Population")
  

# Aggregate ages from 0-17
prop_of_children_in_county<-age_groups_by_county_Texas %>% 
  mutate(age_group1=ifelse(age_group=="18+","Adult","Children")) %>%
  select(-age_group) %>% group_by(County,age_group1) %>% summarise_each(sum) %>% group_by(County) %>% 
  mutate(total_popu=sum(estimate),prop_of_children=estimate/total_popu) %>%
  ungroup() %>% filter(age_group1 == "Children") %>% arrange(desc(prop_of_children))

prop_of_children_in_county %>% filter(County %in% cases$County)

# prop_of_children_in_county %>% 
#   left_join(cases %>% select(County,Cases)) %>% drop_na() %>%
#   ggplot(aes(x=estimate,y=Cases)) + geom_point() +
#   scale_y_log10() + scale_x_log10() +  
#   geom_smooth(method = "lm")

prop_of_children_in_county %>% 
  left_join(cases %>% select(County,Cases)) %>% drop_na() %>%
  ggplot(aes(x=prop_of_children,y=Cases/estimate)) + geom_point() +
  scale_y_log10() + geom_smooth(method = "lm")

population_county_TX %>% 
  left_join(prop_of_children_in_county %>% select(County,prop_of_children)) %>%
  ggplot(aes(fill=prop_of_children)) + geom_sf() + theme_void() +
  scale_fill_gradient(low="lightyellow", high="slateblue4") 

children_with_threshold<-population_county_TX %>% 
  left_join(prop_of_children_in_county %>% select(County,prop_of_children)) %>%
  mutate(more_than_30=ifelse(prop_of_children>0.25,"Yes","No"))

ggplot() + theme_void() +
  geom_sf(data=children_with_threshold,aes(fill=more_than_30)) +
  geom_sf(data = counties_with_cases,color="red",fill=NA) +
  geom_text(data = counties_with_cases, aes(x = lon, y = lat, label = County),
            color = "black", fontface = "bold", size = 5)+
  scale_fill_manual(values=c("#0072B2","lightyellow")) +
  theme(legend.title = element_blank(),legend.position = c(0.15,0.7),
        text=element_text(size=18))

ggsave(last_plot(),file="map_prop_children.png",width=25,height = 18)

cases %>% arrange(desc(Cases))

#### ZCTA level analysis of distributions ---------
## I want to see how heterogeneous are households and age distributions 
## in these counties at ZCTA level

#Extracting data used by the CDC to get household distributions
house_from_cdc_zcta <- get_acs(
  geography = "zcta",
#  state = "TX",
  variables = house_cdc_vars,
  #  summary_var = "B11016_002",
  year = 2022
)

load("zcta_county_list_final.RData")

household_distribution_ZCTAs<-house_from_cdc_zcta %>% ungroup() %>% 
  select(ZCTA=GEOID,type=variable,totalHomes=estimate) %>%
  left_join(zcta_county_list_final %>% select(ZCTA,County=County_Name)) %>%
  drop_na() %>%
  mutate(type=type %>% str_remove_all("_nonFam")) %>%
  mutate(type=type %>% str_remove_all("_Fam")) %>%
  group_by(County,ZCTA,type) %>% summarise_each(sum) %>% 
  mutate(totalZCTA=sum(totalHomes),probHomes=totalHomes/totalZCTA)

household_distribution_ZCTAs %>% filter(County %in% cases$County) %>%
  mutate(type=type %>% fct_relevel(c("one_person","two_person","three_person",
                                     "four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=type,y=probHomes,fill=ZCTA)) +
  geom_col(position = position_dodge()) + facet_wrap(~County) + 
  theme(legend.position = "none")
  
household_distribution_ZCTAs %>% ungroup() %>%
  select(-ZCTA,-totalZCTA,-probHomes) %>% group_by(County,type) %>%
  summarise_each(sum) %>% filter(County %in% cases$County) %>% ungroup() %>%
  group_by(County) %>% mutate(totalInCounty=sum(totalHomes),probHomes=totalHomes/totalInCounty) %>%
  mutate(type=type %>% fct_relevel(c("one_person","two_person","three_person",
                                     "four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=type,y=probHomes,fill=County)) +
  geom_col(position = position_dodge())

#I want to see the probability of having large households (5 or more people in them)
prob_of_large_households<-household_distribution_ZCTAs %>% ungroup() %>%
  select(-ZCTA,-totalZCTA,-probHomes) %>% group_by(County,type) %>%
  summarise_each(sum) %>% filter(County %in% cases$County) %>% ungroup() %>%
  mutate(size_house=ifelse(type %in% c("five_person","six_person","seven_plus"),"Large","Small")) %>%
  select(-type) %>% group_by(County,size_house) %>% summarise_each(sum) %>%
  ungroup() %>% group_by(County) %>% mutate(totalInCounty=sum(totalHomes),prob_house_size=totalHomes/totalInCounty) %>%
  mutate(size_house=size_house %>% fct_relevel(c("Small","Large"))) 

prob_of_large_households %>% filter(size_house=="Large") %>%
  ggplot(aes(x=reorder(County,prob_house_size,decreasing = T),y=prob_house_size,fill=size_house)) + 
  geom_col(position = position_dodge())

all_compare_data<-cases %>% select(County,Cases) %>%
  left_join(population_county_TX %>% as_tibble() %>% select(County,pop_county=estimate)) %>%
  left_join(prob_of_large_households %>% filter(size_house=="Large") %>% 
              select(County,prob_large_house=prob_house_size)) 

all_compare_data %>%
  ggplot(aes(x=prob_large_house,y=Cases/pop_county)) + geom_point()

# Ages 

# Get census data by ZIP code
population_by_age_zcta <- get_acs(
  geography = "zcta", # ZIP Code Tabulation Area
#  state="TX",
  variables = vars,
  year = 2022,
  survey = "acs5"
)

age_groups_by_zcta_Texas<-population_by_age_zcta %>% mutate(age_group = case_when(
  variable == "B01001_003" ~ "0-4",
  variable <= "B01001_006" ~ "5-17",
  variable <= "B01001_025" ~ "18+",
  variable == "B01001_027" ~ "0-4",
  variable <= "B01001_030" ~ "5-17",
  variable >= "B01001_031" ~ "18+")) %>% 
  select(ZCTA=GEOID,age_group,estimate) %>%
  group_by(ZCTA,age_group) %>% summarise_each(sum) %>% ungroup() %>%
  left_join(zcta_county_list_final %>% select(ZCTA,County=County_Name)) %>% drop_na()

age_groups_by_zcta_Texas %>% filter(County %in% cases$County) %>%
  select(-ZCTA) %>% group_by(County,age_group) %>% summarise_each(sum) %>%
  ungroup() %>% group_by(County) %>% 
  mutate(totalCounty=sum(estimate),propInAge=estimate/totalCounty) %>%
  mutate(age_group=age_group %>% fct_relevel(c("0-4","5-17","18+"))) %>%
  ggplot(aes(x=age_group,y=propInAge,fill=County)) +
  geom_col(position = position_dodge())

age_groups_by_zcta_Texas %>% filter(County %in% cases$County) %>%
 # group_by(County,ZCTA,age_group) %>% summarise_each(sum) %>%
  ungroup() %>% group_by(ZCTA) %>% 
  mutate(totalZCTA=sum(estimate),propInAge=estimate/totalZCTA) %>%
  mutate(age_group=age_group %>% fct_relevel(c("0-4","5-17","18+"))) %>%
  ggplot(aes(x=age_group,y=propInAge,fill=ZCTA)) +
  geom_col(position = position_dodge()) + 
  facet_wrap(~County) + theme(legend.position = "none")

#Divide age groups by high and low according to age
prop_popu_age_by_risk<-age_groups_by_zcta_Texas %>% filter(County %in% cases$County) %>%
  select(-ZCTA) %>% group_by(County,age_group) %>% summarise_each(sum) %>%
  ungroup() %>% mutate(risk_group=ifelse(age_group %in% c("0-4","5-17"),"High","Low")) %>%
  select(County,risk_group,estimate) %>% group_by(County,risk_group) %>%
  summarise_each(sum) %>% ungroup() %>% group_by(County) %>%
  mutate(total_popu=sum(estimate),prop_in_risk_group=estimate/total_popu) 

prop_popu_age_by_risk %>% filter(risk_group=="High") %>%
  ggplot(aes(x=reorder(County,prop_in_risk_group,decreasing = T),y=prop_in_risk_group)) + geom_col()


all_compare_data<-all_compare_data %>%
  left_join(prop_popu_age_by_risk %>% filter(risk_group=="High") %>%
              select(County,popu_in_high_risk=estimate,prop_in_risk_group))

all_compare_data %>%
  ggplot(aes(x=popu_in_high_risk,y=Cases)) + geom_point()

all_compare_data %>%
  ggplot(aes(x=prob_large_house,y=Cases/pop_county)) + geom_point()

all_compare_data %>%
  ggplot(aes(x=prob_large_house,y=Cases/popu_in_high_risk)) + geom_point()

all_compare_data %>%
  ggplot(aes(x=prop_in_risk_group,y=Cases/popu_in_high_risk)) + geom_point()

### Check mobility in counties and in counties with cases


### Temporal data: yearly cases in Texas
# Historic data from :https://www.dshs.texas.gov/vaccine-preventable-diseases/vaccine-preventable-disease-conditions/measles-rubeola/measles-rubeola/measles-rubeola-data

temporal_measles_TX<-read_csv("Data/yearly_measles_Texas.csv")
t(temporal_measles_TX)
temporal_measles_TX %>% as_tibble() %>% column_to_rownames("...1") %>% t() %>% as_tibble()
mutate_all(as.numeric)
  
temporal_measles_TX %>%
  mutate(From2006_to_2014=`2006`+`2007`+`2008`+`2009`+`2010`+`2011`+`2012`+`2013`+`2014`) %>%
  mutate(From2015_to_2023=`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`+`2022`+`2023`) %>%
  select(County="...1",contains("From"))

