library(tidyverse)
#library(tidycensus)
#library(readxl)
library(sf)

### School districts ------

#Data extracted from: https://schoolsdata2-tea-texas.opendata.arcgis.com
#Followinf the link "current districts"
#https://schoolsdata2-tea-texas.opendata.arcgis.com/datasets/edbb3c145304494382da3aa30c154b5e/explore?showTable=true
school_districts_map_original <- read_sf("Data/school_districts/Current_Districts_2025/Current_Districts_2025.shp")

school_districts_map_original %>% glimpse()

ggplot() + theme_void() + 
  geom_sf(data=school_districts_map_original,fill="white",color="gray")

ggsave(last_plot(),file="school_districts_Texas.png",bg="white")

#prueba <- read_csv("Data/school_districts/Texas_measles_infected_mean_lookup_table.csv")
#prueba <- read_csv("Data/school_districts/Texas_measles_infected_mean_lookup_with_county.csv")
prueba <- read_csv("Data/school_districts/Texas_measles_full_lookup_table_more.csv")
prueba %>% glimpse()

risk_eval_clean_with_counties<-prueba %>% 
  select(County,SchoolDistrict_or_Name=`School District or Name`,metric,
         for500=`500`,ageGroup=`Age Group`)

risk_eval_clean_with_counties$metric %>% unique()

risk_eval_clean_with_counties<-risk_eval_clean_with_counties %>% 
  mutate(County=toupper(County),SchoolDistrict_or_Name=toupper(SchoolDistrict_or_Name))

# risk_eval_clean<-prueba %>% 
#   select(SchoolDistrict_or_Name=`School District or Name`,metric,
#          average_school_size,metric_average_school_size,ageGroup=`Age Group`)

risk_eval_kindergarten<-risk_eval_clean_with_counties %>% 
  filter(ageGroup=="Kindergarten")

#Homogenization of names
# to upper all letters in names:
school_districts_map<-school_districts_map %>% select(NAME) %>%
  mutate(NAME=toupper(NAME))
risk_eval_kindergarten<-risk_eval_kindergarten %>% 
  select(County,NAME=SchoolDistrict_or_Name,metric,for500) %>%
  mutate(NAME=toupper(NAME))

#There are a 7 districts that are either CSD or CONS CSD or MSD
school_districts_map %>% filter(!str_detect(NAME,"ISD"))
#Many names of schools are not in the map for districts. We need to address this
risk_eval_kindergarten %>% filter(!str_detect(NAME,"SD"))

#Changing only one case in map data
school_districts_map<-school_districts_map %>% 
  mutate(NAME=NAME %>% str_replace_all(c("CONSOLIDATED"="CONS"))) 

#Changing in risk from CISD to CONS ISD to match with map
risk_eval_kindergarten<- risk_eval_kindergarten %>% filter(str_detect(NAME,"SD")) %>%
  mutate(NAME=NAME %>% str_replace_all(c("CISD"="CONS ISD",
                                         "MT-SAGINAW"="MOUNTAIN-SAGINAW",
                                         "FT SAM"="FORT SAM",
                                         "GOLD BURG"="GOLD-BURG",
                                         "CONSOLIDATED"="CONS",
                                         "FT DAVIS"="FORT DAVIS",
                                         "FT HANCOCK"="FORT HANCOCK",
                                         "COUNTY WIDE"="COUNTY-WIDE",
                                         "SCHERTZ-CIBOLO-U CITY ISD"="SCHERTZ-CIBOLO-UNIVERSAL CITY ISD")))

#To add school districts with the same name in different counties
risk_eval_kindergarten<-risk_eval_kindergarten %>%
  mutate(NAME=ifelse(NAME=="BIG SANDY ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="CHAPEL HILL ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="CENTERVILLE ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="DAWSON ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="EDGEWOOD ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="HIGHLAND PARK ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="HUBBARD ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="MIDWAY ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="NORTHSIDE ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="VALLEY VIEW ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="WYLIE ISD",paste0(NAME," (",County,")"),NAME)) #%>%
#  filter(str_detect(NAME,"BIG SANDY ISD|CHAPEL HILL ISD|WYLIE ISD"))

#With this simple initial homogeneization of names we have:

#setdiff(school_districts_map$NAME,risk_eval_kindergarten$NAME)
#There are 14 ISDs that are in the risk file, but not in the school map
setdiff(risk_eval_kindergarten$NAME,school_districts_map$NAME)
school_districts_map %>% filter(str_detect(NAME,"WYLIE"))
risk_eval_kindergarten %>% filter(str_detect(NAME,"WYLIE"))
#BETHESDA - not here map
#GOLDTHWAITE ISD - here GOLDTHWAITE CONS ISD
#SPRING CREEK ISD - not in map

risk_eval_kindergarten$metric %>% unique()

risk_eval_kindergarten %>% filter(metric %in% c("probability_exceeding_10_total_cases,
                                                probability_exceeding_10_total_cases_per_100"))

risk_eval_kindergarten %>% filter(metric=="probability_exceeding_10_total_cases_per_100")

data_prob_kinder<-school_districts_map %>% 
  left_join(risk_eval_kindergarten %>% select(-County)) %>%
  filter(metric=="probability_exceeding_10_total_cases") %>%
  filter(NAME!="ROSCOE COLLEGIATE ISD") 

ggplot() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=data_prob_kinder,aes(fill=for500)) + theme_void() + 
  scale_fill_gradient(low = "cornsilk1",high = "blue4",name="") +
  ggtitle("Probability of outbreaks larger 10 cases - kindergarten")

ggsave(last_plot(),file="map_prob_exceeding10_kinder.png",bg="white")

my_breaks_1 = c(0.1,1,10,50)

data_mean_kinder <- school_districts_map %>% 
  left_join(risk_eval_kindergarten) %>%
  filter(metric=="total_infected_mean_per_100") %>% #arrange(desc(metric_average_school_size))
  filter(NAME!="ROSCOE COLLEGIATE ISD")

ggplot() + theme_void() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=data_mean_kinder,aes(fill=for500))+ 
  scale_fill_gradient(trans = "log",
                      breaks = my_breaks_1, labels = my_breaks_1,
                      low = "cornsilk1", #"#075AFF",
                      high = "blue4",
                      name="") +
  ggtitle("Average size of outbreak per 100 students - kindergarten")

ggsave(last_plot(),file="map_mean_total_inf_kinder.png",bg="white")

### Seventh grade

risk_eval_clean$ageGroup %>% unique()
risk_eval_seventh<-risk_eval_clean_with_counties %>% 
  filter(ageGroup=="7th Grade")


#Homogenization of names
# to upper all letters in names:
school_districts_map<-school_districts_map %>% select(NAME) %>%
  mutate(NAME=toupper(NAME))
risk_eval_seventh<-risk_eval_seventh %>% 
  select(County,NAME=SchoolDistrict_or_Name,metric,for500) %>%
  mutate(NAME=toupper(NAME))

#There are a 7 districts that are either CSD or CONS CSD or MSD
school_districts_map %>% filter(!str_detect(NAME,"ISD"))
#Many names of schools are not in the map for districts. We need to address this
risk_eval_seventh %>% filter(!str_detect(NAME,"SD"))

#Changing only one case in map data
school_districts_map<-school_districts_map %>% 
  mutate(NAME=NAME %>% str_replace_all(c("CONSOLIDATED"="CONS"))) 

#Changing in risk from CISD to CONS ISD to match with map
risk_eval_seventh<- risk_eval_seventh %>% filter(str_detect(NAME,"SD")) %>%
  mutate(NAME=NAME %>% str_replace_all(c("CISD"="CONS ISD",
                                         "MT-SAGINAW"="MOUNTAIN-SAGINAW",
                                         "FT SAM"="FORT SAM",
                                         "GOLD BURG"="GOLD-BURG",
                                         "CONSOLIDATED"="CONS",
                                         "FT DAVIS"="FORT DAVIS",
                                         "FT HANCOCK"="FORT HANCOCK",
                                         "COUNTY WIDE"="COUNTY-WIDE",
                                         "SCHERTZ-CIBOLO-U CITY ISD"="SCHERTZ-CIBOLO-UNIVERSAL CITY ISD")))

#To add school districts with the same name in different counties
risk_eval_seventh<-risk_eval_seventh %>%
  mutate(NAME=ifelse(NAME=="BIG SANDY ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="CHAPEL HILL ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="CENTERVILLE ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="DAWSON ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="EDGEWOOD ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="HIGHLAND PARK ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="HUBBARD ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="MIDWAY ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="NORTHSIDE ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="VALLEY VIEW ISD",paste0(NAME," (",County,")"),NAME)) %>%
  mutate(NAME=ifelse(NAME=="WYLIE ISD",paste0(NAME," (",County,")"),NAME))

#With this simple initial homogeneization of names we have:
#setdiff(school_districts_map$NAME,risk_eval_kindergarten$NAME)

#There are 24 ISDs that are in the risk file, but not in the school map
setdiff(risk_eval_seventh$NAME,school_districts_map$NAME)

risk_eval_seventh$metric %>% unique()

data_prob_seventh<-school_districts_map %>% 
  left_join(risk_eval_seventh) %>%
  filter(metric=="probability_exceeding_10_total_cases") %>%
  filter(NAME!="ROSCOE COLLEGIATE ISD")

ggplot() + theme_void() + 
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=data_prob_seventh,aes(fill=for500)) + 
  scale_fill_gradient(low = "cornsilk1",high = "blue4",name="") +
  ggtitle("Probability of outbreaks larger 10 cases - 7th grade")

ggsave(last_plot(),file="map_prob_exceeding10_seventh.png",bg="white")

my_breaks = c(0.1,1,10,30)

data_mean_seventh<-school_districts_map %>% 
  left_join(risk_eval_seventh) %>%
  filter(metric=="total_infected_mean_per_100") %>% #arrange(desc(metric_average_school_size))
  filter(NAME!="ROSCOE COLLEGIATE ISD")

ggplot() + theme_void() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=data_mean_seventh,aes(fill=for500))+
  scale_fill_gradient(trans = "log",
                      breaks = my_breaks, labels = my_breaks,
                      low = "cornsilk1", #"#075AFF",
                      high = "blue4",
                      name="") +
  ggtitle("Average size of outbreak per 100 students- 7th grade")

ggsave(last_plot(),file="map_mean_total_inf_seventh.png",bg="white")

# Schools with location in map

schools2023$USER_Sch_1 %>% unique() %>% sort() %>% head(30)

risk_non_SD_schools<-risk_eval_clean_with_counties %>% 
  filter(!grepl("SD$", SchoolDistrict_or_Name))  #all names that end with SD

#Location of schools that are not an SD
non_SD_schools_location<-schools2023 %>% 
  select(County=Subregion,Grade_range=USER_Grade,School_enroll_2023=USER_Sch15,
         School_Type=School_Typ,District_ISD=USER_Dis_1,
         School_name=USER_Sch_1) %>%
  filter(School_enroll_2023>0) %>% select(-School_enroll_2023) %>%
  mutate(County=County %>% str_remove_all(" County")) %>% arrange(School_name) %>%
  filter(!grepl("SD$", District_ISD)) %>%  #all names that end with SD
  drop_na()

risk_non_SD_schools$ageGroup %>% unique()
risk_non_SD_schools %>% filter(ageGroup=="7th Grade")

#I will assume that schools are located in the same place, regardless it
#it is kindergarten or 7th grade.
test_location_schools<-non_SD_schools_location %>% ungroup() %>% 
  select(County,District_ISD,School_name) %>%
  arrange(District_ISD) %>% select(County,District_ISD) %>% 
  group_by(County,District_ISD) %>% slice(1) %>% arrange(District_ISD) %>%
  mutate(County=toupper(County))

test_location_schools %>% as_tibble() %>% ungroup() %>%
  select(District_ISD) %>% group_by(District_ISD) %>% count() %>% arrange(desc(n))

test_location_schools %>% filter(str_detect(District_ISD,"PREMIER HIGH SCHOOLS"))
risk_non_SD_schools %>% filter(str_detect(SchoolDistrict_or_Name,"PREMIER HIGH SCHOOLS"))

ver_kinder<-test_location_schools %>%
  left_join(risk_non_SD_schools %>% rename_at("SchoolDistrict_or_Name",~"District_ISD") %>% 
              filter(ageGroup=="Kindergarten") %>%
              filter(metric=="probability_exceeding_10_total_cases")) %>%
  drop_na()

ver_seventh<-test_location_schools %>%
  left_join(risk_non_SD_schools %>% rename_at("SchoolDistrict_or_Name",~"District_ISD") %>% 
              filter(ageGroup=="7th Grade") %>%
              filter(metric=="probability_exceeding_10_total_cases")) %>%
  drop_na()


kinder<-ggplot()+
  theme_void() + 
  geom_sf(data=school_districts_map_original,fill="white",color="gray")+
  geom_sf(data=ver_kinder,aes(alpha=metric_average_school_size))

seventh<-ggplot()+
  theme_void() + 
  geom_sf(data=school_districts_map_original,fill="white",color="gray")+
  geom_sf(data=ver_seventh,aes(alpha=metric_average_school_size))

library(cowplot)

plot_grid(kinder,seventh)
1+1
