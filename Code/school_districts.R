library(tidyverse)
#library(tidycensus)
#library(readxl)
library(sf)

setwd("~/Documents/GitHub/Measles_in_Texas/")

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

#risk_non_SD_schools

risk_non_SD_schools <- risk_eval_clean_with_counties %>%
  filter(!grepl("SD$", SchoolDistrict_or_Name),metric=="probability_exceeding_10_total_cases") %>%
  drop_na()

# risk_eval_clean<-prueba %>% 
#   select(SchoolDistrict_or_Name=`School District or Name`,metric,
#          average_school_size,metric_average_school_size,ageGroup=`Age Group`)

risk_eval_kindergarten<-risk_eval_clean_with_counties %>% 
  filter(ageGroup=="Kindergarten")

#Homogenization of names
# to upper all letters in names:
school_districts_map<-school_districts_map_original %>% select(NAME) %>%
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

#There are 3 ISDs that are in the risk file, but not in the school map
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

schools2023<-read_sf("Data/Schools_2023_to_2024/Schools_2023_to_2024.shp")
schools2023 %>% glimpse()

#This is only to show that we have this data for 2023-2024
# data_enrollment_schools<-schools2023 %>% 
#   select(County=Subregion,Grade_range=USER_Grade,School_enroll_2023=USER_Sch15,
#          School_Type=School_Typ,District_ISD=USER_Dis_1,District_enroll_2023=USER_Dis16,
#          School_name=USER_Sch_1) %>%
#   filter(School_enroll_2023>0) %>% mutate(County=County %>% str_remove_all(" County")) %>%
#   mutate(County=toupper(County),District_ISD=toupper(District_ISD),School_name=toupper(School_name)) %>%
#   arrange(District_ISD) %>% drop_na() %>% unique()
# 
# write_csv(data_enrollment_schools,file="data_enrollment_schools.csv")

schools2023$School_Typ %>% unique()
schools2023$Type %>% unique()

all_charter_schools<-schools2023 %>% as_tibble() %>%
  select(County=Subregion,Grade_range=USER_Grade,School_enroll_2023=USER_Sch15,School_status=USER_Sch16,
         School_Type=School_Typ,District_ISD=USER_Dis_1,District_enroll_2023=USER_Dis16,
         School_number=USER_Schoo,School_name=USER_Sch_1) %>%
  filter(!grepl("SD$", District_ISD)) %>% filter(School_enroll_2023>0) %>% 
  select(County,District_ISD,School_name,Grade_range,School_enroll_2023) %>% mutate(County=County %>% str_remove_all(" County")) %>%
  mutate(County=toupper(County),District_ISD=toupper(District_ISD),School_name=toupper(School_name)) %>%
  arrange(District_ISD) %>% drop_na() %>% unique()

write_csv(all_charter_schools,file="all_charter_schools.csv")

#This includes all schools that "might" have kindergarten's
#Also, I am using district and name of school columns, since the risk data
#mixes both in the name column
charterSchools_kinder<-schools2023 %>% 
  filter(School_Typ %in% c("Elementary School","Elementary/Secondary")) %>%
  select(County=Subregion,Grade_range=USER_Grade,School_enroll_2023=USER_Sch15,School_status=USER_Sch16,
         School_Type=School_Typ,District_ISD=USER_Dis_1,District_enroll_2023=USER_Dis16,
         School_number=USER_Schoo,School_name=USER_Sch_1) %>%
  #filter(School_enroll_2023>0) %>% 
  filter(!grepl("SD$", District_ISD)) %>% 
  select(County,District_ISD,School_name,School_enroll_2023) %>% mutate(County=County %>% str_remove_all(" County")) %>%
  mutate(County=toupper(County),District_ISD=toupper(District_ISD),ageGroup="Kindergarten",School_name=toupper(School_name)) %>%
  arrange(District_ISD) %>% drop_na() %>% unique()

## This is for the last part: schools with enrollment
# 
# privateSchools_kinder_with_enrollment<-schools2023 %>% 
#   filter(School_Typ %in% c("Elementary School","Elementary/Secondary")) %>%
#   select(County=Subregion,Grade_range=USER_Grade,School_enroll_2023=USER_Sch15,School_status=USER_Sch16,
#          School_Type=School_Typ,District_ISD=USER_Dis_1,District_enroll_2023=USER_Dis16,
#          School_number=USER_Schoo,School_name=USER_Sch_1) %>%
#   #filter(School_enroll_2023>0) %>% 
#   filter(!grepl("SD$", District_ISD)) %>% 
#   select(County,District_ISD,School_name,School_enroll_2023) %>% mutate(County=County %>% str_remove_all(" County")) %>%
#   mutate(County=toupper(County),District_ISD=toupper(District_ISD),ageGroup="Kindergarten",School_name=toupper(School_name)) %>%
#   arrange(District_ISD) %>% drop_na() %>% unique()
# 
##

charterSchools_kinder %>% pull(District_ISD) %>% table() %>% as_tibble() %>%arrange(desc(n))

charterSchools_kinder %>% filter(str_detect(District_ISD,"YES PREP PUBLIC")) %>% print(n=21)
charterSchools_kinder %>% filter(str_detect(District_ISD,"IDEA PUBLIC SCHOOLS"),County=="HIDALGO")

risk_eval_clean_with_counties %>% filter(str_detect(SchoolDistrict_or_Name,"YES PREP PUBLIC")) %>%
  filter(metric=="probability_exceeding_10_total_cases",ageGroup=="Kindergarten")
risk_eval_clean_with_counties %>% filter(str_detect(SchoolDistrict_or_Name,"IDEA PUBLIC SCHOOLS")) %>%
  filter(metric=="probability_exceeding_10_total_cases",ageGroup=="Kindergarten")

#Location of all charter school that "might" have kindergarten students
ggplot() + theme_void() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=charterSchools_kinder) +
  ggtitle("Location of private schools - kindergarten (654)")

ggsave(last_plot(),file="private_kinder.png")  
#This includes all middle/junior/secondary that have a 7th grade
seventh_stu_1<-schools2023 %>% filter(School_Typ %in% c("Middle School","Junior High School","Elementary/Secondary")) %>%
  select(County=Subregion,Grade_range=USER_Grade,District_ISD=USER_Dis_1,School_name=USER_Sch_1,
         School_enroll_2023=USER_Sch15) %>%
  filter(!Grade_range %in% c("06","08-09","08","PK 09-12","EE 09-12")) %>%
  filter(!grepl("SD$", District_ISD))
  
#This includes all high schools with 7th grade
seventh_stu_2<-schools2023 %>% filter(School_Typ=="High School") %>%
  select(County=Subregion,Grade_range=USER_Grade,District_ISD=USER_Dis_1,School_name=USER_Sch_1,
         School_enroll_2023=USER_Sch15) %>%
  filter(str_detect(Grade_range,"07")) %>% filter(!grepl("SD$", District_ISD))

#This includes all private schools that may contain 7th grade students
charterSchools_seventh<-bind_rows(seventh_stu_1,seventh_stu_2) %>%
  select(County,District_ISD,School_name,School_enroll_2023) %>% mutate(County=County %>% str_remove_all(" County")) %>%
  mutate(County=toupper(County),District_ISD=toupper(District_ISD),School_name=toupper(School_name),ageGroup="7th Grade") %>% 
  drop_na() %>% unique()

#Location of all private school that 7th grade
ggplot() + theme_void() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=charterSchools_seventh) +
  ggtitle("Location of private schools - 7th grade (399)")

ggsave(last_plot(),file="private_seventh.png")  

#Name matching tests

#In the risk data, the field with names is either the name of the school or
#the district. Hence, I am merging with both columns independently and then 
#binding rows and keeping unique rows

#Merging with District
by_district_kinder<-charterSchools_kinder %>% select(-School_name) %>% 
  mutate(countyAndName=paste0(County,"-",District_ISD)) %>%
  group_by(countyAndName) %>% slice(1) %>% arrange(District_ISD)
#Merging with school name
by_school_kinder<-charterSchools_kinder %>% select(-District_ISD) %>% 
  mutate(countyAndName=paste0(County,"-",School_name)) %>%
  group_by(countyAndName) %>% slice(1) %>% arrange(School_name)

estos_risk_kinder <- risk_non_SD_schools %>% filter(ageGroup=="Kindergarten") %>%
  filter(metric=="probability_exceeding_10_total_cases") %>%
  select(-metric,-ageGroup) %>%
  mutate(countyAndName=paste0(County,"-",SchoolDistrict_or_Name)) %>% unique()

ver_1<-by_district_kinder %>% 
  merge(estos_risk_kinder %>% select(for500,countyAndName), by="countyAndName") %>%
  rename_at("District_ISD",~"NAME")
ver_2<-by_school_kinder %>% 
  merge(estos_risk_kinder %>% select(for500,countyAndName), by="countyAndName") %>%
  rename_at("School_name",~"NAME")

bind_rows(ver_1,ver_2) %>% pull(countyAndName) %>% table() %>% as_tibble() %>% arrange(desc(n)) %>% print(n=40)

all_charter_schools_kinder<-bind_rows(ver_1,ver_2) %>% unique() 

ggplot() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=data_prob_kinder,aes(fill=100*for500)) + theme_void() + 
  scale_fill_distiller(palette = "PuBuGn", direction = 1,name="") +
  geom_sf(data=all_charter_schools_kinder %>% filter(for500>0,School_enroll_2023>0) %>% arrange((for500)),
          aes(color=100*for500,size=School_enroll_2023),shape=1,stroke=1)+
  scale_color_gradientn(
    colors = c("#91cf60", "#d9ef8b", "#ffb347", "#990000", "#660000"), 
    name = ""
  ) +
  labs(
    title = "Texas School District Measles Outbreak Risks – Kindergarten",
    subtitle = "Colors indicate the probability that a single measles introduction will spark an outbreak with at least 10 additional infections.\nShaded regions correspond to public school districts (white regions are districts without vaccination rate); circles correspond to \nindividual charter schools, with diameters proportional to 2023 school enrollments."
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face="italic",size = 12, margin = margin(t = 5, b = 10)),
    legend.position = "right"
  ) + guides(
    size = guide_legend(title = "Enrollment", order = 1),   # Enrollment on top
    fill = guide_colorbar(order = 2)                        # Fill legend comes after
  )

ggsave(last_plot(),file="map_Kinder_with_charter_with_size.png",bg="white")

all_charter_schools_kinder %>% as_tibble() %>% select(NAME) %>% group_by(NAME) %>% count() %>% arrange(desc(n))

all_charter_schools_kinder %>% filter(for500>0,School_enroll_2023>0) %>%
  ggplot(aes(x=School_enroll_2023)) + geom_histogram() + theme_bw()

ggsave(last_plot(),file="histogram_size.png")

data_prob_kinder %>% arrange(desc(for500))
all_charter_schools_kinder %>% arrange(desc(for500))
#### For 7th grade

#Merging with District
by_district_seventh<-charterSchools_seventh %>% select(-School_name) %>% 
  mutate(countyAndName=paste0(County,"-",District_ISD)) %>%
  group_by(countyAndName) %>% slice(1) %>% arrange(District_ISD) %>% unique()

#Merging with school name
by_school_seventh<-charterSchools_seventh %>% select(-District_ISD) %>% 
  mutate(countyAndName=paste0(County,"-",School_name)) %>%
  group_by(countyAndName) %>% slice(1) %>% arrange(School_name) %>% unique()

estos_risk_seventh <- risk_non_SD_schools %>% filter(ageGroup=="7th Grade") %>%
  filter(metric=="probability_exceeding_10_total_cases") %>%
  select(-metric,-ageGroup) %>%
  mutate(countyAndName=paste0(County,"-",SchoolDistrict_or_Name)) %>% unique()

ver_1_seventh<-by_district_seventh %>% 
  merge(estos_risk_seventh %>% select(for500,countyAndName), by="countyAndName") %>%
  rename_at("District_ISD",~"NAME")
ver_2_seventh<-by_school_seventh %>% 
  merge(estos_risk_seventh %>% select(for500,countyAndName), by="countyAndName") %>%
  rename_at("School_name",~"NAME")

bind_rows(ver_1_seventh,ver_2_seventh) %>% pull(countyAndName) %>% table() %>% as_tibble() %>% arrange(desc(n)) %>% print(n=40)

all_charter_schools_seventh<-bind_rows(ver_1_seventh,ver_2_seventh) %>% unique() 

ggplot() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=data_prob_seventh,aes(fill=100*for500)) + theme_void() + 
  scale_fill_distiller(palette = "PuBuGn", direction = 1,name="") +
  geom_sf(data=all_charter_schools_seventh %>% filter(for500>0,School_enroll_2023>0) %>% arrange((for500)),
          aes(color=100*for500,size=School_enroll_2023),shape=1,stroke=1)+
  scale_color_gradientn(
    colors = c("#91cf60", "#d9ef8b", "#ffb347", "#990000", "#660000"), 
    name = ""
  ) +
  labs(
    title = "Texas School District Measles Outbreak Risks – 7th grade",
    subtitle = "Colors indicate the probability that a single measles introduction will spark an outbreak with at least 10 additional infections.\nShaded regions correspond to public school districts (white regions are districts without vaccination rate); circles correspond to \nindividual charter schools, with diameters proportional to 2023 school enrollments."
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face="italic",size = 12, margin = margin(t = 5, b = 10)),
    legend.position = "right"
  ) + guides(
    size = guide_legend(title = "Enrollment", order = 1),   # Enrollment on top
    fill = guide_colorbar(order = 2)                        # Fill legend comes after
  )

ggsave(last_plot(),file="map_Seventh_with_charter_with_size.png",bg="white")

## Now, these here is for private schools

#I use the data from the definite tables already
prueba_kinder <- read_csv("Data/Tables_definite/MeaslesElementarySchoolsOutbreakProbability.csv")
prueba_seventh <- read_csv("Data/Tables_definite/MeaslesMiddleSchoolsOutbreakProbability.csv")

prueba_kinder %>% filter(County=="Houston",`School type`=="Private") %>% select(`Name of school or school district`,County)
all_charter_schools %>% filter(County=="DALLAS",District_ISD=="A+ ACADEMY")

#I am adding a _1 to the name to keep the other names unchanged. Update later
risk_eval_kindergarten_1<-prueba_kinder %>% 
  select(County,SchoolType=`School type`,NAME=`Name of school or school district`,
         size500) %>% mutate(across(where(is.character), toupper)) %>% filter(SchoolType=="PRIVATE")

risk_eval_seventh_1<-prueba_seventh %>% 
  select(County,SchoolType=`School Type`,NAME=`Name of school or school district`,
         size500) %>% mutate(across(where(is.character), toupper)) %>% filter(SchoolType=="PRIVATE")

# Schools with location in map
#Location of private schools in Texas for school year 2021-2022
#Extracted from https://catalog.data.gov/dataset/private-school-locations-current-f7d96
private_schools<-read_sf("Data/Private_School_Locations_-_Current/Private_School_Locations_-_Current.shp")

private_schools_TX<-private_schools %>% filter(STATE=="TX") %>% 
  select(County=NMCNTY,NAME) %>% unique() %>% arrange(NAME) %>% drop_na() %>%
  mutate(County=County %>% str_remove_all(" County")) %>%
  mutate(across(where(is.character), toupper)) %>% unique()

#According to google, this school 262866927 is called St. John's School
private_schools_TX<-private_schools_TX %>% 
  mutate(NAME=NAME %>% str_replace_all(c("262866927"="ST JOHN'S SCHOOL")))

private_schools_TX_names_and_counties <- private_schools_TX %>% as_tibble() %>%
  select(County,NAME)

write_csv(private_schools_TX_names_and_counties,file="private_schools_TX_names_and_counties.csv")

private_schools_kinder_TX_with_risk<-private_schools_TX %>%
  left_join(risk_eval_kindergarten_1) %>% drop_na()

ggplot() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=data_prob_kinder,aes(fill=100*for500)) + theme_void() + 
  scale_fill_distiller(palette = "PuBuGn", direction = 1,name="") +
  geom_sf(data=private_schools_kinder_TX_with_risk %>% filter(size500>0) %>% arrange((size500)),
          aes(color=size500),shape=1,stroke=1,size=3)+
  scale_color_gradientn(
    colors = c("#91cf60", "#d9ef8b", "#ffb347", "#990000", "#660000"), 
    name = ""
  ) +
  labs(
    title = "Texas School District Measles Outbreak Risks – Kindergarten",
    subtitle = "Colors indicate the probability that a single measles introduction will spark an outbreak with at least 10 additional infections.\nShaded regions correspond to public school districts (white regions are districts without vaccination rate); circles correspond to \nindividual private schools."
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face="italic",size = 12, margin = margin(t = 5, b = 10)),
    legend.position = "right"
  ) + guides(
    fill = guide_colorbar(order = 1)                          # Fill legend comes after
  )

ggsave(last_plot(),file="map_Kinder_with_private_with_size.png",bg="white")

private_schools_seventh_TX_with_risk<-private_schools_TX %>%
  left_join(risk_eval_seventh_1) %>% drop_na()

ggplot() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=data_prob_seventh,aes(fill=100*for500)) + theme_void() + 
  scale_fill_distiller(palette = "PuBuGn", direction = 1,name="") +
  geom_sf(data=private_schools_seventh_TX_with_risk %>% arrange((size500)),
          aes(color=size500),shape=1,stroke=1,size=3)+
  scale_color_gradientn(
    colors = c("#91cf60", "#d9ef8b", "#ffb347", "#990000", "#660000"), 
    name = ""
  ) +
  labs(
    title = "Texas School District Measles Outbreak Risks – 7th grade",
    subtitle = "Colors indicate the probability that a single measles introduction will spark an outbreak with at least 10 additional infections.\nShaded regions correspond to public school districts (white regions are districts without vaccination rate); circles correspond to \nindividual private schools."
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(face="italic",size = 12, margin = margin(t = 5, b = 10)),
    legend.position = "right"
  ) + guides(
    fill = guide_colorbar(order = 1)                          # Fill legend comes after
  )

ggsave(last_plot(),file="map_Seventh_with_private_with_size.png",bg="white")

### Including both charter and private schools

private_schools_TX<-private_schools %>% filter(STATE=="TX") %>% 
  select(County=NMCNTY,NAME) %>% unique() %>% arrange(NAME) %>% drop_na() %>%
  mutate(County=County %>% str_remove_all(" County")) %>%
  mutate(across(where(is.character), toupper)) %>% unique()

#According to google, this school 262866927 is called St. John's School
private_schools_TX<-private_schools_TX %>% 
  mutate(NAME=NAME %>% str_replace_all(c("262866927"="ST JOHN'S SCHOOL")))

private_schools_TX_names_and_counties <- private_schools_TX %>% as_tibble() %>%
  select(County,NAME)

write_csv(private_schools_TX_names_and_counties,file="private_schools_TX_names_and_counties.csv")

data_private_schools_kinder<-private_schools_kinder_TX_with_risk %>% select(-SchoolType)
data_charter_schools_kinder<-all_charter_schools_kinder %>% select(County,NAME,size500=for500) %>%
  mutate(size500=100*size500)

data_charter_schools_kinder <- st_transform(data_charter_schools_kinder, st_crs(data_private_schools_kinder))

charter_and_private_kinder<- bind_rows(data_private_schools_kinder %>% mutate(Type="Private"),
                                       data_charter_schools_kinder %>% mutate(Type="Charter")) %>% unique()

ggplot() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=data_prob_kinder,aes(fill=100*for500)) + theme_void() + 
  scale_fill_distiller(palette = "PuBuGn", direction = 1,name="",
                       breaks = c(0, 20, 40, 60, 80),labels = c("0%", "20%", "40%", "60%", "80%")) +
  geom_sf(data=charter_and_private_kinder %>% arrange((size500)),
          aes(color=size500,shape=Type,size=Type),stroke=1)+
  scale_color_gradientn(
    colors = c("#91cf60", "#d9ef8b", "#ffb347", "#990000", "#660000"), 
    name = "", breaks = c(0, 20, 40, 60, 80),
    labels = c("0%", "20%", "40%", "60%", "80%")
  ) +
  scale_shape_manual(name="",values = c("Private" = 2, "Charter" = 1)) +  # 2 = open triangle, 1 = open circle
  scale_size_manual(values = c(4,3))+
  labs(
    title = "Measles Outbreak Risk Map – Texas Schools (Kindergarten)",
    subtitle = "Colors indicate the probability that a single measles case will lead to an outbreak of at least 10 additional infections, \nbased on reported kindergarten MMR vaccination rates for the 2023–2024 school year. Shaded regions represent \npublic school districts; circles and triangles represent charter and private schools, respectively. Districts shown in white \ndid not report immunization rates."
    #subtitle = "Colors indicate the probability that a single measles introduction will spark an outbreak with at least 10 additional infections.\nShaded regions correspond to public school districts (white regions are districts without vaccination rate)."
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
#    plot.subtitle = element_text(face="italic",size = 12, margin = margin(t = 5, b = 10)),
    plot.subtitle = element_text(size = 12, margin = margin(t = 5, b = 10)),
    legend.position = "right"
  ) + guides(
    shape = guide_legend(override.aes = list(size = 4), order = 1),  # Bigger shape legend
    #shape = guide_legend(order = 1),
    fill = guide_colorbar(order = 2),                          # Fill legend comes after
    size = "none"
  )

ggsave(last_plot(),file="map_Kinder_with_private_with_size.png",bg="white")

private_schools_seventh_TX_with_risk<-private_schools_TX %>%
  left_join(risk_eval_seventh_1) %>% drop_na()

data_private_schools_seventh<-private_schools_seventh_TX_with_risk %>% select(-SchoolType)
data_charter_schools_seventh<-all_charter_schools_seventh %>% select(County,NAME,size500=for500) %>%
  mutate(size500=100*size500)

data_charter_schools_seventh <- st_transform(data_charter_schools_seventh, st_crs(data_private_schools_seventh))

charter_and_private_seventh<- bind_rows(data_private_schools_seventh %>% mutate(Type="Private"),
                                        data_charter_schools_seventh %>% mutate(Type="Charter")) %>% unique()

ggplot() +
  geom_sf(data=school_districts_map_original,fill="white",color="gray") +
  geom_sf(data=data_prob_seventh,aes(fill=100*for500)) + theme_void() + 
  scale_fill_distiller(palette = "PuBuGn", direction = 1,name="",
                       breaks = c(0, 20, 40, 60, 80),labels = c("0%", "20%", "40%", "60%", "80%")) +
  geom_sf(data=charter_and_private_seventh %>% arrange((size500)),
          aes(color=size500,shape=Type,size=Type),stroke=1)+
  scale_color_gradientn(
    colors = c("#91cf60", "#d9ef8b", "#ffb347", "#990000", "#660000"), 
    name = "",
    breaks = c(0, 20, 40, 60, 80),labels = c("0%", "20%", "40%", "60%", "80%")
  ) +
  scale_shape_manual(name="",values = c("Private" = 2, "Charter" = 1)) +  # 2 = open triangle, 1 = open circle
  scale_size_manual(values = c(4,3))+
  labs(
    title = "Measles Outbreak Risk Map – Texas Schools (7th grade)",
    subtitle = "Colors indicate the probability that a single measles case will lead to an outbreak of at least 10 additional infections, \nbased on reported 7th grade MMR vaccination rates for the 2023–2024 school year. Shaded regions represent \npublic school districts; circles and triangles represent charter and private schools, respectively. Districts shown in white \ndid not report immunization rates."
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
#    plot.subtitle = element_text(face="italic",size = 12, margin = margin(t = 5, b = 10)),
    plot.subtitle = element_text(size = 12, margin = margin(t = 5, b = 10)),
    legend.position = "right"
  ) + guides(
    shape = guide_legend(override.aes = list(size = 4), order = 1),  # Bigger shape legend
    #shape = guide_legend(order = 1),
    fill = guide_colorbar(order = 2),                          # Fill legend comes after
    size = "none"
  )

ggsave(last_plot(),file="map_Seventh_with_private_with_size.png",bg="white")






