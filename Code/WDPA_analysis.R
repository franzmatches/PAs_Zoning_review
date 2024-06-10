################################################################
## Code for the paper: "Are we zoning out? Biases in the assessment of protected areas zoning and a blueprint for a way forward" 
# by Cerini et al.
####Analysis of data from World Database on Protected Areas ####
################################################################

#packages
library(tidyverse)
library(data.table)
library(openxlsx)

#load data ("WDPA_WDOECM_Sep2023_Public_all_csv.csv"), is put in R.Data file to avoid space issue

load("Data/WDPA_WDOECM_Sep2023_Public_all.RData") 


#select just the areas that have been "Designated" (following "Watson, J et al. 
# The performance and potential of protected areas. Nature 515, 67–73 (2014). https://doi.org/10.1038/nature13947)
wdpa_d_des<-wdpa_d %>% filter(STATUS == "Designated")

#select just the MAB reserve of UNESCO, where occurrence of core, buffer and transition zone is recommended.
tt<-wdpa_d_des %>% filter(DESIG_ENG == "UNESCO-MAB Biosphere Reserve")

table(tt$MARINE)


#create a summarised table with Code country and the ID of each protected areas, associated with possible zones withing the PA (WDPA_PID)
wdpa_d_des_sum<-wdpa_d_des %>% group_by(PARENT_ISO3, WDPAID, WDPA_PID, MARINE) %>% 
  summarise()

#create a table with counts of WPAID per each coutry code (we remove the duplicated rows in case of ZONES within PAs per contry)
table_pa_x_c<-wdpa_d_des_sum[, 1:2] %>% unique() %>% 
  rename(Country_code = PARENT_ISO3) %>% group_by(Country_code) %>% 
  summarise(N = n())


#load country codes datasheet
c_codes<-read.xlsx("Data/WDPA_WDOECM_Sep2023_Public_all_csv/iso_3digit_alpha_country_codes.xlsx", 
                   sheet = "codes",
                   colNames = T) %>% rename(Contry_name = Definition)

#merge the table with the country names, some of them get lost like the code
# (ABNJ) which stays for areas beyond national jurisdiction 
table_pa_c_names<-merge(table_pa_x_c,c_codes,
                        by.x = "Country_code", by.y =  "Code.Value")

#write excel with such table
write.xlsx(table_pa_c_names, file = "Data/table_pa_c_names.xlsx")


#create a table with differentiation between marine and terrestrial PAs, 0 (predominantly or entirely terrestrial), 1 (Coastal: marine and terrestrial), and 2 (predominantly or entirely marine). The value ‘1’ is only used for polygons. We used this information for the section "Environmental and taxonomic biases"
table_mar_ter<-table(unique(wdpa_d_des_sum[, c(1,2,4)])$MARINE) %>% as.data.frame()

combined_table_mar_ter <- table_mar_ter %>%
  mutate(Var1 = ifelse(Var1 %in% c(1, 2), "Comb_coast_mar", as.character(Var1))) %>%
  group_by(Var1) %>%
  summarise(Freq = sum(Freq))


