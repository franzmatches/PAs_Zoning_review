####################################################################################################
## Code for the paper: "Are we zoning out? Biases in the assessment of protected areas zoning and a blueprint for a way forward" 
# by Cerini et al.
# Selected studies analyses                    
####################################################################################################

#packages
library(tidyverse)
library(data.table)
library(googlesheets4)
library(RColorBrewer)
library(rworldmap)
library(circlize)
library(openxlsx)
library(ggrepel)
library(plotrix)
library(patchwork)
library(ggwordcloud)
library(scales)

#### 1. Connect google sheet file and load data/ or load data from offline tables ####

# data_rev<-read_sheet("https://docs.google.com/spreadsheets/d/1R7gBz8SRkoTMmVyizuPKBHCds_tnm0sLY05pOdEHekk/edit?pli=1#gid=829938918",
#                      sheet = "Direct_list_analysis") 

data_rev<-read.xlsx("Data/Supplementary Tables 1_2_3.xlsx",
                    sheet = "Table S3 Direct_list")

# data_rev_all<-read_sheet("https://docs.google.com/spreadsheets/d/1R7gBz8SRkoTMmVyizuPKBHCds_tnm0sLY05pOdEHekk/edit?pli=1#gid=829938918",
#                          sheet = "raw_list") 

data_rev_all<-read.xlsx("Data/Supplementary Tables 1_2_3.xlsx",
                        sheet = "Table S1 raw list")

#quick count of how many papers per string
# string_tab<-table(data_rev_all$string) %>% as.data.frame()

#### 2. World map plots for PAs and selected studies #### 

#load also data on number of PAs per country (Note, this table is the "table_pa_c_names.xlsx", generated in the "WDPA_analysis.R" code, manually corrected to match WDPA nomenclature of countries with the "rworldmap" nomenclature)
data_pa_c<-read.xlsx("Data/table_pa_c_names_corrected.xlsx")

#add variable with decomposed country name to obtain just the region (e.g. split USA_Virginia)
data_rev <- data_rev %>%
  mutate(Country = str_extract(Location, "^[^_]+")) 

#table with study counts per country
study_counts <- table(data_rev$Country) %>% as.data.frame() %>%
  rename(region = Var1) %>% mutate(region = as.character(region)) %>%
  filter(region != "NA")

#create world map
map.world <- map_data(map="world")

#extract the name of the countries following the rworldmap database
name_countries_map<-unique(map.world$region)

#merge the world map dataframe with the study counts
world_map_studies <- merge(map.world, study_counts, by = "region", all.x = T) %>%
  rename(N_studies = Freq)

#merge the world map dataframe with the PAs counts from other data
world_map_pa<-merge(map.world, data_pa_c, by.x = "region", by.y = "Country_name", all.x = T)

#check 
dplyr::setdiff(unique(map.world$region), data_pa_c$Country_name)
#this are the countries not present in the dataset on protected areas (see code WDPA_analysis.R)

#now back to our list

#check to see if all studies regions from the dataset found a match name in the world map (remember that the only review paper has NAs)
map_no_na<-na.omit(unique(world_map_studies %>% select(c("region", "N_studies"))))

#if not take a look at the not matched names
setdiff(study_counts$region,map_no_na$region)

#plot with color code on regions based on number of studies
map<-ggplot()+
  geom_map(data= world_map_studies 
           # %>% replace_na(list(N_studies = 0))
           ,
           map=map.world,
           aes(map_id=region,
               x=long,
               y=lat,
               fill = N_studies),
           colour="black",
           linewidth =0.25)+
  # scale_fill_gradient(low = "white", high = "#8F443F")+
  scale_fill_viridis_c(na.value = "white",option = "C",
                       breaks = c(min(na.omit(world_map_studies$N_studies)),6, 12,
                                  max(na.omit(world_map_studies$N_studies))))+
  coord_equal()+
  theme_void()+
  theme(legend.position='top')+
  labs(fill = "Number of studies")

ggsave(map, file = "Results/studies_map.png",
       width = 25, height = 15, units ="cm", dpi = 600)

#map with number of PAs per country
map_pas<-ggplot()+
  geom_map(data=world_map_pa, map=map.world,
           aes(map_id=region,
               x=long,
               y=lat,
               fill = log(N+1)),
           colour="black",
           linewidth =0.25)+
  scale_fill_viridis_c(na.value = "white")+
  # scale_fill_gradient(low = "white", high = "#8F443F")+
  coord_equal()+
  theme_void()+
  theme(legend.position='top')+
  labs(fill = "Number of PAs (log scale)")

combined_mapz<-map/map_pas + plot_annotation(tag_levels = 'A')

ggsave(combined_mapz, file = "Results/combined_mapz2_Figure3.png",
       width = 15, height = 15, units ="cm", dpi = 600)


#merge regions with the study counts with the number of PAs for that region
counts_PAs<-merge(study_counts, data_pa_c, by.x = "region", by.y = "Country_name", all.x = T)

# exploratory correlation of number of PAs with number of studies found per country
# 
# ggplot(data = na.omit(counts_PAs),
#        aes(y = log(Freq+1), x = log(N+1)))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   theme_bw()


#### 3. Plot with proportion of marine vs terrestrial studies #### 
table_env<-table(data_rev$environment) %>% as.data.frame() %>% rename(Habitat = Var1)

pie<-ggplot(data=table_env)+
  geom_bar(aes(x="", y=Freq, fill=Habitat), stat="identity", width = 1, alpha = 0.8)+
  coord_polar("y", start=0)+
  scale_fill_manual(values = c("#30ACF0", "#F06430"))+
  # scale_fill_fermenter(palette = "Spectral")+
  theme_void(base_size = 15)


# facultative code for plotting histograms for habitat within marine and terrestrial study  
# marine_hab_table<-as.data.frame(
#   table(
#     unlist(
#       strsplit((
#         data_rev %>% filter(environment == "marine"))$Habitat, split = "&")))) %>% 
#   rename(Habitat = Var1) %>% filter(Habitat != "NA") %>% 
#   mutate(tot = sum(Freq)) %>% mutate(perc = (Freq/tot)*100)
# 
# mar_hist<-ggplot(marine_hab_table, aes(y = reorder(Habitat,perc), x = perc))+
#   geom_bar(stat = "identity",
#            show.legend = FALSE, fill = "#1B9E77") +
#   theme_minimal(base_size = 15)+
#   xlab("%")+
#   ylab(NULL)+
#   ggtitle("Habitat")+
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_y_discrete(labels = wrap_format(30))


# terrest_hab_table<-as.data.frame(
#   table(
#     unlist(
#       strsplit((
#         data_rev %>% filter(environment == "terrestrial"))$Habitat, split = "&")))) %>% 
#   rename(Habitat = Var1) %>% filter(Habitat != "NA") %>% 
#   mutate(tot = sum(Freq)) %>% mutate(perc = (Freq/tot)*100)
# 
# ter_hist<-ggplot(terrest_hab_table, aes(y = reorder(Habitat,perc), x = perc))+
#   geom_bar(stat = "identity",
#            show.legend = FALSE, fill = "#D95F02") +
#   theme_minimal(base_size = 15)+
#   xlab("%")+
#   ylab(NULL)+
#   ggtitle("Habitat")+
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_y_discrete(labels = wrap_format(30))
  

# 
# env_hab_plot<-pie+mar_hist+ter_hist+plot_annotation(tag_levels = 'A')
# 
# env_hab_plot<-wrap_plots(pie,patch)
# ggsave(env_hab_plot, file = "Results/env_hab_plot.png", dpi = 600
#        ,width = 40, height = 20, units = "cm"
#        )

#plot proportion of selected studies actually focusing on effect of zonation
# names(data_rev)
# data_rev$`Direct vs Indirect Zonation focus`
# 
# stats<-ggplot(data_rev, aes(fill = `Direct vs Indirect Zonation focus`))+
#   geom_bar(aes(x = `Direct vs Indirect Zonation focus`,
#                stat = "identity"))+
#   theme_bw()+
#   scale_fill_viridis_d()+
#   theme(axis.title.x = element_blank())
# 
# ggsave(stats, file = "Results/Zonation_focus_hist.png", dpi = 600)



#### 4. Plot time series of the papers frequency ####
years<-ggplot(data_rev, aes(x = Year))+
  geom_bar(fill = "grey",color = "black")+
  # scale_fill_viridis_d()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Publication Year", y = "Selected papers")

# ggsave(years, file = "Results/Years_freq.png", dpi = 600)

years_all<-ggplot(data_rev_all, aes(x = Year))+
  geom_bar(fill = "grey",color = "black")+
  # scale_fill_viridis_d()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(x = "Publication Year", y = "Total papers")

#normalize number of selected papers per years with the total number of papers found that yeat with our keywords 
years_sel_table<-table(data_rev$Year) %>% as.data.frame()

years_all_table<-table(data_rev_all$Year) %>% as.data.frame()

years_table<-na.omit(merge(years_sel_table,years_all_table, by = "Var1", all = T)) %>% 
  mutate(norm_freq = Freq.x/Freq.y) %>% mutate(Years = as.character(Var1))

years_norm<-ggplot(years_table, aes(x = as.numeric(Years), y = norm_freq))+
  geom_bar(fill = "grey",color = "black", stat = "identity")+
  # scale_fill_viridis_d()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(x = "Publication Year", y = "Selected papers/Total papers")

years_comb<-years_all/years_norm+plot_annotation(tag_levels = 'A')

ggsave(years_comb, file = "Results/Years_comb_Figure2.png", 
       width = 15, height = 15, units ="cm", dpi = 600)

#### 5. Plots with frequency of studied taxa ####
#extract the taxa studies 
taxa_list<-na.omit(data_rev$`Taxon(Kingdom_Phylum(Clade)_Class_Family)`)

# unlist the single components (so if one study has more than one the resulting list will treat everything as separated cases)
taxa_list_s<-unlist(strsplit(taxa_list, split = "&"))
taxa_list_s<-strsplit(taxa_list_s, split = "_") 

#take first element of each member of the list (each taxon studied)
first_elements <- lapply(taxa_list_s, "[[", 1)

# Convert the result into a dataframe
df_kingdom <- as.data.frame(matrix(unlist(first_elements),
                                    nrow = length(first_elements), byrow = TRUE)) %>% 
  rename(Kingdom = V1) %>% filter(Kingdom != "NA")

df_kingdom.tb<-table(df_kingdom) %>% as.data.frame() %>% 
  arrange(desc(Freq)) %>% mutate(tot = sum(Freq)) %>% mutate(perc = (Freq/tot)*100)

k_barplot<-ggplot(df_kingdom.tb, aes(y = reorder(Kingdom, perc), x = perc, fill = perc)) +
  scale_fill_distiller(palette = "Spectral")+
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  theme_minimal(base_size = 12)+
  xlab("%")+
  ylab(NULL)

#for loop to extract Phylum table
list_phyla_animals<-list()
list_phyla_plants<-list()
list_phyla_other<-list()



for (i in 1:length(taxa_list_s)) {
  #i = 105
  # length<-length(unlist(taxa_list_s[i]))
  kingdom<-str_replace_all(string=unlist(taxa_list_s[i])[1], pattern=" ", repl="")
  if(kingdom == "Animalia") {
    list_phyla_animals[i] <-unlist(taxa_list_s[i])[2]
  } 
  else if (kingdom == "Plantae") {
    list_phyla_plants[i] = unlist(taxa_list_s[i])[2]
  }
  else if (kingdom != "Animalia" & kingdom != "Plantae") {
    list_phyla_other[i] = unlist(taxa_list_s[i])[1]
  }
}

#create summary dataframes with N and % of studies for class
phyla_table_animals<-as.character(list_phyla_animals) %>% as.data.frame() %>% 
  rename(Phylum = ".") %>% filter(Phylum != "NULL") %>% table() %>% as.data.frame() %>%
  mutate(tot = sum(Freq)) %>% mutate(perc = (Freq/tot)*100)

# write.xlsx(phyla_table_animals, file = "Results/phyla_table_animals.xlsx")

phy_barplot<-ggplot(phyla_table_animals, aes(y = reorder(Phylum, perc), x = perc, fill = perc)) +
  scale_fill_distiller(palette = "Spectral")+
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  theme_minimal(base_size = 12)+
  xlab("%")+
  ylab(NULL)

#for loop to extract class level taxa
list_class_chordata<-list()

for (i in 1:length(taxa_list_s)) {
  #i = 100
  # length<-length(unlist(taxa_list_s[i]))
  kingdom<-str_replace_all(string=unlist(taxa_list_s[i])[1], pattern=" ", repl="")
  if(kingdom == "Animalia") {
    phylum<-str_replace_all(string=unlist(taxa_list_s[i])[2], pattern=" ", repl="")
  } 
  if(phylum == "Chordata") {
    list_class_chordata[i]<-str_replace_all(string=unlist(taxa_list_s[i])[3], pattern=" ", repl="")
  }
}

#create summary dataframes with N of studies for class
class_table_chordata<-table(as.character(list_class_chordata)) %>% as.data.frame() %>% 
  rename(Class = Var1) %>% filter(Class != "NULL")  %>% 
  mutate(tot = sum(Freq)) %>% mutate(perc = (Freq/tot)*100)

#barplot
class_barplot<-ggplot(class_table_chordata, aes(y = reorder(Class, perc), x = perc, fill = perc)) +
  scale_fill_distiller(palette = "Spectral")+
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  theme_minimal(base_size = 12)+
  xlab("%")+
  ylab(NULL)

#combine barplot for Figure 4
total_barplot<-k_barplot+phy_barplot+class_barplot+
  plot_annotation(tag_levels = list(c('B','C','D')))

# plot the habitat pie plot patch
pie_patch<-pie+plot_annotation(tag_levels = list(c('A')))

# Figure 4
final_env_taxa_plot<-pie_patch/total_barplot+plot_annotation(tag_levels = "A")

ggsave(final_env_taxa_plot, file = "Results/taxa_barplot2_Figure4.png",
       width = 25, height = 12, units ="cm", dpi = 600)


#### 6. Plot the parameters studied ####
#create table with the extracted parameter and their frequency of appearance 
parameters_table<-as.data.frame(table(unlist(strsplit(data_rev$study_parameter_new, split = "&")))) %>% 
  rename(parameter = Var1) %>% filter(parameter != "NA") %>% mutate(tot = sum(Freq)) %>% 
  mutate(perc = (Freq/tot)*100)

# par_plot_hist<-ggplot(parameters_table, aes(y = reorder(parameter, perc), x = perc, fill = perc)) +
#   scale_fill_distiller(palette = "Spectral")+
#   geom_bar(stat = "identity",
#            show.legend = FALSE) +
#   theme_minimal(base_size = 12)+
#   xlab("%")+
#   ylab(NULL)


#make word plot (Note, the rendering of the words order in space can be different each time
par_plot_word<-ggplot(parameters_table) + 
  geom_text_wordcloud_area(aes(label = parameter, size = Freq)) +
  scale_size_area(max_size = 45)+
  theme_bw()

ggsave(par_plot_word, file = "Results/parameter_word_plot_Figure5.png", 
       width = 12, height = 8, units ="cm", dpi = 600)
 
#### 7. Plot the summarized effects of zonation in studies (not in the paper) ####
# colnames(data_rev)
# 
# eff_tab<-table(data_rev$Effects_of_zonation) %>% as.data.frame() %>% rename(Effect = Var1) %>% 
#   mutate(sum = sum(Freq)) %>% mutate(perc = (Freq/sum)*100)
# 
# hist_effect<-ggplot(eff_tab, aes(y = reorder(Effect, perc), x = perc, fill = perc)) +
#   scale_fill_distiller(palette = "Spectral")+
#   geom_bar(stat = "identity",
#            show.legend = FALSE) +
#   theme_minimal(base_size = 12)+
#   xlab("%")+
#   ylab(NULL)+
#   ggtitle("Does Zonation have an effect?")
# 
# eff_eval_tab<-table(data_rev$Zonation_effect_evaluation) %>% as.data.frame() %>% rename(EffEval = Var1) %>% 
#   mutate(sum = sum(Freq)) %>% mutate(perc = (Freq/sum)*100)
# 
# eval_effect<-ggplot(eff_eval_tab, aes(y = reorder(EffEval, perc), x = perc, fill = perc)) +
#   scale_fill_distiller(palette = "Spectral")+
#   geom_bar(stat = "identity",
#            show.legend = FALSE) +
#   theme_minimal(base_size = 12)+
#   xlab("%")+
#   ylab(NULL)+
#   ggtitle("What kind of effect?")
# 
# eff_summary_plot<-hist_effect+eval_effect+plot_annotation(tag_levels = 'A')
# ggsave(eff_summary_plot, file = "Results/eff_summary_plot.png",
#        width = 20, height = 10, units ="cm", dpi = 600)
