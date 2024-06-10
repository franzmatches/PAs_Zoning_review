#########################################################
## Code for the paper: "Are we zoning out? Biases in the assessment of protected areas zoning and a blueprint for a way forward" 
# by Cerini et al.
##    Literature research lists and dataframe creation ##
#########################################################

#packages
library(tidyverse)
library(openxlsx)
library(patchwork)
library(data.table)
library(readxl)

#### 1. Original strings ####
##1.1. Load scopus data #####

#take all csv files in the "scopus" folder
files_sc<-fs::dir_ls(path = "Data/Scopus", glob = "*.csv")

# create empty list for the loop
list_sc<- list()

# for loop to paste together all the csv files 

for (i in (1:length(files_sc))) {
  #i = 1
  data_i<-read.csv(files_sc[i])
  # Word to find
  word_to_find <- "scopus"
  # Extract everything after the word using regular expressions
  result <- sub(paste0(".*", word_to_find), "", files_sc[i])
  # Remove the ".csv" extension
  result <- sub("\\.csv$", "", result)
  data_i<-data_i %>% mutate(string = result)
  print(paste((nrow(data_i)), files_sc[i], sep = ";"))
  list_sc[[i]]<-data_i
}

# rdbind the list of file in a single dataframe
scopus_df<-rbindlist(list_sc, fill = T) %>% as.data.table()

# remove all columns with just empty values
scopus_df_clean<-scopus_df[,which(unlist(lapply(scopus_df, function(x)!all(is.na(x))))),with=F]


#### 1.2. Load web of science data ####
#take all xls files in the "wos" folder

files_wos<-fs::dir_ls(path = "Data/wos", glob = "*.xls")

# create empty list for the loop
list_wos<- list()

# for loop to paste together all the csv files 

for (i in (1:length(files_wos))) {
  #i = 1
  data_i<-read_excel(files_wos[i])
  # Word to find
  word_to_find <- "wos"
  # Extract everything after the word using regular expressions
  result <- sub(paste0(".*", word_to_find), "", files_wos[i])
  # Remove the ".csv" extension
  result <- sub("\\.xls$", "", result)
  data_i<-data_i %>% mutate(string = result)
  print(paste((nrow(data_i)), files_wos[i], sep = ";"))
  list_wos[[i]]<-data_i
}

# rdbind the list of file in a single dataframe and add a column to specify that the source is "wos"

wos_df<-rbindlist(list_wos) %>% mutate(Source = "wos") %>% as.data.table()

# remove all columns with just empty values

wos_df_clean<-wos_df[,which(unlist(lapply(wos_df, function(x)!all(is.na(x))))),with=F]

##### 2. Data wrangling to merge the two dataframes ####

# change column names to match the "scopus" dataframe
colnames(scopus_df_clean)
colnames(wos_df_clean)

wos_df_clean<- wos_df_clean %>% rename(Title = `Article Title`,
                          Author.full.names = `Author Full Names`,
                          Year = `Publication Year`)

# find the titles that are repeated in both dataframes and remove them from wos one  
wos_df_new<- anti_join(wos_df_clean, scopus_df_clean, by = "Title") %>% as.data.table()

#find common columns names 
comm_col<-intersect(names(scopus_df_clean), names(wos_df_new))

#combine the two dataframes
all_df<-merge(scopus_df, wos_df_new, 
              by = comm_col
              , all = TRUE)

# remove potential duplicated 
all_df_clean<-subset(all_df, !duplicated(all_df))

# select just variables of interest for this stage to have a cleaner dataset
all_df_clean_print<-all_df_clean %>% select("Authors",
                                            "Author.full.names",
                                            "Title" ,
                                            "Year",
                                            "Abstract",
                                            "Author.Keywords",
                                            "Document.Type",
                                            "Publication Type",
                                            "Conference Title" ,
                                            "Publication Date",
                                            "DOI",
                                            "Source",
                                            "Source.title",
                                            "string")

# use the unique function to remove potential dulicates still there
all_df_clean_print_unique<-unique(all_df_clean_print)

# use regex() to escape special characters in the Title column
title_regex <- regex(all_df_clean_print_unique$Title)


# use grepl() with the escaped regex to search for partial matches
partial_match <- sapply(all_df_clean_print_unique$Title,
                        grepl, 
                        pattern = title_regex)


# remove rows with partial matches
all_df_clean_print_unique_pm<- all_df_clean_print_unique %>%
  filter(!partial_match)



# which classes of Document.type we have?
unique(all_df_clean_print_unique_pm$Document.Type)

# from the columns Document.Type (filled just with scopus), let's select just the contribution types  ""Article", "Review", and the NAs coming from the wos. Additionally, we select just the Publication type "J" of "Journal" and the NAs (as such column is not in the scopus lists)
all_df_clean_print_unique_selection<-all_df_clean_print_unique_pm %>% 
  filter(Document.Type %in% c("Article", "Review", NA)) %>% 
  filter(`Publication Type` %in% c("J", NA))

# check that it worked
unique(all_df_clean_print_unique_selection$Document.Type)
unique(all_df_clean_print_unique_selection$`Publication Type`)

# additional steps to remove replicated
df_clean<-all_df_clean_print_unique_selection
#subset the dataset in those that just have the DOI 
df_doi<-all_df_clean_print_unique_selection %>% 
  filter(!is.na(DOI))

# remove potential replicate based on the DOI
df_doi_clean<- distinct(df_doi, DOI, .keep_all = TRUE)

#subset the dataset in those that don't have the DOI 
df_no_doi<-all_df_clean_print_unique_selection %>% 
  filter(is.na(DOI))

# repaste together the clean doin and nondoi dataframes
df_clean_final<-rbind.data.frame(df_no_doi,df_doi_clean )

#do a last double check on abstract duplicates
df_clean_final_abstract<-distinct(df_clean_final, Abstract, .keep_all = T)%>% 
  filter(!is.na(Abstract))

# create the excel file
# write.xlsx(df_clean_final_abstract, file = "Results/raw_article_list_2nd.xlsx")

##### 3. Redo same corrections for the strings "nature reserve" and "national reserve", added as a second pool of data #####
#### 3.1.load scopus data ####

#take all csv files in the "scopus" folder
files_sc_r<-fs::dir_ls(path = "Data/Scopus/reserves", glob = "*.csv")

# create empty list for the loop
list_sc_r<- list()

# for loop to paste together all the csv files 

for (i in (1:length(files_sc_r))) {
  #i = 1
  data_i<-read.csv(files_sc_r[i])
  # Word to find
  word_to_find <- "scopus"
  # Extract everything after the word using regular expressions
  result <- sub(paste0(".*", word_to_find), "", files_sc_r[i])
  # Remove the ".csv" extension
  result <- sub("\\.csv$", "", result)
  data_i<-data_i %>% mutate(string = result)
  print(paste((nrow(data_i)), files_sc_r[i], sep = ";"))
  list_sc_r[[i]]<-data_i
}

# rdbind the list of file in a single dataframe
scopus_df_r<-rbindlist(list_sc_r, fill = T) %>% as.data.table()

# remove all columns with just empty values
scopus_df_clean_r<-scopus_df_r[,which(unlist(lapply(scopus_df_r, function(x)!all(is.na(x))))),with=F]

#### 3.2. Load web of science data ####

#take all xls files in the "wos" folder

files_wos_r<-fs::dir_ls(path = "Data/wos/reserves", glob = "*.xls")

# create empty list for the loop
list_wos_r<- list()

# for loop to paste together all the csv files 

for (i in (1:length(files_wos_r))) {
  #i = 1
  data_i<-read_excel(files_wos_r[i])
  # Word to find
  word_to_find <- "wos"
  # Extract everything after the word using regular expressions
  result <- sub(paste0(".*", word_to_find), "", files_wos_r[i])
  # Remove the ".csv" extension
  result <- sub("\\.xls$", "", result)
  data_i<-data_i %>% mutate(string = result)
  print(paste((nrow(data_i)), files_wos_r[i], sep = ";"))
  list_wos_r[[i]]<-data_i
}

# rdbind the list of file in a single dataframe and add a column to specify that the source is "wos"

wos_df_r<-rbindlist(list_wos_r) %>% mutate(Source = "wos") %>% as.data.table()

# remove all columns with just empty values

wos_df_clean_r<-wos_df_r[,which(unlist(lapply(wos_df_r, function(x)!all(is.na(x))))),with=F]

##### 3.3. Data wrangling to merge the two dataframes ####
#rematch column names with the Scopus one
wos_df_clean_r<- wos_df_clean_r %>% rename(Title = `Article Title`,
                                       Author.full.names = `Author Full Names`,
                                       Year = `Publication Year`)

# find the titles that are repeated in both dataframes and remove them from wos one  
wos_df_new_r<- anti_join(wos_df_clean_r, scopus_df_clean_r, by = "Title") %>% as.data.table() %>% 
  mutate(Volume = as.character(Volume))

#find common columns names 
comm_cols_r<-intersect(names(scopus_df_clean_r), names(wos_df_new_r))

#combine the two dataframes

all_df_r<-merge(scopus_df_r, wos_df_new_r, 
              by = comm_cols_r
              , all = TRUE)

# remove potential duplicated 
all_df_clean_r<-subset(all_df_r, !duplicated(all_df_r))

# select just variables of interest for this stage to have a cleaner dataset
all_df_clean_print_r<-all_df_clean_r %>% select("Authors",
                                            "Author.full.names",
                                            "Title" ,
                                            "Year",
                                            "Abstract",
                                            "Author.Keywords",
                                            "Document.Type",
                                            "Publication Type",
                                            "Conference Title" ,
                                            "Publication Date",
                                            "DOI",
                                            "Source",
                                            "Source.title",
                                            "string")

# use the unique function to remove potential dulicates still there
all_df_clean_print_unique_r<-unique(all_df_clean_print_r)


# use regex() to escape special characters in the Title column
title_regex_r <- regex(all_df_clean_print_unique_r$Title)


# use grepl() with the escaped regex to search for partial matches
partial_match_r <- sapply(all_df_clean_print_unique_r$Title,
                        grepl, 
                        pattern = title_regex_r)


# remove rows with partial matches

all_df_clean_print_unique_pm_r<- all_df_clean_print_unique_r %>%
  filter(!partial_match_r)


# which classes of Document.type we have?
unique(all_df_clean_print_unique_pm_r$Document.Type)
unique(all_df_clean_print_unique_pm_r$`Publication Type`)

# from the columns Document.Type (filled just with scopus), let's select just the contribution types ""Article", "Review", and the NAs coming from the wos. Additionaly, we select just the Publication type "J"  of "Journal" and the NAs (as such column is not in the scopus lists)
all_df_clean_print_unique_selection_r<-all_df_clean_print_unique_pm_r %>% 
  filter(Document.Type %in% c("Article", "Review", NA)) %>% 
  filter(`Publication Type` %in% c("J", "C", NA))

# check that it worked
unique(all_df_clean_print_unique_selection_r$Document.Type)
unique(all_df_clean_print_unique_selection_r$`Publication Type`)

# additional steps to remove replicated
df_clean_r<-all_df_clean_print_unique_selection_r
#subset the dataset in those that just have the DOI 
df_doi_r<-all_df_clean_print_unique_selection_r %>% 
  filter(!is.na(DOI))

# remove potential replicate based on the DOI
df_doi_clean_r<- distinct(df_doi_r, DOI, .keep_all = TRUE)

#subset the dataset in those that don't have the DOI 
df_no_doi_r<-all_df_clean_print_unique_selection_r %>% 
  filter(is.na(DOI))

# repaste together the clean doi and nondoi dataframes
df_clean_final_r<-rbind.data.frame(df_no_doi_r,df_doi_clean_r)

#do a last double check on abstract duplicates
df_clean_final_abstract_r<-distinct(df_clean_final_r, Abstract, .keep_all = T)%>% 
  filter(!is.na(Abstract))


# # create the excel file
# write.xlsx(df_clean_final_abstract, file = "Results/reserves_list.xlsx")


## rbind the final datasets (the first and second pool of literature review metadata) to create the final dataset on which we performed article selection (Supplementary Table 1)
raw_list_table1<-rbind(df_clean_final_abstract_r, df_clean_final_abstract)
write.xlsx(raw_list_table1, file = "Results/raw_list_SuppTable1.xlsx")

## Please note, the file "raw_list_SuppTable1.xlsx", contains 4 lines that are not in the Table S1, which is used in the code "Selected_studies_analysis.R". This is becasue it contains 3 duplicated studies that were not removed with the above code. In addition, it contains one line with a study without the author names and other metadata, which was removed. This bring the number of studies to the final total of 3783 in the paper. 
