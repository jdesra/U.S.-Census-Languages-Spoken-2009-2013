
# code description --------------------------------------------------------

# cleaning up data on languages spoke at home from US Census 

# This package is for reading in data from excel
# https://urldefense.proofpoint.com/v2/url?u=https-3A__readxl.tidyverse.org_&d=DwIGAg&c=sJ6xIWYx-zLMB3EPkvcnVg&r=RV7qcKi0oWDy7-3ayLH72w&m=EwrI7TK7Jcb5fPr7b0SWctkLYFcl-89yywXqDj_oNvfuDO7PbOz7TDrOoYeTHYSK&s=OmiEDiSwVuPcvGyH3YSx66vi89CFj8JbqCs7R6MJNa0&e= 


# load the libraries ------------------------------------------------------

library(tidyverse)
library(readxl)

us_lang <- read_excel("./data_raw/2009-2013-acs-lang-tables-nation (1) (2).xls")
# read in the raw data  ---------------------------------------------------

# delete the first 7 rows
us_lang<-us_lang %>% slice(8:nrow(us_lang))

# Option 1
# read in the sheet
# us_lang <- read_excel("./data_raw/us_lang.xls")
# add a column with the name of the sheet
names(us_lang)
names(us_lang)
us_lang <- us_lang %>% 
  rename('lang_family'='Table with column headers in row 5 and row headers in column A') %>% #reanme the clumn with the name we want 
  mutate('lang_category'=lang_family, .after='lang_family') %>% #mutate creates a new columns
  mutate('language'=lang_family, .after='lang_category') 


us_lang$lang_family<-tolower(us_lang$lang_family)


# first - tell it to keep the language families in column 1

# second  - tell it to delete the lang_families and languages from column 2

# third  keep only thinkgs NOT in column 1 or 2
# 
# 
# lang_fams<-c(
#   "spanish and spanish creole",
# "other indo-european langauges",
# "asian and pacific island languages",
# "all other languages")

# us_lang <- us_lang %>% mutate(lang_family=if_else(lang_family %in% lang_fams, lang_family,"X"))
# 

us_lang <- us_lang %>% mutate(lang_family=case_when((lang_family == "spanish and spanish creole" |
                                                       lang_family == "other indo-european languages" |
                                                       lang_family == "asian and pacific island languages"|
                                                       lang_family == "all other languages" )~ lang_family))

# '.' (periods) are special characters, so you have 
# to put \\ behind *each one* to find the strings that have it.  
str_detect(us_lang$lang_category, "^\\.\\.+[:alpha:]")

## This will only keep the ones that start with EXACTLY the following: 
# a . followed by any letter 
# \\. = . and [:alpha:] = letter
# that will leave ONLY the ones in the language_category
us_lang <- us_lang %>% mutate(lang_category=case_when(str_starts(us_lang$lang_category,"\\.[:alpha:]")==TRUE~lang_category))

# part 3
# this "detects the strings that have a .. at the beginning" - 
str_detect(us_lang$language, "\\.\\.")

# we want to keep ONLY the ones that are TRUE - they have exactly two dots
# at the beginning. these are the languages that stay in the language column
us_lang <- us_lang %>% mutate(language=case_when((str_detect(us_lang$language, "\\.\\.")==TRUE~language)))

# Then you have to delete the two dots at the start using the gsub command - 
# this substitutes characters EXACTLY as they appear in your command. 
# Don't forget the \\ before each . 
us_lang$language <- gsub("\\.\\.","", us_lang$language)


# OK so what's next? try using gsub to replace the (C) and (B) etc footnotes. 
# The format is like this: us_lang$columname <- gsub("old","new", us_lang$columname)
# to delete the periods in language category:
us_lang$lang_category<- gsub("\\.","", us_lang$lang_category)


# now we have to "fill in " or "copy-paste" the language families

us_lang$lang_family<-as.factor(us_lang$lang_family)

us_lang <- us_lang %>% fill(lang_family,.direction = "down")


# before we fill in column 2, lets delete (slice) awy the first row of the lang_family, which is 
# the totals for the lang_category
# grop by lang_family, then delet the first row
us_lang<-us_lang %>% 
  group_by(lang_family) %>% slice(2:n())

us_lang<-us_lang %>% 
  mutate(lang_category=if_else(lang_family=="spanish and spanish creole","spanish and spanish creole",lang_category))

# tell it to put "spanish and spanish creole" in the top row of column lang_category

us_lang <- us_lang %>% fill(lang_category,.direction = "down")

# copy over the lang_category to language BUT ONLY IF
# there is an NA in Language

us_lang<-us_lang %>% 
  mutate(language=if_else(is.na(language),lang_category,language))

# NEXT
# This is odd, but we need to add a row to spanish (it is the only one where the top line is already deleted)
us_lang<-us_lang %>% filter(lang_family=="spanish and spanish creole" & language=="Spanish") %>% 
  bind_rows(us_lang)
# THIS NEXT PART WILL ELIMINATE THAT FIRST ROW WITH THE TOTALS. CAN YOU FIGURE OUT HOW IT DOES SO? 
us_lang<-us_lang %>% 
  group_by(lang_category) %>% 
  mutate(lang_icat=n(),.after=lang_category) %>% 
  arrange(desc(lang_family))

us_lang_1<-us_lang %>% filter(lang_icat==1) 
us_lang_2<-us_lang %>% 
  filter(lang_icat>1) %>%
  group_by(lang_category) %>% 
  slice(2:n())
us_lang<-bind_rows(us_lang_1,us_lang_2) %>% select(-lang_icat)

# Replace all the footnotes and weird stuff in the columns using either gsub or mutate


# rename columns using "rename")
# file<-file %>% rename(new_name=old_name)
# to get the old names:
names(us_lang)

# delete any sum columns replacing

# add a column called "notes" and put the footnote info back in.



# You wioll have to rename the columns below after the changes I made above...
# rename column headers 
us_lang <- us_lang %>% 
  rename('spkrs'= '...5') 

us_lang <- us_lang %>% 
  rename('spkrs_me'= '...6') 

us_lang <- us_lang %>% 
  rename('spkrs_ltvw'= '...7') 

us_lang <- us_lang %>%
  rename('spkrs_ltvw_me'='...8')


#to duplicate or copy paste a column do the following
#us_lang <- us lang %>% 
#mutate(new_columname=name_of_columbeing_copied)


us_lang <- us_lang %>% 
  mutate(spkrs_notes=if_else(spkrs=="(D)","D",NULL),.after="spkrs_me") %>% 
  mutate(spkrs= na_if(spkrs, "(D)")) %>% 
  mutate(spkrs_me= na_if(spkrs_me, "(D)")) 

names(us_lang)

us_lang <- us_lang %>% 
  mutate(spkrs_ltvw_notes=if_else(spkrs_ltvw=="(D)","D",NULL),.after="spkrs_ltvw_me") %>% 
  mutate(spkrs_ltvw= na_if(spkrs_ltvw, "(D)")) %>% 
  mutate(spkrs_ltvw_me= na_if(spkrs_ltvw_me, "(D)")) 


# TODO: 

# Copy the B over 
# Delete the original (B)
# delete the dashes

# make sure no oother footnotes: final notes (1,. 2, 3, )

us_lang <- us_lang %>% 
  mutate(spkrs_ltvw_notes=if_else(spkrs_ltvw=="(B)","B",NULL),.after="spkrs_ltvw_me") %>% 
  mutate(spkrs_ltvw= na_if(spkrs_ltvw, "(B)")) %>% 
  mutate(spkrs_ltvw_me= na_if(spkrs_ltvw_me, "(B)")) %>%
  mutate(spkrs_ltvw_me=if_else(spkrs_ltvw_notes=="(-/-/)", "NONE",NULL)

us_lang <- us_lang %>% 
  mutate(spkrs_ltvw_notes=if_else(spkrs_ltvw_me=="(-)","N",NULL),.after="spkrs_ltvw_me") %>% 
  mutate(spkrs_ltvw= na_if(spkrs_ltvw, "(N)")) %>% 
  mutate(spkrs_ltvw_me= na_if(spkrs_ltvw_me, "(N)")) 
