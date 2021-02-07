
library(tidyr)
library(ggplot2)
library(lubridate)

e_data <- read.delim("C:/Users/Zak/Documents/R/EBD/ebd_US-AK_prv_relJun-2020.txt",
                     header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t")
head(e_data)
edata.s <- e_data %>% select(COUNTY, OBSERVATION.DATE, COMMON.NAME, SCIENTIFIC.NAME, OBSERVATION.COUNT, 
                              CATEGORY, APPROVED, TAXONOMIC.ORDER, OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER) %>%
                      mutate(YEAR = year(e_data$OBSERVATION.DATE))
                  

#Number of checklists submitted per year
checklist <- edata.s %>% 
  group_by(YEAR) %>%
  summarise(CHECKLIST.COUNT = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  filter(YEAR > 2000)

ggplot(checklist, aes(x = YEAR,y = CHECKLIST.COUNT)) +
  geom_bar(stat = "identity", fill = "#004C99") +
  theme_minimal()

#Number of users per year
users <- edata.s %>% 
  group_by(YEAR) %>%
  summarise(USER.COUNT = n_distinct(OBSERVER.ID)) %>%
  filter(YEAR > 2000)

ggplot(users, aes(x = YEAR,y = USER.COUNT)) +
  geom_bar(stat = "identity") +
  theme_minimal()


#Unique users by year

edata.s[with(edata.s, order(OBSERVATION.DATE)), ]
unique.users <- edata.s[!duplicated(edata.s$OBSERVER.ID), ]

unique.users1 <- unique.users %>% 
  group_by(YEAR) %>%
  summarise(USER.COUNT = n_distinct(OBSERVER.ID)) %>%
  filter(YEAR > 2000)

ggplot(unique.users1, aes(x = YEAR,y = USER.COUNT)) +
  geom_bar(stat = "identity") +
  theme_minimal()

#Number of species per year
species <- edata.s %>% 
  mutate(sp.sci.name = sub("^\\s*(\\S+\\s+\\S+).*", "\\1", edata.s$SCIENTIFIC.NAME)) %>%
  filter(APPROVED == 1 & CATEGORY != "slash" & CATEGORY != "spuh" & CATEGORY != "hybrid" & CATEGORY != "domestic") %>%
  group_by(YEAR) %>%
  summarise(SPECIES.COUNT = n_distinct(sp.sci.name)) %>%
  filter(YEAR > 2010)

#% change in checklists last year
checklist.change <- edata.s %>% 
  mutate(sp.sci.name = sub("^\\s*(\\S+\\s+\\S+).*", "\\1", edata.s$SCIENTIFIC.NAME)) %>%
  filter(APPROVED == 1 & CATEGORY != "slash" & CATEGORY != "spuh" & CATEGORY != "hybrid" & CATEGORY != "domestic") %>%
  group_by(YEAR, COUNTY) %>%
  summarise(SPECIES.COUNT = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  filter(YEAR > 2000)
  
  checklist.wide <- checklist.change %>% 
                    filter(YEAR >2017 & YEAR < 2020) %>%
                    pivot_wider(names_from = YEAR, values_from = SPECIES.COUNT, names_prefix = "y") %>%
                    mutate(dif = (as.numeric(y2019) - as.numeric(y2018)) / as.numeric(y2018))
  
ggplot(checklist.wide, aes(x=reorder(COUNTY, dif), y= dif, fill = dif > 0)) +
  geom_bar(stat= "identity") +
  coord_flip() +
  labs(x = "Region", y = "Change in Checklists Submitted (%)",
       title = "Percent Change in Submitted Checklists by Alaska Region",
       subtitles = "2019 to 2020")+
  theme_minimal()+
  scale_fill_manual(values = c("#9E1A00", "#006600")) +
  guides(fill = FALSE)

  