require(splitstackshape)
require(magrittr)
require(dplyr)
require(tidyverse)
require(reshape2)

indialist = read.csv("fullspecieslist.csv") %>% select(1,2)

dat = read.delim("ebird_L2265071__2015_2024_12_2_barchart.txt", 
                  na.strings = c("NA", "", "null"), 
                  as.is=TRUE, 
                  sep="\t",
                  header = FALSE,
                  quote="")
dat = dat[-c(1,2,3,4),]

# Split the species name
dat <- cSplit(dat, 'V1', sep="=", type.convert=FALSE)
colnames(dat) = c(1:49,"COMMON.NAME","SCIENTIFIC.NAME")

# Clean the species name
dat = dat %>% 
  within (COMMON.NAME <- substr(COMMON.NAME,1,nchar(COMMON.NAME)-11)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,1,nchar(SCIENTIFIC.NAME)-6)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,7,nchar(SCIENTIFIC.NAME))) 

species_list1 = indialist %>%
  filter(SCIENTIFIC.NAME %in% dat$SCIENTIFIC.NAME |
           COMMON.NAME %in% dat$COMMON.NAME) %>%
  mutate(site = "Lodhi Gardens")


## site 2

dat = read.delim("ebird_L3915579__2015_2024_12_2_barchart.txt", 
                  na.strings = c("NA", "", "null"), 
                  as.is=TRUE, 
                  sep="\t",
                  header = FALSE,
                  quote="")
dat = dat[-c(1,2,3,4),]

# Split the species name
dat <- cSplit(dat, 'V1', sep="=", type.convert=FALSE)
colnames(dat) = c(1:49,"COMMON.NAME","SCIENTIFIC.NAME")

# Clean the species name
dat = dat %>% 
  within (COMMON.NAME <- substr(COMMON.NAME,1,nchar(COMMON.NAME)-11)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,1,nchar(SCIENTIFIC.NAME)-6)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,7,nchar(SCIENTIFIC.NAME))) 

species_list2 = indialist %>%
  filter(SCIENTIFIC.NAME %in% dat$SCIENTIFIC.NAME |
           COMMON.NAME %in% dat$COMMON.NAME) %>%
  mutate(site = "Sundarbans")



## site 3

dat = read.delim("ebird_L921977__2015_2024_12_2_barchart.txt", 
                 na.strings = c("NA", "", "null"), 
                 as.is=TRUE, 
                 sep="\t",
                 header = FALSE,
                 quote="")
dat = dat[-c(1,2,3,4),]

# Split the species name
dat <- cSplit(dat, 'V1', sep="=", type.convert=FALSE)
colnames(dat) = c(1:49,"COMMON.NAME","SCIENTIFIC.NAME")

# Clean the species name
dat = dat %>% 
  within (COMMON.NAME <- substr(COMMON.NAME,1,nchar(COMMON.NAME)-11)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,1,nchar(SCIENTIFIC.NAME)-6)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,7,nchar(SCIENTIFIC.NAME))) 

species_list3 = indialist %>%
  filter(SCIENTIFIC.NAME %in% dat$SCIENTIFIC.NAME |
           COMMON.NAME %in% dat$COMMON.NAME) %>%
  mutate(site = "Taj Mahal")



## site 4

dat = read.delim("ebird_L1131614__2015_2024_12_2_barchart.txt", 
                 na.strings = c("NA", "", "null"), 
                 as.is=TRUE, 
                 sep="\t",
                 header = FALSE,
                 quote="")
dat = dat[-c(1,2,3,4),]

# Split the species name
dat <- cSplit(dat, 'V1', sep="=", type.convert=FALSE)
colnames(dat) = c(1:49,"COMMON.NAME","SCIENTIFIC.NAME")

# Clean the species name
dat = dat %>% 
  within (COMMON.NAME <- substr(COMMON.NAME,1,nchar(COMMON.NAME)-11)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,1,nchar(SCIENTIFIC.NAME)-6)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,7,nchar(SCIENTIFIC.NAME))) 

species_list4 = indialist %>%
  filter(SCIENTIFIC.NAME %in% dat$SCIENTIFIC.NAME |
           COMMON.NAME %in% dat$COMMON.NAME) %>%
  mutate(site = "Kaziranga")




## site 5

dat = read.delim("ebird_L5560945__2015_2024_12_2_barchart.txt", 
                 na.strings = c("NA", "", "null"), 
                 as.is=TRUE, 
                 sep="\t",
                 header = FALSE,
                 quote="")
dat = dat[-c(1,2,3,4),]

# Split the species name
dat <- cSplit(dat, 'V1', sep="=", type.convert=FALSE)
colnames(dat) = c(1:49,"COMMON.NAME","SCIENTIFIC.NAME")

# Clean the species name
dat = dat %>% 
  within (COMMON.NAME <- substr(COMMON.NAME,1,nchar(COMMON.NAME)-11)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,1,nchar(SCIENTIFIC.NAME)-6)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,7,nchar(SCIENTIFIC.NAME))) 

species_list5 = indialist %>%
  filter(SCIENTIFIC.NAME %in% dat$SCIENTIFIC.NAME |
           COMMON.NAME %in% dat$COMMON.NAME) %>%
  mutate(site = "Bharatpur")


trip_list = indialist %>% 
  left_join(species_list1) %>% rename(site1 = site) %>%
  left_join(species_list2) %>% rename(site2 = site) %>%
  left_join(species_list3) %>% rename(site3 = site) %>%
  left_join(species_list4) %>% rename(site4 = site) %>%
  left_join(species_list5) %>% rename(site5 = site)


trip_list = trip_list %>%
  filter(!is.na(site1) | !is.na(site2) | !is.na(site3) |
           !is.na(site4) | !is.na(site5))


write.csv(trip_list,"Harsha_trip_list.csv",row.names = F)
