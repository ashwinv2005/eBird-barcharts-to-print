require(splitstackshape)
require(magrittr)
require(dplyr)
require(tidyverse)
require(reshape2)

indialist = read.csv("eBird_mapping_2023.csv") %>% select(eBird.English.Name.2023,
                                                         eBird.Scientific.Name.2023)

# English India

dat = read.delim("ebird_IN__1900_2024_1_12_barchart_EN-IN.txt", 
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

total_list_IN = dat %>%
  select(COMMON.NAME,SCIENTIFIC.NAME) %>%
  rename(eBird.India.Name.2023 = COMMON.NAME)


# English US

dat = read.delim("ebird_IN__1900_2024_1_12_barchart_EN-US.txt", 
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

total_list_US = dat %>%
  select(COMMON.NAME,SCIENTIFIC.NAME) %>%
  rename(eBird.English.Name.2023 = COMMON.NAME)

total_list = total_list_US %>% left_join(total_list_IN) %>%
  rename(eBird.Scientific.Name.2023 = SCIENTIFIC.NAME)
write.csv(total_list,"India_species_spuhs_slashes_2023.csv", row.names = F)


indialist = indialist %>%
  left_join(total_list)
write.csv(indialist,"eBird_mapping_2023.csv", row.names = F)
