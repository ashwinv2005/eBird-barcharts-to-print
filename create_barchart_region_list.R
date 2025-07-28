require(splitstackshape)
require(magrittr)
require(dplyr)
require(tidyverse)
require(reshape2)
require(rmarkdown)

source("functions.R")

type = "India"
name = "ebird_IN__1900_2025_1_12_barchart.txt"
region = "India"

indialist = read.csv("eBird_mapping_2023.csv") %>% 
  rename(SCIENTIFIC.NAME = eBird.Scientific.Name.2023)

if (type == "India")
  indialist = indialist %>% rename(COMMON.NAME = eBird.India.Name.2023)
if (type == "World")
  indialist = indialist %>% rename(COMMON.NAME = eBird.English.Name.2023)

indialist = indialist %>% dplyr::select(COMMON.NAME,SCIENTIFIC.NAME)

dat <- read.delim(name, 
                  na.strings = c("NA", "", "null"), 
                  as.is=TRUE, 
                  sep="\t",
                  header = FALSE,
                  quote="")

# Extract Sample Size into an array
sample_size <- dat[4,][2:49]
colnames(sample_size) <- 1:48

# Remove first four rows that has no data
dat <- dat[-c(1,2,3,4),]

# Split the species name
dat  <- cSplit(dat, 'V1', sep="=", type.convert=FALSE)
colnames(dat) <- c(1:49,"COMMON.NAME","SCIENTIFIC.NAME")

# Clean the species name
dat <- dat %>% 
  within (COMMON.NAME <- substr(COMMON.NAME,1,nchar(COMMON.NAME)-11)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,1,nchar(SCIENTIFIC.NAME)-6)) %>%
  within (SCIENTIFIC.NAME <- substr(SCIENTIFIC.NAME,7,nchar(SCIENTIFIC.NAME)))

write.csv(data.frame(dat %>% select(COMMON.NAME,SCIENTIFIC.NAME)),
          "English_India_list_Jan2025.csv",row.names = F)

## create barchart

plotbarchart(n.spec.page=15,dat,indialist,region)


## create region list

species_list = indialist %>%
  filter(COMMON.NAME %in% dat$COMMON.NAME)

write.csv(species_list,paste(region,"_region_list.csv",sep=""),row.names = F)
