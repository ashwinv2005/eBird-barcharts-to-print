require(splitstackshape)
require(magrittr)
require(dplyr)
require(tidyverse)
require(reshape2)
require(rmarkdown)

name = "ebird_IN__1800_2024_1_12_barchart.txt"
species = read.csv("ebird_taxonomy_v2023.csv") 

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

species_India = species %>%
  filter(eBird.English.Name.2023 %in% dat$COMMON.NAME)

write.csv(species_India,"species_India.csv",row.names=F)

#################

species_India = read.csv("species_India.csv")
map = read.csv("SoIB_mapping_2024_temp.csv")

new_tax = species_India %>% left_join(map)

write.csv(new_tax,"new_tax.csv",row.names=F)
write.csv(map,"eBird_taxonomy_2022to2023_mapping.csv",row.names=F)

