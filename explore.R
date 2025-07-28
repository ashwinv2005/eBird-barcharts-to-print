library(stringr)

## export each file based on the percentage of species in each and the part in questions
plotbarchart(8,11)

dat <- read.delim("ebird_IN__1900_2022_1_12_barchart.txt", 
                  na.strings = c("NA", "", "null"), 
                  as.is=TRUE, 
                  sep="\t",
                  header = FALSE,
                  quote="")

list = dat$V1

list = list[-c(1:4)]

listcom = str_split(list, "em class")
listcom1 = character(length(listcom))

for (i in 1:length(listcom1))
{
  listcom1[i] = listcom[[i]][1]
}

listcom2 = str_sub(listcom1, end=-4)

list1 = str_split(list, "sci")

list2 = character(length(list1))

for (i in 1:length(list2))
{
  list2[i] = list1[[i]][2]
}

list3 = str_sub(list2, end=-7)
list3 = str_sub(list3, start=3)

csv_upload_specieslist = data.frame(COMMON.NAME = listcom2, SCIENTIFIC.NAME = list3)
write.csv(csv_upload_specieslist, "csv_upload_specieslist.csv", row.names = F)

a = read.csv("fullspecieslist.csv")
list4 = a$eBird.Scientific.Name

b = setdiff(list3,list4)
b = b[!grepl("sp", b)]
b = b[!grepl("/", b)]
b = b[!grepl("x", b)]
b = b[!grepl("Domestic", b)]

