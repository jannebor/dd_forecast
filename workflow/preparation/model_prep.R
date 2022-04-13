# get data frames created in 2_data_extraction_batch.R for all spatial data available at IUCN
load("dataframes/full_data/df_ml_v2")

# reclassify to threatened and not threatened species
df_ml$category <- as.character(df_ml$category)
df_ml$category_group <- as.character(df_ml$category)

df_ml$category_group[df_ml$category_group == "LC"] <- "not threatened"
df_ml$category_group[df_ml$category_group == "LR/lc"] <- "not threatened"
df_ml$category_group[df_ml$category_group == "LR/cd"] <- "not threatened"
df_ml$category_group[df_ml$category_group == "NT"] <- "not threatened"
df_ml$category_group[df_ml$category_group == "VU"] <- "threatened"
df_ml$category_group[df_ml$category_group == "EN"] <- "threatened"
df_ml$category_group[df_ml$category_group == "CR"] <- "threatened"
df_ml$category_group[df_ml$category_group == "EW"] <- "threatened"
df_ml$category_group[df_ml$category_group == "EX"] <- "threatened"
df_ml$category_group<-factor(df_ml$category_group, levels = c("not threatened",
                                                              "threatened","DD"))

# check whether assessment needs to be updated
# search results from https://www.iucnredlist.org/search?permalink=6cdddcbe-db29-4169-9160-0dbcc7695a29 (Version 2020-3)
load("dataframes/full_data/rl_needs_update")
df_ml$needs_update<-"No"
df_ml$needs_update[which(df_ml$binomial %in% rl_needs_update$scientificName)]<-"Yes"  


# introduce environmental domains
df_ml$marine<-tolower(df_ml$marine)
df_ml$freshwater<-tolower(df_ml$freshwater)
df_ml$terrestial<-tolower(df_ml$terrestial)
df_ml$compartement<-"Non Marine"
df_ml$compartement[which(df_ml$marine=="true")]<-"Marine"

# create combinations of environmental domains
df_ml$systems<-NA
df_ml$systems[which(tolower(df_ml$marine)=="true" & 
                      tolower(df_ml$freshwater)=="true" &
                      tolower(df_ml$terrestial)=="true")] <- "MFT"
df_ml$systems[which(tolower(df_ml$marine)=="true" & 
                      tolower(df_ml$freshwater)=="true" &
                      tolower(df_ml$terrestial)=="false")] <- "MF"
df_ml$systems[which(tolower(df_ml$marine)=="true" & 
                      tolower(df_ml$freshwater)=="false" &
                      tolower(df_ml$terrestial)=="true")] <- "MT"
df_ml$systems[which(tolower(df_ml$marine)=="true" & 
                      tolower(df_ml$freshwater)=="false" &
                      tolower(df_ml$terrestial)=="false")] <- "M"
df_ml$systems[which(tolower(df_ml$marine)=="false" & 
                      tolower(df_ml$freshwater)=="true" &
                      tolower(df_ml$terrestial)=="true")] <- "FT"
df_ml$systems[which(tolower(df_ml$marine)=="false" & 
                      tolower(df_ml$freshwater)=="true" &
                      tolower(df_ml$terrestial)=="false")] <- "F"
df_ml$systems[which(tolower(df_ml$marine)=="false" & 
                      tolower(df_ml$freshwater)=="false" &
                      tolower(df_ml$terrestial)=="true")] <- "T"

df_ml$systems<-as.factor(df_ml$systems)


# remove if records are duplicated (based on species name & seasonal map)
df_ml<-df_ml[!duplicated(df_ml[c(2,5)]),]

# assign numerics
df_ml$CI_future<-as.numeric(df_ml$CI_future)
df_ml$CI_current<-as.numeric(df_ml$CI_current)
df_ml$countries<-as.numeric(df_ml$countries)
df_ml$observations<-as.numeric(df_ml$observations)
df_ml$obs_cells<-as.numeric(df_ml$obs_cells)
df_ml$range_extent<-as.numeric(df_ml$range_extent)
df_ml$n_habitats<-as.numeric(df_ml$n_habitats)
df_ml$n_subhabitats<-as.numeric(df_ml$n_subhabitats)
df_ml$n_importanthabitats<-as.numeric(df_ml$n_importanthabitats)
df_ml$perc_importanthabitats<-as.numeric(df_ml$perc_importanthabitats)

# assign factors
df_ml$presence<-as.factor(df_ml$presence)
df_ml$origin<-as.factor(df_ml$origin)
df_ml$seasonal<-as.factor(df_ml$seasonal)
df_ml$kingdom<-as.factor(df_ml$kingdom)
df_ml$phylum<-as.factor(df_ml$phylum)
df_ml$class<-as.factor(df_ml$class)
df_ml$order_<-as.factor(df_ml$order_)
df_ml$family<-as.factor(df_ml$family)
df_ml$genus<-as.factor(df_ml$genus)
df_ml$marine<-as.factor(df_ml$marine)
df_ml$terrestial<-as.factor(df_ml$terrestial)
df_ml$freshwater<-as.factor(df_ml$freshwater)
df_ml$habitat1<-as.factor(df_ml$habitat1)
df_ml$habitat2<-as.factor(df_ml$habitat2)
df_ml$habitat3<-as.factor(df_ml$habitat3)
df_ml$habitat4<-as.factor(df_ml$habitat4)
df_ml$habitat5<-as.factor(df_ml$habitat5)
df_ml$habitat6<-as.factor(df_ml$habitat6)
df_ml$habitat7<-as.factor(df_ml$habitat7)
df_ml$habitat8<-as.factor(df_ml$habitat8)
df_ml$habitat9<-as.factor(df_ml$habitat9)
df_ml$habitat10<-as.factor(df_ml$habitat10)
df_ml$habitat11<-as.factor(df_ml$habitat11)
df_ml$habitat12<-as.factor(df_ml$habitat12)
df_ml$habitat13<-as.factor(df_ml$habitat13)
df_ml$habitat14<-as.factor(df_ml$habitat14)
df_ml$habitat15<-as.factor(df_ml$habitat15)
df_ml$habitat16<-as.factor(df_ml$habitat16)
df_ml$habitat17<-as.factor(df_ml$habitat17)
df_ml$habitat18<-as.factor(df_ml$habitat18)


# remove non finite values
for(i in 1:ncol(df_ml)){
  if(!is.character(df_ml[,i])){
    df_ml[which(!is.finite(df_ml[,i])),i]<-NA
    print(i)
  }
}


###
## fill NA's to increase number of complete cases
####
library(Hmisc)
set.seed(5)
for(i in 1:ncol(df_ml)){
  #impute with random values to not introduce any statistically relevant pattern
  if(is.numeric(df_ml[,i])){
    df_ml[,i] <- with(df_ml, impute(df_ml[,i], 'random'))
    df_ml[,i] <- as.numeric(df_ml[,i])
  }
  if(is.integer(df_ml[,i])){
    df_ml[,i] <- with(df_ml, impute(df_ml[,i], 'random'))
    df_ml[,i] <- as.numeric(df_ml[,i])
  }
  if(is.character(df_ml[,i])){
    df_ml[,i] <- with(df_ml, impute(df_ml[,i], 'random'))
    df_ml[,i] <- as.factor(df_ml[,i])
  }
  if(is.factor(df_ml[,i])){
    df_ml[,i] <- with(df_ml, impute(df_ml[,i], 'random'))
    df_ml[,i] <- as.factor(df_ml[,i])
  }
}

# make sure no NA's left
df_ml<-na.exclude(df_ml)

# set aside one version of the data frame for predictions, with only "resident" range maps
df_ml1<-subset(df_ml, df_ml$seasonal==1)
#save(df_ml1, file="~/GitHub/dd_forecast/dataframes/full_data/df_ml1_v2")

# remove extinct and data deficient species from dataset for training/testing
df_ml<-subset(df_ml, df_ml$category!="EX")
df_ml<-subset(df_ml, df_ml$category!="EW")
df_ml<-subset(df_ml, df_ml$category!="DD")

# set correct levels after removal
df_ml$category<-factor(df_ml$category, levels = c("LC","LR/lc","LR/cd","NT","VU","EN","CR"))
df_ml$category_group<-factor(df_ml$category_group, levels = c("not threatened",
                                                              "threatened"))
# remove columns that are not considered as variables
df_ml<-df_ml[c(2:5,16:25,28:ncol(df_ml))]


# DATA SPLIT
###
# create partitions for splitting data equally (taxonomic family and threatened/not threatened) in marine/non marine system
###
part_mar<-subset(df_ml, tolower(df_ml$marine)=="true")
part_terr<-subset(df_ml, tolower(df_ml$marine)=="false")

library(caTools)
## set seed to make partition reproducible
set.seed(5)
# data split for marine species
train_mar<-data.frame()
test_mar<-data.frame()
for(i in 1:length(unique(part_mar$family))){

  # individual data splits per family to assure the classes (threatened/not threatened) are present for each family in training/testing data
  subpart_mar<-subset(part_mar, part_mar$family==unique(part_mar$family)[i])
  
  # removal of outdated assessments if number of remaining records at least 5
  if(length(which(part_mar$class==subpart_mar$class[1] & part_mar$needs_update=="No"))>=5){
    subpart_mar<-subset(subpart_mar, subpart_mar$needs_update=="No")    
  }
  
  if(nrow(subpart_mar)>0){
    subpart_mar1<-subpart_mar[c(1,11,length(subpart_mar)-3)]
    subpart_mar1<-subpart_mar1[!duplicated(subpart_mar1),]
    
    # make split 75/25
    spl = caTools::sample.split(subpart_mar1$category_group, SplitRatio = 0.75)
    subpart_mar2 = subset(subpart_mar1, spl==TRUE)
    subpart_mar3 = subset(subpart_mar1, spl==FALSE)
    
    # divide species based on split
    subtrain_mar = subpart_mar[which(subpart_mar$binomial %in% subpart_mar2$binomial),]
    subtest_mar = subpart_mar[which(subpart_mar$binomial %in% subpart_mar3$binomial),]

    # bind to full marine data frame (training/testing)    
    train_mar<-rbind(train_mar, subtrain_mar)
    test_mar<-rbind(test_mar, subtest_mar)
    
  }
  
}


set.seed(5)
train_terr<-data.frame()
test_terr<-data.frame()
for(i in 1:length(unique(part_terr$family))){
  
  # individual data splits per family to assure the classes (threatened/not threatened) are present for each family in training/testing data
  subpart_terr<-subset(part_terr, part_terr$family==unique(part_terr$family)[i])
  
  # removal of outdated assessments if number of remaining records at least 5
  if(length(which(part_terr$class==subpart_terr$class[1] & part_terr$needs_update=="No"))>=5){
    subpart_terr<-subset(subpart_terr, subpart_terr$needs_update=="No")    
  }
  
  if(nrow(subpart_terr)>0){
    subpart_terr1<-subpart_terr[c(1,11,length(subpart_terr)-3)]
    subpart_terr1<-subpart_terr1[!duplicated(subpart_terr1),]
    
    # make split 75/25
    spl = caTools::sample.split(subpart_terr1$category_group, SplitRatio = 0.75)
    subpart_terr2 = subset(subpart_terr1, spl==TRUE)
    subpart_terr3 = subset(subpart_terr1, spl==FALSE)
    
    # divide species based on split
    subtrain_terr = subpart_terr[which(subpart_terr$binomial %in% subpart_terr2$binomial),]
    subtest_terr = subpart_terr[which(subpart_terr$binomial %in% subpart_terr3$binomial),]

    # bind to full non-marine data frame (training/testing)    
    train_terr<-rbind(train_terr, subtrain_terr)
    test_terr<-rbind(test_terr, subtest_terr)
    
  }
  
}

# remove all outdated assessments in testing data
test_mar<-subset(test_mar, test_mar$needs_update=="No")
test_terr<-subset(test_terr, test_terr$needs_update=="No")

# bind marine/non-marine data to full data for partition 1
train_df<-rbind(train_mar, train_terr)
test_df<-rbind(test_mar, test_terr)

#save(train_terr, file="dataframes/Partition2/train_terr_v2")
#save(test_terr, file="dataframes/Partition2/test_terr_v2")
#save(train_mar, file="dataframes/Partition2/train_mar_v2")
#save(test_mar, file="dataframes/Partition2/test_mar_v2")
#save(train_df, file="dataframes/Partition1/train_df_v2")
#save(test_df, file="dataframes/Partition1/test_df_v2")

