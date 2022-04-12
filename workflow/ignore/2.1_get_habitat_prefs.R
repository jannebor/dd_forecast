load("~/GitHub/dd_forecast/dataframes/df_ml")

species<-(unique(df_ml$binomial))

library(rredlist)
iucn_key_n <- "..."

library(doSNOW)
cl <- makeCluster(8)
registerDoSNOW(cl)
iterations <- length(unique(species))
iterations <- 50
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

hab_prefs<-foreach(i = 1:iterations, .combine = "rbind",
                 .options.snow = opts,
                 .packages = c("rredlist")) %dopar% {
                   
                   
                   hab_ls<-NULL
                   t0<-Sys.time()
                   while(length(hab_ls)<1){
                     tryCatch({
                       hab_ls <- rl_habitats(as.character(species[i]), key=iucn_key_n)
                     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                     t1<-Sys.time()
                     print((t1-t0)[[1]])
                     Sys.sleep(2)
                     if((t1-t0)[[1]]>30){
                       break
                     }
                   }
                   
                   
                   
                   hab_ls<-hab_ls$result
                   hab_ls$species<-species[i]
                   if(length(hab_ls)==6){
                     
                     return(hab_ls)  
                     
                   }
                   
                   
                   
                   
                 }

stopCluster(cl)

#save(hab_prefs, file="~/GitHub/dd_forecast/dataframes/hab_prefs")
load("~/GitHub/dd_forecast/dataframes/hab_prefs")

df_ml$n_habitats<-NA
df_ml$n_subhabitats<-NA
df_ml$n_importanthabitats<-NA
df_ml$perc_importanthabitats<-NA
df_ml$habitat1<-NA
df_ml$habitat2<-NA
df_ml$habitat3<-NA
df_ml$habitat4<-NA
df_ml$habitat5<-NA
df_ml$habitat6<-NA
df_ml$habitat7<-NA
df_ml$habitat8<-NA
df_ml$habitat9<-NA
df_ml$habitat10<-NA
df_ml$habitat11<-NA
df_ml$habitat12<-NA
df_ml$habitat13<-NA
df_ml$habitat14<-NA
df_ml$habitat15<-NA
df_ml$habitat16<-NA
df_ml$habitat17<-NA
df_ml$habitat18<-NA


library(stringr)

for (i in 1:length(unique(species))) {
  print(i)
  habs_s<-subset(hab_prefs, hab_prefs$species==unique(species)[i])
  
  
  if(nrow(habs_s)>0){
    df_ml$n_subhabitats[which(df_ml$binomial==unique(species)[i])]<-length(unique(habs_s$code))
    
    habs_s$code<-str_sub(habs_s$code, 1, 4)
    df_ml$n_habitats[which(df_ml$binomial==unique(species)[i])]<-length(unique(as.integer(habs_s$code)))
    
    
    habs_s$code<-as.integer(habs_s$code)
    
    df_ml$n_importanthabitats[which(df_ml$binomial==unique(species)[i])]<-length(unique(habs_s$code[which(habs_s$majorimportance=="Yes")]))
    
    
    if(1 %in% habs_s$code){
      df_ml$habitat1[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat1[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(2 %in% habs_s$code){
      df_ml$habitat2[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat2[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(3 %in% habs_s$code){
      df_ml$habitat3[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat3[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(4 %in% habs_s$code){
      df_ml$habitat4[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat4[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(5 %in% habs_s$code){
      df_ml$habitat5[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat5[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(6 %in% habs_s$code){
      df_ml$habitat6[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat6[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(7 %in% habs_s$code){
      df_ml$habitat7[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat7[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(8 %in% habs_s$code){
      df_ml$habitat8[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat8[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(9 %in% habs_s$code){
      df_ml$habitat9[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat9[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(10 %in% habs_s$code){
      df_ml$habitat10[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat10[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(11 %in% habs_s$code){
      df_ml$habitat11[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat11[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(12 %in% habs_s$code){
      df_ml$habitat12[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat12[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(13 %in% habs_s$code){
      df_ml$habitat13[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat13[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(14 %in% habs_s$code){
      df_ml$habitat14[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat14[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(15 %in% habs_s$code){
      df_ml$habitat15[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat15[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(16 %in% habs_s$code){
      df_ml$habitat16[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat16[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(17 %in% habs_s$code){
      df_ml$habitat17[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat17[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    habs_s$code<-as.integer(habs_s$code)
    if(18 %in% habs_s$code){
      df_ml$habitat18[which(df_ml$binomial==unique(species)[i])]<-"Yes"
    } else {
      df_ml$habitat18[which(df_ml$binomial==unique(species)[i])]<-"No"
    }
    
  }
  
}


df_ml$perc_importanthabitats<-df_ml$n_importanthabitats/df_ml$n_habitats
#save(df_ml,file="~/GitHub/dd_forecast/dataframes/df_ml_v2")

