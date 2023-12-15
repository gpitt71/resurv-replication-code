library(data.table)
library(dplyr)

location <- "~/modi_mount/Scoring/Scoring_results"
names <- list.files(location)

tmp <-unlist(lapply(names,function(names){return(substr(names,
       unlist(gregexpr('_', names))[1]+1,
       unlist(gregexpr('_', names))[2]-1))}))

names_relevant <- as.data.table(c(names=names[tmp %in% c("CL",'xgboost','deepsurv','cox')]) )
colnames(names_relevant) = 'org'

names_relevant=names_relevant[,c('model','date') := .(
    substr(org,
       unlist(gregexpr('_', org))[1]+1,
       unlist(gregexpr('_', org))[2]-1),
    as.POSIXct(substr(org,
                unlist(gregexpr('_', org))[2] +1,
                unlist(gregexpr('\\.', org))[1]-1),
                format = "%Y_%m_%d_%H:%M")),by=org]


names_relevant<-names_relevant[complete.cases(names_relevant)] %>%
group_by(model) %>%
filter(date==max(date))

dt = data.table()

for(i in 1:length(names_relevant$org)){

    tmp <- fread(paste0(location,'/',names_relevant$org[i]))
    tmp$model=names_relevant$model[i]
    if(names_relevant$model[i]=="CL"){tmp$crps_result=NA}
    if("V1" %in% colnames(tmp)){tmp=tmp[,-c("V1")]}
    
    dt <- rbind(dt,tmp)


}


dt = dt[, lapply(.SD, mean) , by=c("model", "scenario")]

name = paste0("~/modi_mount/Scoring/Scoring_results",  "/simulations","_",format(Sys.time(), "%Y_%m_%d_%H:%M"),".csv")
fwrite(dt,name)








