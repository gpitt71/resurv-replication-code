library(data.table)

location <- "~/modi_mount/Scoring/Fitting_results"
names <- list.files(location)

tmp <-unlist(lapply(names,function(names){return(substr(names,
       unlist(gregexpr('_', names))[2]+1,
       unlist(gregexpr('_', names))[3]-1))}))

names_relevant <- as.data.table(c(names=names[tmp %in% "CL"]) )
colnames(names_relevant) = 'org'

names_relevant=names_relevant[,c("seed","date") := list(substr(org,
       unlist(gregexpr('_', org))[1]+1,
       unlist(gregexpr('_', org))[2]-1),
                                     as.POSIXct(substr(org,
                unlist(gregexpr('_', org))[3] +1,
                unlist(gregexpr('\\.', org))[1]-1),
                format = "%Y_%m_%d_%H_%M")),by=org][date==max(date),
                                                    ,
                                                    by=seed]


dt = data.frame()

for(ix in names_relevant$org){

    tmp <- fread(paste0(location,'/',ix))
    
    dt <- rbind(dt,tmp)


}


name = paste0("~/modi_mount/Scoring/Scoring_results",  "/sim_","CL","_",format(Sys.time(), "%Y_%m_%d_%H:%M"),".csv")

colnames(dt)

fwrite(dt,name)