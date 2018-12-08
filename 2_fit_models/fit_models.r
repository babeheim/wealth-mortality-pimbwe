
# In absence of a principled way to do the CI for both cluster(code) and frailty(Parentcode) simultaneously,
# I'm just going with the former approach - no bootstrapping.

# to do : 
# figure out exactly what the imputation is doing
# figure out exactly how cox ph works
# make sure all labels are leveled properly <- they are in the bivariates, at least
# double-check my model average calculations, and the SE model average calculations
# type up more explicitly what we did

if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

source("./fit_models_support.r")
# seems unnecessary since you don't use the function more than once?

dir_init('./temp')

mice.imp.1 = read.csv("./inputs/imputed_data_1.csv",header=TRUE)
mice.imp.2 = read.csv("./inputs/imputed_data_2.csv",header=TRUE)
mice.imp.3 = read.csv("./inputs/imputed_data_3.csv",header=TRUE)

imp.1.go = data.ordering(mice.imp.1)
imp.2.go = data.ordering(mice.imp.2)
imp.3.go = data.ordering(mice.imp.3)

prelim.models = model.spec()

# imputation 1: 

# preliminary round
prelim.est1 = model.est(prelim.models,imp.1.go)
erm.full(prelim.est1)
prelim.table1 = IC.table(prelim.est1)
contestants1 = contestant.id(prelim.models,prelim.table1,prelim.est1,include.real.est=F)

print(paste0(Sys.time(), " - models for imputation 1 preliminary round complete")) # ~3 minutes
# long, this takes: 

# final round
est1 = model.est(contestants1,imp.1.go)
table1 = IC.table(est1)
est1.avgs = model.avgs(est1,table1)

print(paste0(Sys.time(), " - models for imputation 1 final round complete")) # ~ 30 secs


# imputation 2: 

# preliminary round
prelim.est2 = model.est(prelim.models,imp.2.go)
erm.full(prelim.est2)
prelim.table2 = IC.table(prelim.est2)
contestants2 = contestant.id(prelim.models,prelim.table2,prelim.est2,include.real.est=F)

print(paste0(Sys.time(), " - models for imputation 2 preliminary round complete")) # ~ 3 mins


# final round
est2 = model.est(contestants2,imp.2.go)
table2 = IC.table(est2)
est2.avgs = model.avgs(est2,table2)

print(paste0(Sys.time(), " - models for imputation 2 final round complete"))


# imputation 3: 

# preliminary round
prelim.est3 = model.est(prelim.models,imp.3.go)
erm.full(prelim.est3)
prelim.table3 = IC.table(prelim.est3)
contestants3 = contestant.id(prelim.models,prelim.table3,prelim.est3,include.real.est=F)

print(paste0(Sys.time(), " - models for imputation 3 preliminary round complete")) # ~ 3 mins


# final round
est3 = model.est(contestants3,imp.3.go)
table3 = IC.table(est3)
est3.avgs = model.avgs(est3,table3)

print(paste0(Sys.time(), " - models for imputation 3 final round complete")) # ~ 30 secs



## original data: 
#
## preliminary round
#prelim.est4 = model.est(prelim.models,original.go)
#erm.full(prelim.est4)
#prelim.table4 = IC.table(prelim.est4)
#contestants4 = contestant.id(prelim.models,prelim.table4,prelim.est4,include.real.est=F)
#
## final round
#est4 = model.est(contestants4,original.go)
#table4 = IC.table(est4)
#est4.avgs = model.avgs(est4,table4)
#
#
#
#

saveas1 = paste( format( Sys.time(), "%b_%d_%H%M" ),"i1.AIC.table", sep="" )
write.table(table1, file = paste('./temp',paste( saveas1,".csv", sep="" ), 
  sep = "/"),row.names = TRUE, col.names = TRUE, sep=",")

saveas2 = paste( format( Sys.time(), "%b_%d_%H%M" ),"i1.av.estimates", sep="" )
write.table(est1.avgs, file = paste('./temp',paste( saveas2,".csv", sep="" ), 
  sep = "/"),row.names = TRUE, col.names = TRUE, sep=",")  

saveas1 = paste( format( Sys.time(), "%b_%d_%H%M" ),"i2.AIC.table", sep="" )
write.table(table2, file = paste('./temp',paste( saveas1,".csv", sep="" ), 
  sep = "/"),row.names = TRUE, col.names = TRUE, sep=",") 

saveas2 = paste( format( Sys.time(), "%b_%d_%H%M" ),"i2.av.estimates", sep="" )
write.table(est2.avgs, file = paste('./temp',paste( saveas2,".csv", sep="" ), 
  sep = "/"),row.names = TRUE, col.names = TRUE, sep=",")  

saveas1 = paste( format( Sys.time(), "%b_%d_%H%M" ),"i3.AIC.table", sep="" )
write.table(table3, file = paste('./temp',paste( saveas1,".csv", sep="" ), 
  sep = "/"),row.names = TRUE, col.names = TRUE, sep=",") 

saveas2 = paste( format( Sys.time(), "%b_%d_%H%M" ),"i3.av.estimates", sep="" )
write.table(est3.avgs, file = paste('./temp',paste( saveas2,".csv", sep="" ), 
  sep = "/"),row.names = TRUE, col.names = TRUE, sep=",")  
                                                        
#saveas1 = paste( format( Sys.time(), "%b_%d_%H%M" ),"o.AIC.table", sep="" )
#write.table(table4, file = paste(getwd(),paste( saveas1,".csv", sep="" ),sep = "/"),row.names = TRUE, col.names = TRUE, sep=",") 
#saveas2 = paste( format( Sys.time(), "%b_%d_%H%M" ),"o.av.estimates", sep="" )
#write.table(est4.avgs, file = paste(getwd(),paste( saveas2,".csv", sep="" ),sep = "/"),row.names = TRUE, col.names = TRUE, sep=",")  


# Diagnostics
# coxph(Surv(Time,status) ~ Sex+Bpar+Med2+Fed2+Ktsh+Mres+Fres+ 
# mo_cm_mean+Hqual+Mrels+Frels+Eprlbm+Pwitch+cluster(code),data=mice.imp.1)
 
# coxph(Surv(Time,status) ~ Sex+cluster(code),data=mice.imp.1)
   
# coxph(Surv(Time,status) ~ Eprlbm+cluster(code),data=imp.1.go) 

# coxph(Surv(Time,status) ~ Eprlbm+cluster(code),data=output.i) 

# to make the table, I need to know which model is ranked what across all three

nam = rownames(table1)
# nam2= rep(NA, length(nam))
# for(i in 1:length(nam)){
#  nam2[i] <- which(rownames(table2)==nam[i])
# }

nam2 <- match(nam, rownames(table2))


# nam3=NA
# for(i in 1:length(nam)){
#  nam3[i] = which(rownames(table3)==nam[i])
# }

nam3 <- match(nam, rownames(table3))

final.table = data.frame("i1.rank" = 1:length(nam), "i1.AIC" = table1$AIC, 
  "i1.weights" = table1$weights, "i2.rank" = nam2, "i2.dAIC" = table2$delta.AIC[nam2], 
  "i2.weights" = table2$weights[nam2], "i3.rank" = nam3, "i3.dAIC" = table3$delta.AIC[nam3], 
  "i3.weights" = table3$weights[nam3])
rownames(final.table) = nam
write.table(final.table, file = paste('./temp',"final.table.csv",sep = "/"),
  row.names = TRUE, col.names = TRUE, sep=",")  
    
# do the confidence intervals in the natural scale, then exponentiate that!  
# i need that for the graph at least
# also do that for the table - we've changing our convention

dir_init('./output')
files <- list.files('./temp', full.names=TRUE)
file.copy(files, './output')

if(!save_temp) unlink('./temp', recursive=TRUE)

print(paste0(Sys.time(), " - analysis complete"))