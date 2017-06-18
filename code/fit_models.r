
# In absence of a principled way to do the CI for both cluster(code) and frailty(Parentcode) simultaneously,
# I'm just going with the former approach - no bootstrapping.

# to do : 
# figure out exactly what the imputation is doing
# figure out exactly how cox ph works
# make sure all labels are leveled properly <- they are in the bivariates, at least
# double-check my model average calculations, and the SE model average calculations
# type up more explicitly what we did

rm(list=ls(all=TRUE))

source('./code/project_functions.r')

library(survival)
 
mice.imp.1 = read.csv("./inputs/imp1.csv",header=TRUE)                                                                  
mice.imp.2 = read.csv("./inputs/imp2.csv",header=TRUE)                            
mice.imp.3 = read.csv("./inputs/imp3.csv",header=TRUE)

dir_init('./temp')

data.ordering = function(ms.data,orig.data=FALSE) {

  code = ms.data$code
  Time = ms.data$Time
  status = ms.data$status
  Sex = ms.data$Sex # 1 = Female, 2 = Male
  Bpar = ms.data$Bpar  # 1 = first, 2 = later (5th and above), 3 = second to fourth
  Bpar = C( Bpar, contr.treatment(n=3,base=3))                                        
  Med2 = as.factor(ms.data$Med2) # 1 = no education,  2 = lower primary  3 = upper primary, 4 = some secondary
  Med2 = C( Med2, contr.treatment(n=4,base=1)) # baseline = no education
  Ktsh = ms.data$Ktsh
  Hqual = as.factor(ms.data$Hqual) 
  levels(Hqual) = c("no burned bricks","burned bricks")
  Hqual = C( Hqual, contr.treatment(n=2,base=1)) # 0 = no burned bricks (baseline), 1 = burned bricks present (better house)
  
  Fres = ms.data$Fres    
  Fres = C( Fres, contr.treatment(n=2,base=2)) # 1 = deceased/elsewhere (0,1), 2 = village/house (2,3) (baseline)
  Mres = ms.data$Mres
  Mres = C( Mres, contr.treatment(n=2,base=2)) # 1 = deceased (0), 2 = other (1,2,3) (baseline)
  
  if(orig.data==TRUE){
    Fres4 = ms.data$Fres4  
    Fres4 = C( Fres4, contr.treatment(n=4,base=3)) # baseline = in house 
    Mres4 = ms.data$Mres4
    Mres4 = C( Mres4, contr.treatment(n=4,base=3)) # baseline = in house
    WAZ = ms.data$WAZ
  }
  
  Mrels = ms.data$Mrels
  Mrels = C( Mrels, contr.treatment(n=3,base=2)) # 1 = "more than 6 named", 2 = "none named" (baseline), 3 = "up to 6 named"
  Frels = ms.data$Frels      # used as control
  Frels = C( Frels, contr.treatment(n=3,base=2)) # 1 = "more than 6 named", 2 = "none named" (baseline), 3 = "up to 6 named"
  Eprlbm = ms.data$Eprlbm  
  Eprlbm = C( Eprlbm, contr.treatment(n=2,base=2)) # 1 = "born in marriage", 2 = "born outside of/before marriage" (baseline)
  Mwitch = ms.data$Mwitch                  
  Mwitch = C( Mwitch, contr.treatment(n=3,base=2)) # 1 = "accused perpetrator", 2 = "uninvolved" (baseline), 3="victim"
  Fwitch = ms.data$Fwitch                  
  Fwitch = C( Fwitch, contr.treatment(n=3,base=2)) # 1 = "accused perpetrator", 2 = "uninvolved" (baseline), 3="victim"
  Pwitch = ms.data$Pwitch                  
  Pwitch = C( Pwitch, contr.treatment(n=3,base=2)) # 1 = "accused perpetrator", 2 = "uninvolved" (baseline), 3="victim"
  Mheights = ms.data$mo_cm_mean

  
  if(orig.data==FALSE){
    output = data.frame(code,Time,status,Sex,Bpar,Med2,Ktsh,Hqual,
      Fres,Mres,Mrels,Frels,Eprlbm,Mwitch,Fwitch,Pwitch,Mheights)
  } else {
    output = data.frame(code,Time,status,Sex,Bpar,Med2,Ktsh,Hqual,
      Fres,Mres,Mrels,Frels,Eprlbm,Mwitch,Fwitch,Pwitch,Mheights,Mres4,Fres4,WAZ)
  }
    
  
}

imp.1.go = data.ordering(mice.imp.1)
imp.2.go = data.ordering(mice.imp.2)
imp.3.go = data.ordering(mice.imp.3)

coxph(Surv(Time,status) ~ Bpar+cluster(code),data=imp.1.go)
coxph(Surv(Time,status) ~ Bpar+cluster(code),data=imp.1.go)

model.spec = function(modeltype="cluster"){
 
  vars = c( "Sex", "Bpar", "Med2", "Ktsh", "Mres+Fres", "Mheights", 
    "Hqual", "Mrels+Frels", "Eprlbm", "Pwitch")
  var.names = c( "Sex", "Bpar", "Edu2", "Ktsh", "Res", "Height", 
    "Hqual", "Rels", "Eprlbm", "Witch") 
  # need this one to make each model's name
                                
  # Runs through every possible combination of variables
  model.combs = function(box){
    cont = combn( box , m=1, simplify=FALSE )
    for(i in 2:length(box)){
      cont = c(cont, combn( box , m=i, simplify=FALSE ))
    }
    return(cont)
  } 
  
  # Create the models themselves
  allcomb = model.combs(vars)
  if(modeltype == "cluster"){
    definemodel = function( m ) paste ( "Surv(Time,status) ~ ", 
      paste(m, collapse="+"), "+cluster(code)", sep="" )
  }
  if(modeltype == "frailty"){
    definemodel = function( m ) paste ( "Surv(Time,status) ~ ", 
      paste(m, collapse="+"), "+frailty(Parentcode)", sep="" )
  }
  models = lapply( allcomb, definemodel )
  
  # Create the names for the models
  allcomb2 = model.combs(var.names)
  namemodel = function( m ) paste ( paste(m, collapse=""), sep="" )
  model.names = lapply( allcomb2, namemodel )
  
  names(models) = model.names 
  models
}
  

model.est = function(models,ms.data){
  
  models = noquote(models) 
  compute = function(model)  coxph( as.formula(model) ,data=ms.data)
  res = lapply( models, compute )
  names(res) = names(models)
  return(res)

}


 
 
# to be used both in the prelims to identify contestants, 
# and then in final round to present tables 
IC.table = function( res ){   
  AICweights = function(ic){
		noModels = length(ic)
		i=1:noModels
		weight = exp(-0.5*(ic[i]-min(ic)))/sum( exp(-0.5*(ic[i]-min(ic))) )
		names(weight) = paste( "model", 1:noModels )
		weight
  }	
	em.AIC = sapply( res, function(z) extractAIC(z)[2]) 
  em.dAIC = em.AIC - min(em.AIC)
  em.AICw = AICweights( em.AIC )
  em.AICwsum = em.AICw
      
  model.names = names(res)
  
  Model.class = rep(".",length(model.names))
  for(i in 1:length(model.names)){
    embodied = c("Edu2","Height") # we relegated Sex and Bpar to "control variable" status
    material = c("Ktsh","Hqual")
    relational = c("Res","Rels","Eprlbm","Witch")
    es = lapply( embodied, function(vars) grep(vars,model.names[i]))
    no.embodied = sum(as.numeric(!is.na(as.numeric(es))))
    es = lapply( material, function(vars) grep(vars,model.names[i]))
    no.material = sum(as.numeric(!is.na(as.numeric(es))))
    es = lapply( relational, function(vars) grep(vars,model.names[i]))
    no.relational = sum(as.numeric(!is.na(as.numeric(es))))
    if(no.embodied>0){
      Model.class[i] = paste(Model.class[i],"E",sep="")
    }
    if(no.material>0){
      Model.class[i] = paste(Model.class[i],"M",sep="")
    }
    if(no.relational>0){
      Model.class[i] = paste(Model.class[i],"R",sep="")
    }
    Model.class[i] = substr(Model.class[i],2,nchar(Model.class[i]))
  }   
	table = data.frame( row.names = names(res), em.AIC, em.dAIC, em.AICw, Model.class) 
	names(table) = c("AIC", "delta.AIC", "weights", "Model.class")
	table = table[ order(em.dAIC, decreasing = FALSE),]
	for(i in 1:length(em.AICw)){
    em.AICwsum[i] = sum(table$weights[1:i])
  }
  table = data.frame(table,em.AICwsum)
 	names(table) = c("AIC", "delta.AIC", "weights", "Model.class", "summed.weights")
  table
}



model.avgs = function(estimates,AICtable){   
    
  row.labels = c("SexMale", "Bpar1", "Bpar2", "Med22", "Med23", 
    "Med24", "Ktsh", "Mres1", "Fres1", "Mheights", "Hqual2",
    "Mrels1", "Mrels3", "Frels1", "Frels3", "Eprlbm1", "Pwitch1", "Pwitch3")
   
  model.names = names(estimates)
  model.ids = 1:length(model.names)
  model.weights = AICtable[,3]
      
  
  estimates.matrix = rep(0,length(model.ids)*length(row.labels))
  dim(estimates.matrix) = c(length(row.labels),length(model.ids))
    
  rownames(estimates.matrix) = row.labels  
  
  for(i in model.ids){
    for(j in 1:length(row.labels)){
      if(!is.na(as.numeric(estimates[[i]]$coeff[row.labels[j]]))){
        estimates.matrix[j,i] = as.numeric(estimates[[i]]$coeff[row.labels[j]]) # that coefficient
      }
    }
  }
  
  estimate.means = NA
  for(j in 1:length(row.labels)){
    estimate.means[j] = sum(estimates.matrix[j,]*model.weights)
  }

  names(estimate.means) = row.labels 

  dev.matrix = estimates.matrix
  for( i in model.ids ){
    for( j in 1:length(row.labels) ){
      if(estimates.matrix[j,i]!=0){
        dev.matrix[j,i] = estimates.matrix[j,i] - estimate.means[j]
      }
    }
  }
  
  # and now variance
  var.matrix = rep(0,length(model.ids)*length(row.labels))
  dim(var.matrix) = c(length(row.labels),length(model.ids))
    
  rownames(var.matrix) = row.labels  
  
  for(i in model.ids){
    for(j in 1:length(row.labels)){
      if(!is.na(as.numeric(estimates[[i]]$coeff[row.labels[j]]))){
        # need to ID which entry the right estimate is in the var-covar matrix
        predictor.id = which(names(estimates[[i]]$coeff)==row.labels[j])
        var.matrix[j,i] = diag(estimates[[i]]$var)[predictor.id] # is this cluster-robust?
      }
    }
  }                                                  
  
  var.means = NA
  var.corrected.means = NA
  for(j in 1:length(row.labels)){
    var.means[j] = (sum( sqrt(var.matrix[j,])*model.weights))^2
    var.corrected.means[j] = (sum( sqrt(var.matrix[j,]+dev.matrix[j,]^2)*model.weights))^2     
    # see Burnham and Anderson ch4
  }
  names(var.means) = row.labels
  names(var.corrected.means) = row.labels
  
   
  z.stats = (estimate.means)/sqrt(var.corrected.means)
    
  p.values = round((1-pnorm(abs(z.stats)))*2,3)  # is this how to get p-value?

  lb.95 = NA
  ub.95 = NA
  for(i in 1:length(estimate.means)){
    lb.95[i] = estimate.means[i] - 1.959964*sqrt(var.corrected.means[i])
    ub.95[i] = estimate.means[i] + 1.959964*sqrt(var.corrected.means[i])
  }
                         
  res = data.frame("exp.beta"=round(exp(estimate.means),3),"95.lb" = round(exp(lb.95),3), 
    "95.ub" = round(exp(ub.95),3), "beta"=round(estimate.means,3),
    "robust.se"=round(sqrt(var.corrected.means),3),"p"=round(p.values,3))
  res
}



erm.full = function( estimates ){     # works off preliminary round estimates
  estimates.erm = list(NA)
  estimates.erm[[1]] = estimates$Edu2Height     # embodied
  estimates.erm[[2]] = estimates$KtshHqual               # material
  estimates.erm[[3]] = estimates$ResRelsEprlbmWitch      # relational
  names(estimates.erm) = c("Edu2Height", "KtshHqual", "ResRelsEprlbmWitch")
  res = IC.table(estimates.erm)
  model.names = names(res)
  res
}


contestant.id = function(prelim.models,prelim.table,prelim.real.est,include.real.est=F){   # prelim.real.est is only used if real.est=T
                                                                                            # i.e. if you want to also have the estimates
    has.weighting = which(prelim.table[,5]<.9999)                                           # in "contestant models"
    contestants = rownames(prelim.table)[has.weighting]
    contestant.locations = NA
    for(i in 1:length(contestants)){
      contestant.locations[i] = which(names(prelim.models)==contestants[i])  # need to fix the name stuff
    }
    if(include.real.est==F){
        contestant.models = prelim.models[contestant.locations]
    } else {
        contestant.models = prelim.est[contestant.locations]
    }
    contestant.models

}




################
# Run >>>>>>   #
################

prelim.models = model.spec()

# imputation 1: 

# preliminary round
prelim.est1 = model.est(prelim.models,imp.1.go)
erm.full(prelim.est1)
prelim.table1 = IC.table(prelim.est1)
contestants1 = contestant.id(prelim.models,prelim.table1,prelim.est1,include.real.est=F)

print(paste0(Sys.time(), " - imputation 1 preliminary round complete")) # ~3 minutes
# long, this takes: 

# final round
est1 = model.est(contestants1,imp.1.go)
table1 = IC.table(est1)
est1.avgs = model.avgs(est1,table1)

print(paste0(Sys.time(), " - imputation 1 final round complete")) # ~ 30 secs


# imputation 2: 

# preliminary round
prelim.est2 = model.est(prelim.models,imp.2.go)
erm.full(prelim.est2)
prelim.table2 = IC.table(prelim.est2)
contestants2 = contestant.id(prelim.models,prelim.table2,prelim.est2,include.real.est=F)

print(paste0(Sys.time(), " - imputation 2 preliminary round complete")) # ~ 3 mins


# final round
est2 = model.est(contestants2,imp.2.go)
table2 = IC.table(est2)
est2.avgs = model.avgs(est2,table2)

print(paste0(Sys.time(), " - imputation 2 final round complete"))


# imputation 3: 

# preliminary round
prelim.est3 = model.est(prelim.models,imp.3.go)
erm.full(prelim.est3)
prelim.table3 = IC.table(prelim.est3)
contestants3 = contestant.id(prelim.models,prelim.table3,prelim.est3,include.real.est=F)

print(paste0(Sys.time(), " - imputation 3 preliminary round complete")) # ~ 3 mins


# final round
est3 = model.est(contestants3,imp.3.go)
table3 = IC.table(est3)
est3.avgs = model.avgs(est3,table3)

print(paste0(Sys.time(), " - imputation 3 final round complete")) # ~ 30 secs



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

print(paste0(Sys.time(), " - analysis module complete"))