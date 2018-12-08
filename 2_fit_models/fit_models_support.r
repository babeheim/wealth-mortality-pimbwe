
model.spec <- function(modeltype="cluster"){
 
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
  

model.est <- function(models,ms.data){
  
  models = noquote(models) 
  compute = function(model)  coxph( as.formula(model) ,data=ms.data)
  res = lapply( models, compute )
  names(res) = names(models)
  return(res)

}


 
 
# to be used both in the prelims to identify contestants, 
# and then in final round to present tables 
IC.table <- function( res ){   
  AICweights <- function(ic){
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



model.avgs <- function(estimates,AICtable){   
    
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



erm.full <- function( estimates ){     # works off preliminary round estimates
  estimates.erm = list(NA)
  estimates.erm[[1]] = estimates$Edu2Height     # embodied
  estimates.erm[[2]] = estimates$KtshHqual               # material
  estimates.erm[[3]] = estimates$ResRelsEprlbmWitch      # relational
  names(estimates.erm) = c("Edu2Height", "KtshHqual", "ResRelsEprlbmWitch")
  res = IC.table(estimates.erm)
  model.names = names(res)
  res
}


contestant.id <- function(prelim.models,prelim.table,prelim.real.est,include.real.est=F){   # prelim.real.est is only used if real.est=T
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