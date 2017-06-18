

rm(list = ls())

source('./code/project_functions.r')

dir_init('./temp')

surv.data <- read.csv('./inputs/original_data.csv', stringsAsFactors=FALSE)

attach(surv.data)

status = 1*(dead=="yes")
Time = time_sv5
Sex = as.factor(sex1)   
levels(Sex) = c("Female","Male")
Sex = C( Sex, contr.treatment(n=2,base=1))      # baseline = female
Hqual = factor(hq, levels=c(0, 1),ordered=TRUE)
Hqual = C( Hqual, contr.treatment(n=2,base=1))
Jzero = junezero
Ktsh = ktsh
Bpar = factor(bpar_cat33, levels=c("first","second to fourth","later"),ordered=TRUE)
Bpar = C( Bpar, contr.treatment(n=3,base=2))
# both Fed2 and Med2 combine the last two categories in the original data
Fed = factor(fa_fin_ed, levels=c("none","lower primary (1-5)",
	"upper primary (6-7)","some secondary","finished secondary"),ordered=TRUE)
Fed2 = as.numeric(Fed)
Fed2[Fed2==5]=4
Fed2 = as.factor(Fed2)
Fed = C( Fed, contr.treatment(n=5,base=1))
Fed2 = C( Fed2, contr.treatment(n=4,base=1))
Med = factor(mo_fin_ed, levels=c("none","lower primary (1-5)",
	"upper primary (6-7)","some secondary","finished secondary"),ordered=TRUE)
Med2 = as.numeric(Med)
Med2[Med2==5]=4
Med = C( Med, contr.treatment(n=5,base=1))
Med2 = as.factor(Med2)
Med2 = C( Med2, contr.treatment(n=4,base=1))
# Mres4 and Fres4 are the original four categories, Mres and Fres compact this to two
Fres4 = factor(fc, levels=c("absent/unknown", "deceased","in village","in household"),ordered=TRUE)
Fres4 = C( Fres4, contr.treatment(n=4,base=4)) # baseline is "in household"
Mres4 = factor(mc, levels=c("absent/unknown", "deceased","in village","in household"),ordered=TRUE)
Mres4 = C( Mres4, contr.treatment(n=4,base=4)) # baseline is "in household"
Fres = factor(fc_simple, levels=c("deceased/elsewhere (0,1)", "village/house (2,3)"),ordered=TRUE)
Fres = C( Fres, contr.treatment(n=2,base=2))
Mres = factor(mc_simple, levels=c("deceased  (1)", "other (0,2,3)"),ordered=TRUE)
Mres = C( Mres, contr.treatment(n=2,base=2))
Feth = factor(fa_eth_cat, levels=c("Pimbwe","Fipa","Rungwa-Mambwe-Konongo","other"),ordered=TRUE)
Feth = C( Feth, contr.treatment(n=4,base=1))
Meth = factor(mo_eth_cat, levels=c("Pimbwe","Fipa","Mambwe-Rungwa-Konongo","Other"),ordered=TRUE)
Meth = C( Meth, contr.treatment(n=4,base=1))
Frels = factor(f_rels, levels=c("none named","up to 6 named","more than 6 named"),ordered=TRUE)
Frels = C( Frels, contr.treatment(n=3,base=1))
Mrels = factor(m_rels, levels=c("none named","up to 6 named","more than 6 named"),ordered=TRUE)
Mrels = C( Mrels, contr.treatment(n=3,base=1))
Eprlbm = factor(eprlb_m, levels=c("born outside of/before marriage", "born in marriage"),ordered=TRUE)
Eprlbm = C( Eprlbm, contr.treatment(n=2,base=1))
mum_uchawi = as.factor(mum_uchawi)
levels(mum_uchawi) = c("uninvolved", "victim", "accused perpetrator")
Mwitch = factor(mum_uchawi, levels=c("uninvolved", "victim", "accused perpetrator"),ordered=TRUE)
Mwitch = C( Mwitch, contr.treatment(n=3,base=1))
dad_uchawi = as.factor(dad_uchawi)
levels(dad_uchawi) = c("uninvolved", "victim", "accused perpetrator")
Fwitch = factor(dad_uchawi, levels=c("uninvolved", "victim", "accused perpetrator"),ordered=TRUE)
Fwitch = C( Fwitch, contr.treatment(n=3,base=1))
either_uchawi = as.factor(either_uchawi)
levels(either_uchawi) = c("uninvolved", "victim", "accused perpetrator")
Pwitch = factor(either_uchawi, levels=c("uninvolved", "victim", "accused perpetrator"),ordered=TRUE)
Pwitch = C( Pwitch, contr.treatment(n=3,base=1))
WAZ = waz5_mean

# The data we now are working with, for the rest of the analysis.
output.i = data.frame(code, Parentcode, Time, status, survey_year, Sex, Bpar, 
	Med2, Fed2, Ktsh, Mres, Fres, Jzero, Hqual, Mrels, Frels, Eprlbm, Mwitch, 
	Fwitch, Pwitch, time_sv, fa_eth_cat, mo_eth_cat, mo_cm_mean, WAZ, Mres4, Fres4)

## Multiple Imputation using mice()
library(mice)
md.pattern(output.i)
md.pairs(output.i)

imp = mice(output.i, print=FALSE,maxit=0)
pred = imp$predictorMatrix
pred[,"Time"] = 0 # don't use for any imputations
pred[,"status"] = 0 # don't use for any imputations
pred[,"time_sv"] = 0 # don't use for any imputations
pred[,"code"] = 0 # don't use for any imputations
pred[,"Parentcode"] = 0 # don't use for any imputations
pred[,"Pwitch"] = 0 # don't use for any imputations
pred[,"survey_year"] = 0 # don't use for any imputations

imp = mice(output.i, m=3, maxit=20, diagnostics=TRUE, seed=500, pred=pred)
plot(imp) # make sure this is mixing well

# In long-form, we need to worry that time-invariant imputations 
# are the same for each individual across their five entries.
# However, we're NOT compacting the time-variant imputaitons, so we leave those alone.
clean.data = function(dataset, compact.timevars=FALSE){
  tag.names = c("Sex", "Bpar", "Med2", "Fed2", "Mrels", "Frels", "mo_cm_mean")
  if(compact.timevars==TRUE){
    tag.names = c(tag.names, "Ktsh", "Jzero", "Hqual", "Mres", "Fres")
  }
  tag.nums = NA
  for(i in 1:length(tag.names)){
    tag.nums[i] = which(names(dataset) == tag.names[i])
  }
  for(i in unique(dataset$code)){
    for(k in tag.nums){
      values = dataset[dataset$code==i,k]
      if(!any(is.na(values))){
        if(is.factor(values)){  # why do i use factors here? blah
          average.entry = names(table(values)[which(table(values)==max(table(values)))])
        if(length(average.entry) > 1){
          average.entry = sample(average.entry,1)
        }
      }
      if(is.numeric(values)){
        average.entry = mean(values)
      }
      for(j in 1:5){
        if(values[j] != average.entry){
          values[j] = average.entry
        }
      }
      dataset[dataset$code==i,k] = values
      }
    }
  }
  dataset
}

# extract and clean of three imputed datasets
mice.imp.1.raw = data.frame(complete(imp,1))
mice.imp.1 = clean.data(mice.imp.1.raw)
mice.imp.2.raw = data.frame(complete(imp,2))
mice.imp.2 = clean.data(mice.imp.2.raw)
mice.imp.3.raw = data.frame(complete(imp,3))
mice.imp.3 = clean.data(mice.imp.3.raw)

## cleanup diagnostics
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Sex, mice.imp.1$Sex)
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Bpar, mice.imp.1$Bpar)
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Med2, mice.imp.1$Med2)
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Fed2, mice.imp.1$Fed2)
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Mrels, mice.imp.1$Mrels)
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Frels, mice.imp.1$Frels)
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Ktsh, mice.imp.1$Ktsh)
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Jzero, mice.imp.1$Jzero)
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Hqual, mice.imp.1$Hqual)
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Mres, mice.imp.1$Mres)
#data.frame(mice.imp.1.raw$code, mice.imp.1.raw$Fres, mice.imp.1$Fres)

write.csv(mice.imp.1, file = './temp/imp1.csv', row.names = TRUE)
write.csv(mice.imp.2, file = './temp/imp2.csv', row.names = TRUE)
write.csv(mice.imp.3, file = './temp/imp3.csv', row.names = TRUE)

dir_init('./output')
files <- list.files('./temp', full.names=TRUE)
file.copy(files, './output')

if(!save_temp) unlink('./temp', recursive=TRUE)

print(paste0(Sys.time(), " - imputation complete"))