#Ahoj. This script provides an analysis of the pilot study for Felix_V2


setwd("C:/Users/ivant/Desktop/MUNI/Krystaly/V2_pilotaz")


library(tidyr)
library(dplyr)
library(stringr)
library(reshape2)
library(mirt)
library(psych)
library(GPArotation)
library(psych)
library(writexl)
library(readxl)
library(lavaan)
library(ltm)
library(pwr)
library(pwrRasch)
library(ggplot2)

data_long= read_excel("Felix_data_pilot_5_2024_v1.xlsx", sheet = 1)
warnings()
names(data_long)

#drop training items
data_long= data_long[!data_long$exercise.isTraining==1,]

#cast, using mean to retain and review possible duplicates
data= reshape2::dcast(data_long, exercise.session.id + exercise.session.grade + exercise.session.schoolCode ~ exercise.testItem.name, 
                value.var = "correct", fun.aggregate = mean)

#cast, time to solve for items
data_t= reshape2::dcast(data_long, exercise.session.id + exercise.session.grade + exercise.session.schoolCode ~ exercise.testItem.name, 
                      value.var = "time", fun.aggregate = mean)

#cast, distractor selected
data_d= reshape2::dcast(data_long, exercise.session.id + exercise.session.grade + exercise.session.schoolCode ~ exercise.testItem.name, 
                      value.var = "distractor")

#cast, distractor position selected
data_p= reshape2::dcast(data_long, exercise.session.id + exercise.session.grade + exercise.session.schoolCode ~ exercise.testItem.name, 
                        value.var = "position")


names(data)
data <- data[, c("exercise.session.id", "exercise.session.grade", "exercise.session.schoolCode", 
                 "tc1", "tc2", "tc3", "tc4", "tc5", "tc6", "tc7", "tc8", "tc9", "tc10", "tc11", 
                 "tc12", "tc13", "tc14", "tc15", "tc16", "tc17", "tc18", "tc19", "tc20", "tc21", 
                 "tc22", "tc23", "tc24", "tc25", "tc26", "tc27", "tc28", "tc29", "tc30", "tc31", 
                 "tc32", "tc33", "tc34", "tc35", "tc36", "tc37", "tc38", "tc39", "tc40", "tc41", 
                 "tc42", "tc43", "tc44", "tc45")]


data=na.omit(data)

data$sum=rowSums(data[,4:48])
histogram(data$sum)

#create a dataframe with items desciptions
items=as.data.frame(colSums(data[4:48]/404))
names(items)="easiness"
items$item=row.names(items)
items=items[,c(2,1)]



# get distractor frequency
tables_list <- list()

for (i in 4:48) {
  tbl <- table(data_d[, i])
  tbl_df <- as.data.frame(tbl)
  tbl_df$Column <- names(data_d)[i]
  tables_list[[i - 3]] <- tbl_df
}

combined_df <- do.call(rbind, tables_list)

combined_df$Var1=as.character(combined_df$Var1)
combined_df$Var1=substr(combined_df$Var1, nchar(combined_df$Var1) - 1, nchar(combined_df$Var1))
combined_df$Var1[combined_df$Var1=="ns"]="Ans"

dist=reshape2::dcast(combined_df, Column ~ Var1, value.var = "Freq")



# get distractor position frequency
tables_list <- list()

for (i in 4:48) {
  tbl <- table(data_p[, i])
  tbl_df <- as.data.frame(tbl)
  tbl_df$Column <- names(data_p)[i]
  tables_list[[i - 3]] <- tbl_df
}

combined_df <- do.call(rbind, tables_list)

combined_df$Var1=as.character(combined_df$Var1)
combined_df$Var1=substr(combined_df$Var1, nchar(combined_df$Var1) - 1, nchar(combined_df$Var1))
combined_df$Var1[combined_df$Var1=="ns"]="Ans"

dist_posi=reshape2::dcast(combined_df, Column ~ Var1, value.var = "Freq")

#check frequency of positions selected
as.data.frame(colSums(dist_posi[2:5], na.rm = T)/(45*405)*100)
positions=reshape2::dcast(data_long, position ~ correct)
positions$rate=positions[3]/ (positions[3]+positions[2])

#get positions of distractors
distractors= merge(dist, dist_posi, by = "Column")

melt= melt(distractors)
melt$position=(1-(grepl("Ans", melt$variable)+grepl("D", melt$variable)))
melt$position= ifelse(as.logical(melt$position), as.character(melt$variable), "TRUE")


distractor=reshape2::dcast(melt, Column + value ~ variable, value.var= "position", fun.aggregate = function(x) {paste(x, collapse = ", ")})
distractor=distractor[,-2]

distractor[2]= ifelse(distractor[2]=="TRUE",names(distractor[2]),"")
distractor[3]= ifelse(distractor[3]=="TRUE",names(distractor[3]),"")
distractor[4]= ifelse(distractor[4]=="TRUE",names(distractor[4]),"")
distractor[5]= ifelse(distractor[5]=="TRUE",names(distractor[5]),"")

distractor$distractor= tidyr::unite(distractor[2:9], distract, sep = "")
distractor=distractor[,c(1,10)]
distractor=as.matrix(distractor)
distractor=as.data.frame(distractor)
distractor$pos = str_sub(distractor$distract, -1)
distractor$which= str_sub(distractor$distract, 1, str_length(distractor$distract) - 1)

distract=distractor[,c(3:4)]
distract= dcast(distract, which ~ pos )
distract= distract[-3,]

#table to compare frequency of chosen options vs distribution of positions of correct answers
compare= as.data.frame(colSums(dist_posi[2:5], na.rm = T)/(45*405))
compare$test=as.numeric(distract[1,-1])/45
names(compare)= c("answers", "test")


#get a cor table and hope for the best
cor(data[,4:48])
#write_xlsx(as.data.frame(cor(data[,4:48])), "correlation.xlsx")


items$biserial= round(biserial(x= data$sum, y = data[,4:48]),2)

#FA
fa.parallel(data[,4:48])
efa1 <- fa(data[,4:48], , cor="tet", nfactors = 1, fm="ml")
efa1

fa.diagram(efa1)
fa.plot(efa1)

items$loadings=round(efa1$loadings,2)

#IRT
model=mirt(data[,4:48], 1, itemtype = "Rasch", SE = T, method = "EM", technical= list(NCYCLES = 10000))

M2(model)

round(as.data.frame(coef(model, simplify=T, IRTpars=T)),3)

plot(model, type='trace', facet_items=T, which.items = 1:45,
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=4, space = 'top', cex = .8), 
     theta_lim = c(-3, 3), 
     main = "")

empirical_plot(data[4:48], which.items = 1:45, smooth=F, facet_items=T, 
               as.table = TRUE, auto.key=list(points=F, lines=T, columns=4, space = 'top', cex = .8), 
               theta_lim = c(-3, 3), 
               main = "")


items=cbind(items, round(as.data.frame(coef(model, simplify=T, IRTpars=T)),3))
items=items[1:7]

itemfit(model, na.rm=T)
itemfit(model, na.rm=T, fit_stats="infit")

#check for misfits in the sample
calibration=personfit(model, na.rm = T, method = "ML")

histogram(calibration$outfit)
histogram(calibration$infit)

boxplot(calibration$infit)
boxplot(calibration$outfit)

misfits = rownames(calibration[calibration$outfit>2 | calibration$infit>2,])
dataNM = data[!(rownames(data) %in% misfits),]
dataM = data[(rownames(data) %in% misfits),]

items=merge(items, distractors, by.x= "item", by.y= "Column", sort=F)



#safe item description and make sounds decision about what to drop
#write_xlsx(items, "item description.xlsx")


#drop suboptimal and estimate new model
data_p = data[,!(colnames(data) %in% c("tc1", "tc8", "tc10", "tc16", "tc17", "tc21",
                                      "tc32", "tc36", "tc43", "tc44"))]

items=as.data.frame(colSums(data[4:48]/404))
names(items)="easiness"
items$item=row.names(items)
items=items[,c(2,1)]
items = items[!(rownames(items) %in% c("tc1", "tc8", "tc10", "tc16", "tc17", "tc21",
                                       "tc32", "tc36", "tc43", "tc44")),]

model2=mirt(data_p[,4:38], 1, itemtype = "2PL", SE = T, method = "EM", technical= list(NCYCLES = 10000))

M2(model2)

res <- residuals(model2, type = 'Q3')

round(as.data.frame(coef(model2, simplify=T, IRTpars=T)),3)

plot(model2, type='trace', facet_items=T, which.items = 1:35,
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=4, space = 'top', cex = .8), 
     theta_lim = c(-3, 3), 
     main = "")

empirical_plot(data_p[4:38], which.items = 1:35, smooth=F, facet_items=T, 
               as.table = TRUE, auto.key=list(points=F, lines=T, columns=4, space = 'top', cex = .8), 
               theta_lim = c(-3, 3), 
               main = "")

itemfit(model2, na.rm=T)
itemfit(model2, na.rm=T, fit_stats="infit")

items=cbind(items, round(as.data.frame(coef(model2, simplify=T, IRTpars=T)),3))
#write_xlsx(items, "item description2.xlsx")

#sample characteristics

table(data$exercise.session.grade)
as.data.frame(table(data$exercise.session.schoolCode))
