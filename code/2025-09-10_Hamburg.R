# Arbeitsspeicher leeren
rm(list=ls())

# Plots schliessen
graphics.off()

# Packete laden
library(data.table)
library(readxl)
library(tableone)
library(splines)
library(ggplot2)
library(writexl)
library(sandwich)
library(lmtest)

# Arbeitsverzeichnis festlegen
setwd ("Z:/#OFFEN/28-Auswertungen/Daten/Datenbankabzuege/csv-Dateien-fuer R_ZFKD-Format/2025Q2/")

# Einlesen aller klinischer ZfKD Daten
load(file="2025_04_02_HH_R_format.RData")


# Tumordaten
tum.1=tumor[diagnose_icd10_code %in% c("c50.0","c50.1", "c50.2", "c50.3", "c50.4", "c50.5", "c50.6") 
            & inzidenzort_bl=="02",
            .(obds_rkipatientid,obds_rkipatienttumorid,inzidenzort,diagnosejahr,diagnosedatum,diagnosealter,t_p,t_c,n_p,n_c,m_p,m_c,
              grading,morphologie_code,seitenlokalisation,y_symbol_c,y_symbol_p)]

setorder(tum.1,obds_rkipatientid,diagnosedatum) # Faelle pro Frau nach Datum ordnen
tum.1=tum.1[,.SD[1],obds_rkipatientid] # ersten Fall pro Person drin lassen

# Patienten und Faelle merken
pats=sort(unique(tum.1$obds_rkipatientid))
tums=sort(unique(tum.1$obds_rkipatienttumorid))

# Patientendaten an Tumordaten anspielen
pat.1=patient[obds_rkipatientid %in% pats,.(obds_rkipatientid,geschlecht,verstorben,datumvitalstatus)]
pat.1=pat.1[,.SD[1],obds_rkipatientid] # nur eine Zeile pro Person

setkey(tum.1,obds_rkipatientid) 
setkey(pat.1,obds_rkipatientid) 
tum.1=pat.1[tum.1,]

# Daten zur OP
rm(op)
op=fread("op.csv")
op.1=op[obds_rkipatienttumorid %in% tums,.(obds_rkipatienttumorid,opid,datum_op)]
opcodes=sort(unique(op.1$opid))

# Korrekte Filterung - verwende dieselbe Spalte wie bei der Erstellung von opcodes
#ops.1 = ops
ops.1 = ops[opsid %in% opcodes, .(opsid, code, op_typid, obds_rkipatienttumorid)]

# Überprüfe, welche der benötigten Spalten tatsächlich existieren
#needed_columns = c("opsid", "opid", "obds_rkipatienttumorid", "op_typid", "code")
#existiert = needed_columns %in% names(ops)
#print(data.frame(spalte = needed_columns, existiert = existiert))
#ops.1=ops[opsid %in% opcodes,.(obds_rkipatienttumorid,opsid,op_typid,code)]

# Eingriffe definieren und filtern
# Mastektomie: 5-877, 5-872, 5-874
ops.1[,mast:=0,]
ops.1[code %like% "5-877"|code %like% "5-872"|code %like% "5-874",mast:=1,]

# BET: 5-870 
ops.1[,bet:=0]
ops.1[code %like% "5-870",bet:=1]

# auf OP aggregieren
ops.2=ops.1[,.(mast_m=max(mast,na.rm=T),bet_m=max(bet,na.rm=T)),.(opsid)]
ops.2=ops.2[mast_m==1|bet_m==1,] # nur Mast und BET behalten
ops.2[mast_m==1&bet_m==1,bet_m:=0,] # wenn beides gemacht, dann gilt es als Mast

# Zusammenführen
setkey(op.1,opid) 
setkey(ops.2,opsid) 
op.2=ops.2[op.1,]

# Zeilen entfernen, wo keine der beiden OP's vorkam
op.2=op.2[mast_m==1|bet_m==1,]

# OP Information an den Fall spielen
setkey(op.2,obds_rkipatienttumorid) 
setkey(tum.1,obds_rkipatienttumorid) 

tum.2=op.2[tum.1,]

# Zeitraum der Datumsangaben
cols=c("datumvitalstatus","datum_op","diagnosedatum")
tum.2[,lapply(.SD,function(x)min(x,na.rm=T)),.SDcols=cols] # ?lteste Datumsangaben
tum.2[,lapply(.SD,function(x)max(x,na.rm=T)),.SDcols=cols] # j?ngste Datumsangaben

# Abstand Diagnose und OP
tum.2[,anzahl_tage_diagnose_op:=as.numeric(difftime(datum_op,diagnosedatum,unit="days"))] # follow-up in Tagen
table(tum.2$anzahl_tage_diagnose_op)

# beschränken auf 182 Tage
tum.3=tum.2[anzahl_tage_diagnose_op>=0&anzahl_tage_diagnose_op<183,]

# BET und Mastektomie ordnen
op.mast=tum.3[mast_m==1,.(tage_bis_mast=min(anzahl_tage_diagnose_op,na.rm=T)),obds_rkipatienttumorid]
op.bet=tum.3[bet_m==1,.(tage_bis_bet=min(anzahl_tage_diagnose_op,na.rm=T)),obds_rkipatienttumorid]
op.all=merge(op.mast, op.bet, all = TRUE)
op.all[tage_bis_mast>=0&is.na(tage_bis_bet),op:="mast",]
op.all[is.na(tage_bis_mast)&tage_bis_bet>=0,op:="bet",]
op.all[tage_bis_mast>tage_bis_bet,op:="bet+mast",]
op.all[tage_bis_mast<tage_bis_bet,op:="mast+bet",]
op.all[,.N,op]

# Einschluss, wenn BET zuerst
op.all=op.all[op=="bet",] 

# nur erste OP pro Person drin lassen
setorder(op.all,obds_rkipatienttumorid,tage_bis_bet)
op.all=op.all[,.SD[1],obds_rkipatienttumorid] 

# Studienkohorte: finale OP Information an den Fall spielen
setkey(op.all,obds_rkipatienttumorid) 
setkey(tum.1,obds_rkipatienttumorid) 
cohort.1=tum.1[op.all,]


setkey(op.mast,obds_rkipatienttumorid)
setkey(op.bet,obds_rkipatienttumorid)

# Bestrahlung hinzuspielen 
rm(bestrahlung)
bestrahlung=fread("bestrahlung.csv")
names(bestrahlung)=tolower(names(bestrahlung))
st.1=bestrahlung[obds_rkipatienttumorid %in% tums,.(obds_rkipatienttumorid,datum_beginn_bestrahlung)]
setkey(cohort.1,obds_rkipatienttumorid)
setkey(st.1,obds_rkipatienttumorid)

cohort.2=st.1[cohort.1,]

# Abstand Diagnose und ST
cohort.2[,anzahl_tage_diagnose_st:=as.numeric(difftime(datum_beginn_bestrahlung,diagnosedatum,unit="days"))] # follow-up in Tagen
cohort.2[,op_bis_st:=anzahl_tage_diagnose_st-tage_bis_bet,]
cohort.2[op_bis_st<0|op_bis_st>730,op_bis_st:=9999,]
cohort.2[anzahl_tage_diagnose_st<0|anzahl_tage_diagnose_st>365.25,anzahl_tage_diagnose_st:=9999,] # unguellige Faelle als 9999
cohort.2[is.na(anzahl_tage_diagnose_st),anzahl_tage_diagnose_st:=9999,]

# nur erste Bestrahlung pro Person drin lassen
setorder(cohort.2,obds_rkipatienttumorid,anzahl_tage_diagnose_st)
cohort.2=cohort.2[,.SD[1],obds_rkipatienttumorid] 
cohort.2[,st:=ifelse(anzahl_tage_diagnose_st<9999,1,0),]

# Sterbedatum berechnen
cohort.2[,dtmende:=as.Date('2023-12-31',tz="CET")]
cohort.2[verstorben=="j",dtmsterbedatum:=datumvitalstatus,]
cohort.2[is.na(dtmsterbedatum),dtmsterbedatum:=as.Date('2999-12-31',tz="CET"),]

# Abstand Diagnose und end of follow-up
cohort.2[,mindate:=min(dtmsterbedatum,dtmende,na.rm=T),obds_rkipatientid]
cohort.2[,status:=0,]
cohort.2[mindate==dtmsterbedatum,status:=1,]
cohort.2[,anzahl_tage_diagnose_fu:=as.numeric(difftime(mindate,diagnosedatum,unit="days")),] 
cohort.2[anzahl_tage_diagnose_fu<366,anzahl_tage_diagnose_fu:=9999,]
cohort.3=cohort.2

cohort.3[anzahl_tage_diagnose_fu>731,status:=0,]
cohort.3[anzahl_tage_diagnose_fu>731,anzahl_tage_diagnose_fu:=731,]

# Systemtherapie hinzuspielen 
sy.1=syst[obds_rkipatienttumorid %in% tums,.(obds_rkipatienttumorid,datum_beginn_syst)]
setkey(cohort.3,obds_rkipatienttumorid)
setkey(sy.1,obds_rkipatienttumorid)

cohort.3=sy.1[cohort.3,]

# Abstand Diagnose und SY
cohort.3[,anzahl_tage_diagnose_sy:=as.numeric(difftime(datum_beginn_syst,diagnosedatum,unit="days"))] # follow-up in Tagen
cohort.3[(anzahl_tage_diagnose_sy)>365.25,anzahl_tage_diagnose_sy:=9999,] 
cohort.3[is.na(anzahl_tage_diagnose_sy),anzahl_tage_diagnose_sy:=9999,]
cohort.3[anzahl_tage_diagnose_sy<0,anzahl_tage_diagnose_sy:=9999,]

# nur erste Systemtherapei pro Person drin lassen
setorder(cohort.3,obds_rkipatienttumorid,anzahl_tage_diagnose_sy)
cohort.3=cohort.3[,.SD[1],obds_rkipatienttumorid] 
cohort.3[,sy:=ifelse(anzahl_tage_diagnose_sy<9999,1,0),]

# T-Stadium
cohort.3[,t_p:=droplevels(t_p),]
cohort.3[,t_c:=droplevels(t_c),]
cohort.3[,t:=substr(t_p,1,1),]
cohort.3[t=="u"|t=="x"|t=="9",t:=NA,]
cohort.3[is.na(t),t:=substr(t_c,1,1),]
cohort.3[t=="a"|t=="x"|t=="9",t:=NA,]
cohort.3[is.na(t),t:=-99]

# N-Stadium
cohort.3[,n_p:=droplevels(n_p),]
cohort.3[,n_c:=droplevels(n_c),]
cohort.3[,n:=substr(n_p,1,1),]
cohort.3[n_p %like% "mi" & n_p %like% "1",n:="1mi",]
cohort.3[n=="u"|n=="("|n=="9",n:=NA,]
cohort.3[n=="x",n:=NA,]
cohort.3[is.na(n)&n_c %like% "mi",n:="1mi",]
cohort.3[is.na(n),n:=substr(n_c,1,1),]
cohort.3[n=="x"|n=="9",n:=NA,]
cohort.3[is.na(n),n:=-99]
cohort.3[n==-99,n:=0,]

# Nodalstatus
cohort.3[,node:=ifelse(n=="0",0,ifelse(n=="1",1,1)),]
cohort.3[,node:=as.factor(node),]
levels(cohort.3$node)=c("nicht befallen","befallen")

# M-Stadium
cohort.3[,m_p:=droplevels(m_p),]
cohort.3[,m_c:=droplevels(m_c),]
cohort.3[,m:=substr(m_p,1,1),]
cohort.3[m=="x"|m=="9",m:=NA,]
cohort.3[is.na(m),m:=substr(m_c,1,1),]
cohort.3[m=="x"|m=="9",m:=NA,]
cohort.3[is.na(m),m:=0]
cohort.3[m==9,m:=0,]

cohort.3[,UICC_new:=
ifelse(t=="i"  & n=="0"   & m=="0","0", 
ifelse(t=="1"  & n=="0"   & m=="0","IA",           
ifelse(t=="1"  & n=="1mi" & m=="0","IB",
ifelse(t=="0"  & n=="1mi" & m=="0","IB",           
ifelse(t=="2"  & n=="0"   & m=="0","IIA",
ifelse(t=="1"  & n=="1"   & m=="0","IIA",
ifelse(t=="0"  & n=="1"   & m=="0","IIA",   
ifelse(t=="3"  & n=="0"   & m=="0","IIB",   
ifelse(t=="2"  & n=="1"   & m=="0","IIB", 
ifelse(t=="2"  & n=="1mi" & m=="0","IIB", 
ifelse(t=="3"  & n=="1"   & m=="0","IIIA",  
ifelse(t=="3"  & n=="1mi" & m=="0","IIIA", 
ifelse(t=="3"  & n=="2"   & m=="0","IIIA",  
ifelse(t=="2"  & n=="2"   & m=="0","IIIA",  
ifelse(t=="1"  & n=="2"   & m=="0","IIIA",  
ifelse(t=="0"  & n=="2"   & m=="0","IIIA",   
ifelse(t=="4"  & n=="0"   & m=="0","IIIB",  
ifelse(t=="4"  & n=="1"   & m=="0","IIIB",
ifelse(t=="4"  & n=="1mi" & m=="0","IIIB",
ifelse(t=="4"  & n=="2"   & m=="0","IIIB",   
ifelse(          n=="3"   & m=="0","IIIC",  
ifelse(                     m=="1","IV",   
NA))))))))))))))))))))))] # UICC

# Grading: # 1,2,L,M gut/maessig differenziert # 3,4,H schlecht differenziert
cohort.3[,grade:=2,]
cohort.3[,grade:=ifelse(grading=="1"|grading=="2"|grading=="L"|grading=="M",0,
                     ifelse(grading=="3"|grading=="4"|grading=="H",1,2)),]
cohort.3[,grade:=as.factor(grade),]
levels(cohort.3$grade)=c("gut/maessig differenziert","schlecht differenziert","unbekannt")

# Alter, numerisch und in Gruppen
cohort.3[,diagnosealter:=as.numeric(as.character(diagnosealter)),]
cohort.3[,ag_2:=cut(diagnosealter,breaks=c(0,seq(40,80,4.999999),Inf))]
levels(cohort.3$ag_2)=c("18-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")

cohort.3=cohort.3[!is.na(diagnosealter),]
cohort.3[,ag_3 := cut(diagnosealter, breaks = c(0, 49.99, 69.99, Inf), labels = c("<50","50-69","70+"))]

# Histologie: duktal 8500; lobul?r 8520; 8480 muzin?s 
cohort.3[,hist:=3,]
cohort.3[morphologie_code %like% "8500",hist:=0,]
cohort.3[morphologie_code %like% "8520",hist:=1,]
cohort.3[morphologie_code %like% "8480",hist:=2,]
cohort.3[,hist:=as.factor(hist),]
levels(cohort.3$hist)=c("ductal","lobular","mucous","other")

## SES einlesen
df.hhstadtortsteilecluster=fread("Z:/#OFFEN/28-Auswertungen/2024/Abzuege/tblref_hhstadtortsteilecluster.csv",sep = ';',header=T) 
df.hhstadtortsteilecluster[,ses2011:=as.numeric(sub(",",".",numses2011)),]
df.hhstadtortsteilecluster[,ses2020:=as.numeric(sub(",",".",numses2020)),]

# SES anfuegen; leftjoin
df.ses=df.hhstadtortsteilecluster[,.(strortsteilnr,ses2011,ses2020,strstadtteil,strbezirk)]
names(df.ses)[1]="inzidenzort"
cohort.3[,inzidenzort:=as.integer(as.character(inzidenzort)),]

setkey(df.ses,inzidenzort)
setkey(cohort.3,inzidenzort)
cohort.3=df.ses[cohort.3,] # leftjoin

cohort.3[,ses:=ses2020,] 
# Grenzen 
low.treshold=qnorm(0.25,mean=0,sd=1) # unteres Quantil
up.treshold =qnorm(0.75,mean=0,sd=1) # oberes Quantil

cohort.3[,ses_gr:=as.factor(ifelse(ses<=(low.treshold),0,ifelse(ses>(low.treshold)&ses<up.treshold,1,ifelse(ses>=(up.treshold),2,NA)))),]
levels(cohort.3$ses_gr)=c("hohe Deprivation","mittel","niedrige Deprivation")

setwd("Z:/#OFFEN/28-Auswertungen/2024/2024-02_OP_Qualitaet_regional/Analysen_HH/")

# Verzeichnis fuer Graphiken und Tabellen erstellen
dir.create("Graphiken/Graphiken_neu", showWarnings = FALSE)
dir.create("Tabellen_Modelle/Tabellen_Modelle_neu", showWarnings = FALSE)

# Zentrum einfuegen?

###################################
# alle Daten zusammenführen
###################################
cohort.4=cohort.3
cohort.4[,dj:=as.numeric(year(diagnosedatum)),]
table(cohort.3$dj,cohort.3$UICC_new)

# Ausschluss hoher Stadien
cohort.5=cohort.4[diagnosealter>18&dj>2019&dj<2023,]
cohort.5=cohort.5[anzahl_tage_diagnose_fu!=9999,] 
cohort.5=cohort.5[!is.na(inzidenzort)&!is.na(ses_gr),]
cohort.5=cohort.5[UICC_new=="IA"|UICC_new=="IB"|UICC_new=="IIA",]
cohort.5[,diagnosejahr:=droplevels(diagnosejahr),]

listVars=c("diagnosejahr","verstorben","tage_bis_bet","diagnosealter","ag_2","UICC_new","hist","grade","node","st","sy","ses_gr","anzahl_tage_diagnose_fu")
catVars=c("diagnosejahr","verstorben","ag_2","UICC_new","hist","grade","node","st","sy","ses_gr")
medianvar=c("diagnosealter","tage_bis_bet","anzahl_tage_diagnose_fu")

tab1a=print(CreateTableOne(listVars,data=cohort.5,factorVars=catVars,strata = "strbezirk",includeNA=T,test=F,addOverall = TRUE),
                    nospaces=T,smd=T,includeNA=T,nonnormal=medianvar,addOverall = TRUE,printToggle = FALSE)
write.table(tab1a,paste(Sys.Date(),"_table1_gesamt_1a.csv",sep=""),sep=";")

######################
# Studienkohorte
cohort.7=cohort.5[,.(all_s=.N,all_st=sum(st,na.rm=T)),strbezirk]
cohort.7[,anteil_st:=all_st/all_s,]
setorder(cohort.7,anteil_st)
plot(cohort.7$anteil_st)

# Modell ST
df.1=cohort.5[,.(all=.N,all_st=sum(st,na.rm=T),mean_op_time=mean(tage_bis_bet,na.rm=T),mean_age=mean(diagnosealter,na.rm=T)),
              .(inzidenzort,ag_2,hist,grade,node,UICC_new,ses_gr,strbezirk,diagnosejahr)] 
setkey(df.1,inzidenzort)

df.1[,ag_2:=relevel(ag_2,ref="50-54")]
df.1[,ses_gr:=relevel(ses_gr,ref="mittel")]

model.1=glm(all_st ~ ag_2 + hist + grade + node + UICC_new + ses_gr + diagnosejahr + strbezirk + offset(log(all)), family = "poisson", data = df.1)
vcov_cluster=vcovCL(model.1,cluster=~strbezirk)
se.cluster=coeftest(model.1,vcov.=vcov_cluster)
se.cluster[-1,2]
sum = summary(model.1)
# Extrahiere die Spalten, die du für die Berechnung der Exponentialwerte benötigst
estimates = sum$coefficients[-1,1]
std_errors = se.cluster[-1,2]
# Berechne die Exponentialwerte und die Konfidenzintervalle
sum.tab = cbind(round(exp(estimates), 3),
                round(exp(estimates - 1.96 * std_errors), 3),
                round(exp(estimates + 1.96 * std_errors), 3))
colnames(sum.tab) = c("HR", "LL", "UL")
sum.tab=as.data.frame(sum.tab)
sum.tab$variable=rownames(sum.tab)
sum.tab
write.table(sum.tab,paste(Sys.Date(),'_HH_ci_model.1_ST.csv',sep=''),sep=';')

# Zahl der Bestrahlungen pro Gruppe basierend auf dem Modell
agg.1=cohort.5[,.(all_st=sum(st,na.rm=T),bev=.N),]
agg.1[,st_HH:=all_st/bev,]
agg.1$st_HH

# Aggregieren auf Bezirksebene und Berechnung der Wahrscheinlichkeiten
df.2=cbind(df.1,all_st_pred=round(predict(model.1,type="response"),digits=1))
agg.2=df.2[,.(all=sum(all,na.rm=T),st_obs=sum(all_st,na.rm=T),digits=0),.(strbezirk)] # Kommentar AS: Fehlermeldung df.2 existiert nicht. Können die Zeilen vielleicht gelöscht werden?
agg.2[,prob_st_obs:=round(st_obs/all,digits=3),]

# Abweichung zum Erwartungswert
agg.2[,diff_st:=prob_st_obs-agg.1$st_HH,]
setorder(agg.2,prob_st_obs)

write.table(agg.2,paste(Sys.Date(),'raten_st.csv',sep=''),sep=';')
getwd()

