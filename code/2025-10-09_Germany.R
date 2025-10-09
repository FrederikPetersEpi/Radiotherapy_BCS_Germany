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
library(forester)

# Arbeitsverzeichnis festlegen
setwd("Z:/#OFFEN/28-Auswertungen/2024/2024-02_OP_Qualitaet_regional/")
getwd()

.libPaths()

# Daten einlesen
load(file="Z:/#OFFEN/44-RKI/# Datenlieferungen/Empfangene Daten 2025/2025_06-24_zfkd_klin_2.RData")

tumor[,diagnosejahr:=year(diagnosedatum),]
tumor[,diagnosealter:=round(z_age,digits=0),]
tumor[,inzidenzort_bl:=substr(inzidenzort,1,2),]

setnames(tumor,c("z_age","z_kkr","z_dy"),c("diagnosealter","inzidenzort_bl","diagnosejahr"))

# Tumordaten
tum.1=tumor[diagnose_icd10_code %in% c("c50.0","c50.1", "c50.2", "c50.3", "c50.4", "c50.5", "c50.6"),.(obds_rkipatientid,obds_rkipatienttumorid,inzidenzort,diagnosejahr,diagnosedatum,diagnosealter,t_p,t_c,n_p,n_c,m_p,m_c,grading,morphologie_code,seitenlokalisation,inzidenzort_bl,hormonrezeptorstatus_oestrogen, hormonrezeptorstatus_progesteron, her2neustatus,z_tum_order)]
setorder(tum.1,obds_rkipatientid,diagnosedatum) # Faelle pro Frau nach Datum ordnen
tum.1=tum.1[,.SD[1],obds_rkipatientid] # ersten Fall pro Person drin lassen

tum.1[,inzidenzort_bl:=as.numeric(as.character(inzidenzort_bl)),]
tum.1=tum.1[inzidenzort_bl>0&inzidenzort_bl<17,]
tum.1=tum.1[diagnosejahr>2019&diagnosejahr<2023,]
tum.1=tum.1[diagnosealter>18&diagnosealter<100,]

# Patienten und Faelle merken
pats=sort(unique(tum.1$obds_rkipatientid))
tums=sort(unique(tum.1$obds_rkipatienttumorid))

# Patientendaten an Tumordaten anspielen
pat.1=patient[obds_rkipatientid %in% pats,.(obds_rkipatientid,geschlecht,verstorben,datumvitalstatus)]
pat.1=pat.1[,.SD[1],obds_rkipatientid] # nur eine Zeile pro Person

setkey(tum.1,obds_rkipatientid) 
setkey(pat.1,obds_rkipatientid) 
tum.1=pat.1[tum.1,]

uniqueN(tum.1$inzidenzort) # 422 Kreise

# Daten zur OP
op[,obds_rkipatienttumorid:=z_tum_id,]
op.1=op[obds_rkipatienttumorid %in% tums,.(obds_rkipatienttumorid,opid,datum_op)]
opcodes=sort(unique(op.1$opid))
ops[,obds_rkipatienttumorid:=z_tum_id,]
ops.1 = ops[op_typid %in% opcodes, .(opsid, code, op_typid, obds_rkipatienttumorid)]

# Eingriffe definieren und filtern
# Mastektomie: 5-877, 5-872, 5-874
ops.1[,mast:=0,]
ops.1[code %like% "5-877"|code %like% "5-872"|code %like% "5-874",mast:=1,]

# BET: 5-870 
ops.1[,bet:=0]
ops.1[code %like% "5-870",bet:=1]

# auf OP aggregieren
ops.2=ops.1[,.(mast_m=max(mast,na.rm=T),bet_m=max(bet,na.rm=T)),.(opsid, op_typid)]
ops.2=ops.2[mast_m==1|bet_m==1,] # nur Mast und BET behalten
ops.2[mast_m==1&bet_m==1,bet_m:=0,] # wenn beides gemacht, dann gilt es als Mast

# Zusammenführen
setkey(op.1,opid) 
setkey(ops.2,op_typid) 
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

# beschränken auf 182 Tage
tum.3=tum.2[anzahl_tage_diagnose_op>=0&anzahl_tage_diagnose_op<183,] # 148844-114869

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
op.mast=op.all[(op=="mast"|op=="mast+bet")&op!="bet"&op!="bet+mast",]
op.all=op.all[op=="bet",] # 114869-87904

# nur erste OP pro Person drin lassen
setorder(op.all,obds_rkipatienttumorid,tage_bis_bet)
setorder(op.mast,obds_rkipatienttumorid,tage_bis_mast)

op.all=op.all[,.SD[1],obds_rkipatienttumorid] 
op.mast=op.mast[,.SD[1],obds_rkipatienttumorid] 

# Studienkohorte: finale OP Information an den Fall spielen
setkey(op.mast,obds_rkipatienttumorid)
setkey(op.all,obds_rkipatienttumorid)

setkey(op.all,obds_rkipatienttumorid) 
setkey(tum.1,obds_rkipatienttumorid) 
cohort.1=tum.1[op.all,] 
nrow(tum.1)-nrow(cohort.1)
uniqueN(cohort.1$inzidenzort) # 422 Kreise

# Mastektomrien als eigene Kohorte
setkey(op.mast,obds_rkipatienttumorid) 
setkey(tum.1,obds_rkipatienttumorid) 
cohort.mast=op.mast[tum.1,]

# Bestrahlung hinzuspielen 
bestrahlung[,obds_rkipatienttumorid:=z_tum_id,]
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
cohort.2[anzahl_tage_diagnose_fu>731,status:=0,]
cohort.2[anzahl_tage_diagnose_fu>731,anzahl_tage_diagnose_fu:=731,]

# Mastektomie entsprechend anpassen
# Sterbedatum berechnen
cohort.mast[,dtmende:=as.Date('2023-12-31',tz="CET")]
cohort.mast[verstorben=="j",dtmsterbedatum:=datumvitalstatus,]
cohort.mast[is.na(dtmsterbedatum),dtmsterbedatum:=as.Date('2999-12-31',tz="CET"),]

# Abstand Diagnose und end of follow-up
cohort.mast[,mindate:=min(dtmsterbedatum,dtmende,na.rm=T),obds_rkipatientid]
cohort.mast[,status:=0,]
cohort.mast[mindate==dtmsterbedatum,status:=1,]
cohort.mast[,anzahl_tage_diagnose_fu:=as.numeric(difftime(mindate,diagnosedatum,unit="days")),] 
cohort.mast[anzahl_tage_diagnose_fu<366,anzahl_tage_diagnose_fu:=9999,]
cohort.mast=cohort.mast[anzahl_tage_diagnose_fu!=9999,]

# Systemtherapie hinzuspielen 
syst[,obds_rkipatienttumorid:=z_tum_id,]
sy.1=syst[obds_rkipatienttumorid %in% tums,.(obds_rkipatienttumorid,datum_beginn_syst)]
setkey(cohort.2,obds_rkipatienttumorid)
setkey(sy.1,obds_rkipatienttumorid)
cohort.3=sy.1[cohort.2,]

# Abstand Diagnose und SY
# Neu: wenn SY vor OP und nach Diagnose, dann neo=ja sonst nein
cohort.3[,anzahl_tage_diagnose_sy:=as.numeric(difftime(datum_beginn_syst,diagnosedatum,unit="days"))] # follow-up in Tagen
cohort.3[(anzahl_tage_diagnose_sy)>365.25,anzahl_tage_diagnose_sy:=9999,] 
cohort.3[is.na(anzahl_tage_diagnose_sy),anzahl_tage_diagnose_sy:=9999,]
cohort.3[anzahl_tage_diagnose_sy<0,anzahl_tage_diagnose_sy:=9999,]

# nur erste Systemtherapie pro Person drin lassen
setorder(cohort.3,obds_rkipatienttumorid,anzahl_tage_diagnose_sy)
cohort.3=cohort.3[,.SD[1],obds_rkipatienttumorid] 
cohort.3[anzahl_tage_diagnose_sy==9999,anzahl_tage_diagnose_sy:=NA,]
cohort.3[,sy:=ifelse(anzahl_tage_diagnose_sy<9999,1,0),]

# Neoadjuvante Therapie identifizieren
cohort.3[,neo:="nein"]
cohort.3[(anzahl_tage_diagnose_sy<tage_bis_bet)&sy==1,neo:="ja"]

cohort.3[, hormonrezeptorstatus_oestrogen := fifelse(hormonrezeptorstatus_oestrogen %like% "p" &
                                                                        hormonrezeptorstatus_oestrogen %like% "n", "u",
                                                                      fifelse(hormonrezeptorstatus_oestrogen %like% "p", "p",
                                                                              fifelse(hormonrezeptorstatus_oestrogen %like% "n", "n", "u")))]

cohort.3[, hormonrezeptorstatus_progesteron := fifelse(hormonrezeptorstatus_progesteron %like% "p" &
                                                                          hormonrezeptorstatus_progesteron %like% "n", "u",
                                                                        fifelse(hormonrezeptorstatus_progesteron %like% "p", "p",
                                                                                fifelse(hormonrezeptorstatus_progesteron %like% "n", "n", "u")))]

cohort.3[, her2neustatus := fifelse(her2neustatus %like% "p" &
                                                       her2neustatus%like% "n", "u",
                                                     fifelse(her2neustatus %like% "p", "p",
                                                             fifelse(her2neustatus %like% "n", "n", "u")))]

#Subtyp Kategorisierung (HER2+/HR-, HER2+/HR+,TNBC, HER2-/HR+)
cohort.3[,subtyp := fifelse(
  her2neustatus=="p" & hormonrezeptorstatus_oestrogen == "n" &hormonrezeptorstatus_progesteron=="n",
  "HER2+/HR-", 
  fifelse(
    her2neustatus=="p" & (hormonrezeptorstatus_oestrogen == "p" | hormonrezeptorstatus_progesteron == "p"),
    "HER2+/HR+",
    fifelse(
      her2neustatus=="n" & hormonrezeptorstatus_oestrogen == "n" & hormonrezeptorstatus_progesteron=="n",
      "TNBC",
      fifelse(
        her2neustatus== "n" & (hormonrezeptorstatus_oestrogen == "p" | hormonrezeptorstatus_progesteron == "p"),
        "HER2-/HR+",
        NA_character_
      ))))] 

cohort.3[is.na(subtyp),subtyp:="unknown",]

# T-Stadium
cohort.3[,t:=substr(t_p,1,1),]
cohort.3[t=="u"|t=="x",t:=NA,]
cohort.3[is.na(t),t:=substr(t_c,1,1),]
cohort.3[t=="a"|t=="x",t:=NA,]
cohort.3[is.na(t),t:=-99]

# N-Stadium
cohort.3[,n:=substr(n_p,1,1),]
cohort.3[n_p %like% "mi" & n_p %like% "1",n:="1mi",]
cohort.3[n=="u"|n=="(",n:=NA,]
cohort.3[n=="x",n:=NA,]
cohort.3[is.na(n)&n_c %like% "mi",n:="1mi",]
cohort.3[is.na(n),n:=substr(n_c,1,1),]
cohort.3[n=="x",n:=NA,]
cohort.3[is.na(n),n:=-99]
cohort.3[n==-99,n:=0,]

# Nodalstatus
cohort.3[,node:=ifelse(n=="0",0,ifelse(n=="1",1,1)),]
cohort.3[,node:=as.factor(node),]
levels(cohort.3$node)=c("nicht befallen","befallen")

# M-Stadium
cohort.3[,m:=substr(m_p,1,1),]
cohort.3[m=="x",m:=NA,]
cohort.3[is.na(m),m:=substr(m_c,1,1),]
cohort.3[m=="x",m:=NA,]
cohort.3[is.na(m),m:=0]
cohort.3[m==9,m:=0,]

table(cohort.3$t,useNA = "always")
table(cohort.3$n,useNA = "always")
table(cohort.3$m,useNA = "always")
table(cohort.3$node,useNA = "always")

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

table(cohort.3$UICC_new,useNA = "always")

# Grading: # 1,2,L,M gut/maessig differenziert # 3,4,H schlecht differenziert
cohort.3[,grade:=2,]
cohort.3[,grade:=ifelse(grading=="1"|grading=="2"|grading=="L"|grading=="M",0,
                     ifelse(grading=="3"|grading=="4"|grading=="H",1,2)),]
cohort.3[,grade:=as.factor(grade),]
levels(cohort.3$grade)=c("gut/maessig differenziert","schlecht differenziert","unbekannt")

# Alter, numerisch und in Gruppen
cohort.3[,diagnosealter:=round(as.numeric(as.character(diagnosealter)),digits=0)]
cohort.3[,ag_2:=cut(diagnosealter,breaks=c(0,seq(40,80,4.999999),Inf))]
levels(cohort.3$ag_2)=c("18-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
table(cohort.3$ag_2,cohort.3$diagnosealter)
cohort.3=cohort.3[!is.na(diagnosealter),]
table(cohort.3$ag_2,cohort.3$diagnosealter,useNA = "always")

# Histologie: duktal 8500; lobul?r 8520; 8480 muzin?s 
cohort.3[,hist:=3,]
cohort.3[morphologie_code %like% "8500",hist:=0,]
cohort.3[morphologie_code %like% "8520",hist:=1,]
cohort.3[morphologie_code %like% "8480",hist:=2,]
cohort.3[,hist:=as.factor(hist),]
levels(cohort.3$hist)=c("ductal","lobular","mucous","other")

# SES einlesen
gisd=fread("Daten/GISD_Bund_Kreis.csv",dec=".",sep=",")
gisd=gisd[year>=2011&year<2022,] # 2009-2021

# GISD fuer die Jahre 2022 und 2023 aus 2021 weiterfuehren
gisd_2021=gisd[year==2021,]
gisd_2022=gisd_2021
gisd_2022[,year:=2022,]
gisd=rbind(gisd,gisd_2022)
gisd_2022[,year:=2023,]
gisd=rbind(gisd,gisd_2022)

# SES
# Jahre auswaehlen
gisd=gisd[year>=2013&year<2024,] # 2013-2023
cohort.3[,inzidenzort:=as.integer(as.character(inzidenzort)),]
cohort.3[,diagnosejahr:=as.integer(as.character(diagnosejahr)),]

setkey(cohort.3,inzidenzort,diagnosejahr)
setkey(gisd,kreis_id,year)
cohort.3=gisd[cohort.3,] 

# SES basierend auf erstem und letztem Quintil
cohort.3[,ses_gr:=as.factor(ifelse(gisd_5==5,0,ifelse(gisd_5>1&gisd_5<5,1,ifelse(gisd_5==1,2,NA)))),]
levels(cohort.3$ses_gr)=c("hohe Deprivation","mittel","niedrige Deprivation")

cohort.3[,region:=as.factor(inzidenzort_bl),]
levels(cohort.3$region)=c("SH","HH","NI","HB","NW","HE","RP","BW","BY","SL","BE","BB","MV","SN","ST","TH") # AGS nach Wikipedia
cohort.3[,.N,region]

cohort.3[,east:=ifelse(inzidenzort_bl>10,"yes","no")]

setwd("Z:/#OFFEN/28-Auswertungen/2024/2024-02_OP_Qualitaet_regional/Analysen_Bund/")

zip.1=fread("Daten/zipcodes.de.csv",sep=",")
zentren.1=fread("Daten/zentren_plz.csv",sep=",")
setkey(zip.1,zipcode) # sortieren f?r Tage zwischen ST und Diagnose
zip.1=zip.1[J(unique(zip.1$zipcode)),mult="first"]
setkey(zentren.1,V1)
setkey(zip.1,zipcode)
zentren.merge=zip.1[zentren.1,c("community_code")]
zentren.merge$zentrum=1
colnames(zentren.merge)=c("inzidenzort","zentrum")

setkey(zentren.merge,inzidenzort) # sortieren f?r Tage zwischen ST und Diagnose
zentren.merge=zentren.merge[J(unique(zentren.merge$inzidenzort)),mult="first"]
zentren.merge=zentren.merge[!is.na(inzidenzort),]

setkey(zentren.merge,inzidenzort)
setkey(cohort.3,kreis_id)

cohort.3=zentren.merge[cohort.3,]
cohort.3[is.na(zentrum),zentrum:=0,]

# weitere Indikatoren von INKAR
other.dat=fread(file="Z:/#OFFEN/28-Auswertungen/2023/2023-03 B-Zell Lymphom Ghandili/Daten/Tabelle Abfrage2.csv",dec=",",sep=";")
other.dat.merge=other.dat[,][,c("Kennziffer","Krankenhausbetten","Vorzeitige Sterblichkeit Frauen","Pflegebedürftige","Vorzeitige Sterblichkeit Männer")]
colnames(other.dat.merge)=c("kreis_id","betten","sterb_f","pflege","sterb_m")
other.dat.merge

# weitere INKAR Merkmale
setkey(cohort.3,inzidenzort)
setkey(other.dat.merge,kreis_id)
cohort.3=other.dat.merge[cohort.3,] 
cohort.3[,sterb:=sterb_f,]

#Raumindikatoren
raum.1=read_excel("Z:/#OFFEN/28-Auswertungen/2023/2023-03 B-Zell Lymphom Ghandili/Daten/raumgliederungen-referenzen-2022.xlsx", sheet = "Kreisreferenz", skip = 0)
raum.1=raum.1[-1,]

raum.merge=raum.1 [,][,c("KRS2022","RLK_NAME", "KTU_NAME")]
colnames(raum.merge)=c("kreis_id","raumtyp", "siedlungsstruktur")
raum.merge$kreis_id=as.numeric(raum.merge$kreis_id)
raum.merge=data.table(raum.merge)
raum.merge$kreis_id=floor((raum.merge$kreis_id)/1000)
raum.merge=unique(raum.merge)

# Raumtyp
setkey(cohort.3,kreis_id)
setkey(raum.merge,kreis_id)
cohort.3=raum.merge[cohort.3] 

cohort.mast[op %like% "mast",mast:=1,]
mast_agg=cohort.mast[,.(mast_n=sum(mast,na.rm=TRUE),c50_n=.N),.(inzidenzort,diagnosejahr)]
mast_agg[,rate_mast:=round(100*(mast_n/c50_n),digits=1),]
mast_agg[,kreis_id:=as.numeric(as.character(inzidenzort))]
mast_agg=mast_agg[,.(kreis_id,diagnosejahr,rate_mast)]

setkey(cohort.3,kreis_id,year)
setkey(mast_agg,kreis_id,diagnosejahr)
cohort.3=mast_agg[cohort.3] 

# Verzeichnis fuer Graphiken und Tabellen erstellen
dir.create("Graphiken/Graphiken_neu", showWarnings = FALSE)
dir.create("Tabellen_Modelle/Tabellen_Modelle_neu", showWarnings = FALSE)

###################################
# alle Daten zusammenführen
###################################
cohort.3[,diagnosealter:=round(diagnosealter,digits=0),]
cohort.3=cohort.3[diagnosealter>18&diagnosejahr>2019&diagnosejahr<2023,]
cohort.3=cohort.3[anzahl_tage_diagnose_fu!=9999,] # 87904-86004
cohort.3=cohort.3[!is.na(kreis_id)&!is.na(ses_gr),] # 86004-85873
cohort.3=cohort.3[UICC_new=="IA"|UICC_new=="IB"|UICC_new=="IIA",] # 85873-66729
#cohort.3=cohort.3[z_tum_order==1,] # 66729-63018
cohort.3=cohort.3[region!="NW",] # 63018-51514

# Studienkohorte
cohort.agg=cohort.3[,.(all_s=.N,all_st=sum(st,na.rm=T)),kreis_id]
cohort.agg[,anteil_st:=all_st/all_s,]
setorder(cohort.agg,anteil_st)
sum(cohort.agg$all_s)
cohort.agg[order(anteil_st)]
sum(cohort.agg$all_s)
cohort.agg=cohort.agg[all_s>29,]   
sum(cohort.agg$all_s) # 51514-51198
# Remove outlier
summary(cohort.agg$anteil_st)
q1=quantile(cohort.agg$anteil_st,0.25)
q3=quantile(cohort.agg$anteil_st,0.75)
iqr=q3-q1
lower_bound=q1-1.5*iqr
upper_bound=q3+1.5*iqr
cohort.agg=cohort.agg[anteil_st>lower_bound,]
sum(cohort.agg$all_s) # 51198-49786
uniqueN(cohort.agg$kreis_id) # 322 Kreise
summary(cohort.agg)

kreis_auswahl=unique(cohort.agg$kreis_id)
cohort.6=cohort.3[kreis_id %in% kreis_auswahl,] # 75980
uniqueN(cohort.6$kreis_id) # 322 Kreise
cohort.6[is.na(grade),grade:="unbekannt",]
cohort.6[,zentrum_location:="none"]
cohort.6[zentrum==1&raumtyp %like% "peripher",zentrum_location:="peripheral district",]
cohort.6[zentrum==1&raumtyp %like% "zentral",zentrum_location:="central district",]
cohort.6[,zentrum_location:=as.factor(zentrum_location),]
levels(cohort.6$zentrum_location)
cohort.6[,zentrum_location:=relevel(zentrum_location,ref = "none"),]
cohort.6[,ag_2:=droplevels(ag_2),]

listVars=c("diagnosejahr","verstorben","tage_bis_bet","diagnosealter","ag_2","UICC_new","hist","grade","node","op","st","sy","ses_gr","anzahl_tage_diagnose_fu","zentrum_location","neo","east","rate_mast","sterb","subtyp")
catVars=c("diagnosejahr","verstorben","ag_2","UICC_new","hist","grade","node","op","st","sy","ses_gr","zentrum_location","neo","east","subtyp")
medianvar=c("diagnosealter","tage_bis_bet","anzahl_tage_diagnose_fu","rate_mast","sterb")

tab1=as.data.frame(print(CreateTableOne(listVars,data=cohort.6,strata="region",factorVars=catVars,includeNA=T,test=F,addOverall = TRUE),
                         nospaces=T,smd=T,includeNA=T,nonnormal=medianvar,addOverall = TRUE))
tab1$variable=rownames(tab1)
rownames(tab1)=NULL

tab1=tab1[,c("variable",setdiff(names(tab1),"variable"))]
write.table(tab1,"table_1_deutschland.csv",sep=";")
getwd()

table(cohort.6$op,cohort.6$st)
summary(cohort.6$anzahl_tage_diagnose_fu)


cohort.6[,neo:=as.factor(neo)]
cohort.6[,neo:=relevel(neo,ref="nein")]

# Modell ST
df.1=cohort.6[,.(all=.N,all_st=sum(st,na.rm=T),mean_op_time=mean(tage_bis_bet,na.rm=T),mean_age=mean(diagnosealter,na.rm=T),rate_mast=max(rate_mast),
                 sterb=max(sterb)),
              .(kreis_id,ag_2,hist,grade,node,UICC_new,ses_gr,region,diagnosejahr,zentrum_location,east)] 
df.1[,ag_2:=relevel(ag_2,ref="50-54")]
df.1[,ses_gr:=relevel(ses_gr,ref="mittel")]
df.1[,east:=as.factor(east)]
levels(df.1$grade)=c("well/moderately","poorly","unknown")
levels(df.1$ses_gr)=c("middle","low","high")
levels(df.1$node)=c("not affected","affected")

df.1$mean_age=df.1$mean_age/10
df.1$rate_mast=df.1$rate_mast/10

library(sandwich)
library(lmtest)
model.1=glm(all_st ~ ag_2 + hist + grade + node + UICC_new + ses_gr + zentrum_location + rate_mast + diagnosejahr + east + sterb + offset(log(all)), family = "poisson", data = df.1)
summary(model.1)
vcov_cluster=vcovCL(model.1,cluster=~kreis_id)
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
write.table(sum.tab,paste(Sys.Date(),'_ci_model.1_ST.csv',sep=''),sep=';')

catVars=c("ag_2","hist","grade","node","UICC_new","zentrum_location","east","ses_gr")
numVars=c("rate_mast","diagnosejahr","sterb")
results.list.cat=lapply(catVars,function(var){
  df.1[!is.na(get(var)),.(total=sum(all,na.rm=T),events=sum(all_st,na.rm=T)),by=var][,variable:=paste0(var,get(var))][,variable2:=var][,variable3:=get(var)][order(get(var))]
})
results.list.num=lapply(numVars,function(var){
  df.1[,.(total=sum(all,na.rm=T),events=sum(all_st,na.rm=T)),][,variable:=var][,variable2:=var][,variable3:=var]
})

res.1=rbind(
  rbindlist(results.list.cat,use.names=FALSE)[,-1],
  rbindlist(results.list.num,use.names=FALSE)
)
res.1

# Join HR und Fallzahl, Events
res.1[,row_nr:=1:nrow(.SD),]
sum.tab=as.data.table(sum.tab)
setkey(sum.tab,variable)
setkey(res.1,variable)
res.2=sum.tab[res.1,][order(row_nr)]
res.2=res.2[order(row_nr)]

res.2[is.na(HR),variable:=variable2,]
res.2[!is.na(HR),variable:=paste0("   ",variable3),]

res.2[variable=="ag_2",variable:="Age group, Ref.: 50-54",]
res.2[variable=="hist",variable:="Histology type, Ref.: ductal",]
res.2[variable=="grade",variable:="Tumor grade, Ref.: well/moderately",]
res.2[variable=="node",variable:="Lymph nodes, Ref.: not affected",]
res.2[variable=="UICC_new",variable:="UICC stage; Ref.: IA",]
res.2[variable=="zentrum_location",variable:="Location of certified centre, Ref.: no centre in district",]
res.2[variable=="east",variable:="Residence Eastern Germany, Ref.: no",]
res.2[variable=="ses_gr",variable:="Regional socioeconomic status, Ref.: middle",]
res.2[variable=="   rate_mast",variable:="Mastectomie proportion",]
res.2[variable=="   diagnosejahr",variable:="Year of diagnosis",]
res.2[variable=="   sterb",variable:="Premature mortality, per 1000 persons",]

res.2[,Variable:=variable,]
res.2[,N:=total,]
res.2[,Events:=events,]

res.2=as.data.frame(res.2)
res.2
# Forest Plot erstellen
forest_plot_os = forester(
  left_side_data = res.2[c("Variable", "N", "Events")],
  estimate = res.2$HR,
  ci_low = res.2$LL,
  ci_high = res.2$UL,
  display=TRUE,
  estimate_precision = 2,
  file_path = "Z:/#OFFEN/28-Auswertungen/2024/2024-02_OP_Qualitaet_regional/Analysen_Bund/forest_radiation.png",
  dpi = 600,
  xlim = c(0.5, 1.5),
  xbreaks = c(0.0, 0.5, 1.0, 1.5),
  font_family="Fira Sans",
  null_line_at = 1,
  arrows=T,
  arrow_labels = c("lower than reference","higher than reference"),
  estimate_col_name = "Incidence Rate Ratio (95% CI)",
  point_sizes=3
)

# Zahl der Bestrahlungen pro Gruppe basierend auf dem Modell
agg.1=cohort.6[,.(all_st=sum(st,na.rm=T),bev=.N),]
agg.1[,st_D:=all_st/bev,]
agg.1$st_D

# Aggregieren auf Bezirksebene und Berechnung der Wahrscheinlichkeiten
df.2=cbind(df.1,all_st_pred=round(predict(model.1,newdata=df.1,type="response"),digits=1))
agg.2=df.2[,.(all=sum(all,na.rm=T),st_obs=sum(all_st,na.rm=T),st_pred=sum(all_st_pred,na.rm=T)),.(kreis_id)] 
agg.2[,prob_st_obs:=round(st_obs/all,digits=2),]
agg.2[,prob_st_pred:=round(st_pred/all,digits=2),]

# Abweichung zum Erwartungswert
agg.2[,diff_st_raw:=prob_st_obs-agg.1$st_D,]
agg.2[,diff_st_adj:=prob_st_obs-prob_st_pred,]
agg.2[,mean_deutschland:=agg.1$st_D,]
germany=agg.1$st_D
write.table(agg.2,paste(Sys.Date(),'raten_st.csv',sep=''),sep=';')

bar.1=agg.2[order(prob_st_obs)]
bar.1$District=1:nrow(bar.1)

p=ggplot(bar.1, aes(x=District,y=prob_st_obs))+
  geom_bar(stat="identity",fill="steelblue") +
  geom_hline(yintercept=germany,col="black") +
  #geom_hline(yintercept=0.6630943,col="darkblue",linetype="dashed") +
  #geom_hline(yintercept=0.74134615,col="red",linetype="dashed") +
  ylim(0,1)+
  labs(x="Districts",y="Proportion with radiotherapy")+
  annotate("text",x=25,y=0.81,label="Germany: 0.79",color="black",hjust=0)+
 #annotate("text",x=25,y=0.68,label="Hamburg: 0.66",color="darkblue",hjust=0)+
 #annotate("text",x=25,y=0.77,label="Bremen: 0.74",color="red",hjust=0)+
  theme_minimal();p

ggsave("proportion_RT_plot.png",plot=p,width=6,height=4,dpi=600)
getwd()

# Modell Zeit bis ST
cohort.6[,anzahl_tage_bet_st:=anzahl_tage_diagnose_st-tage_bis_bet,]
df.2=cohort.6[st==1&anzahl_tage_diagnose_st<548,.(all=.N,all_st=sum(st,na.rm=T),mean_st_time=mean(anzahl_tage_bet_st,na.rm=T),mean_op_time=round(mean(tage_bis_bet,na.rm=T)/30.4375,digits=1),mean_age=mean(diagnosealter,na.rm=T),rate_mast=max(rate_mast),
                 sterb=max(sterb)),
              .(kreis_id,ag_2,hist,grade,node,UICC_new,ses_gr,region,diagnosejahr,zentrum_location,east)] 
df.2[,ag_2:=relevel(ag_2,ref="50-54")]
df.2[,ses_gr:=relevel(ses_gr,ref="mittel")]
df.2[,east:=as.factor(east)]
df.2[,nw:=ifelse(region=="NW","yes","no"),]
levels(df.2$grade)=c("well/moderately","poorly","unknown")
levels(df.2$ses_gr)=c("middle","low","high")
levels(df.2$node)=c("not affected","affected")

library(sandwich)
library(lmtest)
model.2=glm(mean_st_time ~ ag_2 +hist + grade + node + UICC_new + ses_gr + zentrum_location + mean_op_time + diagnosejahr + east, family=gaussian(link="identity"),data = df.2)
summary(model.2)
vcov_cluster=vcovCL(model.2,cluster=~kreis_id)
se.cluster=coeftest(model.2,vcov.=vcov_cluster)
se.cluster[-1,2]
sum = summary(model.2)
# Extrahiere die Spalten, die du für die Berechnung der Exponentialwerte benötigst
estimates = sum$coefficients[-1,1]
std_errors = se.cluster[-1,2]
# Berechne die Exponentialwerte und die Konfidenzintervalle
sum.tab = cbind(round(estimates, 3),
                round(estimates - 1.96 * std_errors, 3),
                round(estimates + 1.96 * std_errors, 3))
colnames(sum.tab) = c("Effect", "LL", "UL")
sum.tab=as.data.frame(sum.tab)
sum.tab$variable=rownames(sum.tab)
sum.tab
write.table(sum.tab,paste(Sys.Date(),'_ci_model.time_to_ST.csv',sep=''),sep=';')

catVars=c("ag_2","hist","grade","node","UICC_new","zentrum_location","east","ses_gr")
numVars=c("mean_op_time","diagnosejahr")
results.list.cat=lapply(catVars,function(var){
  df.2[!is.na(get(var)),.(total=sum(all,na.rm=T),events=sum(all_st,na.rm=T)),by=var][,variable:=paste0(var,get(var))][,variable2:=var][,variable3:=get(var)][order(get(var))]
})
results.list.num=lapply(numVars,function(var){
  df.2[,.(total=sum(all,na.rm=T),events=sum(all_st,na.rm=T)),][,variable:=var][,variable2:=var][,variable3:=var]
})

res.1=rbind(
  rbindlist(results.list.cat,use.names=FALSE)[,-1],
  rbindlist(results.list.num,use.names=FALSE)
)
res.1

# Join HR und Fallzahl, Events
res.1[,row_nr:=1:nrow(.SD),]
sum.tab=as.data.table(sum.tab)
setkey(sum.tab,variable)
setkey(res.1,variable)
res.2=sum.tab[res.1,][order(row_nr)]
res.2=res.2[order(row_nr)]

res.2[is.na(Effect),variable:=variable2,]
res.2[!is.na(Effect),variable:=paste0("   ",variable3),]

res.2[variable=="ag_2",variable:="Age group, Ref.: 50-54",]
res.2[variable=="hist",variable:="Histology type, Ref.: ductal",]
res.2[variable=="grade",variable:="Tumor grade, Ref.: well/moderately",]
res.2[variable=="node",variable:="Lymph nodes, Ref.: not affected",]
res.2[variable=="UICC_new",variable:="UICC stage; Ref.: IA",]
res.2[variable=="zentrum_location",variable:="Location of certified centre, Ref.: no centre in district",]
res.2[variable=="east",variable:="Residence Eastern Germany, Ref.: no",]
res.2[variable=="ses_gr",variable:="Regional socioeconomic status, Ref.: middle",]
res.2[variable=="   mean_op_time",variable:="Time to surgery, months",]
res.2[variable=="   diagnosejahr",variable:="Year of diagnosis",]

res.2[,Variable:=variable,]
res.2[,N:=total,]
res.2[,Events:=events,]

res.2=as.data.frame(res.2)
res.2
# Forest Plot erstellen
forest_plot_os = forester(
  left_side_data = res.2[c("Variable", "N", "Events")],
  estimate = res.2$Effect,
  ci_low = res.2$LL,
  ci_high = res.2$UL,
  display=TRUE,
  estimate_precision = 2,
  file_path = "Z:/#OFFEN/28-Auswertungen/2024/2024-02_OP_Qualitaet_regional/Analysen_Bund/forest_time_to_RT.png",
  dpi = 600,
  xlim = c(-60, 60),
  xbreaks = c(-60,30,0,30,60),
  font_family="Fira Sans",
  null_line_at = 0,
  arrows=T,
  arrow_labels = c("earlier than reference","later than reference"),
  estimate_col_name = "Effect (95% CI)",
  point_sizes=3
)

# Survivalmodell für Unterschied nach Bestrahlung
library(survival)

#cohort.6$tage_bis_bet=cohort.6$tage_bis_bet/30.4375
cohort.6[,ag_2:=relevel(ag_2,ref="75-79")]
cohort.6[,east:=as.factor(east)]
levels(cohort.6$grade)=c("well/moderately","poorly","unknown")
levels(cohort.6$ses_gr)=c("middle","low","high")
cohort.6[,ses_gr:=relevel(ses_gr,ref="middle")]
levels(cohort.6$node)=c("not affected","affected")
cohort.6[,radiotherapy:=as.factor(st),]
levels(cohort.6$radiotherapy)=c("no","yes")
model_all = coxph(Surv(anzahl_tage_diagnose_fu, status) ~ radiotherapy + ag_2 + hist + grade + node + UICC_new + ses_gr + zentrum_location + diagnosejahr + east + sterb,data = cohort.6)
summary(model_all)

vcov_cluster=vcovCL(model_all,cluster=~kreis_id)
se.cluster=coeftest(model_all,vcov.=vcov_cluster)
se.cluster[,2]
sum = summary(model_all)
# Extrahiere die Spalten, die du für die Berechnung der Exponentialwerte benötigst
estimates = sum$coefficients[,1]
std_errors = se.cluster[,2]
# Berechne die Exponentialwerte und die Konfidenzintervalle
sum.tab = cbind(round(exp(estimates), 3),
                round(exp(estimates - 1.96 * std_errors), 3),
                round(exp(estimates + 1.96 * std_errors), 3))
colnames(sum.tab) = c("HR", "LL", "UL")
sum.tab=as.data.frame(sum.tab)
sum.tab$variable=rownames(sum.tab)
sum.tab
write.table(sum.tab,paste(Sys.Date(),'_ci_model_surival_ST.csv',sep=''),sep=';')

catVars=c("radiotherapy","ag_2","hist","grade","node","UICC_new","zentrum_location","east","ses_gr")
numVars=c("diagnosejahr","sterb")

results.list.cat=lapply(catVars,function(var){
  cohort.6[!is.na(get(var)),.(total=.N,events=sum(status,na.rm=T)),by=var][,variable:=paste0(var,get(var))][,variable2:=var][,variable3:=get(var)][order(get(var))]
})
results.list.num=lapply(numVars,function(var){
  cohort.6[,.(total=.N,events=sum(status,na.rm=T)),][,variable:=var][,variable2:=var][,variable3:=var]
})

res.1=rbind(
  rbindlist(results.list.cat,use.names=FALSE)[,-1],
  rbindlist(results.list.num,use.names=FALSE)
)
res.1

# Join HR und Fallzahl, Events
res.1[,row_nr:=1:nrow(.SD),]
sum.tab=as.data.table(sum.tab)
setkey(sum.tab,variable)
setkey(res.1,variable)
res.2=sum.tab[res.1,][order(row_nr)]
res.2=res.2[order(row_nr)]

res.2[is.na(HR),variable:=variable2,]
res.2[!is.na(HR),variable:=paste0("   ",variable3),]

res.2[variable=="radiotherapy",variable:="Radiotherapy, Ref.: no",]
res.2[variable=="ag_2",variable:="Age group, Ref.: 75-79",]
res.2[variable=="hist",variable:="Histology type, Ref.: ductal",]
res.2[variable=="grade",variable:="Tumor grade, Ref.: well/moderately",]
res.2[variable=="node",variable:="Lymph nodes, Ref.: not affected",]
res.2[variable=="UICC_new",variable:="UICC stage; Ref.: IA",]
res.2[variable=="zentrum_location",variable:="Location of certified centre, Ref.: no centre in district",]
res.2[variable=="east",variable:="Residence Eastern Germany, Ref.: no",]
res.2[variable=="ses_gr",variable:="Regional socioeconomic status, Ref.: middle",]
res.2[variable=="   rate_mast",variable:="Mastectomie proportion",]
res.2[variable=="   diagnosejahr",variable:="Year of diagnosis",]
res.2[variable=="   sterb",variable:="Premature mortality, per 1000 persons",]

res.2[,Variable:=variable,]
res.2[,N:=total,]
res.2[,Events:=events,]

res.2=as.data.frame(res.2)
res.2
# Forest Plot erstellen
forest_plot_os = forester(
  left_side_data = res.2[c("Variable", "N", "Events")],
  estimate = res.2$HR,
  ci_low = res.2$LL,
  ci_high = res.2$UL,
  display=TRUE,
  estimate_precision = 2,
  file_path = "Z:/#OFFEN/28-Auswertungen/2024/2024-02_OP_Qualitaet_regional/Analysen_Bund/forest_survival.png",
  dpi = 600,
  xlim = c(0, 3),
  xbreaks = c(0.0, 1.0, 3.0),
  font_family="Fira Sans",
  null_line_at = 1,
  arrows=T,
  arrow_labels = c("better than reference","worse than reference"),
  estimate_col_name = "Hazard Ratio (95% CI)",
  point_sizes=3
)

