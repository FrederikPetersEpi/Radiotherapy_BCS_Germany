# =============================================================================
# Geographic variation in radiotherapy after breast-conserving surgery
# for early invasive breast cancer in Germany
# A population-based, ecological analysis of clinical cancer registry data
#
# Analysis code accompanying Schultz et al.
#
# Data sources
#   - Pooled clinical cancer registry data (ZfKD, Robert Koch Institute)
#   - German Index of Socioeconomic Deprivation (GISD)
#   - Regional indicators (BBSR / INKAR): hospital beds, premature mortality
#   - Spatial reference (BBSR Raumgliederungen 2022)
#   - Certified breast-centre locations (German Cancer Society / OncoMap)
#
# Individual-level registry data are not publicly available under German
# data-protection law; see the data-sharing statement in the manuscript.
#
# Tested with R 4.3.2. Adjust the paths in the CONFIGURATION block before use.
# =============================================================================


# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------
library(data.table)
library(readxl)
library(tableone)
library(sandwich)
library(lmtest)

# German locale is used for date handling; remove or adapt on non-German systems.
Sys.setlocale("LC_ALL", "German")

# -----------------------------------------------------------------------------
# Configuration: input files and output directory
# -----------------------------------------------------------------------------
# Registry extract (must contain the objects: tumor, patient, op, ops, bestrahlung).
setwd("~PATH TO PROJECT")
getwd()

rdata_file   = "data2025_06-24_zfkd_klin_2.RData"

gisd_file    = "data/GISD_Bund_Kreis.csv"            # deprivation index
zip_file     = "data/zipcodes.de.csv"                # ZIP -> district lookup
centres_file = "data/zentren_plz.csv"                # certified breast centres (ZIP)
inkar_file   = "data/inkar_indicators.csv"           # beds, premature mortality (BBSR)
spatial_file = "data/raumgliederungen-referenzen-2022.xlsx"  # spatial reference

output_dir   = "output"
dir.create(output_dir, showWarnings = FALSE)


# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------
load(file = rdata_file)


# -----------------------------------------------------------------------------
# Study cohort: early invasive breast cancer, first primary per patient
# -----------------------------------------------------------------------------
tumor[, diagnosejahr := year(diagnosedatum)]
tumor[, diagnosealter := round(z_age, digits = 0)]
tumor[, inzidenzort_bl := substr(inzidenzort, 1, 2)]
setnames(tumor,
         c("z_age", "z_kkr", "z_dy"),
         c("diagnosealter", "inzidenzort_bl", "diagnosejahr"))

# Invasive breast cancer, precise anatomical site (C50.0-C50.6)
tum.1 = tumor[diagnose_icd10_code %in% c("c50.0", "c50.1", "c50.2", "c50.3",
                                         "c50.4", "c50.5", "c50.6"),
              .(obds_rkipatientid, obds_rkipatienttumorid, inzidenzort, diagnosejahr,
                diagnosedatum, diagnosealter, t_p, t_c, n_p, n_c, m_p, m_c, grading,
                morphologie_code, seitenlokalisation, inzidenzort_bl, z_tum_order)]

# First primary per patient (earliest diagnosis date)
setorder(tum.1, obds_rkipatientid, diagnosedatum)
tum.1 = tum.1[, .SD[1], obds_rkipatientid]

# Restrict to the 16 federal states, diagnosis years 2020-2022, adults
tum.1[, inzidenzort_bl := as.numeric(as.character(inzidenzort_bl))]
tum.1 = tum.1[inzidenzort_bl > 0 & inzidenzort_bl < 17]
tum.1 = tum.1[diagnosejahr > 2019 & diagnosejahr < 2023]
tum.1 = tum.1[diagnosealter >= 18 & diagnosealter < 100]

pats = sort(unique(tum.1$obds_rkipatientid))
tums = sort(unique(tum.1$obds_rkipatienttumorid))

# Attach vital status
pat.1 = patient[obds_rkipatientid %in% pats,
                .(obds_rkipatientid, geschlecht, verstorben, datumvitalstatus)]
pat.1 = pat.1[, .SD[1], obds_rkipatientid]

setkey(tum.1, obds_rkipatientid)
setkey(pat.1, obds_rkipatientid)
tum.1 = pat.1[tum.1]


# -----------------------------------------------------------------------------
# Surgery: identify BCS and mastectomy, and timing relative to diagnosis
# -----------------------------------------------------------------------------
op[, obds_rkipatienttumorid := z_tum_id]
op.1 = op[obds_rkipatienttumorid %in% tums, .(obds_rkipatienttumorid, opid, datum_op)]
opcodes = sort(unique(op.1$opid))

ops[, obds_rkipatienttumorid := z_tum_id]
ops.1 = ops[op_typid %in% opcodes, .(opsid, code, op_typid, obds_rkipatienttumorid)]

# Mastectomy: OPS 5-877, 5-872, 5-874
ops.1[, mast := 0]
ops.1[code %like% "5-877" | code %like% "5-872" | code %like% "5-874", mast := 1]

# Breast-conserving surgery (BCS): OPS 5-870
ops.1[, bet := 0]
ops.1[code %like% "5-870", bet := 1]

# Aggregate to the surgical procedure; keep BCS/mastectomy only
ops.2 = ops.1[, .(mast_m = max(mast, na.rm = TRUE), bet_m = max(bet, na.rm = TRUE)),
              .(opsid, op_typid)]
ops.2 = ops.2[mast_m == 1 | bet_m == 1]
ops.2[mast_m == 1 & bet_m == 1, bet_m := 0]   # if both coded, count as mastectomy

setkey(op.1, opid)
setkey(ops.2, op_typid)
op.2 = ops.2[op.1]
op.2 = op.2[mast_m == 1 | bet_m == 1]

setkey(op.2, obds_rkipatienttumorid)
setkey(tum.1, obds_rkipatienttumorid)
tum.2 = op.2[tum.1]

# Days from diagnosis to surgery; keep surgery within 182 days of diagnosis
tum.2[, anzahl_tage_diagnose_op := as.numeric(difftime(datum_op, diagnosedatum, unit = "days"))]
tum.3 = tum.2[anzahl_tage_diagnose_op >= 0 & anzahl_tage_diagnose_op < 183]

# Order BCS and mastectomy per tumour and classify the surgical sequence
op.mast = tum.3[mast_m == 1, .(tage_bis_mast = min(anzahl_tage_diagnose_op, na.rm = TRUE)),
                obds_rkipatienttumorid]
op.bet  = tum.3[bet_m == 1,  .(tage_bis_bet  = min(anzahl_tage_diagnose_op, na.rm = TRUE)),
                obds_rkipatienttumorid]
op.all = merge(op.mast, op.bet, all = TRUE)
op.all[tage_bis_mast >= 0 & is.na(tage_bis_bet), op := "mast"]
op.all[is.na(tage_bis_mast) & tage_bis_bet >= 0, op := "bet"]
op.all[tage_bis_mast > tage_bis_bet, op := "bet+mast"]
op.all[tage_bis_mast < tage_bis_bet, op := "mast+bet"]

# BCS cohort: BCS only; mastectomy cases form a separate cohort
op.mast = op.all[(op == "mast" | op == "mast+bet") & op != "bet" & op != "bet+mast"]
op.all  = op.all[op == "bet"]

# First surgery per tumour
setorder(op.all, obds_rkipatienttumorid, tage_bis_bet)
setorder(op.mast, obds_rkipatienttumorid, tage_bis_mast)
op.all  = op.all[, .SD[1], obds_rkipatienttumorid]
op.mast = op.mast[, .SD[1], obds_rkipatienttumorid]


# -----------------------------------------------------------------------------
# BCS cohort and mastectomy cohort
# -----------------------------------------------------------------------------
setkey(op.all, obds_rkipatienttumorid)
setkey(tum.1, obds_rkipatienttumorid)
cohort.1 = tum.1[op.all]

setkey(op.mast, obds_rkipatienttumorid)
setkey(tum.1, obds_rkipatienttumorid)
cohort.mast = op.mast[tum.1]


# -----------------------------------------------------------------------------
# Radiotherapy and follow-up
# -----------------------------------------------------------------------------
bestrahlung[, obds_rkipatienttumorid := z_tum_id]
st.1 = bestrahlung[obds_rkipatienttumorid %in% tums,
                   .(obds_rkipatienttumorid, datum_beginn_bestrahlung)]
setkey(cohort.1, obds_rkipatienttumorid)
setkey(st.1, obds_rkipatienttumorid)
cohort.2 = st.1[cohort.1]

# Days from diagnosis to radiotherapy start; valid if within 12 months
cohort.2[, anzahl_tage_diagnose_st := as.numeric(difftime(datum_beginn_bestrahlung, diagnosedatum, unit = "days"))]
cohort.2[anzahl_tage_diagnose_st < 0 | anzahl_tage_diagnose_st > 365.25, anzahl_tage_diagnose_st := 9999]
cohort.2[is.na(anzahl_tage_diagnose_st), anzahl_tage_diagnose_st := 9999]

# First radiotherapy per tumour; flag receipt
setorder(cohort.2, obds_rkipatienttumorid, anzahl_tage_diagnose_st)
cohort.2 = cohort.2[, .SD[1], obds_rkipatienttumorid]
cohort.2[, st := ifelse(anzahl_tage_diagnose_st < 9999, 1, 0)]

# Vital status and follow-up (end of follow-up: 2023-12-31)
cohort.2[, dtmende := as.Date("2023-12-31", tz = "CET")]
cohort.2[verstorben == "j", dtmsterbedatum := datumvitalstatus]
cohort.2[is.na(dtmsterbedatum), dtmsterbedatum := as.Date("2999-12-31", tz = "CET")]

cohort.2[, mindate := min(dtmsterbedatum, dtmende, na.rm = TRUE), obds_rkipatientid]
cohort.2[, status := 0]
cohort.2[mindate == dtmsterbedatum, status := 1]
cohort.2[, anzahl_tage_diagnose_fu := as.numeric(difftime(mindate, diagnosedatum, unit = "days"))]
cohort.2[anzahl_tage_diagnose_fu < 366, anzahl_tage_diagnose_fu := NA]
cohort.2[anzahl_tage_diagnose_fu > 731, status := 0]
cohort.2[anzahl_tage_diagnose_fu > 731, anzahl_tage_diagnose_fu := 731]

# Same follow-up handling for the mastectomy cohort
cohort.mast[, dtmende := as.Date("2023-12-31", tz = "CET")]
cohort.mast[verstorben == "j", dtmsterbedatum := datumvitalstatus]
cohort.mast[is.na(dtmsterbedatum), dtmsterbedatum := as.Date("2999-12-31", tz = "CET")]

cohort.mast[, mindate := min(dtmsterbedatum, dtmende, na.rm = TRUE), obds_rkipatientid]
cohort.mast[, status := 0]
cohort.mast[mindate == dtmsterbedatum, status := 1]
cohort.mast[, anzahl_tage_diagnose_fu := as.numeric(difftime(mindate, diagnosedatum, unit = "days"))]
cohort.mast[anzahl_tage_diagnose_fu < 366, anzahl_tage_diagnose_fu := 9999]
cohort.mast = cohort.mast[anzahl_tage_diagnose_fu != 9999]

# Carry the BCS cohort forward
cohort.3 = copy(cohort.2)

# -----------------------------------------------------------------------------
# TNM and UICC stage (pathological where available, otherwise clinical)
# -----------------------------------------------------------------------------
# T category
cohort.3[, t := substr(t_p, 1, 1)]
cohort.3[t == "u" | t == "x", t := NA]
cohort.3[is.na(t), t := substr(t_c, 1, 1)]
cohort.3[t == "a" | t == "x", t := NA]
cohort.3[is.na(t), t := -99]

# N category
cohort.3[, n := substr(n_p, 1, 1)]
cohort.3[n_p %like% "mi" & n_p %like% "1", n := "1mi"]
cohort.3[n == "u" | n == "(", n := NA]
cohort.3[n == "x", n := NA]
cohort.3[is.na(n) & n_c %like% "mi", n := "1mi"]
cohort.3[is.na(n), n := substr(n_c, 1, 1)]
cohort.3[n == "x", n := NA]
cohort.3[is.na(n), n := -99]
cohort.3[n == -99, n := 0]

# Nodal status
cohort.3[, node := ifelse(n == "0", 0, ifelse(n == "1", 1, 1))]
cohort.3[, node := as.factor(node)]
levels(cohort.3$node) = c("nicht befallen", "befallen")

# M category
cohort.3[, m := substr(m_p, 1, 1)]
cohort.3[m == "x", m := NA]
cohort.3[is.na(m), m := substr(m_c, 1, 1)]
cohort.3[m == "x", m := NA]
cohort.3[is.na(m), m := 0]
cohort.3[m == 9, m := 0]

# UICC stage
cohort.3[, UICC_new :=
  ifelse(t == "i" & n == "0"   & m == "0", "0",
  ifelse(t == "1" & n == "0"   & m == "0", "IA",
  ifelse(t == "1" & n == "1mi" & m == "0", "IB",
  ifelse(t == "0" & n == "1mi" & m == "0", "IB",
  ifelse(t == "2" & n == "0"   & m == "0", "IIA",
  ifelse(t == "1" & n == "1"   & m == "0", "IIA",
  ifelse(t == "0" & n == "1"   & m == "0", "IIA",
  ifelse(t == "3" & n == "0"   & m == "0", "IIB",
  ifelse(t == "2" & n == "1"   & m == "0", "IIB",
  ifelse(t == "2" & n == "1mi" & m == "0", "IIB",
  ifelse(t == "3" & n == "1"   & m == "0", "IIIA",
  ifelse(t == "3" & n == "1mi" & m == "0", "IIIA",
  ifelse(t == "3" & n == "2"   & m == "0", "IIIA",
  ifelse(t == "2" & n == "2"   & m == "0", "IIIA",
  ifelse(t == "1" & n == "2"   & m == "0", "IIIA",
  ifelse(t == "0" & n == "2"   & m == "0", "IIIA",
  ifelse(t == "4" & n == "0"   & m == "0", "IIIB",
  ifelse(t == "4" & n == "1"   & m == "0", "IIIB",
  ifelse(t == "4" & n == "1mi" & m == "0", "IIIB",
  ifelse(t == "4" & n == "2"   & m == "0", "IIIB",
  ifelse(          n == "3"   & m == "0", "IIIC",
  ifelse(                       m == "1", "IV",
  NA))))))))))))))))))))))]


# -----------------------------------------------------------------------------
# Grade, age groups, histology
# -----------------------------------------------------------------------------
# Grade: 1/2/L/M well/moderately, 3/4/H poorly, else unknown
cohort.3[, grade := 2]
cohort.3[, grade := ifelse(grading == "1" | grading == "2" | grading == "L" | grading == "M", 0,
                    ifelse(grading == "3" | grading == "4" | grading == "H", 1, 2))]
cohort.3[, grade := as.factor(grade)]
levels(cohort.3$grade) = c("gut/maessig differenziert", "schlecht differenziert", "unbekannt")

# Age (numeric and grouped)
cohort.3[, diagnosealter := round(as.numeric(as.character(diagnosealter)), digits = 0)]
cohort.3[, ag_2 := cut(diagnosealter, breaks = c(0, seq(40, 80, 4.999999), Inf))]
levels(cohort.3$ag_2) = c("18-39", "40-44", "45-49", "50-54", "55-59",
                          "60-64", "65-69", "70-74", "75-79", "80+")
cohort.3 = cohort.3[!is.na(diagnosealter)]

# Histology: ductal 8500, lobular 8520, mucinous 8480, other
cohort.3[, hist := 3]
cohort.3[morphologie_code %like% "8500", hist := 0]
cohort.3[morphologie_code %like% "8520", hist := 1]
cohort.3[morphologie_code %like% "8480", hist := 2]
cohort.3[, hist := as.factor(hist)]
levels(cohort.3$hist) = c("ductal", "lobular", "mucous", "other")

# -----------------------------------------------------------------------------
# Regional deprivation (GISD), region and East/West
# -----------------------------------------------------------------------------
gisd = fread(gisd_file, dec = ".", sep = ",")
gisd = gisd[year >= 2011 & year < 2022]

# Carry the 2021 index forward to 2022 and 2023
gisd_2021 = gisd[year == 2021]
gisd_2022 = copy(gisd_2021)
gisd_2022[, year := 2022]
gisd = rbind(gisd, gisd_2022)
gisd_2022[, year := 2023]
gisd = rbind(gisd, gisd_2022)
gisd = gisd[year >= 2013 & year < 2024]

cohort.3[, inzidenzort := as.integer(as.character(inzidenzort))]
cohort.3[, diagnosejahr := as.integer(as.character(diagnosejahr))]

setkey(cohort.3, inzidenzort, diagnosejahr)
setkey(gisd, kreis_id, year)
cohort.3 = gisd[cohort.3]

# SES from deprivation quintiles: 5th quintile = low, 2nd-4th = middle, 1st = high
cohort.3[, ses_gr := as.factor(ifelse(gisd_5 == 5, 0,
                              ifelse(gisd_5 > 1 & gisd_5 < 5, 1,
                              ifelse(gisd_5 == 1, 2, NA))))]
levels(cohort.3$ses_gr) = c("hohe Deprivation", "mittel", "niedrige Deprivation")


# Federal state (AGS-based) and East/West classification
cohort.3[, region := as.factor(inzidenzort_bl)]
levels(cohort.3$region) = c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW",
                            "BY", "SL", "BE", "BB", "MV", "SN", "ST", "TH")
cohort.3[, east := ifelse(inzidenzort_bl > 10, "yes", "no")]

# -----------------------------------------------------------------------------
# Certified breast centres (mapped from ZIP to district)
# -----------------------------------------------------------------------------
zip.1 = fread(zip_file, sep = ",")
zentren.1 = fread(centres_file, sep = ",")

setkey(zip.1, zipcode)
zip.1 = zip.1[J(unique(zip.1$zipcode)), mult = "first"]
setkey(zentren.1, V1)
setkey(zip.1, zipcode)
zentren.merge = zip.1[zentren.1, c("community_code")]
zentren.merge$zentrum = 1
colnames(zentren.merge) = c("inzidenzort", "zentrum")

setkey(zentren.merge, inzidenzort)
zentren.merge = zentren.merge[J(unique(zentren.merge$inzidenzort)), mult = "first"]
zentren.merge = zentren.merge[!is.na(inzidenzort)]

setkey(zentren.merge, inzidenzort)
setkey(cohort.3, kreis_id)
cohort.3 = zentren.merge[cohort.3]
cohort.3[is.na(zentrum), zentrum := 0]


# -----------------------------------------------------------------------------
# Regional indicators (BBSR / INKAR) and spatial classification
# -----------------------------------------------------------------------------
other.dat = fread(inkar_file, dec = ",", sep = ";")
other.dat.merge = other.dat[, c("Kennziffer", "Krankenhausbetten",
                                "Vorzeitige Sterblichkeit Frauen", "Pflegebedürftige",
                                "Vorzeitige Sterblichkeit Männer")]
colnames(other.dat.merge) = c("kreis_id", "betten", "sterb_f", "pflege", "sterb_m")

setkey(cohort.3, inzidenzort)
setkey(other.dat.merge, kreis_id)
cohort.3 = other.dat.merge[cohort.3]
cohort.3[, sterb := sterb_f]   # female premature mortality

# Spatial reference (district type)
raum.1 = read_excel(spatial_file, sheet = "Kreisreferenz", skip = 0)
raum.1 = raum.1[-1, ]
raum.merge = raum.1[, c("KRS2022", "RLK_NAME", "KTU_NAME")]
colnames(raum.merge) = c("kreis_id", "raumtyp", "siedlungsstruktur")
raum.merge$kreis_id = as.numeric(raum.merge$kreis_id)
raum.merge = data.table(raum.merge)
raum.merge$kreis_id = floor((raum.merge$kreis_id) / 1000)
raum.merge = unique(raum.merge)

setkey(cohort.3, kreis_id)
setkey(raum.merge, kreis_id)
cohort.3 = raum.merge[cohort.3]

# -----------------------------------------------------------------------------
# District-level mastectomy rate
# -----------------------------------------------------------------------------
cohort.mast[op %like% "mast", mast := 1]
mast_agg = cohort.mast[, .(mast_n = sum(mast, na.rm = TRUE), c50_n = .N),
                       .(inzidenzort, diagnosejahr)]
mast_agg[, rate_mast := round(100 * (mast_n / c50_n), digits = 1)]
mast_agg[, kreis_id := as.numeric(as.character(inzidenzort))]
mast_agg = mast_agg[, .(kreis_id, diagnosejahr, rate_mast)]

setkey(cohort.3, kreis_id, year)
setkey(mast_agg, kreis_id, diagnosejahr)
cohort.3 = mast_agg[cohort.3]


# -----------------------------------------------------------------------------
# Final analytic cohort and exclusions
# -----------------------------------------------------------------------------
cohort.3[, diagnosealter := round(diagnosealter, digits = 0)]
cohort.3 = cohort.3[diagnosealter > 18 & diagnosejahr > 2019 & diagnosejahr < 2023]
cohort.3 = cohort.3[!is.na(anzahl_tage_diagnose_fu)]                 # follow-up >= 1 year
cohort.3 = cohort.3[!is.na(kreis_id) & !is.na(ses_gr)]               # valid district and SES
cohort.3 = cohort.3[UICC_new == "IA" | UICC_new == "IB" | UICC_new == "IIA"]  # early stage
cohort.3 = cohort.3[region != "NW"]                                  # exclude North Rhine-Westphalia

# District-level exclusions: minimum 30 cases and remove implausibly low RT rates
cohort.agg = cohort.3[, .(all_s = .N, all_st = sum(st, na.rm = TRUE)), kreis_id]
cohort.agg[, anteil_st := all_st / all_s]
cohort.agg = cohort.agg[all_s > 29]

q1 = quantile(cohort.agg$anteil_st, 0.25)
q3 = quantile(cohort.agg$anteil_st, 0.75)
iqr = q3 - q1
lower_bound = q1 - 1.5 * iqr
upper_bound = q3 + 1.5 * iqr
cohort.agg = cohort.agg[anteil_st > lower_bound]

kreis_auswahl = unique(cohort.agg$kreis_id)
cohort.6 = cohort.3[kreis_id %in% kreis_auswahl]

# Recode for analysis
cohort.6[is.na(grade), grade := "unbekannt"]
cohort.6[, zentrum_location := "none"]
cohort.6[zentrum == 1 & raumtyp %like% "peripher", zentrum_location := "peripheral district"]
cohort.6[zentrum == 1 & raumtyp %like% "zentral", zentrum_location := "central district"]
cohort.6[, zentrum_location := as.factor(zentrum_location)]
cohort.6[, zentrum_location := relevel(zentrum_location, ref = "none")]
cohort.6[, ag_2 := droplevels(ag_2)]

# -----------------------------------------------------------------------------
# Table 1
# -----------------------------------------------------------------------------
listVars  = c("diagnosejahr", "verstorben", "tage_bis_bet", "diagnosealter", "ag_2",
              "UICC_new", "hist", "grade", "node", "op", "st", "ses_gr",
              "anzahl_tage_diagnose_fu", "zentrum_location", "east",
              "rate_mast", "sterb")
medianvar = c("diagnosealter", "tage_bis_bet", "anzahl_tage_diagnose_fu", "rate_mast", "sterb")

# Baseline characteristics stratified by receipt of radiotherapy (manuscript Table 1)
catVars = c("diagnosejahr", "verstorben", "ag_2", "UICC_new", "hist", "grade",
            "node", "op", "ses_gr", "zentrum_location",
            "east")
tab1 = as.data.frame(print(CreateTableOne(listVars, data = cohort.6, strata = "st",
                                          factorVars = catVars, includeNA = TRUE,
                                          test = FALSE, addOverall = TRUE),
                           nospaces = TRUE, smd = TRUE, includeNA = TRUE,
                           nonnormal = medianvar, addOverall = TRUE))
tab1$variable = rownames(tab1)
rownames(tab1) = NULL
tab1 = tab1[, c("variable", setdiff(names(tab1), "variable"))]
write.table(tab1, file.path(output_dir, "table1_baseline_by_rt.csv"),
            sep = ";", row.names = FALSE)


# -----------------------------------------------------------------------------
# Poisson model for district-level RT counts (cluster-robust SE by district)
# -----------------------------------------------------------------------------
# Aggregate to district x covariate strata; offset = number of women per stratum
df.1 = cohort.6[, .(all = .N, all_st = sum(st, na.rm = TRUE),
                    rate_mast = max(rate_mast), sterb = max(sterb)),
                .(kreis_id, ag_2, hist, grade, node, UICC_new, ses_gr,
                  region, diagnosejahr, zentrum_location, east)]

df.1[, ag_2 := relevel(ag_2, ref = "50-54")]
df.1[, ses_gr := relevel(ses_gr, ref = "mittel")]
df.1[, east := as.factor(east)]
levels(df.1$grade)  = c("well/moderately", "poorly", "unknown")
levels(df.1$ses_gr) = c("middle", "low", "high")
levels(df.1$node)   = c("not affected", "affected")

df.1$rate_mast = df.1$rate_mast / 10   # per 10 percentage points

model.1 = glm(all_st ~ ag_2 + hist + grade + node + UICC_new + ses_gr +
                zentrum_location + rate_mast + diagnosejahr + east + sterb +
                offset(log(all)),
              family = "poisson", data = df.1)

vcov_cluster = vcovCL(model.1, cluster = ~kreis_id)
se.cluster = coeftest(model.1, vcov. = vcov_cluster)

estimates  = summary(model.1)$coefficients[-1, 1]
std_errors = se.cluster[-1, 2]

sum.tab = cbind(round(exp(estimates), 3),
                round(exp(estimates - 1.96 * std_errors), 3),
                round(exp(estimates + 1.96 * std_errors), 3))
colnames(sum.tab) = c("IRR", "LL", "UL")
sum.tab = as.data.frame(sum.tab)
sum.tab$variable = rownames(sum.tab)
write.table(sum.tab, file.path(output_dir, "model_irr.csv"),
            sep = ";", row.names = FALSE)


# -----------------------------------------------------------------------------
# Observed vs. expected district RT rates
# -----------------------------------------------------------------------------
agg.1 = cohort.6[, .(all_st = sum(st, na.rm = TRUE), bev = .N)]
agg.1[, st_D := all_st / bev]

df.2 = cbind(df.1, all_st_pred = round(predict(model.1, newdata = df.1, type = "response"), digits = 1))
agg.2 = df.2[, .(all = sum(all, na.rm = TRUE),
                 st_obs = sum(all_st, na.rm = TRUE),
                 st_pred = sum(all_st_pred, na.rm = TRUE)), .(kreis_id)]
agg.2[, prob_st_obs := round(st_obs / all, digits = 2)]
agg.2[, prob_st_pred := round(st_pred / all, digits = 2)]
agg.2[, diff_st_raw := prob_st_obs - agg.1$st_D]
agg.2[, diff_st_adj := prob_st_obs - prob_st_pred]
agg.2[, mean_deutschland := agg.1$st_D]

write.table(agg.2, file.path(output_dir, "district_rt_rates.csv"),
            sep = ";", row.names = FALSE)