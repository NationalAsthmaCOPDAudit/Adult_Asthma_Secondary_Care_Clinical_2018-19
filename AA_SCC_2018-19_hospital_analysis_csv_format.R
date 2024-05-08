#----------------------------------------#
# Secondary Care Clinical 2018 Analysis  #
# Author: Alex Adamson                   #
# Date: 14/11/18                         #
# Version updates:                       #
#----------------------------------------#

# Set up the libraries

library(dplyr)
library(readstata13)
library(xlsx)
source("H:/My R functions/MySummary.R")
library(janitor)
library(officer)
library(flextable)
library(tidyverse)
library(survival)
library(survminer)
library(ggplot2)
library(survsup)
# library(epitools)
library(psych)



CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}




# Functions

makeFlatNPerc <- function(subana.N) {
  
  colnames(subana.N) <- gsub(" ", "_", colnames(subana.N))
  rownames(subana.N) <- gsub(" ", "_", rownames(subana.N))
  subana.perc <- round(prop.table(subana.N, 2)*100, 1)  # create the prop table
 # totals <- matrix(margin.table(subana.N, 2), nrow = 1, ncol = 2)
 # colnames(totals) <- paste0(colnames(subana.N), "_N")
  
  
  subana.N_flat <- matrix(subana.N, nrow = 1, ncol = 4, byrow = FALSE)
  cols <- paste(rep(colnames(subana.N)[1:2], each = 2),
                rownames(subana.N)[1:2], sep = "_with_")
  cols <- paste0(cols, "_N")
  
  colnames(subana.N_flat) <- cols
  subana.N_flat <- as.data.frame(subana.N_flat)
  
  subana.perc_flat <- matrix(subana.perc, nrow = 1, ncol = 4, byrow = FALSE)
  cols <- paste(rep(colnames(subana.perc)[1:2], each = 2),
                rownames(subana.perc)[1:2], sep = "_with_")
  cols <- paste0(cols, "_perc")
  
  colnames(subana.perc_flat) <- cols
  subana.perc_flat <- as.data.frame(subana.perc_flat)
  
  # subana.flat <- cbind(totals, subana.N_flat, subana.perc_flat)
  subana.flat <- cbind(subana.N_flat, subana.perc_flat)
  
  return(subana.flat)
}



makeFlatNPercnot4 <- function(subana.N, nrow) {
  
  colnames(subana.N) <- gsub(" ", "_", colnames(subana.N))
  rownames(subana.N) <- gsub(" ", "_", rownames(subana.N))
  subana.perc <- round(prop.table(subana.N, 2)*100, 1)  # create the prop table
 # totals <- matrix(margin.table(subana.N, 2), nrow = 1, ncol = 2)
 # colnames(totals) <- paste0(colnames(subana.N), "_N")
  
  
  subana.N_flat <- matrix(subana.N, nrow = 1, ncol = nrow*2, byrow = FALSE)
  cols <- paste(rep(colnames(subana.N)[1:2], each = nrow),
                rownames(subana.N)[1:3], sep = "_with_")
  cols <- paste0(cols, "_N")
  
  colnames(subana.N_flat) <- cols
  subana.N_flat <- as.data.frame(subana.N_flat)
  
  subana.perc_flat <- matrix(subana.perc, nrow = 1, ncol = nrow*2, byrow = FALSE)
  cols <- paste(rep(colnames(subana.perc)[1:2], each = nrow),
                rownames(subana.perc)[1:3], sep = "_with_")
  cols <- paste0(cols, "_perc")
  
  colnames(subana.perc_flat) <- cols
  subana.perc_flat <- as.data.frame(subana.perc_flat)
  
 # subana.flat <- cbind(totals, subana.N_flat, subana.perc_flat)
  subana.flat <- cbind(subana.N_flat, subana.perc_flat)
  
  return(subana.flat)
}


mediSum <- function(x, variable) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable)
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
}

mediSumRound <- function(x, variable, roundno) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable)
  varcol[ ,3:5] <- format(round(varcol[ ,3:5], roundno), nsmall = roundno)
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
}


mediSumAdmiss <- function(x, variable) {
  variable <- as.character(variable)
  varcol <- filter(psychic.admiss, vars == variable)
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ ,  c(-1, -2)])
}



FreqSum <- function(x, varname) {
  
  varname <- as.character(varname)
  gen <- x %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  colnames(var_N) <- paste0(varname, "_N")
 
  if(nrow(gen) == 0) {return(var_N)}

  else {
            
  gen0 <- as.data.frame(table(gen[[1]]))
  gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
    dplyr::rename(perc = Freq)
  gen2 <- inner_join(gen0, gen1, by = "Var1")
  # gen.E2$England <- paste(gen.E2$Freq, " (", gen.E2$perc, ")", sep = "")
  # gen.E2 <- select(gen.E2, Var1, England)
  for (i in 1:nrow(gen2)) {
    gen3 <- gen2
    gen3$Var1 <- as.character(gen3$Var1)
    gen3 <- gen3[i, ]
    colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                        paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
    var_N <- cbind(var_N, gen3[ ,2:3])
  }
  return(var_N)
  
  }
}


nrow(filter(dat, country == "England"))
nrow(filter(dat, country == "Wales"))
nrow(filter(dat, country == "Scotland"))
nrow(dat)


######## s t a r t  h e r e . . .  ###########   



# Need to set up the summary table for numeric variables:

psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
psychic <- as.data.frame(psychic)
psychic$vars <- row.names(psychic)
psychic <- psychic %>% rename(median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75) %>%
  dplyr::select(vars, n, median, lo.quart, hi.quart)

admissions <- dat %>% group_by(HospCode) %>% summarise(admiss.no = n()) 

psychic.admiss <- psych::describe(admissions, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
psychic.admiss <- as.data.frame(psychic.admiss)
psychic.admiss$vars <- row.names(psychic.admiss)
psychic.admiss <- psychic.admiss %>% rename(median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75) %>%
  dplyr::select(vars, n, median, lo.quart, hi.quart)

# 1.1 Age

levlev <- mediSum(dat, "Age")
levlev$hospital.code <- "All"
levlev$hospital.name <- "All"
levlev$trust.code <- "All"
levlev$trust.name <- "All"
levlev$cases_n <- levlev$Age_n
levlev <- levlev[ ,c(5:9, 1:4)]


# 1.2 Gender


levlev <- cbind(levlev, FreqSum(dat, "Gender"))

# 1.3.1 IMD quintiles

levlev <- cbind(levlev, FreqSum(dat, "IMDeng.quintile"))
levlev <- cbind(levlev, FreqSum(dat, "IMDwales.quintile"))
levlev <- cbind(levlev, FreqSum(dat, "IMDscot.quintile"))


# median admissions


levlev <- cbind(levlev, mediSumAdmiss(admissions, "admiss.no"))

# 1.4.4 Day and time of admission to hospital



admisstimedow.N <- table(dat$arrival2hourtimes, dat$admiss.day)
rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")

admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
                rownames(admisstimedow.N)[1:12], "admiss_n", sep = "_")

colnames(admisstime_flat) <- colsss
admisstime_flat <- as.data.frame(admisstime_flat)
levlev <- cbind(levlev, admisstime_flat)


admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)
rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")

admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
                rownames(admisstimedow.perc)[1:12], "admiss_perc", sep = "_")

colnames(admisstime_flat_perc) <- colsssperc
admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
levlev <- cbind(levlev, admisstime_flat_perc)

admisstimedow.N.all <- admisstimedow.N


levlev$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
levlev$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
levlev$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
levlev$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
levlev$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
levlev$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
levlev$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]






# Q1.5 length of stay days

levlev <- cbind(levlev, mediSum(dat, "lengthofstaydays"))


# Q1.7 Lifestatusatdischarge

levlev <- cbind(levlev, FreqSum(dat, "Lifestatusatdischarge"))


# question 2.1 - smoking status

levlev <- cbind(levlev, FreqSum(dat, "Smokingstatus"))


# question 2.2 - smokers with tobacco dependency addressed

levlev <- cbind(levlev, FreqSum(dat[dat$Smokingstatus == "Current smoker", ], "Dischargetobaccofac"))



# Q2.1 Heart rate


levlev <- cbind(levlev, mediSum(dat, "Heartrate"))


# Q2.2.1 Resp rate

levlev <- cbind(levlev, mediSum(dat, "Respiratoryrate"))


# Q2.3.1 Oxygen Saturation recorded

levlev <- cbind(levlev, FreqSum(dat, "Oxygensatrecorded"))


# Q2.3.2 Oxygen saturation value

levlev <- cbind(levlev, mediSum(dat, "Oxygensatvalue"))


# 2.3.3 Was the measurement taken whilst the patient was on supplementary oxygen?

levlev <- cbind(levlev, FreqSum(dat, "Oxygenmeasurement"))



# Q2.4.1 Did the patient have a peak flow (PEF) following arrival at hospital ?

levlev <- cbind(levlev, FreqSum(dat, "Peakflowonarrival"))


# Q2.4.2 Median time, in hours, from arrival at hospital to PEF measurement

levlev <- cbind(levlev, mediSumRound(dat, "arrivaltoPEFhours", 1))


# Arrival to PEF categories

levlev <- cbind(levlev, FreqSum(dat, "arrivaltoPEF1hourcat"))

levlev <- cbind(levlev, FreqSum(dat, "arrivaltoPEF4hourcat"))


# Q2.4.something Day and time of admission to hospital in those with PEF



admisstimedow.N <- table(dat$arrival2hourtimes[!is.na(dat$X3.4.Peak.Flow.Pre.Arrival)], 
                         dat$admiss.day[!is.na(dat$X3.4.Peak.Flow.Pre.Arrival)])
rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")

admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
                rownames(admisstimedow.N)[1:12], "admiss_with_PEF_n", sep = "_")

colnames(admisstime_flat) <- colsss
admisstime_flat <- as.data.frame(admisstime_flat)
levlev <- cbind(levlev, admisstime_flat)


admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.all)*100, 1)
rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")

admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
                    rownames(admisstimedow.perc)[1:12], "admiss_with_PEF_perc", sep = "_")

colnames(admisstime_flat_perc) <- colsssperc
admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
levlev <- cbind(levlev, admisstime_flat_perc)




levlev$Monday_admit_with_PEF_N <- margin.table(admisstimedow.N, 2)[1]
levlev$Tuesday_admit_with_PEF_N <- margin.table(admisstimedow.N, 2)[2]
levlev$Wednesday_admit_with_PEF_N <- margin.table(admisstimedow.N, 2)[3]
levlev$Thursday_admit_with_PEF_N <- margin.table(admisstimedow.N, 2)[4]
levlev$Friday_admit_with_PEF_N <- margin.table(admisstimedow.N, 2)[5]
levlev$Saturday_admit_with_PEF_N <- margin.table(admisstimedow.N, 2)[6]
levlev$Sunday_admit_with_PEF_N <- margin.table(admisstimedow.N, 2)[7]





# 2.4.4 Did the patient  have a previous best PEF?
levlev <- cbind(levlev, FreqSum(dat, "PreviousbestPEF"))

# 2.4.5 If previous best PEF was not known, did the patient  have a predicted PEF  calculated?

levlev <- cbind(levlev, FreqSum(dat[dat$PreviousbestPEF == "Not recorded", ], "PredictedPEF"))



# Out of patients who had PEF taken on arrival, what proportion had a previous best or predicted taken?

levlev <- cbind(levlev, FreqSum(dat[dat$Peakflowonarrival == "Yes", ], "prevorpredtaken"))




# 2.4.6 PEF on admission as a percentage of best PEF or predicted PEF  

# Create a new variable that refers to either their best previous or predicted PEF

levlev <- cbind(levlev, mediSumRound(dat, "percpredPEF", 1))


levlev <- cbind(levlev, FreqSum(dat, "percpredPEF75cat"))






# 3.1.1 Was the patient reviewed by a respiratory specialist during their admission?
levlev <- cbind(levlev, FreqSum(dat, "Specrespreview"))


# Q3.1.2 Median time, in hours, from arrival at hospital to spec rev 
levlev <- cbind(levlev, mediSumRound(dat, "arrivaltospecresprevhours", 1))


# specicialist review 24 hour categories

levlev <- cbind(levlev, FreqSum(dat, "arrivaltospecresprev24hourcat"))


# 3.2.1 Was oxygen  prescribed/administered for the patient at any point during admission?
levlev <- cbind(levlev, FreqSum(dat, "Oxygenprescribed"))


# 3.3.1 Was the patient administered systemic steroids following arrival at hospital?
levlev <- cbind(levlev, FreqSum(dat, "Steroidsadminist"))



# 3.3.2 Median time, in hours, from arrival at hospital to administration of systemic steroids

levlev <- cbind(levlev, mediSumRound(dat, "arrivaltosteroidshours", 1))


# 3.3.3 Number of patients  receiving systemic steroids within 4 hours

levlev <- cbind(levlev, FreqSum(dat, "steroids1hourcat"))


# 3.3.3 Number of patients  receiving systemic steroids within 4 hours

levlev <- cbind(levlev, FreqSum(dat, "steroids4hourcat"))


# filter(dat, steroids4hourcat == "Received within 4 hours")

# 3.3.4 Percentage of patients who received systemic steroids within 4 hours by day and time of arrival 



admisstimedow.N <- table(dat$arrival2hourtimes[dat$steroids4hourcat == "Steroids received within 4 hours"], 
                         dat$admiss.day[dat$steroids4hourcat == "Steroids received within 4 hours"])
rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")

admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
                rownames(admisstimedow.N)[1:12], "admiss_with_4hour_steroids_n", sep = "_")

colnames(admisstime_flat) <- colsss
admisstime_flat <- as.data.frame(admisstime_flat)
levlev <- cbind(levlev, admisstime_flat)


admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.all)*100, 1)
rownames(admisstimedow.perc) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")

admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 84, byrow = FALSE)
colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 12),
                    rownames(admisstimedow.perc)[1:12], "admiss_with_4hour_steroids_perc", sep = "_")

colnames(admisstime_flat_perc) <- colsssperc
admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
levlev <- cbind(levlev, admisstime_flat_perc)




levlev$Monday_admit_with_4hour_steroids_N <- margin.table(admisstimedow.N, 2)[1]
levlev$Tuesday_admit_with_4hour_steroids_N <- margin.table(admisstimedow.N, 2)[2]
levlev$Wednesday_admit_with_4hour_steroids_N <- margin.table(admisstimedow.N, 2)[3]
levlev$Thursday_admit_with_4hour_steroids_N <- margin.table(admisstimedow.N, 2)[4]
levlev$Friday_admit_with_4hour_steroids_N <- margin.table(admisstimedow.N, 2)[5]
levlev$Saturday_admit_with_4hour_steroids_N <- margin.table(admisstimedow.N, 2)[6]
levlev$Sunday_admit_with_4hour_steroids_N <- margin.table(admisstimedow.N, 2)[7]



# 3.4.1 Was the patient  administered beta agonists following arrival at hospital?

levlev <- cbind(levlev, FreqSum(dat, "B2agonistsadminist"))


# 3.4.2 Median time, in minutes, from arrival at hospital to administration of beta agonists

levlev <- cbind(levlev, mediSum(dat, "arrivaltob2agonistsminutes"))



# 3.4.3 Beta agonists one hour category

levlev <- cbind(levlev, FreqSum(dat, "b2agonists1hourcat"))


# 3.4.4 Beta agonists 4 hours category

levlev <- cbind(levlev, FreqSum(dat, "b2agonists4hourcat"))



# 4.1 Day of discharge

levlev <- cbind(levlev, FreqSum(dat, "discharge.day"))


# 4.2 Discharge bundle given

levlev <- cbind(levlev, FreqSum(dat, "dischargebundle"))


# discharge bundle by day of discharge

DBday.N <- table(dat$discharge.day, dat$dischargebundle)
colnames(DBday.N) <- gsub(" ", "_", colnames(DBday.N))
DBday.perc <- round(prop.table(DBday.N, 1)*100, 1)

DBday.N_flat <- matrix(DBday.N, nrow = 1, ncol = 21, byrow = FALSE)
cols <- paste(rep(colnames(DBday.N)[1:3], each = 7),
                rownames(DBday.N)[1:7], sep = "_")
cols <- paste0(cols, "_N")

colnames(DBday.N_flat) <- cols 
DBday.N_flat <- as.data.frame(DBday.N_flat)
levlev <- cbind(levlev, DBday.N_flat)


DBday.perc_flat <- matrix(DBday.perc, nrow = 1, ncol = 21, byrow = FALSE)
cols <- paste(rep(colnames(DBday.perc)[1:3], each = 7),
              rownames(DBday.perc)[1:7], sep = "_")
cols <- paste0(cols, "_perc")


colnames(DBday.perc_flat) <- cols
DBday.perc_flat <- as.data.frame(DBday.perc_flat)

levlev <- cbind(levlev, DBday.perc_flat)








# Discharge elements

levlev <- cbind(levlev, FreqSum(dat, "Dischargeinhalercheckfac"))
levlev <- cbind(levlev, FreqSum(dat, "Dischargemaintmedfac"))
levlev <- cbind(levlev, FreqSum(dat, "Dischargeadherencefac"))
levlev <- cbind(levlev, FreqSum(dat, "DischargePAAPfac"))
levlev <- cbind(levlev, FreqSum(dat, "Dischargetriggersfac"))
levlev <- cbind(levlev, FreqSum(dat, "DischargecommFU2daysfac"))
levlev <- cbind(levlev, FreqSum(dat, "Dischargespecrev4weeksfac"))
levlev <- cbind(levlev, FreqSum(dat, "Dischargenonefac"))


levlev <- cbind(levlev, FreqSum(dat, "goodpracticecarefac"))


#### Include this at some point at the end #### !!!!!!!!!!!!!!!!!!!!!!!!!!!

# colnames(levlev) <- gsub("_0_", "_no_", levlev)
# colnames(levlev) <- gsub("_1_", "_yes_", levlev)



# 5.1.1 Was the patient in receipt of inhaled steroids at discharge?
levlev <- cbind(levlev, FreqSum(dat, "Inhaledsteroidsatdischarge"))


# 5.1.1 Was the patient prescribed at least 5 days of oral steroids for treatment of their asthma attack?
levlev <- cbind(levlev, FreqSum(dat, "Oralsteroidsatdischarge"))


# 5.3.1 Has the patient been prescribed more than 2 courses of oral steroids in the last 12 months ?
levlev <- cbind(levlev, FreqSum(dat, "Oralsteroids2in12months"))


# 5.4.1 Was the patient referred for hospital assessment/follow up for asthma?
levlev <- cbind(levlev, FreqSum(dat, "referredforFU"))


# 5.4.2 If the patient was prescribed more than 2 courses of oral steroids in the last 12 months, were they
# referred for hospital assessment/follow up for asthma?
Oralsteroids2in12monthswithreferredforFU <- FreqSum(dat[dat$Oralsteroids2in12months == "Yes", ], "referredforFU")
colnames(Oralsteroids2in12monthswithreferredforFU) <- paste0("Oralsteroids2in12monthswith", 
                                                             colnames(Oralsteroids2in12monthswithreferredforFU))

levlev <- cbind(levlev, Oralsteroids2in12monthswithreferredforFU)




# - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# This is how far is required for hospital level data   #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - #




levlev <- levlev[ ,!(duplicated(colnames(levlev)) == TRUE)]

# ncol(levlev)
# ncol(levlev[ ,!(duplicated(colnames(levlev)) == TRUE)])
# 
# colnames(levlev)



