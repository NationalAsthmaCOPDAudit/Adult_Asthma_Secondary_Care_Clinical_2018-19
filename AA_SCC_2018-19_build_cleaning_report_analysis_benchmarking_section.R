#-----------------------------------------------------#
# Adult Asthma Secondary Care Clinical 2019 Analysis  #
# Author: Alex Adamson                                #
#-----------------------------------------------------#

# Set up the libraries

library(dplyr)
library(readstata13)
# library(xlsx)
source("H:/My R functions/MySummary.R")
source("H:/My R functions/createUniqueKey.R")
source("H:/My R functions/niceN.R")
source("H:/My R functions/niceP.R")
library(janitor)
library(officer)
library(flextable)
library(tidyverse)
library(survival)
library(survminer)
# library(ggplot2)
library(survsup)
# library(epitools)
library(psych)
library(chron)


CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}

# Data cleaning:

# Read in the SCC data

dat <- read.csv("D:/Alex/Adult Asthma/Secondary Care Clinical 2018-2019/rawData/NACAP-AA-1811-1903-v101.csv",
                header = TRUE, stringsAsFactors = FALSE)
nrow(dat)
# Going to create more easily identified study IDs (unique at a record level) and patient IDs (unique at a patient level)


dat <- createUniqueKey(dat, "PatientID", "alexpatID")
dat <- createUniqueKey(dat, "STUDYID", "alexstudyID")



# If you want to re-identify the old pseudo-anonymised ID, this is the key

# IDkey <- select(dat, STUDYID, PatientID, alexstudyID, alexpatID)

# Let's start of with all the CLEANING ####


# STUDYID is unique for each hospital visit
# PatientID is unique for each patient

# We should filter out all those with an invalid NHS number. Invalid numbers are due to being overseas,
# having no fixed abode, having the wrong NHS number, or being a private patient (check email from Tim)


dat <- dat %>% filter(is.na(InvalidNHS))

# And now, let's get all our data in a suitable format


# Convert data and time strings to suitable values, along with creating a new variable 'DT' that combines
# the two

dat$X1.1a.Arrival.Date <- chron(dat$X1.1a.Arrival.Date, format = "d/m/Y")
dat$X1.1b.Arrival.Time <- paste0(dat$X1.1b.Arrival.Time, ":00") # Need to add this in so chron recognises it
dat$X1.1b.Arrival.Time <- chron(times. = dat$X1.1b.Arrival.Time, format = "h:m:s")
dat$X1.1.Arrival.DT <- chron(dat$X1.1a.Arrival.Date, dat$X1.1b.Arrival.Time, format = c("d/m/Y", "h:m:s"))

# Filter out invalid dates (N = 0)

dat %>% filter(X1.1a.Arrival.Date < "01/11/2018") %>% nrow()
dat <- dat %>% filter(X1.1a.Arrival.Date >= "01/11/2018")

dat %>% filter(X1.1a.Arrival.Date > "31/03/2019") %>% nrow()
dat <- dat %>% filter(X1.1a.Arrival.Date <= "31/03/2019")


dat$X2.3.Gender <- as.factor(dat$X2.3.Gender)
dat$X2.5.Smoking <- as.factor(dat$X2.5.Smoking)


# Filter out invalid heart rates (0-200) or resp rates (0-60)

dat %>% filter(X3.1.Heart.Rate < 0) %>% nrow()
dat <- dat %>% filter(X3.1.Heart.Rate >= 0 | is.na(X3.1.Heart.Rate))

dat %>% filter(X3.1.Heart.Rate > 200) %>% nrow()
dat <- dat %>% filter(X3.1.Heart.Rate <= 200 | is.na(X3.1.Heart.Rate))


dat %>% filter(X3.2.Respiratory.Rate <0) %>% nrow()
dat <- dat %>% filter(X3.2.Respiratory.Rate >= 0 | is.na(X3.2.Respiratory.Rate))

dat %>% filter(X3.2.Respiratory.Rate >60) %>% nrow()
dat <- dat %>% filter(X3.2.Respiratory.Rate <= 60 | is.na(X3.2.Respiratory.Rate))


# Filter out invalid oxygen saturation

summary(dat$X3.3.Oxygen.Saturation.)

dat %>% filter(X3.3.Oxygen.Saturation. <60) %>% nrow()
dat <- dat %>% filter(X3.3.Oxygen.Saturation. >= 60 | is.na(X3.3.Oxygen.Saturation.))

dat %>% filter(X3.3.Oxygen.Saturation. >100) %>% nrow()
dat <- dat %>% filter(X3.3.Oxygen.Saturation. <= 100 | is.na(X3.3.Oxygen.Saturation.))


# Let's not recode this. apart from to change the blank to "Recorded" and change it to a factor.
dat$X3.3.1.OxygenSaturation.NR[dat$X3.3.1.OxygenSaturation.NR == ""] <- "Recorded"
dat$X3.3.1.OxygenSaturation.NR <- as.factor(dat$X3.3.1.OxygenSaturation.NR)

dat$X3.3a.Oxygen.Measurement[dat$X3.3a.Oxygen.Measurement == ""] <- NA
dat$X3.3a.Oxygen.Measurement <- as.factor(dat$X3.3a.Oxygen.Measurement)


dat$X3.4.1.Peak.Flow.NR[dat$X3.4.1.Peak.Flow.NR == ""] <- "Yes"
dat$X3.4.1.Peak.Flow.NR <- as.factor(dat$X3.4.1.Peak.Flow.NR)


summary(dat$X3.4.Peak.Flow.Pre.Arrival)
summary(dat$X3.5.Previous.Peak.Flow)
summary(dat$X3.5a.Predicted.Peak.Flow)

# No one has a peak flow with a wrong value.


# Convert the peak flow recorded date and times


dat$X3.4a.Peak.Flow.Recorded.Date <- chron(dat$X3.4a.Peak.Flow.Recorded.Date, format = "d/m/Y")


# Make sure you are not pasting ':00' onto empty strings
dat$X3.4b.Peak.Flow.Recorded.Time[dat$X3.4b.Peak.Flow.Recorded.Time != ""] <- paste0(
  dat$X3.4b.Peak.Flow.Recorded.Time[dat$X3.4b.Peak.Flow.Recorded.Time != ""], ":00") # Need to add this in so chron recognises it

# Some times aren't recorded where the dates are recorded - what do we do about these?
# Currently, the date+time variable is missing if either time or date is missing

dat$X3.4b.Peak.Flow.Recorded.Time <- chron(times. = dat$X3.4b.Peak.Flow.Recorded.Time, format = "h:m:s")
dat$X3.4b.Peak.Flow.Recorded.DT <- chron(dat$X3.4a.Peak.Flow.Recorded.Date, 
                                             dat$X3.4b.Peak.Flow.Recorded.Time, format = c("d/m/Y", "h:m:s"))


dat$X3.4.1.Peak.Flow.NR <- as.factor(dat$X3.4.1.Peak.Flow.NR)

summary(dat$X3.4.Peak.Flow.Pre.Arrival)
summary(dat$X3.4.1.Peak.Flow.NR)



dat %>% filter(!is.na(dat$X3.4.Peak.Flow.Pre.Arrival)) %>% nrow()

dat %>% filter(X3.4.1.Peak.Flow.NR == "Not recorded" | X3.4.1.Peak.Flow.NR == "Patient too unwell") %>% nrow()

dat %>% filter((X3.4.1.Peak.Flow.NR == "Not recorded" | X3.4.1.Peak.Flow.NR == "Patient too unwell") & 
  !is.na(X3.4.Peak.Flow.Pre.Arrival)) %>% nrow()

dat %>% filter((X3.4.1.Peak.Flow.NR == "Not recorded" | X3.4.1.Peak.Flow.NR == "Patient too unwell") & 
                        !is.na(X3.4.Peak.Flow.Pre.Arrival)) %>% nrow()

dat %>% filter((X3.4.1.Peak.Flow.NR == "Not recorded" | X3.4.1.Peak.Flow.NR == "Patient too unwell") & 
                 !is.na(X3.4.Peak.Flow.Pre.Arrival)) %>% nrow()

# 7539 have a value for peak flow

dat %>% filter(!is.na(X3.4.Peak.Flow.Pre.Arrival)) %>% nrow()

dat %>% filter(X3.4.1.Peak.Flow.NR == "Not recorded" | X3.4.1.Peak.Flow.NR == "Patient too unwell") %>% nrow()

dat %>% filter((X3.4.1.Peak.Flow.NR == "Not recorded" | X3.4.1.Peak.Flow.NR == "Patient too unwell") & 
                 !is.na(X3.4.Peak.Flow.Pre.Arrival)) %>% nrow()

# 4 people have a value for peak flow but are marked as not recorded or patient too unwell, so we remove them

dat <- dat %>% filter(!((X3.4.1.Peak.Flow.NR == "Not recorded" | X3.4.1.Peak.Flow.NR == "Patient too unwell") & 
                 !is.na(X3.4.Peak.Flow.Pre.Arrival)) | X3.4.1.Peak.Flow.NR == "Yes")


dat %>% filter(!is.na(X3.4.Peak.Flow.Pre.Arrival) &
                 X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded == "Not recorded" &
                 !is.na(X3.4a.Peak.Flow.Recorded.Date)) %>% nrow()

dat %>% filter(X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded == "Not recorded" & 
                 !is.na(X3.4a.Peak.Flow.Recorded.Date)) %>% nrow()

# 2 People had a value for peak flow, were marked as "date not recorded", but still had a date for recorded
# Both removed.

dat <- dat %>% filter(!(!is.na(X3.4.Peak.Flow.Pre.Arrival) &
                 X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded == "Not recorded" &
                 !is.na(X3.4a.Peak.Flow.Recorded.Date)))

# No one has 'date not recorded' or 'Time not recorded' as a value when they don't have a peak flow value

dat %>% filter(is.na(X3.4.Peak.Flow.Pre.Arrival) &
                          X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded == "Not recorded") %>% nrow()

dat %>% filter(is.na(X3.4.Peak.Flow.Pre.Arrival) &
                 X3.4b.1.Peak.Flow.Recoding.Time.Not.Recorded == "Not recorded") %>% nrow()


# 4 people are missing a value for peak flow but have a date for recording it
# Inconsistent so removed
                      
dat <- dat %>% filter(!(is.na(X3.4.Peak.Flow.Pre.Arrival) &
                 !is.na(X3.4a.Peak.Flow.Recorded.Date)))

# No one has a missing value for peak flow but then had a recorded time.

dat %>% filter(is.na(X3.4.Peak.Flow.Pre.Arrival) &
                   !is.na(X3.4b.Peak.Flow.Recorded.Time)) %>% nrow()



dat$PFminusA.DT <- dat$X3.4b.Peak.Flow.Recorded.DT - dat$X1.1.Arrival.DT

dat$PFminusA.Date <- dat$X3.4a.Peak.Flow.Recorded.Date - dat$X1.1a.Arrival.Date


summary(dat$PFminusA.DT)
summary(dat$PFminusA.Date)

table(dat$X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded)
table(dat$X3.4b.1.Peak.Flow.Recoding.Time.Not.Recorded)
table(dat$X3.4.1.Peak.Flow.NR, useNA = "ifany")
dat %>% filter(!is.na(X3.4.Peak.Flow.Pre.Arrival)) %>% nrow()

# all the time not recorded, date not recorded, and peak flow values are now consistent.

table(dat$X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded, dat$X3.4b.1.Peak.Flow.Recoding.Time.Not.Recorded)

dat %>% filter(!is.na(X3.4b.Peak.Flow.Recorded.DT)) %>% nrow()
dat %>% filter(!is.na(X3.4a.Peak.Flow.Recorded.Date)) %>% nrow()


# It's allowed for peak flow to be measured up to 1 hour before arrival at hospital.

# Does anyone without a time arrive the day before?
# No. This makes things easier

dat %>% filter(PFminusA.Date <0 & X3.4b.1.Peak.Flow.Recoding.Time.Not.Recorded == "Not recorded") %>% nrow()

# Therefore, we need to filter out anyone who has a peak flow measurement over an hour before arriving

# 15 people had a peak flow measurement over an hour before arrival, so need to be removed.

dat %>% filter(PFminusA.DT  < -((1/24)+(1/24/60/2))) %>% nrow()
dat %>% filter(PFminusA.DT  < -((1/24)+(1/24/60/2))) %>% select(X1.1b.Arrival.Time, X3.4b.Peak.Flow.Recorded.Time)



dat <- dat %>% filter(PFminusA.DT >= -((1/24)+1/24/60/2) | is.na(PFminusA.DT))

summary(dat$PFminusA.DT)
summary(dat$PFminusA.Date)

# Change "" to "recorded or N/A"
dat$X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded[
  dat$X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded == ""] <- "Recorded or N/A"
dat$X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded <- as.factor(dat$X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded)

# Change "" to "recorded or N/A"
dat$X3.4b.1.Peak.Flow.Recoding.Time.Not.Recorded[
  dat$X3.4b.1.Peak.Flow.Recoding.Time.Not.Recorded == ""] <- "Recorded or N/A"
dat$X3.4b.1.Peak.Flow.Recoding.Time.Not.Recorded <- as.factor(dat$X3.4b.1.Peak.Flow.Recoding.Time.Not.Recorded)



# One person here who is recorded as having their previous peak flow as 'not recorded' does actually have a value
# for previous peak flow

dat %>% filter((X3.5.1.Previous.Peak.Flow.Not.Recorded == "Not recorded" & !is.na(X3.5.Previous.Peak.Flow))) %>%
  nrow()

# REMOVE - inconsistent. N = 1

dat <- dat %>% filter(!(X3.5.1.Previous.Peak.Flow.Not.Recorded == "Not recorded" & !is.na(X3.5.Previous.Peak.Flow)))



# Change "" to "recorded"
dat$X3.5.1.Previous.Peak.Flow.Not.Recorded[dat$X3.5.1.Previous.Peak.Flow.Not.Recorded == ""] <- "Recorded"
dat$X3.5.1.Previous.Peak.Flow.Not.Recorded <- as.factor(dat$X3.5.1.Previous.Peak.Flow.Not.Recorded)

# Change "" to "recorded"
dat$X3.5a.1.Predicted.Peak.Flow.Not.Recorded[dat$X3.5a.1.Predicted.Peak.Flow.Not.Recorded == ""] <- "Recorded"
dat$X3.5a.1.Predicted.Peak.Flow.Not.Recorded <- as.factor(dat$X3.5a.1.Predicted.Peak.Flow.Not.Recorded)

table(dat$X3.5a.1.Predicted.Peak.Flow.Not.Recorded)
table(dat$X3.5.1.Previous.Peak.Flow.Not.Recorded)

# Everyone who has a previous peak flow DOESN'T have a previous peak flow, and vice versa.

dat %>% filter(!is.na(X3.5.Previous.Peak.Flow) & !is.na(X3.5a.Predicted.Peak.Flow)) %>% nrow()
dat %>% filter(!is.na(X3.5.Previous.Peak.Flow) | !is.na(X3.5a.Predicted.Peak.Flow)) %>% nrow()

# The 'not recorded' for previous peak flow is consistent.

dat %>% filter(X3.5.1.Previous.Peak.Flow.Not.Recorded == "Not recorded" & !is.na(X3.5a.Predicted.Peak.Flow)) %>% nrow()
dat %>% filter(is.na(X3.5.Previous.Peak.Flow) & !is.na(X3.5a.Predicted.Peak.Flow)) %>% nrow()

# The 'not recorded' for predicted peak flow is also consistent.

dat %>% filter(X3.5.1.Previous.Peak.Flow.Not.Recorded == "Recorded" & is.na(X3.5a.Predicted.Peak.Flow)) %>% nrow()
dat %>% filter(!is.na(X3.5.Previous.Peak.Flow) & is.na(X3.5a.Predicted.Peak.Flow)) %>% nrow()


# Everyone with a previous peak flow is not marked as having a previos peak flow as not recorded.
dat %>% filter(!is.na(X3.5.Previous.Peak.Flow) & X3.5.1.Previous.Peak.Flow.Not.Recorded == "Not recorded") %>% nrow()
dat %>% filter(!is.na(X3.5a.Predicted.Peak.Flow) & 
                 X3.5a.1.Predicted.Peak.Flow.Not.Recorded == "Not recorded") %>% nrow()

dat %>% filter(X3.5.1.Previous.Peak.Flow.Not.Recorded == "Not recorded" & 
                 X3.5a.1.Predicted.Peak.Flow.Not.Recorded == "Not recorded") %>% nrow()

# 2841 people don't have a previous peak flow or a predicted peak flow
# (was 2849 until inconsistencies removed)

# Specialist review:

dat %>% filter(X4.1.Specialist.Respiratory.Review == "No" &
                 (X4.1a.Date.of.respiratory.review. != "" | X4.1b.Time.of.respiratory.review. != "")) %>%
  nrow()

dat <- dat %>% filter(!(X4.1.Specialist.Respiratory.Review == "No" &
                 (X4.1a.Date.of.respiratory.review. != "" | X4.1b.Time.of.respiratory.review. != "")))


# 5 people have a date or time, but marked as no for specialist resp review
# Removed.

 
dat$X4.1a.Date.of.respiratory.review. <- chron(dat$X4.1a.Date.of.respiratory.review., format = "d/m/Y")

# Make sure you are not pasting ':00' onto empty strings
dat$X4.1b.Time.of.respiratory.review.[dat$X4.1b.Time.of.respiratory.review. != ""] <- paste0(
  dat$X4.1b.Time.of.respiratory.review.[dat$X4.1b.Time.of.respiratory.review. != ""], ":00") # Need to add this in so chron recognises it

# Some times aren't recorded where the dates are recorded - what do we do about these?
# Currently, the date+time variable is missing if either time or date is missing

dat$X4.1b.Time.of.respiratory.review. <- chron(times. = dat$X4.1b.Time.of.respiratory.review., format = "h:m:s")
dat$X4.1.respiratory.review.DT <- chron(dat$X4.1a.Date.of.respiratory.review., 
                                         dat$X4.1b.Time.of.respiratory.review., format = c("d/m/Y", "h:m:s"))





# Create a new variable for time until specialist respiratory review from arrival:

dat$SRRminusA.DT <- dat$X4.1.respiratory.review.DT - dat$X1.1.Arrival.DT


# 55 people had the specialist review at the same time as they came in.
# No one had their specialist review before they came in.

hist(dat$X4.1.respiratory.review.DT - dat$X1.1.Arrival.DT)
hist((dat$X4.1.respiratory.review.DT - dat$X1.1.Arrival.DT)[dat$X4.1.respiratory.review.DT - dat$X1.1.Arrival.DT<10])



# 46 people had their review after 10 days. 
# However, data is highly skewed - not necessarily an issue. Should there be a cut-off?
# Will find out later if they had their review before being discharged.


# Change 'Administered, prescribed' to 'prescribed, administered' because it's the same.
dat$X4.2.Oxygen.Prescribed[dat$X4.2.Oxygen.Prescribed == "Administered,Prescribed"] <- "Prescribed,Administered"
dat$X4.2.Oxygen.Prescribed <- as.factor(dat$X4.2.Oxygen.Prescribed)
summary(dat$X4.2.Oxygen.Prescribed)

# Ignore the other little variables for oxygen prescribed as I don't think we need them...


# Steroids:
table(dat$X4.3.Steroids.Administered, useNA = "ifany")

# Check for logical inconsistencies:


# Change steroids to a factor:

dat$X4.3.Steroids.Administered <- as.factor(dat$X4.3.Steroids.Administered)


# Change the steroids time and dates:


dat$X4.3a.Steroids.Date <- chron(dat$X4.3a.Steroids.Date, format = "d/m/Y")

# Make sure you are not pasting ':00' onto empty strings
dat$X4.3b.Steroids.Time[dat$X4.3b.Steroids.Time != ""] <- paste0(
  dat$X4.3b.Steroids.Time[dat$X4.3b.Steroids.Time != ""], ":00") # Need to add this in so chron recognises it

# Some times aren't recorded where the dates are recorded - what do we do about these?
# Currently, the date+time variable is missing if either time or date is missing

dat$X4.3b.Steroids.Time <- chron(times. = dat$X4.3b.Steroids.Time, format = "h:m:s")
dat$X4.3.Steroids.DT <- chron(dat$X4.3a.Steroids.Date, 
                                         dat$X4.3b.Steroids.Time, format = c("d/m/Y", "h:m:s"))



# Check steroids time with arrival time
# Exclude those with logical inconsistencies? 

summary(dat$X4.3.Steroids.DT)
summary(dat$X4.3a.Steroids.Date)
table(dat$X4.3.Steroids.Administered, useNA = "ifany")
head(sort(dat$X4.3.Steroids.DT - dat$X1.1.Arrival.DT), 50)
tail(sort(dat$X4.3.Steroids.DT - dat$X1.1.Arrival.DT), 50)

# 23 people had steroids over 10 days after they came in



length(sort(dat$X4.3.Steroids.DT - dat$X1.1.Arrival.DT)[
  sort(dat$X4.3.Steroids.DT - dat$X1.1.Arrival.DT) == 0])

# 158 people had steroids at the same time as they came in.
# No one had steroids before they came in.

hist(dat$X4.3.Steroids.DT - dat$X1.1.Arrival.DT)
hist((dat$X4.3.Steroids.DT - dat$X1.1.Arrival.DT)[dat$X4.3.Steroids.DT - dat$X1.1.Arrival.DT<1])


# 36 people have a date or time for administered when marked as "not administered" (All of these have no date)
# 2 of these people had a date and time for beta-agonists when they weren't recorded


# 16 people have a date or time for administered when marked as "not recorded" (All of these have no date)
# 7 of these had a date for beta-agonists when not recorded

dat %>% filter(X4.3.Steroids.Administered == "Not administered" &
                 (!is.na(X4.3a.Steroids.Date) | !is.na(X4.3b.Steroids.Time)))

dat <- dat %>% filter(!(X4.3.Steroids.Administered == "Not administered" &
                 (!is.na(X4.3a.Steroids.Date) | !is.na(X4.3b.Steroids.Time))))

dat %>% filter(is.na(dat$X4.3.Steroids.Administered))

dat %>% filter(X4.3.Steroids.Administered == "Not recorded" &  
  (!is.na(X4.3a.Steroids.Date) | !is.na(X4.3b.Steroids.Time))) %>% nrow()

dat <- dat %>% filter(!(X4.3.Steroids.Administered == "Not recorded" &
  (!is.na(X4.3a.Steroids.Date) | !is.na(X4.3b.Steroids.Time))))



# Nobody has a date but no time
dat %>% filter(is.na(X4.3a.Steroids.Date) & !is.na(X4.3b.Steroids.Time)) %>% nrow()
dat %>% filter(!is.na(X4.3a.Steroids.Date) & is.na(X4.3b.Steroids.Time)) %>% nrow()

# Nobody has them administered but no time
dat %>% filter(X4.3.Steroids.Administered == "Yes" & is.na(X4.3a.Steroids.Date)) %>% nrow()




# Beta agonists:
table(dat$X4.4.Agonists.B2.On.Arrival, useNA = "ifany")

# Check for logical inconsistencies:



# Change beta-agonists to a factor:

dat$X4.4.Agonists.B2.On.Arrival <- as.factor(dat$X4.4.Agonists.B2.On.Arrival)


# Change the steroids time and dates:


dat$X4.4a.Agonists.B2.Date <- chron(dat$X4.4a.Agonists.B2.Date, format = "d/m/Y")

# Make sure you are not pasting ':00' onto empty strings
dat$X4.4b.Agonists.B2.Time[dat$X4.4b.Agonists.B2.Time != ""] <- paste0(
  dat$X4.4b.Agonists.B2.Time[dat$X4.4b.Agonists.B2.Time != ""], ":00") # Need to add this in so chron recognises it

# Some times aren't recorded where the dates are recorded - what do we do about these?
# Currently, the date+time variable is missing if either time or date is missing

dat$X4.4b.Agonists.B2.Time <- chron(times. = dat$X4.4b.Agonists.B2.Time, format = "h:m:s")
dat$X4.4.Agonists.B2.DT <- chron(dat$X4.4a.Agonists.B2.Date, 
                              dat$X4.4b.Agonists.B2.Time, format = c("d/m/Y", "h:m:s"))



# Check beta agonist time with arrival time
# Exclude those with logical inconsistencies? 

summary(dat$X4.4.Agonists.B2.DT)
summary(dat$X4.4a.Agonists.B2.Date)
summary(dat$X4.4b.Agonists.B2.Time)

table(dat$X4.4.Agonists.B2.On.Arrival)
head(sort(dat$X4.4.Agonists.B2.DT - dat$X1.1.Arrival.DT), 50)

length(sort(dat$X4.4.Agonists.B2.DT - dat$X1.1.Arrival.DT)[
  sort(dat$X4.4.Agonists.B2.DT - dat$X1.1.Arrival.DT) == 0])

# 238 people had beta-agonists at the same time as they came in.
# No one had beta-agonists before they came in.

table(dat$X4.4.Agonists.B2.On.Arrival)
tail(sort(dat$X4.4.Agonists.B2.DT - dat$X1.1.Arrival.DT), 50)


# 28 people had beta-agonists over 10 days after they came in.

hist(dat$X4.4.Agonists.B2.DT - dat$X1.1.Arrival.DT)
hist((dat$X4.4.Agonists.B2.DT - dat$X1.1.Arrival.DT)[dat$X4.4.Agonists.B2.DT - dat$X1.1.Arrival.DT<1])


# NOTE: 1 person has a date or time for administered when marked as "not administered", and this person also had
# a date for steroids when they weren't administered. However, this person was removed in the previous 
# round.
# NOTE: 13 people had a date or time for administered when marked as "not recorded" (All of these have no date),
# but due to previous rounds this number has dropped to 4.
# NOTE: 12 people had a date but no time before inconsistencies were excluded. Now, there are no people with
# a date but no time.

dat %>% filter(X4.4.Agonists.B2.On.Arrival == "Not administered") %>%
  filter(!is.na(X4.4a.Agonists.B2.Date) | !is.na(X4.4b.Agonists.B2.Time)) %>% nrow()

dat %>% filter(X4.4.Agonists.B2.On.Arrival == "Not administered") %>%
  filter(!is.na(X4.4a.Agonists.B2.Date)) %>% nrow()

dat %>% filter(X4.4.Agonists.B2.On.Arrival == "Not administered") %>%
  filter(!is.na(X4.4a.Agonists.B2.Date)) %>% print()



dat %>% filter(X4.4.Agonists.B2.On.Arrival == "Not recorded" &
  (!is.na(X4.4a.Agonists.B2.Date) | !is.na(X4.4b.Agonists.B2.Time))) %>% nrow()

dat <- dat %>% filter(!(X4.4.Agonists.B2.On.Arrival == "Not recorded" &
                 (!is.na(X4.4a.Agonists.B2.Date) | !is.na(X4.4b.Agonists.B2.Time))))


dat %>% filter(X4.4.Agonists.B2.On.Arrival == "Not recorded") %>% 
  filter(!is.na(X4.4a.Agonists.B2.Date)) %>% nrow()

dat %>% filter(X4.4.Agonists.B2.On.Arrival == "Not recorded") %>% 
  filter(!is.na(X4.4a.Agonists.B2.Date)) %>% print()



# 12 people used to have a date but no time, but have since been removed due to the earlier inconsistencies
dat %>% filter(is.na(X4.4a.Agonists.B2.Date) & !is.na(X4.4b.Agonists.B2.Time)) %>% nrow()
dat %>% filter(!is.na(X4.4a.Agonists.B2.Date) & is.na(X4.4b.Agonists.B2.Time)) %>% nrow()

# No one is marked as having beta agonists without also providing a date and time
dat %>% filter(X4.4.Agonists.B2.On.Arrival == "Yes" & is.na(X4.4a.Agonists.B2.Date)) %>% nrow()
dat %>% filter(X4.4.Agonists.B2.On.Arrival == "Yes" & is.na(X4.4b.Agonists.B2.Time)) %>% nrow()


# For discharge life status:

table(dat$X5.1.Discharge.Life.Status)

# 23 died as inpatient (this is true for the overall dataset with no exclusions too)

dat %>% filter(X5.1.Discharge.Life.Status == "Died as inpatient") %>% select(X.Age.At.Arrival)

# Those who died don't have a discharge date or time, but everyone else does
dat %>% filter(X5.2a.Discharge.Date == "") %>% nrow()
dat %>% filter(X5.2b.Discharge.Time == "") %>% nrow()



# Change discharge life status to a factor:

dat$X5.1.Discharge.Life.Status <- as.factor(dat$X5.1.Discharge.Life.Status)
summary(dat$X5.1.Discharge.Life.Status, useNA = "ifany")

# Change the discharge time and dates:


dat$X5.2a.Discharge.Date <- chron(dat$X5.2a.Discharge.Date, format = "d/m/Y")

# Make sure you are not pasting ':00' onto empty strings
dat$X5.2b.Discharge.Time[dat$X5.2b.Discharge.Time != ""] <- paste0(
  dat$X5.2b.Discharge.Time[dat$X5.2b.Discharge.Time != ""], ":00") # Need to add this in so chron recognises it

# Some times aren't recorded where the dates are recorded - what do we do about these?
# Currently, the date+time variable is missing if either time or date is missing

dat$X5.2b.Discharge.Time <- chron(times. = dat$X5.2b.Discharge.Time, format = "h:m:s")
dat$X5.2.Discharge.DT <- chron(dat$X5.2a.Discharge.Date, 
                                 dat$X5.2b.Discharge.Time, format = c("d/m/Y", "h:m:s"))

# Everyone has a date and time
summary(dat$X5.2.Discharge.DT)

# Check steroids time with arrival time
# Exclude those with logical inconsistencies? 

summary(dat$X5.2.Discharge.DT)
summary(dat$X5.2a.Discharge.Date)
summary(dat$X5.2b.Discharge.Time)

# 2 people were discharged at the same time as they arrived
# No one was discharged before they came in.

head(sort(dat$X5.2.Discharge.DT - dat$X1.1.Arrival.DT), 50)

length(sort(dat$X5.2.Discharge.DT - dat$X1.1.Arrival.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X1.1.Arrival.DT) == 0])

# Some people stayed for a very long time... maximum was 93 days...
tail(sort(dat$X5.2.Discharge.DT - dat$X1.1.Arrival.DT), 50)


hist(dat$X5.2.Discharge.DT - dat$X1.1.Arrival.DT)
hist((dat$X5.2.Discharge.DT - dat$X1.1.Arrival.DT)[dat$X5.2.Discharge.DT - dat$X1.1.Arrival.DT<10])



# Discharge bundle
# 24 people have "" for the discharge bundle elements. 23 of these people died as an inpatient,
# but 1 person didn't...

table(dat$X5.3.Discharge.Bundle, useNA = "ifany")

dat %>% filter(X5.4.Discharge.Elements == "") %>% nrow()

# All people marked as 'none' have no other elements (this is good)

dat %>% filter(X5.4.Discharge.Elements == "None") %>% select(
  X5.4.Discharge.Elements...Inhaler.technique.checked:X5.4.Discharge.Elements...Specialist.review.requested.within.4.weeks) %>%
  filter_all(any_vars(.==1))


dat %>% filter(X5.3.Discharge.Bundle == "No") %>% select(
  X5.4.Discharge.Elements...Inhaler.technique.checked:X5.4.Discharge.Elements...Specialist.review.requested.within.4.weeks) %>%
  filter_all(any_vars(.==1)) %>% nrow()

dat %>% filter(X5.3.Discharge.Bundle == "Self discharge") %>% select(
  X5.4.Discharge.Elements...Inhaler.technique.checked:X5.4.Discharge.Elements...Specialist.review.requested.within.4.weeks) %>%
  filter_all(any_vars(.==1)) %>% nrow()


# 3581 people have some people some of the discharge elements without receiving a bundle... but this is okay
# 103 people who self-discharged have some elements of good discharge practice - I think this is okay


dat$X5.3.Discharge.Bundle[dat$X5.3.Discharge.Bundle == ""] <- NA
dat$X5.3.Discharge.Bundle <- as.factor(dat$X5.3.Discharge.Bundle)


# One person is marked as "" instead of "None" for discharge elements despite being alive.
# Inconsistent so removed

dat %>% filter(X5.4.Discharge.Elements == "") %>% 
  select(X5.4.Discharge.Elements, X5.1.Discharge.Life.Status) %>% table() 

dat %>% filter(X5.4.Discharge.Elements == "" & X5.1.Discharge.Life.Status == "Alive") %>% nrow()
dat <- dat %>% filter(!(X5.4.Discharge.Elements == "" & X5.1.Discharge.Life.Status == "Alive"))



# Those who died in hospital do not have anything for these parts of discharge

# Let's create a function that turns "" to factors with missing

space2MF <- function(x) {
  x[x == ""] <- NA
  x <- as.factor(x)
  return(x)
}

table(dat$X6.1.Inhaled.Steroids.At.Discharge, dat$X5.1.Discharge.Life.Status, useNA = "ifany")
table(dat$X6.2.Oral.Steroids.at.Discharge, dat$X5.1.Discharge.Life.Status, useNA = "ifany")
table(dat$X6.3.Oral.Steroids...2, dat$X5.1.Discharge.Life.Status, useNA = "ifany")
table(dat$X6.4.Referred.for.Followup, dat$X5.1.Discharge.Life.Status, useNA = "ifany")

dat$X6.1.Inhaled.Steroids.At.Discharge <- space2MF(dat$X6.1.Inhaled.Steroids.At.Discharge)
dat$X6.2.Oral.Steroids.at.Discharge <- space2MF(dat$X6.2.Oral.Steroids.at.Discharge)
dat$X6.3.Oral.Steroids...2 <- space2MF(dat$X6.3.Oral.Steroids...2)
dat$X6.4.Referred.for.Followup <- space2MF(dat$X6.4.Referred.for.Followup)

# All dataset versions are V1 and all completion is 100%.

summary(dat)


# Are all the rest of the times before discharge?
# Peak flow recorded

head(sort(dat$X5.2.Discharge.DT - dat$X3.4b.Peak.Flow.Recorded.DT), 100)

tail(sort(dat$X5.2.Discharge.DT - dat$X3.4b.Peak.Flow.Recorded.DT), 100)

summary(dat$X5.2.Discharge.DT - dat$X3.4b.Peak.Flow.Recorded.DT)




length(sort(dat$X5.2.Discharge.DT - dat$X3.4b.Peak.Flow.Recorded.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X3.4b.Peak.Flow.Recorded.DT) == 0])

length(sort(dat$X5.2.Discharge.DT - dat$X3.4b.Peak.Flow.Recorded.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X3.4b.Peak.Flow.Recorded.DT) < 0])

length(sort(dat$X5.2.Discharge.DT - dat$X3.4b.Peak.Flow.Recorded.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X3.4b.Peak.Flow.Recorded.DT) > 0])

# 15 people had their peak flow recorded at the same time as their discharge
# 45 people were discharged before they had their peak flow recorded
# Some people had their peak flow recorded up to 90 days before discharge...



# Some people stayed for a very long time... maximum was 93 days...
tail(sort(dat$X5.2.Discharge.DT - dat$X3.4b.Peak.Flow.Recorded.DT), 50)



# Respiratory review


head(sort(dat$X5.2.Discharge.DT - dat$X4.1.respiratory.review.DT), 100)

tail(sort(dat$X5.2.Discharge.DT - dat$X4.1.respiratory.review.DT), 100)

summary(dat$X5.2.Discharge.DT - dat$X4.1.respiratory.review.DT)

length(sort(dat$X5.2.Discharge.DT - dat$X4.1.respiratory.review.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X4.1.respiratory.review.DT) == 0])

length(sort(dat$X5.2.Discharge.DT - dat$X4.1.respiratory.review.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X4.1.respiratory.review.DT) < 0])

length(sort(dat$X5.2.Discharge.DT - dat$X4.1.respiratory.review.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X4.1.respiratory.review.DT) > 0])

# 2 people were discharged before their respiratory review BEFORE inconsistencies were removed.
# Now, no one was discharged before their respiratory review.
# 31 people had their respiratory review at the same time as their discharge
# Some people stayed up to 90 days before discharge




# Steroids

head(sort(dat$X5.2.Discharge.DT - dat$X4.3.Steroids.DT), 100)

tail(sort(dat$X5.2.Discharge.DT - dat$X4.3.Steroids.DT), 100)

summary(dat$X5.2.Discharge.DT - dat$X4.3.Steroids.DT)

length(sort(dat$X5.2.Discharge.DT - dat$X4.3.Steroids.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X4.3.Steroids.DT) == 0])

length(sort(dat$X5.2.Discharge.DT - dat$X4.3.Steroids.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X4.3.Steroids.DT) < 0])

length(sort(dat$X5.2.Discharge.DT - dat$X4.3.Steroids.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X4.3.Steroids.DT) > 0])


# No one given steroids before discharge
# 10 people given steroids on arrival to hospital at the same time as being discharged 
# Some people stayed up to 90 days before discharge


# Beta-agonists

head(sort(dat$X5.2.Discharge.DT - dat$X4.4.Agonists.B2.DT), 100)

tail(sort(dat$X5.2.Discharge.DT - dat$X4.4.Agonists.B2.DT), 100)

summary(dat$X5.2.Discharge.DT - dat$X4.4.Agonists.B2.DT)

length(sort(dat$X5.2.Discharge.DT - dat$X4.4.Agonists.B2.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X4.4.Agonists.B2.DT) == 0])

length(sort(dat$X5.2.Discharge.DT - dat$X4.4.Agonists.B2.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X4.4.Agonists.B2.DT) < 0])

length(sort(dat$X5.2.Discharge.DT - dat$X4.4.Agonists.B2.DT)[
  sort(dat$X5.2.Discharge.DT - dat$X4.4.Agonists.B2.DT) > 0])


# No one given beta-agonists before discharge
# 18 people given beta-agonists on arrival to hospital at the same time as being discharged 
# Some people stayed up to 90 days before discharge


# I'm going to do a bit of recoding here, because I think this is a good place to do it.
# Those who died as inpatient do not have a chance to get any discharge elements. Rather than being
# marked as '0', I'm going to change it so they are missing.


# Here is a bit of code I found on the internet - subsets columns based on a text string.
# Grep returns the index of the pattern. So, here, grep is returning the column indexes so that they
# can be easily subset.

dat[, grep('X5.4.Discharge', colnames(dat))][dat$X5.1.Discharge.Life.Status == "Died as inpatient", ] <- NA

# Everyone who had their tobacco dependency addressed was a current smoker or not recorded
dat %>% filter(X5.4.Discharge.Elements...Tobacco.dependency.addressed == 1) %>% select(X2.5.Smoking) %>% table()



# Now, select people with an identical patient ID and arrival date

IDoi <- dat[duplicated(dat[, c("alexpatID", "X1.1a.Arrival.Date")]) == TRUE, ] %>% 
  select(alexpatID, X1.1a.Arrival.Date)

# Give them a marker
IDoi$dup <- 1

# rejoin them together
dat <- left_join(dat, IDoi, by = c("alexpatID", "X1.1a.Arrival.Date"))

# Check it's what you expect (it is what we expect)

sum(IDoi$dup)
sum(dat$dup, na.rm = TRUE)

table(dat$dup, useNA = "ifany")

dat$dup[is.na(dat$dup) == TRUE] <- 0

table(dat$dup, useNA = "ifany")

# Drop thoses which are replicated
dat <- dat %>% filter(dup != 1)


# write.csv(IDoi, "D:/Alex/Adult Asthma/Secondary Care Clinical 2018-2019/tidyData/check2.csv")



# ------------ END OF THE CLEANING!!!! ------------#



# Read in the IMD data
# Takes absolutely ages for R to do this for some reason I read them in earlier and created RDS files for them to
# be used instead

# IMDeng <- xlsx::read.xlsx(
#           "Z:/Group_work/PS_AA/General UK data/IMD/File_1_ID_2015_Index_of_Multiple_Deprivation.xlsx",
#                           sheetIndex = 2, stringsAsFactors = FALSE)
# saveRDS(IMDeng, "Z:/Group_work/PS_AA/General UK data/IMD/2015_Index_of_Multiple_Deprivation_Eng.RDS")

# IMDwales <- xlsx::read.xlsx(
#       "Z:/Group_work/PS_AA/General UK data/IMD/150812-wimd-2014-overall-domain-ranks-each-lsoa-revised-en.xlsx",
#            sheetIndex = 3, stringsAsFactors = FALSE)
# saveRDS(IMDwales, "Z:/Group_work/PS_AA/General UK data/IMD/2014_Index_of_Multiple_Deprivation_Wales.RDS")

# IMDscot <- xlsx::read.xlsx(
#      "Z:/Group_work/PS_AA/General UK data/IMD/Scottish IMD 2016.xlsx",
#           sheetIndex = 2, stringsAsFactors = FALSE)
# saveRDS(IMDscot, "Z:/Group_work/PS_AA/General UK data/IMD/2016_Index_of_Multiple_Deprivation_Scotland.RDS")


# Read in the RDS files

# IMDeng <- readRDS("Z:/Group_work/PS_AA/General UK data/IMD/2015_Index_of_Multiple_Deprivation_Eng.RDS")
# IMDwales <- readRDS("Z:/Group_work/PS_AA/General UK data/IMD/2014_Index_of_Multiple_Deprivation_Wales.RDS")
# IMDscot <- readRDS("Z:/Group_work/PS_AA/General UK data/IMD/2016_Index_of_Multiple_Deprivation_Scotland.RDS")

IMDeng <- readRDS("Z:/Group_work/PS_AA/General UK data/IMD/Archive/2015_Index_of_Multiple_Deprivation_Eng.RDS")
IMDwales <- readRDS("Z:/Group_work/PS_AA/General UK data/IMD/Archive/2014_Index_of_Multiple_Deprivation_Wales.RDS")
IMDscot <- readRDS("Z:/Group_work/PS_AA/General UK data/IMD/Archive/2016_Index_of_Multiple_Deprivation_Scotland.RDS")


# rename the IMD datasets to facilitate linkage

IMDeng <- dplyr::rename(IMDeng, lsoa11 = LSOA.code..2011.,
                  IMD.decile = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.)

IMDwales <- dplyr::rename(IMDwales, lsoa11 = LSOA.Code, IMD.quintile = WIMD.2014.Overall.Quintile)

IMDscot <- dplyr::rename(IMDscot, lsoa11 = Data_Zone, IMD.rank = Overall_SIMD16_rank)


# Create the quitiles for the English IMD data

IMDeng$IMD.quintile <- NA

IMDeng$IMD.quintile[IMDeng$IMD.decile == 1] <- 1
IMDeng$IMD.quintile[IMDeng$IMD.decile == 2] <- 1
IMDeng$IMD.quintile[IMDeng$IMD.decile == 3] <- 2
IMDeng$IMD.quintile[IMDeng$IMD.decile == 4] <- 2
IMDeng$IMD.quintile[IMDeng$IMD.decile == 5] <- 3
IMDeng$IMD.quintile[IMDeng$IMD.decile == 6] <- 3
IMDeng$IMD.quintile[IMDeng$IMD.decile == 7] <- 4
IMDeng$IMD.quintile[IMDeng$IMD.decile == 8] <- 4
IMDeng$IMD.quintile[IMDeng$IMD.decile == 9] <- 5
IMDeng$IMD.quintile[IMDeng$IMD.decile == 10] <- 5


# Create the quintiles for the Scottish IMD data

IMDscot <- IMDscot %>% mutate(IMD.quintile = ntile(IMD.rank, 5))

# Just checking that the quintile is going the right way; it is. 1 is most deprived, 5 is least deprived.
IMDscot %>% filter(IMD.rank == 1)
head(IMDscot)


# Get rid of all the useless columns for IMDeng and IMDwales

IMDeng <- dplyr::select(IMDeng, lsoa11, IMD.quintile) %>% rename(IMDeng.quintile = IMD.quintile)
IMDwales <- dplyr::select(IMDwales, lsoa11, IMD.quintile) %>% rename(IMDwales.quintile = IMD.quintile)
IMDscot <- dplyr::select(IMDscot, lsoa11, IMD.quintile) %>% rename(IMDscot.quintile = IMD.quintile)


# Merge them with the SCC data


dat <- left_join(dat, IMDeng, by = "lsoa11")
dat <- left_join(dat, IMDwales, by = "lsoa11")
dat <- left_join(dat, IMDscot, by = "lsoa11")

dat %>% nrow()

dat %>% filter(is.na(IMDeng.quintile) & is.na(IMDwales.quintile) & is.na(IMDscot.quintile)) %>% nrow()

# 197 people don't have an IMD quintiles. These are all people who do not have an lsoa11 code.

dat %>% filter(is.na(IMDeng.quintile) & is.na(IMDwales.quintile) & is.na(IMDscot.quintile)) %>% select(lsoa11)

dat %>% filter(lsoa11 == "") %>% nrow()


# I want to rename 'Org' to 'hosp' so that it makes more sense

# Some baseline statistics:

totN <- nrow(dat)      

dat <- rename(dat, HospCode = Org)

country <- read.csv("Z:/Group_work/PS_AA/General UK data/Hospital_codes_R_format.csv",
                    stringsAsFactors = FALSE)

country <- rename(country, country = Country)

dat <- left_join(dat, country, by = "HospCode")

dat %>% filter(country == "") %>% nrow()
dat %>% filter(is.na(country)) %>% nrow()
table(dat$country)

dat %>% filter(HospCode == "") %>% nrow()


# Everyone has been assigned a country according to their hospital code, and everyone has a hospital code.

table(dat$HospCode, dat$country)


# Only 96 people from Scottish hospitals - verified


# Let's create a function that will create median and interquartile tables, and one that will create N and % tables.

medTable <- function(x, varname) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")

  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- length(scot[!is.na(scot)])
  scotIQR <- quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(all, eng, scot, wal), nrow = 1, ncol = 4)
  
  colnames(ret) <- c(paste("All (N=", AN, ")", sep = ""),
                           paste("England (N=", EN, ")", sep = ""),
                           paste("Scotland (N=", SN, ")", sep = ""),
                           paste("Wales (N=", WN, ")", sep = ""))
  
  
  return(ret)
}

# And another one that will work for calculatng frequencies:

# Changing this so it's inline with what Sophie wants

myFreqTable <- function(x, varname) {
  
  
  varname <- as.character(varname)
  print(varname)
  gen.E <- x %>% filter(country == "England") %>% dplyr::select(!!varname) %>% drop_na()
  EN <- nrow(gen.E)
  gen.E0 <- as.data.frame(table(gen.E[[1]]))
  gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
  gen.E2$England <- paste(format(gen.E2$Freq, big.mark=",", trim=TRUE), " (", # N
                          trimws(format(round(gen.E2$perc, 1), nsmall = 1)), "%)", sep = "") # %
  gen.E2 <- select(gen.E2, Var1, England)
  print(gen.E2)
  
  
  gen.W <- x %>% filter(country == "Wales") %>% dplyr::select(!!varname) %>% drop_na()
  WN <- nrow(gen.W)
  gen.W0 <- as.data.frame(table(gen.W[[1]]))
  gen.W1 <- as.data.frame(round(prop.table(table(gen.W[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.W2 <- inner_join(gen.W0, gen.W1, by = "Var1")
  gen.W2$Wales <- paste(format(gen.W2$Freq, big.mark=",", trim=TRUE), " (",
                        trimws(format(round(gen.W2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.W2 <- select(gen.W2, Var1, Wales)
  print(gen.W2)
  
  gen.S <- x %>% filter(country == "Scotland") %>% dplyr::select(!!varname) %>% drop_na()
  SN <- nrow(gen.S)
  gen.S0 <- as.data.frame(table(gen.S[[1]]))
  gen.S1 <- as.data.frame(round(prop.table(table(gen.S[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.S2 <- inner_join(gen.S0, gen.S1, by = "Var1")
  gen.S2$Scotland <- paste(format(gen.S2$Freq, big.mark=",", trim=TRUE)," (",
                           trimws(format(round(gen.S2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.S2 <- select(gen.S2, Var1, Scotland)
  print(gen.S2)
  
  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(format(gen.A2$Freq, big.mark=",", trim=TRUE), " (",
                      trimws(format(round(gen.A2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
  print(gen.A2)
  
  gen.table <- inner_join(gen.A2, gen.E2, by = "Var1") %>% inner_join(gen.S2, by = "Var1") %>%
    inner_join(gen.W2, by = "Var1")
  colnames(gen.table) <- c(varname, paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  
  # row.names(gen.table) <- gen.table$Var1
  
  return(gen.table)
  }


myFreqTableonlyAll <- function(x, varname) {
  
  
  varname <- as.character(varname)

  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(format(gen.A2$Freq, big.mark=",", trim=TRUE), " (",
                      trimws(format(round(gen.A2$perc, 1), nsmall = 1)), "%)", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
  print(gen.A2)
  
  gen.table <- gen.A2
  colnames(gen.table) <- c(varname, paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  # row.names(gen.table) <- gen.table$Var1
  
  return(gen.table)
}






# At this point I'm going to remove the identifiable data columns.

dat.save <- dat
dat <- dat.save



dat <- select(dat, -STUDYID, -PatientID, -lsoa11)

# And now I'm going to remove the useless variables

dat <- select(dat, -InvalidNHS, -Overseas)

table(dat$TrustCode.x, dat$TrustCode.y)

dat %>% filter(TrustCode.x != TrustCode.y) %>% select(TrustCode.x, TrustCode.y, HospCode) %>% unique()

dat %>% filter(TrustCode.y %in% c("RTG", "RDE", "RRK", "RJF", "RGQ", "RR1")) %>% select(TrustCode.y, HospCode) %>% unique()
dat %>% filter(TrustCode.x %in% c("RTG", "RDE", "RRK", "RJF", "RGQ", "RR1")) %>% select(TrustCode.x, HospCode) %>% unique()

# Not sure which one to use. Will come back to this. Until then, bear in mind there are two trust codes. The 'y' 
# trustcodes should be the updated ones, but the dataset itself has the un-updated ones... I think.

head(dat)

# Overall numbers

# Number in England, Wales, and overall:

nrow(filter(dat, country == "England"))
nrow(filter(dat, country == "Scotland"))
nrow(filter(dat, country == "Wales"))

# dat.save <- dat

# Can rename variables on the go

# 1.1 Age

dat <- rename(dat, Age = X.Age.At.Arrival)
CP(medTable(dat, "Age"))

# 1.2 Gender

dat <- rename(dat, Gender = X2.3.Gender)
CP(myFreqTable(dat, "Gender"))



# 1.3.1 IMD quintiles


CP(myFreqTableonlyAll(dat, "IMDeng.quintile"))
CP(myFreqTableonlyAll(dat, "IMDscot.quintile"))
CP(myFreqTableonlyAll(dat, "IMDwales.quintile"))


dat$IMDeng.quintile <- as.factor(dat$IMDeng.quintile)
dat$IMDscot.quintile <- as.factor(dat$IMDscot.quintile)
dat$IMDwales.quintile <- as.factor(dat$IMDwales.quintile)



# 1.4.1 Average number of admissions with interquartile range
admissions <- dat %>% group_by(country, HospCode) %>% summarise(admiss.no = n()) 

# Overall
quantile(admissions$admiss.no, prob = c(0.25, 0.5, 0.75), type = 2)

# For each country:
quantile(admissions$admiss.no[admissions$country == "England"],
         prob = c(0.25, 0.5, 0.75), type = 2)
quantile(admissions$admiss.no[admissions$country == "Scotland"],
         prob = c(0.25, 0.5, 0.75), type = 2)
quantile(admissions$admiss.no[admissions$country == "Wales"],
         prob = c(0.25, 0.5, 0.75), type = 2)



# 1.4.2 Day and time of admission to hospital

# We need the time and day of the week.

dat$admiss.day <- weekdays(dat$X1.1a.Arrival.Date, abbreviate = FALSE)
head(dat)

dat$admiss.day <- ordered(dat$admiss.day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                            "Friday", "Saturday", "Sunday"))

# Let's cut the arrival times into 2 hour slots:

# This might work dependent on the data
# dat$arrival2hourtimes <- cut(dat$X1.1b.Arrival.Time, breaks = 12, labels = paste0(
#   "lessthan", seq(2, 24, 2)))

# This will always work
# Taking away by something less than a second ensures that those on the break point are in the right category

dat$arrival2hourtimes <- cut(dat$X1.1b.Arrival.Time, breaks = seq(0-(1/24/60/2), 12/12-(1/24/60/2), 1/12),
                             labels = paste0("lessthan", seq(2, 24, 2)))


table(dat$arrival2hourtimes, dat$admiss.day)
      
# Day of the week N
admisstimedow.N <- table(dat$arrival2hourtimes, dat$admiss.day)
sum(admisstimedow.N)

admisstimedow.N.all <- admisstimedow.N

# Day of the week %
admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)

summary(admisstimedow.perc)

# Put them together into a matrix that can then be copy and pasted

togeth <- paste(niceN(admisstimedow.N), " (", niceP(admisstimedow.perc), ")", sep = "")
togeth <- matrix(togeth, nrow = 12, ncol = 7)
write.table(togeth, "clipboard", sep = "\t")

# We need to know how many admissions there were for each day as well

margin.table(admisstimedow.N, 2) %>% CP()

# Right. 


# question 1.5 - smoking status

dat <- rename(dat, Smokingstatus = X2.5.Smoking)

CP(myFreqTable(dat, "Smokingstatus"))


# Q1.6 length of stay days

dat$lengthofstaydays <- dat$X5.2a.Discharge.Date - dat$X1.1a.Arrival.Date

CP(medTable(dat, "lengthofstaydays"))


# Q1.7 Lifestatusatdischarge
dat <- rename(dat, Lifestatusatdischarge = X5.1.Discharge.Life.Status)
CP(myFreqTable(dat, "Lifestatusatdischarge"))


# Q2.1 Heart rate

dat <- rename(dat, Heartrate = X3.1.Heart.Rate)
CP(medTable(dat, "Heartrate"))



# Q2.2.1 Resp rate

dat <- rename(dat, Respiratoryrate = X3.2.Respiratory.Rate)
CP(medTable(dat, "Respiratoryrate"))


# Q2.3.1 Oxygen Saturation recorded

dat <- rename(dat, Oxygensatrecorded = X3.3.1.OxygenSaturation.NR)
CP(myFreqTable(dat, "Oxygensatrecorded"))


# Q2.3.2 Oxygen saturation value
dat <- rename(dat, Oxygensatvalue = "X3.3.Oxygen.Saturation.")
CP(medTable(dat, "Oxygensatvalue"))


# 2.3.3 Was the measurement taken whilst the patient was on supplementary oxygen?
dat <- rename(dat, Oxygenmeasurement = "X3.3a.Oxygen.Measurement")
CP(myFreqTable(dat, "Oxygenmeasurement"))



# Q2.4.1 Did the patient have a peak flow (PEF) following arrival at hospital ?
dat <- rename(dat, Peakflowonarrival = X3.4.1.Peak.Flow.NR)
CP(myFreqTable(dat, "Peakflowonarrival"))
summary(dat$X3.4.1.Peak.Flow.NR)
summary(dat$X3.4.Peak.Flow.Pre.Arrival)


# Q2.4.2 Median time, in hours, from arrival at hospital to PEF measurement 
dat$arrivaltoPEF <- dat$X3.4b.Peak.Flow.Recorded.DT - dat$X1.1.Arrival.DT


dat %>% mutate(arrivaltoPEF, arrivaltoPEF = as.numeric(arrivaltoPEF*24)) %>% medTable("arrivaltoPEF") %>% CP()


# KM curve
# We don't want Scotland anymore

datKMPEF <- filter(dat, !is.na(arrivaltoPEF)) %>% filter(country != "Scotland")
datKMPEF$seen <- 1

# Optionally we can replicate this to give all, but it basically just follows the English curve
# sccKMall <- sccKM
# sccKMall$country <- "All"
# Now we bind it together
# sccKM <- rbind(sccKM, sccKMall)

head(sort(datKMPEF$arrivaltoPEF))

datKMPEF %>% filter(country != "Scotland") %>% filter(arrivaltoPEF<(-0.0001)) %>% select(arrivaltoPEF) %>% nrow()

# If they arrive earlier then just change it to zero for the sake of the kaplan-meier 
datKMPEF$arrivaltoPEF[datKMPEF$arrivaltoPEF<(-0.000001)] <- 0


head(sort(datKMPEF$arrivaltoPEF), 10)

# 48 hours
survfit(Surv(datKMPEF$arrivaltoPEF*24, datKMPEF$seen) ~ datKMPEF$country, data = datKMPEF) %>% 
  plot_survfit(ci = TRUE, legend.title = "Country", xmax = 48, xbreaks = seq(0, 48, 6)) + 
  labs(x = "Time (hours)", y = "Percentage of patients who have received PEF (%)")







# We don't do it like this any more...:
# # After getting it into exact terms, we need to round it to hours for it to be used
# dat$arrivaltoPEFhours <- round(dat$arrivaltoPEF, units = "hours") %>% "*"(24) %>% as.numeric()
# 
# hist(dat$arrivaltoPEFhours[dat$arrivaltoPEFhours <= 24])
# 
# CP(medTable(dat, "arrivaltoPEFhours"))

######### KAPLAN-MEIER RATHER THAN HISTOGRAM WOULD BE BETTER I THINK ##########

summary(dat$Peakflowonarrival)

dat %>% filter(!is.na(arrivaltoPEF)) %>% nrow()

dat %>% filter(!is.na(X3.4.Peak.Flow.Pre.Arrival) &
                  X3.4a.1.Peak.Flow.Recoding.Date.Not.Recorded != "Not recorded") %>% nrow()


# arrival to PEF categories

head(dat$arrivaltoPEF)

dat$arrivaltoPEF1hourcat <- cut(as.numeric(dat$arrivaltoPEF), breaks = c(-1, (1/24 + 1/24/60/2), 1000000),
                                labels = c("PEF taken within 1 hour", "PEF not taken within 1 hour"))

myFreqTable(dat, "arrivaltoPEF1hourcat") %>% CP()


dat$arrivaltoPEF4hourcat <- cut(as.numeric(dat$arrivaltoPEF), breaks = c(-1, (4/24 + 1/24/60/2), 1000000),
                                labels = c("PEF taken within 4 hours", "PEF not taken within 4 hours"))

myFreqTable(dat, "arrivaltoPEF4hourcat") %>% CP()


# 2.4.3 Percentage of patients with a PEF measurement by day and time of arrival 


# Let's just casually create  a new function:

timeDayTableForPEF <- function(dat) {

  dat <- filter(dat, !is.na(X3.4.Peak.Flow.Pre.Arrival))
  
# Day of the week N
admisstimedow.N <- table(dat$arrival2hourtimes, dat$admiss.day)
sum(admisstimedow.N)

# Day of the week %
admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.all)*100, 1)

summary(admisstimedow.perc)

# Put them together into a matrix that can then be copy and pasted

togeth <- paste(niceN(admisstimedow.N), " (", niceP(admisstimedow.perc), "%)", sep = "")
togeth <- matrix(togeth, nrow = 12, ncol = 7)

# We need to know how many admissions there were for each day as well

tot.admiss <- margin.table(admisstimedow.N, 2)


returnlist <- list(togeth, tot.admiss)

}

listy <- timeDayTableForPEF(dat)

CP(listy[[1]])
CP(listy[[2]])








# 2.4.4 Did the patient  have a previous best PEF?
dat <- rename(dat, PreviousbestPEF = X3.5.1.Previous.Peak.Flow.Not.Recorded)
CP(myFreqTable(dat, "PreviousbestPEF"))


# 2.4.5 If previous best PEF was not known, did the patient  have a predicted PEF  calculated?

dat <- rename(dat, PredictedPEF = X3.5a.1.Predicted.Peak.Flow.Not.Recorded)

# Suitable for the report, but for other analysis need to make a more clear variable...
CP(myFreqTable(dat[dat$PreviousbestPEF == "Not recorded", ], "PredictedPEF"))


# Should also change a value in predicted PEF variable from 'recorded' to 'previous best PEF recorded'
# as otherwise it's quite misleading. But for the report, need to leave it as it is so I've created a 
# new variable with 'clear' at the end.
# Change it to character first as makes everything easier



dat$PredictedPEFclear <- dat$PredictedPEF
dat$PredictedPEFclear <- as.character(dat$PredictedPEFclear)
dat$PredictedPEFclear[dat$PreviousbestPEF == "Recorded"] <- "Previous best PEF Recorded"
dat$PredictedPEFclear <- factor(dat$PredictedPEFclear)




dat %>% filter(!is.na(X3.5a.Predicted.Peak.Flow) & !is.na(X3.5.Previous.Peak.Flow)) %>% nrow()




# 2.4.6 PEF on admission as a percentage of best PEF or predicted PEF  

# Create a new variable that refers to either their best previous or predicted PEF
dat$prevorpredvalue <- dat$X3.5.Previous.Peak.Flow
dat$prevorpredvalue[is.na(dat$X3.5.Previous.Peak.Flow)] <- dat$X3.5a.Predicted.Peak.Flow[
  is.na(dat$X3.5.Previous.Peak.Flow)]

dat$percpredPEF <- (dat$X3.4.Peak.Flow.Pre.Arrival/dat$prevorpredvalue)*100

CP(medTable(dat, "percpredPEF"))

# What proportion of those with a PEF calculated had a previous or predicted PEF?

dat %>% filter(Peakflowonarrival == "Yes")
  
  str(dat$prevorpredvalue)

dat$prevorpredtaken <- NA
dat$prevorpredtaken <- as.character(dat$prevorpredtaken)
dat$prevorpredtaken[!is.na(dat$prevorpredvalue)] <- "Recorded"
dat$prevorpredtaken[is.na(dat$prevorpredvalue)] <- "Not recorded"
dat$prevorpredtaken <- as.factor(dat$prevorpredtaken)

summary(dat$prevorpredtaken)
  
summary(dat$Peakflowonarrival)

CP(myFreqTable(dat[dat$Peakflowonarrival == "Yes", ], "prevorpredtaken"))


# The value below should be equal to PEF measured N - (prev PEF N + the pred PEF N)
dat %>% filter(!is.na(prevorpredvalue) & is.na(X3.4.Peak.Flow.Pre.Arrival)) %>% nrow() 

# This cuts it how I want it.
dat$percpredPEF75cat <- cut(dat$percpredPEF, breaks = c(-1, 74.99999, 100000000),
                            labels = c("Admitted with PEF <75%", "Admitted with PEF >= 75%"))

CP(myFreqTable(dat, "percpredPEF75cat"))



# 3.1.1 Was the patient reviewed by a respiratory specialist during their admission?
dat <- rename(dat, Specrespreview = "X4.1.Specialist.Respiratory.Review")
CP(myFreqTable(dat, "Specrespreview"))

summary(dat$X4.1.respiratory.review.DT)

# Q3.1.2 Median time, in hours, from arrival at hospital to specrev measurement 
dat$arrivaltospecresprev <- dat$X4.1.respiratory.review.DT - dat$X1.1.Arrival.DT

summary(dat$arrivaltospecresprev)

dat %>% mutate(arrivaltospecresprev, arrivaltospecresprev = as.numeric(arrivaltospecresprev*24)) %>% 
  medTable("arrivaltospecresprev") %>% CP()


# arrival to specresprev 24 hours category.


dat$arrivaltospecresprev24hourcat <- cut(as.numeric(dat$arrivaltospecresprev), breaks = c(-1, (1 + 1/24/60/2), 1000000),
                              labels = c("Specialist respiratory review within 24 hours", 
                                         "Specialist respiratory review after 24 hours"))

dat %>% select(arrivaltospecresprev, arrivaltospecresprev24hourcat) %>% group_by(arrivaltospecresprev24hourcat) %>%
  summarise(min = min(arrivaltospecresprev), max = max(arrivaltospecresprev),
            med = median(arrivaltospecresprev, na.rm = TRUE))

myFreqTable(dat, "arrivaltospecresprev24hourcat") %>% CP()


# 3.2.1 Was oxygen  prescribed/administered for the patient at any point during admission?
dat <- rename(dat, Oxygenprescribed = X4.2.Oxygen.Prescribed)
CP(myFreqTable(dat, "Oxygenprescribed"))


# 3.3.1 Was the patient administered systemic steroids following arrival at hospital?
dat <- rename(dat, Steroidsadminist = "X4.3.Steroids.Administered")
CP(myFreqTable(dat, "Steroidsadminist"))



# 3.3.2 Median time, in hours, from arrival at hospital to administration of systemic steroids
dat$arrivaltosteroids <- dat$X4.3.Steroids.DT - dat$X1.1.Arrival.DT

dat %>% mutate(arrivaltosteroids, arrivaltosteroids = as.numeric(arrivaltosteroids)*24) %>% 
  medTable("arrivaltosteroids") %>% CP()


# 3.3.3 Number of patients  receiving systemic steroids within 4 hours
# Need toa add the equivalent of half a minute on to ensure your categories are correct!
# Due to rounding errors...
# This uses <= 4 hours vs > 4 hours

dat$steroids1hourcat <- cut(as.numeric(dat$arrivaltosteroids), breaks = c(-1, (1/24 + 1/24/60/2), 1000000),
                            labels = c("Steroids received within 1 hour", "Steroids not received within 1 hour"))

CP(myFreqTable(dat, "steroids1hourcat"))


dat$steroids4hourcat <- cut(as.numeric(dat$arrivaltosteroids), breaks = c(-1, (4/24 + 1/24/60/2), 1000000),
                            labels = c("Steroids received within 4 hours", "Steroids not received within 4 hours"))


dat %>% filter(steroids4hourcat == "Steroids received within 4 hours") %>% nrow()

# Add the equivalent of half a minute on to ensure your categories are correct!
# Needed because when times are converted to numeric there are rounding errors etc because 1 day is equal to 1.

# Put the minimum break point as -1, because cut missed off those who were '0' otherwise

# Note; tested and the cut offs are fine with just writing 4/24

CP(myFreqTable(dat, "steroids4hourcat"))


# New KM curve
# Get rid of Scotland


datKMster <- filter(dat, !is.na(arrivaltosteroids)) %>% filter(country != "Scotland")
datKMster$seen <- 1




# Optionally we can replicate this to give all, but it basically just follows the English curve
# sccKMall <- sccKM
# sccKMall$country <- "All"
# Now we bind it together
# sccKM <- rbind(sccKM, sccKMall)


# in minutes
# survfit(Surv(datKMBA$arrivaltob2agonists*24*60, datKMBA$seen) ~ datKMBA$country, data = datKMBA) %>% 
#   plot_survfit(ci = TRUE, legend.title = "Country", xmax = 360, xbreaks = seq(0, 360, 30)) + 
#   labs(x = "Time (minutes)", y = "Percentage of patients who have received PEF within 48 hours (%)")

# in hours

survfit(Surv(datKMster$arrivaltosteroids*24, datKMster$seen) ~ datKMster$country, data = datKMster) %>% 
  plot_survfit(ci = TRUE, legend.title = "Country", xmax = 48, xbreaks = seq(0, 48, 6)) + 
  labs(x = "Time (hours)", y = "Percentage of patients who have received steroids (%)")


dat %>% filter(country == "Scotland") %>% select(arrivaltosteroids)


head(dat$arrivaltosteroids)

# Let's just casually make another function:

timeDayTableForSteroids4hours <- function(dat) {
  
  dat <- filter(dat, steroids4hourcat == "Steroids received within 4 hours")
  
  # Day of the week N
  admisstimedow.N <- table(dat$arrival2hourtimes, dat$admiss.day)
  sum(admisstimedow.N)
  
  # Day of the week %
  admisstimedow.perc <- round((admisstimedow.N/admisstimedow.N.all)*100, 1)
  
  summary(admisstimedow.perc)
  
  # Put them together into a matrix that can then be copy and pasted
  
  togeth <- paste(niceN(admisstimedow.N), " (", niceP(admisstimedow.perc), "%)", sep = "")
  togeth <- matrix(togeth, nrow = 12, ncol = 7)
  
  # We need to know how many admissions there were for each day as well
  
  tot.admiss <- margin.table(admisstimedow.N, 2)
  
  
  returnlist <- list(togeth, tot.admiss)
  
}

listy2 <- timeDayTableForSteroids4hours(dat)

CP(listy2[[1]])
CP(listy2[[2]])


# 3.4.1 Was the patient  administered beta agonists following arrival at hospital?

dat <- rename(dat, B2agonistsadminist = X4.4.Agonists.B2.On.Arrival)
CP(myFreqTable(dat, "B2agonistsadminist"))




# 3.4.2 Median time, in minutes, from arrival at hospital to administration of beta agonists

dat$arrivaltob2agonists <- dat$X4.4.Agonists.B2.DT - dat$X1.1.Arrival.DT

dat %>% mutate(arrivaltob2agonists = as.numeric(arrivaltob2agonists)*24*60) %>% 
  medTable("arrivaltob2agonists") %>% CP()



# 3.4.3 KM curve
# remove Scotland

datKMBA <- filter(dat, !is.na(arrivaltob2agonists)) %>% filter(country != "Scotland")
datKMBA$seen <- 1

# Optionally we can replicate this to give all, but it basically just follows the English curve
# sccKMall <- sccKM
# sccKMall$country <- "All"
# Now we bind it together
# sccKM <- rbind(sccKM, sccKMall)


# in minutes
# survfit(Surv(datKMBA$arrivaltob2agonists*24*60, datKMBA$seen) ~ datKMBA$country, data = datKMBA) %>% 
#   plot_survfit(ci = TRUE, legend.title = "Country", xmax = 360, xbreaks = seq(0, 360, 30)) + 
#   labs(x = "Time (minutes)", y = "Percentage of patients who have received PEF within 48 hours (%)")

# in hours
survfit(Surv(datKMBA$arrivaltob2agonists*24, datKMBA$seen) ~ datKMBA$country, data = datKMBA) %>% 
  plot_survfit(ci = TRUE, legend.title = "Country", xmax = 12, xbreaks = seq(0, 12, 1)) + 
  labs(x = "Time (hours)", y = expression("Percentage of patients who have received (beta) "[2]*" agonists (%)"))



dat$b2agonists1hourcat <- cut(as.numeric(dat$arrivaltob2agonists), breaks = c(-1, (1/24 + 1/24/60/2), 1000000),
                              labels = c("b2agonists received within 1 hour", "b2agonists not received within 1 hour"))

CP(myFreqTable(dat, "b2agonists1hourcat"))


dat$b2agonists4hourcat <- cut(as.numeric(dat$arrivaltob2agonists), breaks = c(-1, (4/24 + 1/24/60/2), 1000000),
                              labels = c("b2agonists received within 4 hours", "b2agonists not received within 4 hours"))


CP(myFreqTable(dat, "b2agonists4hourcat"))



# 4.1 Day of discharge  

dat$discharge.day <- weekdays(dat$X5.2a.Discharge.Date, abbreviate = FALSE)
head(dat)

dat$discharge.day <- ordered(dat$discharge.day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                   "Friday", "Saturday", "Sunday"))

CP(myFreqTable(dat, "discharge.day"))


CPwithRowNames <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = TRUE)}

CPwithRowNames(table(dat$discharge.day, dat$X5.3.Discharge.Bundle))
# CPwithRowNames(prop.table(table(dat$discharge.day, dat$X5.3.Discharge.Bundle), 1))


dat <-  rename(dat, Dischargeinhalercheck = X5.4.Discharge.Elements...Inhaler.technique.checked,
            Dischargemaintmed = X5.4.Discharge.Elements...Maintenance.medication.reviewed,                    
            Dischargeadherence = X5.4.Discharge.Elements...Adherence.discussed,                   
            DischargePAAP = X5.4.Discharge.Elements...PAAP.issued.reviewed,                           
            Dischargetriggers = X5.4.Discharge.Elements...Triggers.discussed,
            Dischargetobacco = X5.4.Discharge.Elements...Tobacco.dependency.addressed,                   
            DischargecommFU2days= X5.4.Discharge.Elements...Community.follow.up.requested.within.2.working.days,
            Dischargespecrev4weeks = X5.4.Discharge.Elements...Specialist.review.requested.within.4.weeks,   
            Dischargenone = X5.4.Discharge.Elements...None)

CP(myFreqTable(dat, "Dischargeinhalercheck"))
CP(myFreqTable(dat, "Dischargemaintmed"))
CP(myFreqTable(dat, "Dischargeadherence"))
CP(myFreqTable(dat, "DischargePAAP"))
CP(myFreqTable(dat, "Dischargetriggers"))
CP(myFreqTable(dat, "DischargecommFU2days"))
CP(myFreqTable(dat, "Dischargespecrev4weeks"))
CP(myFreqTable(dat, "Dischargenone"))



# This is how many people only received 'tobacco dependency addressed'.
dat %>% filter(X5.4.Discharge.Elements == "Tobacco dependency addressed") %>% nrow()


# Finding those who had tobacco dependency addressed and are current smokers. Don't want to change everyone
# else to 'missing' because some of the 'not recorded' also had smoking status addressed.
CP(myFreqTable(dat[dat$Smokingstatus == "Current smoker", ], "Dischargetobacco"))


# 5.1.1 Was the patient in receipt of inhaled steroids at discharge?
dat <- rename(dat, Inhaledsteroidsatdischarge = X6.1.Inhaled.Steroids.At.Discharge)
CP(myFreqTable(dat, "Inhaledsteroidsatdischarge"))


# 5.1.1 Was the patient prescribed at least 5 days of oral steroids for treatment of their asthma attack?
dat <- rename(dat, Oralsteroidsatdischarge = X6.2.Oral.Steroids.at.Discharge)
CP(myFreqTable(dat, "Oralsteroidsatdischarge"))


# 5.3.1 Has the patient been prescribed more than 2 courses of oral steroids in the last 12 months ?
dat <- rename(dat, Oralsteroids2in12months = X6.3.Oral.Steroids...2)
CP(myFreqTable(dat, "Oralsteroids2in12months"))


# 5.4.1 Was the patient referred for hospital assessment/follow up for asthma?
dat <- rename(dat, referredforFU = X6.4.Referred.for.Followup)
CP(myFreqTable(dat, "referredforFU"))


# 5.4.2 If the patient was prescribed more than 2 courses of oral steroids in the last 12 months, were they
# referred for hospital assessment/follow up for asthma?
CP(myFreqTable(dat[dat$Oralsteroids2in12months == "Yes", ], "referredforFU"))




# 6.1.1 Time to systemic steroids and associations with length of stay

# Need to create the length of stay median category. Here, because the length of stay is done in days, we
# just create the break point as the median + 0.1 (as length of stay will only be integers)
dat$lengthofstaydaysmediancat <- cut(as.numeric(dat$lengthofstaydays),
                                     breaks = c(-1,
                                                median(as.numeric(dat$lengthofstaydays), na.rm = TRUE) + 0.1,
                                                1000000),
                                     labels = c("Length of stay <= median", "Length of stay > median"))

# Check it's fine... it is fine.
table(dat$lengthofstaydaysmediancat)
dat %>% filter(lengthofstaydays < 4) %>% nrow()


# For the table:
CPwithRowNames(table(dat$lengthofstaydaysmediancat, dat$steroids4hourcat))
CPwithRowNames(round(prop.table(table(dat$lengthofstaydaysmediancat, dat$steroids4hourcat), 2)*100, 1))
margin.table(table(dat$lengthofstaydaysmediancat, dat$steroids4hourcat), 2)

table(dat$steroids4hourcat)



# WATCH OUT AS epidisplay BLOCKS 'SELECT'
# Should detach it afterwards

# library(epiDisplay)

glm(lengthofstaydaysmediancat ~ steroids4hourcat, family=binomial(link='logit'), data = dat) %>% 
logistic.display()


# 6.1.1 Time to systemic steroids and associations with inpatient mortality

CPwithRowNames(table(dat$Lifestatusatdischarge, dat$steroids4hourcat))
CPwithRowNames(round(prop.table(table(dat$Lifestatusatdischarge, dat$steroids4hourcat), 2)*100, 1))
margin.table(table(dat$Lifestatusatdischarge, dat$steroids4hourcat), 2)

glm(Lifestatusatdischarge ~ steroids4hourcat, family=binomial(link='logit'), data = dat) %>%
  logistic.display()






# 6.2.1 Time to b2-agonists and associations with length of stay



CPwithRowNames(table(dat$lengthofstaydaysmediancat, dat$b2agonists4hourcat))
CPwithRowNames(round(prop.table(table(dat$lengthofstaydaysmediancat, dat$b2agonists4hourcat), 2)*100, 1))
margin.table(table(dat$lengthofstaydaysmediancat, dat$b2agonists4hourcat), 2)

glm(lengthofstaydaysmediancat ~ b2agonists4hourcat, family=binomial(link='logit'), data = dat) %>%
  logistic.display()


# 6.2.2 Time to b2-agonists and associations with inpatient mortality

CPwithRowNames(table(dat$Lifestatusatdischarge, dat$b2agonists4hourcat))
CPwithRowNames(round(prop.table(table(dat$Lifestatusatdischarge, dat$b2agonists4hourcat), 2)*100, 1))
margin.table(table(dat$Lifestatusatdischarge, dat$b2agonists4hourcat), 2)

glm(Lifestatusatdischarge ~ b2agonists4hourcat, family=binomial(link='logit'), data = dat) %>%
  logistic.display()


str(dat$Lifestatusatdischarge)


# PEF

# PEF ever vs death

# Create a binary variable for peak flow. Those who didn't have itbecause they were too ill count
# as missing.

dat$peakflowbin <- NA
dat$peakflowbin[dat$Peakflowonarrival == "Not recorded"] <- 0
dat$peakflowbin[dat$Peakflowonarrival == "Patient too unwell"] <- 0
dat$peakflowbin[dat$Peakflowonarrival == "Yes"] <- 1

table(dat$Peakflowonarrival, dat$peakflowbin, useNA = "ifany")

table(dat$peakflowbin, dat$Peakflowonarrival)



table(dat$Lifestatusatdischarge, dat$Peakflowonarrival)
table(dat$Lifestatusatdischarge, dat$peakflowbin)

dat$peakflowbinnoill <- dat$peakflowbin
dat$peakflowbinnoill[dat$Peakflowonarrival == "Patient too unwell"] <- NA

table(dat$peakflowbinnoill, dat$Peakflowonarrival, useNA = "ifany")

CPwithRowNames(table(dat$Lifestatusatdischarge, dat$Peakflowonarrival))
CPwithRowNames(round(prop.table(table(dat$Lifestatusatdischarge, dat$Peakflowonarrival), 2)*100, 1))
margin.table(table(dat$Lifestatusatdischarge, dat$Peakflowonarrival), 2)

glm(Lifestatusatdischarge ~ peakflowbinnoill, family=binomial(link='logit'), data = dat) %>% coef() %>% exp()
glm(Lifestatusatdischarge ~ peakflowbinnoill, family=binomial(link='logit'), data = dat) %>% confint() %>% exp()


# PEF ever vs LOS

CPwithRowNames(table(dat$lengthofstaydaysmediancat, dat$Peakflowonarrival))
CPwithRowNames(round(prop.table(table(dat$lengthofstaydaysmediancat, dat$Peakflowonarrival), 2)*100, 1))
margin.table(table(dat$lengthofstaydaysmediancat, dat$Peakflowonarrival), 2)

glm(lengthofstaydaysmediancat ~ peakflowbinnoill, family=binomial(link='logit'), data = dat) %>% coef() %>% exp()
glm(lengthofstaydaysmediancat ~ peakflowbinnoill, family=binomial(link='logit'), data = dat) %>% confint() %>% exp()



# PEF 4 hour cat vs death

CPwithRowNames(table(dat$Lifestatusatdischarge, dat$arrivaltoPEF4hourcat))
CPwithRowNames(round(prop.table(table(dat$Lifestatusatdischarge, dat$arrivaltoPEF4hourcat), 2)*100, 1))
margin.table(table(dat$Lifestatusatdischarge, dat$arrivaltoPEF4hourcat), 2)

fisher.test(as.matrix(table(dat$Lifestatusatdischarge, dat$arrivaltoPEF4hourcat)))

glm(Lifestatusatdischarge ~ arrivaltoPEF4hourcat, family=binomial(link='logit'), data = dat) %>% coef() %>% exp()
glm(Lifestatusatdischarge ~ arrivaltoPEF4hourcat, family=binomial(link='logit'), data = dat) %>% confint() %>% exp()

# PEF 4 hour cat vs LOS

table(dat$arrivaltoPEF4hourcat, dat$Peakflowonarrival, useNA = "ifany")

CPwithRowNames(table(dat$lengthofstaydaysmediancat, dat$arrivaltoPEF4hourcat))
CPwithRowNames(round(prop.table(table(dat$lengthofstaydaysmediancat, dat$arrivaltoPEF4hourcat), 2)*100, 1))
margin.table(table(dat$lengthofstaydaysmediancat, dat$arrivaltoPEF4hourcat), 2)

dat$arrivaltoPEF4hourcat <- relevel(dat$arrivaltoPEF4hourcat, ref = "PEF not taken within 4 hours")
dat$lengthofstaydaysmediancat <- relevel(dat$lengthofstaydaysmediancat, ref = "Length of stay > median")


glm(lengthofstaydaysmediancat ~ arrivaltoPEF4hourcat, family=binomial(link='logit'), data = dat) %>% coef() %>% exp()
glm(lengthofstaydaysmediancat ~ arrivaltoPEF4hourcat, family=binomial(link='logit'), data = dat) %>% confint() %>% exp()

glm(lengthofstaydaysmediancat ~ arrivaltoPEF4hourcat, family=binomial(link='logit'), data = dat) %>% logistic.display()

# Just including this so that things remain the same as they were before
dat$lengthofstaydaysmediancat <- relevel(dat$lengthofstaydaysmediancat, ref = "Length of stay <= median")



# 6.3.1 Respiratory specialist review and associations with length of stay

# We should make spec resp rev a factor:

dat$Specrespreview <- factor(dat$Specrespreview, levels = c("Yes", "No"))


dat$Specrespreviewbin <- NA
dat$Specrespreviewbin[dat$Specrespreview == "Yes"] <- 1
dat$Specrespreviewbin[dat$Specrespreview == "No"] <- 0
dat$Specrespreviewbin <- factor(dat$Specrespreviewbin)


table(dat$lengthofstaydaysmediancat, dat$Specrespreview)

CPwithRowNames(table(dat$lengthofstaydaysmediancat, dat$Specrespreview))
CPwithRowNames(round(prop.table(table(dat$lengthofstaydaysmediancat, dat$Specrespreview), 2)*100, 1))
margin.table(table(dat$lengthofstaydaysmediancat, dat$Specrespreview), 2)


glm(lengthofstaydaysmediancat ~ Specrespreview, family=binomial(link='logit'), data = dat) %>%
  logistic.display()

# !!!!!! GLM produces a different result if your data is ordered!!!!




# 6.3.2 Respiratory specialist review and associations with inpatient mortality

CPwithRowNames(table(dat$Lifestatusatdischarge, dat$Specrespreview))
CPwithRowNames(round(prop.table(table(dat$Lifestatusatdischarge, dat$Specrespreview), 2)*100, 1))
margin.table(table(dat$Lifestatusatdischarge, dat$Specrespreview), 2)

glm(Lifestatusatdischarge ~ Specrespreview, family=binomial(link='logit'), data = dat) %>%
  logistic.display()


dat$Lifestatusatdischargebin <- NA
dat$Lifestatusatdischargebin[dat$Lifestatusatdischarge == "Alive"] <- 0
dat$Lifestatusatdischargebin[dat$Lifestatusatdischarge == "Died as inpatient"] <- 1

table(dat$Lifestatusatdischarge)
# 6.3.3 Respiratory specialist review and associations with smokers where tobacco dependency was addressed

# Create a new variable that subsets the discharge element for tobacco into just those who are smokers
# Could have done this before...

str(dat$Dischargetobacco)

dat$DCtobaconlycurrsmokers <- dat$Dischargetobacco
dat$DCtobaconlycurrsmokers[dat$Smokingstatus != "Current smoker"] <- NA

table(dat$Dischargetobacco, dat$DCtobaconlycurrsmokers, useNA = "ifany")

table(dat$DCtobaconlycurrsmokers, dat$Smokingstatus, useNA = "ifany")

CPwithRowNames(table(dat$DCtobaconlycurrsmokers, dat$Specrespreview))
CPwithRowNames(round(prop.table(table(dat$DCtobaconlycurrsmokers, dat$Specrespreview), 2)*100, 1))
margin.table(table(dat$DCtobaconlycurrsmokers, dat$Specrespreview), 2)
table(dat$DCtobaconlycurrsmokers, dat$Specrespreview)

dat$DCtobaconlycurrsmokers <- as.factor(dat$DCtobaconlycurrsmokers)

str(dat$DCtobaconlycurrsmokers)
dat$DCtobaconlycurrsmokers <- relevel(dat$DCtobaconlycurrsmokers, ref = )
dat$Specrespreview <- relevel(dat$Specrespreview, ref = "No")

                                        

glm(DCtobaconlycurrsmokers ~ Specrespreview, family=binomial(link='logit'), data = dat) %>%
  logistic.display()

dat$Specrespreview <- relevel(dat$Specrespreview, ref = "Yes")
dat$Specrespreview <- relevel(dat$Specrespreview, ref = "No")


glm(DCtobaconlycurrsmokers ~ Specrespreview, family=binomial(link='logit'), data = dat) %>% coef() %>% exp()
glm(DCtobaconlycurrsmokers ~ Specrespreview, family=binomial(link='logit'), data = dat) %>% confint() %>% exp()


# 6.3.4 Respiratory specialist review and associations with those receiving a  discharge bundle

dat$dischargebundlebin <- dat$X5.3.Discharge.Bundle
table(dat$dischargebundlebin, useNA = "ifany")
dat$dischargebundlebin[dat$dischargebundlebin == "Self discharge"] <- "No"
table(dat$dischargebundlebin, dat$X5.3.Discharge.Bundle, useNA = "ifany")

# Convert it to character and back to factor because that's easier than trying to remove a factor level.
dat$dischargebundlebin <- as.character(dat$dischargebundlebin) %>% as.factor()
str(dat$dischargebundlebin)
summary(dat$dischargebundlebin)


CPwithRowNames(table(dat$dischargebundlebin, dat$Specrespreview))
CPwithRowNames(round(prop.table(table(dat$dischargebundlebin, dat$Specrespreview), 2)*100, 1))
margin.table(table(dat$dischargebundlebin, dat$Specrespreview), 2)
table(dat$dischargebundlebin, dat$Specrespreview)
addmargins(table(dat$dischargebundlebin, dat$Specrespreview))

dat$Specrespreview <- relevel(dat$Specrespreview, ref = "No")

glm(dischargebundlebin ~ Specrespreview, family=binomial(link='logit'), data = dat) %>% coef() %>% exp()
glm(dischargebundlebin ~ Specrespreview, family=binomial(link='logit'), data = dat) %>% confint() %>% exp()


glm(dischargebundlebin ~ Specrespreview, family=binomial(link='logit'), data = dat) %>%
  logistic.display()

sum(summary(dat$Smokingstatus))
(10242-884)/10242


detach(package:epiDisplay)





difFreqTable <- function(x, varname) {
  
  
  varname <- as.character(varname)
  print(varname)
  gen.E <- x %>% filter(Specrespreview == "Yes") %>% dplyr::select(!!varname) %>% drop_na()
  EN <- nrow(gen.E)
  gen.E0 <- as.data.frame(table(gen.E[[1]]))
  gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
  gen.E2$England <- paste(format(gen.E2$Freq, big.mark=",", trim=TRUE), " (", # N
                          trimws(format(round(gen.E2$perc, 1), nsmall = 1)), "%)", sep = "") # %
  gen.E2 <- select(gen.E2, Var1, England)
  print(gen.E2)
  
  
  gen.W <- x %>% filter(Specrespreview == "No") %>% dplyr::select(!!varname) %>% drop_na()
  WN <- nrow(gen.W)
  gen.W0 <- as.data.frame(table(gen.W[[1]]))
  gen.W1 <- as.data.frame(round(prop.table(table(gen.W[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.W2 <- inner_join(gen.W0, gen.W1, by = "Var1")
  gen.W2$Wales <- paste(format(gen.W2$Freq, big.mark=",", trim=TRUE), " (",
                        trimws(format(round(gen.W2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.W2 <- select(gen.W2, Var1, Wales)
  print(gen.W2)
  
 
  
  gen.table <- inner_join(gen.E2, gen.W2, by = "Var1") 
  colnames(gen.table) <- c(varname, paste("Respiratory specialist review (N=", format(EN, big.mark=",", trim=TRUE), ")",
                                          sep = ""),
                           paste("No Respiratory specialist review (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  
  # row.names(gen.table) <- gen.table$Var1
  
  return(gen.table)
}




# dif2FreqTable <- function(x, varname, byvar, varlevels) {
#   
#   formlist <- list()
#   
#   quo(colour)
#   
#   
#   for (i in 1:length(varlevels)) {
#   
#   
#     vari <- varlevels[i]
#     
#     byvar <- as.character(byvar)
#     
#     pastetest <- paste(byvar, "==", "'", , "'")
#     
#     vv <- varlevels[i]
#     
#     byvar <- quo(byvar)
#     
#   varname <- as.character(varname)
#   print(varname)
#   gen.E <- x %>% filter(UQ(byvar) == vari) #  %>% dplyr::select(!!varname) %>% drop_na()
#   EN <- nrow(gen.E)
#   gen.E0 <- as.data.frame(table(gen.E[[1]]))
#   gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
#   gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
#   gen.E2$England <- paste(format(gen.E2$Freq, big.mark=",", trim=TRUE), " (", # N
#                           trimws(format(round(gen.E2$perc, 1), nsmall = 1)), "%)", sep = "") # %
#   gen.E2 <- select(gen.E2, Var1, England)
#   print(gen.E2)
#   
#   formlist[[i]] <- gen.E2 }
#   
#   gen.table <- list[[1]]
#   
#   for (j in 2:length(varlevels)) {
#   gen.table <- inner_join(gen.table, formlist[[j]], by = "Var1")
#   }
#   
#   colnames(gen.table)[1] <- varname
#   
#   for(i in 2:length(varlevels)) {
#   colnames(gen.table)[i] <- paste(levels[i], " (N=", format(EN, big.mark=",", trim=TRUE), ")",
#                                           sep = "")
#   }
#                         
#   
#   
#   # row.names(gen.table) <- gen.table$Var1
#   
#   return(gen.table)
# }
# 
# table(dat$Specrespreview)
# 
# dif2FreqTable(dat, "Lifestatusatdischarge", "Specrespreview", c("Yes", "No"))
# 
# x <- dat
# varname <- "Lifestatusatdischarge"
# byvar <- "Specrespreview"
# varlevels <- c("Yes", "No")
# i <- 1
# 
# table(dat$Specrespreview)


CP(difFreqTable(dat, "Dischargeinhalercheck"))
CP(difFreqTable(dat, "Dischargemaintmed"))
CP(difFreqTable(dat, "Dischargeadherence"))
CP(difFreqTable(dat, "DischargePAAP"))
CP(difFreqTable(dat, "Dischargetriggers"))
CP(difFreqTable(dat, "DischargecommFU2days"))
CP(difFreqTable(dat, "Dischargespecrev4weeks"))
CP(difFreqTable(dat, "Dischargenone"))


dat$Dischargeinhalercheckfac <- as.factor(dat$Dischargeinhalercheck)
dat$Dischargemaintmedfac <- as.factor(dat$Dischargemaintmed)
dat$Dischargeadherencefac <- as.factor(dat$Dischargeadherence)
dat$DischargePAAPfac <- as.factor(dat$DischargePAAP)
dat$Dischargetriggersfac <- as.factor(dat$Dischargetriggers)
dat$DischargecommFU2daysfac <- as.factor(dat$DischargecommFU2days)
dat$Dischargespecrev4weeksfac <- as.factor(dat$Dischargespecrev4weeks)
dat$Dischargenonefac <- as.factor(dat$Dischargenone)
dat$Dischargetobaccofac <- as.factor(dat$Dischargetobacco)

dat$IMDeng.quintile <- as.factor(dat$IMDeng.quintile)
dat$IMDscot.quintile <- as.factor(dat$IMDscot.quintile)
dat$IMDwales.quintile <- as.factor(dat$IMDwales.quintile)

str(dat$arrivaltospecresprev24hourcat)


dat$lengthofstaycont <- dat$X5.2.Discharge.DT - dat$X1.1.Arrival.DT
summary(dat$lengthofstaycont)

mean(dat$lengthofstaycont[dat$Specrespreview == "Yes"], na.rm = TRUE)
mean(dat$lengthofstaycont[dat$Specrespreview == "No"], na.rm = TRUE)

median(dat$lengthofstaycont[dat$Specrespreview == "Yes"], na.rm = TRUE)
median(dat$lengthofstaycont[dat$Specrespreview == "No"], na.rm = TRUE)


median(dat$lengthofstaydays[dat$Specrespreview == "Yes"], na.rm = TRUE)
median(dat$lengthofstaydays[dat$Specrespreview == "No"], na.rm = TRUE)

summary(dat$lengthofstaydays)


tail(sort(dat$lengthofstaycont[dat$Specrespreview == "Yes"]), 50)
tail(sort(dat$lengthofstaycont[dat$Specrespreview == "No"]), 50)






dat$arrivaltoPEFhours <- as.numeric(dat$arrivaltoPEF)*24
dat$arrivaltospecresprevhours <- as.numeric(dat$arrivaltospecresprev)*24 
dat$arrivaltosteroidshours <- as.numeric(dat$arrivaltosteroids)*24 
dat$arrivaltob2agonistsminutes <- as.numeric(dat$arrivaltob2agonists)*24*60

dat$Specrespreview.nicename <- as.character(dat$Specrespreview)
table(dat$Specrespreview.nicename)
dat$Specrespreview.nicename[dat$Specrespreview.nicename == "Yes"] <- "Resp_review_given"
dat$Specrespreview.nicename[dat$Specrespreview.nicename == "No"] <- "Resp_review_not_given"
dat$Specrespreview.nicename <- as.factor(dat$Specrespreview.nicename)


dat$dischargebundle <- as.character(dat$X5.3.Discharge.Bundle)
table(dat$dischargebundle)
dat$dischargebundle[dat$dischargebundle == "No"] <- "No_discharge_bundle"
dat$dischargebundle[
  dat$dischargebundle == "Self discharge"] <- "No_discharge_bundle_due_to_self_discharge"
dat$dischargebundle[dat$dischargebundle == "Yes"] <- "Discharge_bundle_given"
dat$dischargebundle <- as.factor(dat$dischargebundle)


str(dat$arrivaltoPEFhours)
str(dat$arrivaltoPEF)
str(dat$arrivaltospecresprev)
str(dat$arrivaltospecresprevhours)
str(dat$arrivaltosteroids)
str(dat$arrivaltosteroidshours)


head(dat)


str(dat$PredictedPEF)







# - - - - - - - - - - - - - -#
#  Defining asthma severity  #
# - - - - - - - - - - - - - -#

dat$percpredbin <-1
dat$percpredbin[is.na(dat$percpredPEF) == TRUE] <- 0
dat$percpredbin <- factor(dat$percpredbin, labels = c("No perc pred", "perc pred"))


# Create a variable for peak flow severity:
# Bear in mind that 396 people were too unwell for peak flow to be measured - let's see how they
# compare with the other stats used to define severity. Only becomes something to worry about if
# their other stats are all normal.


# Don't need to worry about floating point rounding errors for this 
# But can't use cut because of the definitions the left and the right are both < and >. 

dat$peakflowsev <- NA
dat$peakflowsev[dat$percpredPEF < 33] <- 3
dat$peakflowsev[dat$percpredPEF >= 33 & dat$percpredPEF <= 50] <- 2
dat$peakflowsev[dat$percpredPEF > 50] <- 1

dat$peakflowsev <- factor(dat$peakflowsev, levels = c(1, 2, 3), labels = c("Moderate", "Severe", "LT"),
                          ordered = TRUE)




dat$respratesev <- cut(dat$Respiratoryrate, breaks = c(-1, 9, 24, Inf),
                       labels = c("Low", "Normal", "High"))



table(dat$respratesev)
dat %>% filter(respratesev == "Low") %>% dplyr::select(Respiratoryrate)
dat %>% filter(is.na(respratesev)) %>% nrow()
dat %>% filter(is.na(Respiratoryrate)) %>% nrow()

# 11 People in the 'low' group
# 8 of these have a resp rate of 0, and out of these 5 people have a heart rate of 0

dat$Respiratoryrate[dat$respratesev == "Low"]
table(dat$Peakflowonarrival)
table(dat$Oxygensatrecorded)
table(dat$Oxygenmeasurement)

dat$heartratesev <- cut(dat$Heartrate, breaks = c(-1, 29, 109, Inf),
                        labels = c("Low", "Normal", "High"))

table(dat$heartratesev, dat$respratesev, useNA = "ifany")
dat %>% filter(heartratesev == "Low") %>% select(Heartrate, Respiratoryrate)
dat %>% filter(is.na(respratesev)) %>% nrow()
dat %>% filter(is.na(Respiratoryrate)) %>% nrow()


# 7 people with a heart rate of 0. 6 of these people have a respiratory rate of 0. 


# 1017 people have a low oxygen saturation level, indicating life-threatening - is this quite high?
# I guess if they've had to come to hospital anyway they must be in fairly bad shape.

dat$O2satsev <- cut(dat$Oxygensatvalue, breaks = c(-1, 92, 101), right = FALSE,
                    labels = c("Low", "Normal"))
table(dat$O2satsev, useNA = "ifany")


dat %>% filter(Oxygensatvalue == 92) %>% select(O2satsev) %>% head()


table(dat$O2satsev, dat$respratesev, useNA = "ifany")
table(dat$O2satsev, dat$heartratesev, useNA = "ifany")
table(dat$respratesev, dat$heartratesev, useNA = "ifany")
table(dat$peakflowsev, useNA = "ifany")
table(dat$Peakflowonarrival, useNA = "ifany")


# Okay now we create our asthma severity variable:
# Asthmasev1 has everyone in it. If missing peak flow or O2 saturation but normal in other values, assumed normal.
# Asthmasev2 excludes anyone missing a peak flow or oxygen saturation value
# Asthmasev3 includes everyone but does not use peak flow when assessing severity
# Asthmasev4 is the same as asthmasev1 but those who were too ill for peak flow assessment marked as severe




# library(epiDisplay)

levels(dat$Lifestatusatdischarge)

dat$peakflowbinnoillinverse <- NA
dat$peakflowbinnoillinverse[dat$peakflowbinnoill == 1] <- 0
dat$peakflowbinnoillinverse[dat$peakflowbinnoill == 0] <- 1


glm(Lifestatusatdischargebin ~ peakflowbinnoillinverse, family=binomial(link='logit'), data = dat) %>%
  logistic.display()



# We don't know whether if they're marked as too ill it means they get put in severe or life-threatening,
# so I don't think we can do anything with that.

dat$asthmasev1 <- NA

table(dat$asthmasev1, useNA = "ifany")



# Doing each of these separately gives:

# 3663 mod
# 6120 sev -> 5192 after LT
# 1433 LT

# Normal people are easy to define: 

# dat$asthmasev[dat$peakflowsev == "Moderate" & 
#                 dat$heartratesev == "Normal" &
#                 dat$respratesev == "Normal" &
#                 dat$O2satsev == "Normal" ] <- 1

dat$asthmasev1[(is.na(dat$peakflowsev) == TRUE | dat$peakflowsev == "Moderate") & 
                 dat$heartratesev == "Normal" &
                 dat$respratesev == "Normal" &
                 (dat$O2satsev == "Normal" | is.na(dat$O2satsev) == TRUE )] <- 1

# Severe people come next (but some of these might then be re-classed as life-threatening):


dat$asthmasev1[(dat$peakflowsev == "Severe" | 
                  dat$heartratesev == "High" | dat$heartratesev == "Low" |
                  dat$respratesev == "High" | dat$respratesev == "Low")] <- 2


# Now we do life-threatening

dat$asthmasev1[dat$peakflowsev == "LT" | dat$O2satsev == "Low"] <- 3


table(dat$asthmasev1, useNA = "ifany")


# Can create asthmasev2 here; people that are missing for peak flow or oxygen

dat$asthmasev2 <- dat$asthmasev1

dat$asthmasev2[dat$asthmasev2 == 1 & (is.na(dat$peakflowsev) | is.na(dat$O2satsev))] %>% length()
dat$asthmasev2[dat$asthmasev2 == 1 & is.na(dat$peakflowsev)] %>% length()


table(dat$percpredbin[dat$asthmasev1 == 1], dat$Oxygensatrecorded[dat$asthmasev1 == 1])

dat$asthmasev2[dat$asthmasev2 == 1 & (is.na(dat$peakflowsev) == TRUE | is.na(dat$O2satsev) == TRUE)] <- NA

# 1821 people removed - 1808 due to missing perc pred PEF, a further 13 due to no O2 saturation  

table(dat$asthmasev1, dat$asthmasev2, useNA = "ifany")



table(dat$peakflowbin)
table(dat$Peakflowonarrival)
table(dat$prevorpred)


colnames(dat)

# How many people are marked as too unwell for peak flow measurement but are fine for other things?

dat %>% filter(asthmasev1 == 1, Peakflowonarrival == "Patient too unwell") %>% nrow()

# 103 people. Could be marked as severe in the sensitivity analysis


# How many people didn't have a perc peak flow but are fine for other things?

dat %>% filter(asthmasev1 == 1, is.na(peakflowsev)) %>% nrow()

# 1808


dat %>% filter(asthmasev1 == 1, is.na(peakflowsev)) %>% select(Peakflowonarrival, prevorpred) %>% table()


str(dat$prevorpred)



# How many people didn't have an oxygen saturation measurement but are fine for other things?

dat %>% filter(asthmasev1 == 1, Oxygensatrecorded == "Not recorded") %>% nrow()

# 37 people



# dat$asthmasev1 <- factor(dat$asthmasev, labels = c("Moderate", "Severe", "LT"))



# Still have 1821 who are missing.

# If we include people who are missing peak flow but have other variables in moderate...:

# dat$asthmasev[(is.na(dat$peakflowsev) == TRUE | is.na(dat$O2satsev) == TRUE ) & 
#                 dat$heartratesev == "Normal" &
#                 dat$respratesev == "Normal"] <- 1

# This changes 2014 people though... because it takes either/or, so the other part could be not normal...
# So instead...:



# dat$asthmasev[(dat$peakflowsev == "Moderate" | is.na(dat$peakflowsev) == TRUE) & 
#                 dat$heartratesev == "Normal" &
#                 dat$respratesev == "Normal" &
#                 is.na(dat$O2satsev) == TRUE] <- 1




# 1821 missing


# 103 of these people were too unwell to have their peak flow measured on arrival


dat %>% filter(is.na(asthmasev2)) %>% select(Peakflowonarrival, prevorpred) %>% table()

# Of the patients who were too unwell, only 74 would be able to give a percent predicted anyway.

colnames(dat)

dat$asthmasev1 <- factor(dat$asthmasev1, labels = c("Moderate", "Severe", "LT"), ordered = TRUE)
dat$asthmasev2 <- factor(dat$asthmasev2, labels = c("Moderate", "Severe", "LT"), ordered = TRUE)

table(dat$asthmasev1, useNA = "ifany")
table(dat$asthmasev2, useNA = "ifany")




# Pretty sure that asthma severity is now sorted! ... But it's not because maybe we shouldn't use predicted
# peak flow to assess severity if we are measuring the time taken to provision...? 

# Sooooooo let's make asthma severity 3! This time completely ignoring peak flow

dat$asthmasev3 <- NA

dat$asthmasev3[dat$heartratesev == "Normal" &
                 dat$respratesev == "Normal" &
                 (dat$O2satsev == "Normal" | is.na(dat$O2satsev) == TRUE )] <- 1

# Severe people come next (but some of these might then be re-classed as life-threatening):


dat$asthmasev3[(dat$heartratesev == "High" | dat$heartratesev == "Low" |
                  dat$respratesev == "High" | dat$respratesev == "Low")] <- 2


# Now we do life-threatening

dat$asthmasev3[dat$O2satsev == "Low"] <- 3

dat %>% filter(asthmasev3 == 1, is.na(dat$O2satsev)) %>% nrow()

# 43 People are in the moderate group without having their O2 saturation measured

dat$asthmasev3 <- factor(dat$asthmasev3, labels = c("Moderate", "Severe", "LT"), ordered = TRUE)
table(dat$asthmasev3, useNA = "ifany")

addmargins(table(dat$asthmasev2, dat$asthmasev3, useNA = "ifany"), c(1, 2))
addmargins(table(dat$asthmasev1, dat$asthmasev3, useNA = "ifany"), c(1, 2))



# For asthmasev4, not much will change
# the 104 people who are marked as having moderate asthma severity otherwise, but marked as 'too ill for PEF',
# are now being changed to severe

table(dat$asthmasev1, dat$Peakflowonarrival)

dat$asthmasev4 <- dat$asthmasev1
dat$asthmasev4[dat$asthmasev1 == "Moderate" & dat$Peakflowonarrival == "Patient too unwell"] <- "Severe"


str(dat$asthmasev4)

glm(Specrespreview ~ asthmasev4, family=binomial(link='logit'), data = dat) # [2]
exp(coef(glm(Specrespreview ~ asthmasev4, family=binomial(link='logit'), data = dat))) # [2]


# Asthmasev4.1 is the same as asthmasev4 but isn't ordered so it's easier to interpret the output.
dat$asthmasev4.1 <- dat$asthmasev4
dat$asthmasev4.1 <- factor(dat$asthmasev4.1, ordered = FALSE)


dat$asthmasev1.1 <- dat$asthmasev1
dat$asthmasev1.1 <- factor(dat$asthmasev1.1, ordered = FALSE)

dat$asthmasev2.1 <- dat$asthmasev2
dat$asthmasev2.1 <- factor(dat$asthmasev2.1, ordered = FALSE)

dat$asthmasev3.1 <- dat$asthmasev3
dat$asthmasev3.1 <- factor(dat$asthmasev3.1, ordered = FALSE)


# For the report!!!!!!!!!!




dat %>% filter(respratesev == "Low") %>% select(alexpatID, heartratesev, O2satsev, peakflowsev, asthmasev4)
dat %>% filter(heartratesev == "Low") %>% select(alexpatID, respratesev, O2satsev, peakflowsev, asthmasev4)
dat %>% filter(Peakflowonarrival == "Patient too unwell") %>% select(asthmasev1, asthmasev4) %>% table()



dat %>% myFreqTable(dat, "asthmasev4.1") %>% CP()


str(dat$Specrespreviewbin)

str(dat$Specrespreview)




table(dat$Specrespreview, dat$asthmasev4) %>% CPwithRowNames()
niceP(prop.table(table(dat$Specrespreview, dat$asthmasev4),2)*100)

m1 <- glm(Specrespreviewbin ~ asthmasev4.1, family=binomial(link='logit'), data = dat) # [2]
exp(coef(m1)) # [2]
exp(confint(m1)) # [2]



# And now we look at whether that is associated with respiratory review, and time until respiratory review





prop.table(table(dat$Specrespreview, dat$asthmasev4), 2)

dat %>% group_by(asthmasev4.1) %>% summarise(med = niceP(summary(arrivaltospecresprevhours, na.rm = TRUE)[3]),
                                           loIQ = niceP(summary(arrivaltospecresprevhours, na.rm = TRUE)[2]),
                                           hiIQ = niceP(summary(arrivaltospecresprevhours, na.rm = TRUE)[5])) %>%
  mutate(comb = paste0(med, " (", loIQ, " to ", hiIQ, ")")) %>% select(Asthma.severity = asthmasev4.1, 
                                                                       median_hours_with_IQR = comb) # %>% CP()



# 
# 
# 
# # let's convert the time to spec resp review so it's on a log scale.
# # this involves changing '0's to a very low number
# 
# dat$arrivaltospecresprevconv <- dat$arrivaltospecresprev
# 
# dat$arrivaltospecresprevconv <- (dat$arrivaltospecresprevconv)^(1/3)
# 
# kruskal.test(arrivaltospecresprevhours ~ asthmasev4, data = dat)
# kruskal.test(arrivaltospecresprevhours ~ asthmasev4.1, data = dat)
# 
# hist(dat$arrivaltospecresprevhours[dat$asthmasev4 == "Moderate"])
# hist(dat$arrivaltospecresprevhours[dat$asthmasev4 == "Severe"])
# hist(dat$arrivaltospecresprevhours[dat$asthmasev4 == "LT"])






table(dat$peakflowbinnoill, useNA = "ifany")


dat$peakflowbinnoillfac <- factor(dat$peakflowbinnoill, labels = c("No PEF", "PEF"))

dat$ageconv <- NA
dat$ageconv <- as.vector(scale(dat$Age, scale = FALSE))


# Now, we look at peak flow taken vs death

table(dat$Lifestatusatdischarge, dat$Peakflowonarrival) %>% CPwithRowNames()
niceP(prop.table(table(dat$Lifestatusatdischarge, dat$Peakflowonarrival),2)*100)


ests <- function(x) {
  cbind(exp(coef(x)),
        exp(confint(x)))
}


# # Is life status associated with receiving peak flow on arrival?
# 
# ests <- function(x) {
#   cbind(exp(coef(x)),
#         exp(confint(x)))
# }
# 
# m1 <- glm(Lifestatusatdischarge ~ peakflowbinnoillfac, family=binomial(link='logit'), data = dat)
# ests(m1)
# 
# 
# m2 <- glm(Lifestatusatdischarge ~ peakflowbinnoill + asthmasev4.1 + ageconv, family=binomial(link='logit'), data = dat)
# ests(m2)
# 
# 
# 
# # Is length of stay associated with receiving peak flow on arrival?
# 
# table(dat$lengthofstaydaysmediancat, dat$Peakflowonarrival) %>% CPwithRowNames()
# niceP(prop.table(table(dat$lengthofstaydaysmediancat, dat$Peakflowonarrival),2)*100)
# 
# 
# m3 <- glm(lengthofstaydaysmediancat ~ peakflowbinnoillfac, family=binomial(link='logit'), data = dat)
# ests(m3)
# 
# m4 <- glm(lengthofstaydaysmediancat ~ peakflowbinnoillfac + asthmasev4.1 + ageconv, family=binomial(link='logit'), data = dat)
# ests(m4)
# 
# summary(m4)
# 
# summary(dat$peakflowbinnoillfac)
# 
# 
# table(dat$Steroidsadminist, dat$Lifestatusatdischarge)
# 

# All the abstract stuff has been hashed out



# Benchmarking section

# Create the good practice care variable:

# Six elements of the good practice care question should be ticked:
#   
#   -	 Inhaler technique check
# -	Maintenance medication reviewed
# -	Adherence discussed
# -	PAAP issued/reviewed
# -	Tobacco dependency addressed
# -	Follow-up (one of either community follow up requested within 2 working days OR (!!!!)
#              specialist review requested within 4 weeks)
# TRIGGERS NOT RELEVANT


dat %>% filter(Age == 16) %>% select(Age)

# Now for the benchmarking

# this bit of prep is the non-smoking elements.
dat <- dat %>% mutate(gpcprep = ifelse(Dischargeinhalercheck == 1 & Dischargemaintmed == 1 &
                                           Dischargeadherence == 1 & DischargePAAP == 1 &
                                           (dat$DischargecommFU2days == 1 | dat$Dischargespecrev4weeks == 1),
                                         1, 0))

# Need to now add the bit about smoking that is only relevant to smokers

dat <- dat %>% mutate(goodpracticecare = ifelse((gpcprep == 1 & Smokingstatus != "Current smoker") |
                                           (gpcprep == 1 & Smokingstatus == "Current smoker" &
                                           Dischargetobacco == 1), 1, 0))


table(dat$goodpracticecare, dat$gpcprep)

dat %>% filter(goodpracticecare == 1) %>% summarise(sum(Dischargeinhalercheck), sum(DischargePAAP), sum(DischargecommFU2days),
                                                    sum(Dischargemaintmed))


summary(dat$goodpracticecare)

myFreqTable(dat, "goodpracticecare") %>% CP()

dat$goodpracticecarefac <- factor(dat$goodpracticecare)



# Because we still need the denominator though, we are going to create another variable representing 
# those who weren't too ill to have their peak flow measured.

dat$nottooillforPEF <- NA
dat$nottooillforPEF[dat$Peakflowonarrival == "Not recorded"] <- 1
dat$nottooillforPEF[dat$Peakflowonarrival == "Yes"] <- 1
dat$nottooillforPEF[dat$Peakflowonarrival == "Patient too unwell"] <- 0


table(dat$nottooillforPEF, dat$Peakflowonarrival, useNA = "ifany")


# Create a binary data for smokers alive at discharge
dat$currsmokbinalive <- NA
dat$currsmokbinalive <- 0
dat$currsmokbinalive[dat$Smokingstatus == "Current smoker"] <- 1
dat$currsmokbinalive[dat$Lifestatusatdischargebin == 1] <- 0

table(dat$currsmokbinalive, dat$Smokingstatus, useNA = "ifany")

dat %>% filter(Lifestatusatdischargebin == 1) %>% select(Smokingstatus)

str(dat$DCtobaconlycurrsmokers)

-



# Use summarise function to get necessary columns
bmk <- dat %>% dplyr::group_by(HospCode) %>% summarise(trustcode = first(TrustCode.y), 
                                                cases.audited = n(),
                                                currsmoktobaccodepencyaddressed_n = sum(DCtobaconlycurrsmokers,
                                                                                      na.rm = TRUE),
                                                currsmokno = sum(currsmokbinalive, na.rm = TRUE),
                                                aliveatdischarge = (n() - sum(Lifestatusatdischargebin)),
                                                PEFonarrival_n = sum(peakflowbin),
                                                PEFnottooill_n = sum(nottooillforPEF),
                                                allelementsofGPC_n = sum(goodpracticecare, na.rm = TRUE), 
                                                median_los = median(lengthofstaydays, na.rm = TRUE))
                                                
                                        
CP(bmk)

# Use mutate function to go on from there
bmk <- mutate(bmk, currsmoktobaccodepencyaddressed_perc = round(currsmoktobaccodepencyaddressed_n / currsmokno, 2)*100,
              PEFonarrival_perc = round(PEFonarrival_n / PEFnottooill_n, 2)*100,
              allelementsofGPC_perc = round(allelementsofGPC_n / aliveatdischarge, 2)*100)

bmk <- bmk %>% select(-currsmokno, -aliveatdischarge, -PEFnottooill_n)

colnames(bmk)
bmk <- bmk[ ,c(1:3, 7, 4, 8, 5, 9, 6, 10)]

bmk <- left_join(bmk, country, by = "HospCode")

bmk <- bmk[, c(13, 12, 1:10)]


# !!! Around here is where you have to change the variables



# write.csv(bmk, file =
# "Z:/Group_work/PS_AA/Adult Asthma/Secondary Care Clinical 2018-2019/Analysis/datafromR/benchmarking_20190628.csv",
#   row.names = FALSE)

# saveRDS(dat,
# "D:/Alex/Adult Asthma/Secondary Care Clinical 2018-2019/tidyData/RDS files/clean_dat_made_by_AA_SCC_analysis_20190730.RDS")


dat.save <- dat

# Now, we are going to change the 'dat' variable, and create the 'levlev' variable, by running a separate script.

dat <- dat.save 

rm(levlev)
rm(completedlevlev)


source(
"Z:/Group_work/PS_AA/Adult Asthma/Secondary Care Clinical 2018-2019/Analysis/scripts/AA_SCC_national_analysis_csv_format.R")

completedlevlev <- levlev


for (i in c("England", "Scotland", "Wales")) {

dat <- filter(dat.save, country == i)

source(
  "Z:/Group_work/PS_AA/Adult Asthma/Secondary Care Clinical 2018-2019/Analysis/scripts/AA_SCC_national_analysis_csv_format.R")

levlev$hospital.code <- i
levlev$hospital.name <- i
levlev$trust.code <- i
levlev$trust.name <- i

completedlevlev <- full_join(completedlevlev, levlev)

}


repcol <- completedlevlev %>% select(-matches("median")) %>% select(-matches("quart")) %>% select(-matches("perc")) %>% 
  select_if(is.numeric) %>% colnames()

# # This subsets completedlevlev into just those columns that match the names in repcol in order to change their class
# completedlevlev[ , colnames(completedlevlev) %in% 
#                    repcol == TRUE] <- completedlevlev[ , colnames(completedlevlev) %in% repcol == TRUE] %>% 
#   mutate_all(funs(as.character))


# Need to change the factors that haven't popped up in the formula to 0 instead of NA.

for (i in repcol) {
  completedlevlev[[i]][is.na(completedlevlev[[i]]) == TRUE] <- 0
}



# write.csv(completedlevlev,
#           "D:/Alex/Adult Asthma/Secondary Care Clinical 2018-2019/tidyData/AA_SCC_country_national_20190731.csv",
#           row.names = FALSE)


rm(levlev)


table(dat.save$HospCode)


dat.no.single <- dat.save[dat.save$HospCode != "HCH", ]





for (i in unique(dat.no.single$HospCode)[1]) {

  dat <- filter(dat.no.single, HospCode == i)

  source(
"Z:/Group_work/PS_AA/Adult Asthma/Secondary Care Clinical 2018-2019/Analysis/scripts/AA_SCC_hospital_analysis_csv_format.R")

  levlev$hospital.code <- dat$HospCode[1]
  levlev$hospital.name <- dat$Hospital[1]
  levlev$trust.code <- dat$TrustCode.y[1]
  levlev$trust.name <- dat$Trust[1]

  completedhosplevlev <- levlev

}



for (i in unique(dat.no.single$HospCode)[2:length(unique(dat.no.single$HospCode))]) {
  
  dat <- filter(dat.no.single, HospCode == i)
  
  source(
    "Z:/Group_work/PS_AA/Adult Asthma/Secondary Care Clinical 2018-2019/Analysis/scripts/AA_SCC_hospital_analysis_csv_format.R")
  
  levlev$hospital.code <- dat$HospCode[1]
  levlev$hospital.name <- dat$Hospital[1]
  levlev$trust.code <- dat$TrustCode.y[1]
  levlev$trust.name <- dat$Trust[1]
  
  completedhosplevlev <- full_join(completedhosplevlev, levlev)
  
}

# Let's get the columns in the same order as the country one. To do this, we select the last column in both,
# which is "Oralsteroids2in12monthswithreferredforFU_Yes_perc", we find its column index, and then we make
# the columns into the same order using the column names of completedlevlev.

untilcol <- which(colnames(completedlevlev) == "Oralsteroids2in12monthswithreferredforFU_Yes_perc")

completedhosplevlev <- completedhosplevlev[names(completedlevlev[ ,1:untilcol])]




# All column names that need to be replaced if NA when should be 0

repcol <- completedhosplevlev %>% select(-matches("median")) %>% select(-matches("quart")) %>% select(-matches("perc")) %>% 
  select_if(is.numeric) %>% colnames()



# For missing variables that actually should be 0 
for (i in repcol) {
  completedhosplevlev[[i]][is.na(completedhosplevlev[[i]]) == TRUE] <- 0
}


# Get rid of the 'median admissions' column
completedhosplevlev <- select(completedhosplevlev, -admiss.no_median, -admiss.no_lo.quart, -admiss.no_hi.quart)


# Try and convert the necessary columns to the currect format/rounding
completedhosplevlev <- completedhosplevlev %>% mutate_at(vars(contains('perc')), ~ sprintf("%.1f", round(., 1), nsmall = 1))

# At this point probably easier to manually change the median columns when needed. Actually - could also round those
# time variables that need it.
completedhosplevlev <- completedhosplevlev %>% mutate_at(vars(contains('median')), ~ sprintf("%.1f", round(., 1), nsmall = 1))


# write.csv(completedhosplevlev,
#          "D:/Alex/Adult Asthma/Secondary Care Clinical 2018-2019/tidyData/AA_SCC_hospital_20190731.csv",
#          row.names = FALSE)







# setdiff(colnames(completedlevlev), colnames(levlev))
# unique(dat.save$HospCode)
