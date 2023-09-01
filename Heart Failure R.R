# Joseph S. Kwaik on Heart Failure
# https://www.kaggle.com/datasets/andrewmvd/heart-failure-clinical-data
# https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/s12911-020-1023-5 (Source of Data)
# https://academic.oup.com/ehjqcco/article/4/1/51/4083514 (New York Heart Association)
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6301865/ (Elderly Age Classifications)
# https://www.mountsinai.org/health-library/tests/creatine-phosphokinase-test#:~:text=Total%20CPK%20normal%20values%3A,per%20liter%20(mcg%2FL) (CPK Levels)
# https://www.hopkinsmedicine.org/health/conditions-and-diseases/what-are-platelets-and-why-are-they-important (Platelets)
# https://www.mayoclinic.org/tests-procedures/creatinine-test/about/pac-20384646#:~:text=The%20typical%20range%20for%20serum,52.2%20to%2091.9%20micromoles%2FL) (Serum creatinine)
# https://www.mayoclinic.org/diseases-conditions/hyponatremia/symptoms-causes/syc-20373711#:~:text=It%20helps%20maintain%20normal%20blood,liter%20(mEq%2FL). (Serium sodium)
# https://my.clevelandclinic.org/health/articles/16950-ejection-fraction (Ejection Fraction)

# Packages
library(RColorBrewer)
library(ggplot2)
install.packages("caTools")
library(caTools)
install.packages("rmarkdown")
library(rmarkdown)
install.packages('xaringan')
library(xaringan)
install.packages('knitr')
library(knitr)
install.packages('kableExtra')
library(kableExtra)
library(formatR)

# Data
Heart <- heart_failure_clinical_records_dataset
View(Heart)

# Renaming columns, information on columns
colnames(Heart)[colnames(Heart) == "creatinine_phosphokinase"] ="cpkEnzyme" # Creatinine Phosphokinase - Level of CPK enzyme in blood (mcg/L)
colnames(Heart)[colnames(Heart) == "ejection_fraction"] ="ejection" # Ejection Fraction - % blood leaving heart at each contraction (percentage)
colnames(Heart)[colnames(Heart) == "high_blood_pressure"] ="hypertension"
colnames(Heart)[colnames(Heart) == "DEATH_EVENT"] ="fatal"
  # Column "time" is the follow-up period in days. Since heart failure is not an event, but a chronic disease, time is the time under supervision till death.
  # For column "sex", 1 = Male, 0 = Female
  # Diabetes, anemia, hypertension, smoking, fatal: 0 = No, 1 = Yes.
colnames(Heart)[colnames(Heart) == "anaemia"] ="anemia" # "Anaemia" is British English

# Breaking age up into age groups
AgeGroup <- cut(Heart$age, breaks = c(40, 64, 74, 84, 100), labels = c("Middle Aged", "Young Elderly", "Middle Elderly", "Oldest Elderly"))
Heart$age_group <- AgeGroup

# Visualizing fatal events based on age group
FatalHeart <- subset(Heart, fatal == 1)
barplot(table(FatalHeart$age_group),
        xlab = "Age Group", 
        ylab = "Fatal",
        main = "Amount of Heart Failure Deaths by Age Group",
        col = c(brewer.pal(11,"Spectral")))
# It appears that there is a higher frequency of middle aged patients in the data, so the count doesn't tell us the right story.
barplot(table(Heart$age_group),
        xlab = "Age Group", 
        ylab = "Amount of Patients",
        main = "Amount of Heart Failure Patients by Age Group",
        col = c(brewer.pal(11,"Spectral"))) # Yes there is.

pMiddleAged <- nrow(Heart[Heart$age_group=="Middle Aged",])/nrow(Heart) # 61.53846% of the patients in the data are middle aged. This is also P(Middle Aged).
# So, it would be more accurate to look at probabilities of fatal events based on age group instead of plain frequencies.

# Remind that Bayes Theorem states P(A|B) = (P(B|A) * P(A)) / P(B) where P(A|B) = probability of A given B.
# P(Fatal | Middle Aged) = (P(Middle Aged | Fatal) * P(Fatal)) / P(Middle Aged)
pFatal <- nrow(Heart[Heart$fatal==1,])/nrow(Heart) # P(fatal) = 0.3210702
pMiddleAgedgivenFatal <- nrow(FatalHeart[FatalHeart$age_group=="Middle Aged",])/nrow(FatalHeart) # P(Middle Aged | Fatal) = 0.4791667
pFatalgivenMiddleAged <- (pMiddleAgedgivenFatal * pFatal) / pMiddleAged
pFatalgivenMiddleAged # P(Fatal | Middle Aged) = 0.25.
# Now, let's compare that to P(Fatal | Oldest Elderly) = (P(Oldest Elderly | Fatal) * P(Fatal)) / P(Oldest Elderly)
pOldestElderly <- nrow(Heart[Heart$age_group=="Oldest Elderly",])/nrow(Heart)
pOldestElderlygivenFatal <- nrow(FatalHeart[FatalHeart$age_group=="Oldest Elderly",])/nrow(FatalHeart)
pFatalgivenOldestElderly <- (pOldestElderlygivenFatal * pFatal) / pOldestElderly
pFatalgivenOldestElderly # P(Fatal | Oldest Elderly) = 0.4761905, higher than the probability of fatality in middle aged.
# Probability of a fatal event is higher given the patient is in the oldest age group.
# Visualizing that
pFatalOld <- c(pFatalgivenMiddleAged, pFatalgivenOldestElderly) # p for probability
cFatalOld <- c("Middle Aged", "Oldest Elderly") # c for condition
barplot(pFatalOld, names.arg = cFatalOld, col = c("red", "blue"), ylab = "Probability of Fatal Event",
        main = "Probability of Fatal Event by Age Group")

# P(Fatal | Diabetes) = (P(Diabetes | Fatal) * P(Fatal)) / P(Diabetes) - NOT USED
pDiabetes <- nrow(Heart[Heart$diabetes==1,])/nrow(Heart)
pDiabetesgivenFatal <- nrow(FatalHeart[FatalHeart$diabetes==1,])/nrow(FatalHeart)
pFatalgivenDiabetes <- (pDiabetesgivenFatal * pFatal) / pDiabetes
pFatalgivenDiabetes # P(Fatal | Diabetes) = 0.32
# Checking against no diabetes.
pNoDiabetes <- nrow(Heart[Heart$diabetes==0,])/nrow(Heart)
pNoDiabetesgivenFatal <- nrow(FatalHeart[FatalHeart$diabetes==0,])/nrow(FatalHeart)
pFatalgivenNoDiabetes <- (pNoDiabetesgivenFatal * pFatal) / pNoDiabetes
pFatalgivenNoDiabetes # P(Fatal | No Diabetes) = 0.3218391
# Probability of a fatal event is slightly higher given the patients don't have diabetes (perhaps bad data)

# P(Fatal | No Smoking) = (P(No Smoking | Fatal) * P(Fatal)) / P(No Smoking) - NOT USED
pNoSmoking <- nrow(Heart[Heart$smoking==0,])/nrow(Heart)
pNoSmokinggivenFatal <- nrow(FatalHeart[FatalHeart$smoking==0,])/nrow(FatalHeart)
pFatalgivenNoSmoking <- (pNoSmokinggivenFatal * pFatal) / pNoSmoking
pFatalgivenNoSmoking # P(Fatal | No Smoking) = 0.3251232
# P(Fatal | Smoking) = (P(Smoking | Fatal) * P(Fatal)) / P(Smoking)
pSmoking <- nrow(Heart[Heart$smoking==1,])/nrow(Heart)
pSmokinggivenFatal <- nrow(FatalHeart[FatalHeart$smoking==1,])/nrow(FatalHeart)
pFatalgivenSmoking <- (pSmokinggivenFatal * pFatal) / pSmoking
pFatalgivenSmoking # P(Fatal | Smoking) = 0.3125
# Probability of a fatal event is slightly higher given the patient doesn't smoke. (perhaps bad data)

# P(Fatal | Not Anemic) = (P(Not Anemic | Fatal) * P(Fatal)) / P(Not Anemic)
pNoAnemia <- nrow(Heart[Heart$anemia==0,])/nrow(Heart)
pNoAnemiagivenFatal <- nrow(FatalHeart[FatalHeart$anemia==0,])/nrow(FatalHeart)
pFatalgivenNoAnemia <- (pNoAnemiagivenFatal * pFatal) / pNoAnemia
pFatalgivenNoAnemia # P(Fatal | Not Anemic) = 0.2941176
# P(Fatal | Anemic) = (P(Anemic | Fatal) * P(Fatal)) / P(Anemic)
pAnemic <- nrow(Heart[Heart$anemia==1,])/nrow(Heart)
pAnemicgivenFatal <- nrow(FatalHeart[FatalHeart$anemia==1,])/nrow(FatalHeart)
pFatalgivenAnemic <- (pAnemicgivenFatal * pFatal) / pAnemic
pFatalgivenAnemic # P(Fatal | Anemic) = 0.3565891
# Probability of a fatal event is higher given the patient is anemic.

# P(Fatal | Not Hypertensive) = (P(Not Hypertensive | Fatal) * P(Fatal)) / P(Not Hypertensive)
pNoHypertension <- nrow(Heart[Heart$hypertension==0,])/nrow(Heart)
pNoHypertensiongivenFatal <- nrow(FatalHeart[FatalHeart$hypertension==0,])/nrow(FatalHeart)
pFatalgivenNoHypertension <- (pNoHypertensiongivenFatal * pFatal) / pNoHypertension
pFatalgivenNoHypertension # P(Fatal | Not Hypertensive) = 0.2938144
# P(Fatal | Hypertensive) = (P(Hypertensive | Fatal) * P(Fatal)) / P(Hypertensive)
pHypertension <- nrow(Heart[Heart$hypertension==1,])/nrow(Heart)
pHypertensiongivenFatal <- nrow(FatalHeart[FatalHeart$hypertension==1,])/nrow(FatalHeart)
pFatalgivenHypertension <- (pHypertensiongivenFatal * pFatal) / pHypertension
pFatalgivenHypertension # P(Fatal | Hypertension) = 0.3714286
# Probability of a fatal event is higher given the patient is hypertensive.
# Visualizing that
pFatalHypo <- c(pFatalgivenNoHypertension, pFatalgivenHypertension) # p for probability
cFatalHypo <- c("No hypertension", "Hypertension") # c for condition
barplot(pFatalHypo, names.arg = cFatalHypo, col = c("blue", "red"), ylab = "Probability of Fatal Event",
        main = "Probability of Fatal Event by Hypertension Status")
legend("topright", legend = cFatalHypo, fill = c("blue", "red"))
text(x = 1:2, y = pFatalHypo, labels = round(pFatalHypo, 3), pos = 4, cex = 0.8)

# Let's get deeper, though. 

# Normal CPK is 10 to 120 micrograms per liter (mcg/L) is normal. Column for normal vs abnormal CPK levels.
Heart$cpkLevel <- ifelse(Heart$cpkEnzyme >= 10 & Heart$cpkEnzyme <= 120, "Normal", "Abnormal")

# Normal count of platelets in blood is 150,000 to 450,000 platelets per microliter of blood. 
# Greater than 450,000 platelets is thrombocytosis, less than 150,000 is known as thrombocytopenia. Columns for it.
plateletLevel <- cut(Heart$platelets, breaks = c(0, 149999, 449999, 999999999), labels = c("Thrombocytopenia", "Normal", "Thrombocytosis"))
Heart$plateletLevel <- plateletLevel

# Normal serum creatinine for men is 0.74 to 1.35 mg/dL and 0.59 to 1.04 mg/dL for women.
Heart$Gender <- ifelse(Heart$sex == 1, "Male", "Female")
# Normal levels of serum creatinine is gender dependent. Column for normal vs. abnormal serum creatinine.
Heart$creatinineLevel <- ifelse(Heart$Gender == "Male", # the test
                                ifelse(Heart$serum_creatinine >= 0.74 & Heart$serum_creatinine <= 1.35, "Normal", "Abnormal"), # if male is true
                                ifelse(Heart$serum_creatinine >= 0.59 & Heart$serum_creatinine <= 1.04, "Normal", "Abnormal")) # if male is false

# Normal levels of serum sodium are 135 and 145 (mEq/L). Hyponatremia occurs when the sodium in your blood falls below 135 mEq/L.
Heart$sodiumLevel <- ifelse(Heart$serum_sodium >= 135 & Heart$serum_sodium <= 145, "Normal", "Abnormal")

# Ejection fraction
# Male	Normal - 52% to 72%, Mildly Abnormal -	41% to 51%, Moderately Abnormal -	30% to 40%,	Severely Abnormal - Below 30%
# Female	Normal: 54% to 74% Mildly Abnormal:	41% to 53%	Moderately Abnormal: 30% to 40%	Severely Abnormal: Below 30%
MaleEjection <- cut(Heart$ejection, breaks = c(0, 30, 40, 51, 72, 100), labels = c("Severely Abnormal", "Moderately Abnormal", "Mildly Abnormal", "Normal", "High"))
FemaleEjection <- cut(Heart$ejection, breaks = c(0, 30, 40, 53, 74, 100), labels = c("Severely Abnormal", "Moderately Abnormal", "Mildly Abnormal", "Normal", "High"))
Heart$ejectionTest <- ifelse(Heart$Gender == "Male", as.character(MaleEjection), as.character(FemaleEjection))

# Now we have classifications for CPK, platelets, sodium, and ejection. Update to FatalHeart data frame.
FatalHeart <- subset(Heart, fatal == 1)

# Some more Bayesian reasoning with our new classifications.

# P(Fatal | Normal CPK) = (P(Normal CPK | Fatal) * P(Fatal)) / P(Normal CPK)
pNormalCPK <- nrow(Heart[Heart$cpkLevel=="Normal",])/nrow(Heart)
pNormalCPKgivenFatal <- nrow(FatalHeart[FatalHeart$cpkLevel=="Normal",])/nrow(FatalHeart)
pFatalgivenNormalCPK <- (pNormalCPKgivenFatal * pFatal) / pNormalCPK
pFatalgivenNormalCPK # P(Fatal | Normal CPK) = 0.2467532
# P(Fatal | Abnormal CPK) = (P(Abnormal CPK | Fatal) * P(Fatal)) / P(Abnormal CPK)
pAbnormalCPK <- nrow(Heart[Heart$cpkLevel=="Abnormal",])/nrow(Heart)
pAbnormalCPKgivenFatal <- nrow(FatalHeart[FatalHeart$cpkLevel=="Abnormal",])/nrow(FatalHeart)
pFatalgivenAbnormalCPK <- (pAbnormalCPKgivenFatal * pFatal) / pAbnormalCPK
pFatalgivenAbnormalCPK # P(Fatal | Abnormal CPK) = 0.3468468
# Probability of a fatal event is higher given the patient's cpk levels are abnormal. 
# Visualizing that
pFatalCPK <- c(pFatalgivenNormalCPK, pFatalgivenAbnormalCPK) # p for probability
cFatalCPK <- c("Normal Level of CPK", "Abnormal Level of CPK") # c for condition
barplot(pFatalCPK, names.arg = cFatalCPK, col = c("blue", "red"), ylab = "Probability of Fatal Event",
        main = "Probability of Fatal Event by Status of Creatine Phosphokinase Enzyme Level")

# P(Fatal | Normal Platelet) = (P(Normal Platelet | Fatal) * P(Fatal)) / P(Normal Platelet)
pNormalPlatelet <- nrow(Heart[Heart$plateletLevel=="Normal",])/nrow(Heart)
pNormalPlateletgivenFatal <- nrow(FatalHeart[FatalHeart$plateletLevel=="Normal",])/nrow(FatalHeart)
pFatalgivenNormalPlatelet <- (pNormalPlateletgivenFatal * pFatal) / pNormalPlatelet
pFatalgivenNormalPlatelet # P(Fatal | Normal Platelet) = 0.3088803
# Thrombocytopenia
pThrombocytopenia <- nrow(Heart[Heart$plateletLevel=="Thrombocytopenia",])/nrow(Heart)
pThrombocytopeniagivenFatal <- nrow(FatalHeart[FatalHeart$plateletLevel=="Thrombocytopenia",])/nrow(FatalHeart)
pFatalgivenThrombocytopenia <- (pThrombocytopeniagivenFatal * pFatal) / pThrombocytopenia
pFatalgivenThrombocytopenia #P(Fatal | Thrombocytopenia) = 0.4074074
# Thrombocytosis
pThrombocytosis <- nrow(Heart[Heart$plateletLevel=="Thrombocytosis",])/nrow(Heart)
pThrombocytosisgivenFatal <- nrow(FatalHeart[FatalHeart$plateletLevel=="Thrombocytosis",])/nrow(FatalHeart)
pFatalgivenThrombocytosis <- (pThrombocytosisgivenFatal * pFatal) / pThrombocytosis
pFatalgivenThrombocytosis # P(Fatal | Thrombocytosis) = 0.3846154
# Probability of a fatal event is higher given the patient has abnormal platelet levels, whether indicative of thrombocytopenia or thrombocytosis.
# Visualizing that
pFatalPlatelet <- c(pFatalgivenNormalPlatelet, pFatalgivenThrombocytosis, pFatalgivenThrombocytopenia) # p for probability
cFatalPlatelet <- c("Normal", "Thrombocytosis", "Thrombocytopenia") # c for condition
barplot(pFatalPlatelet, names.arg = cFatalPlatelet, col = c("blue", "red", "red"), ylab = "Probability of Fatal Event",
        main = "Probability of Fatal Event by Status of Platelets in Blood")

# P(Fatal | Normal Creatinine) = (P(Normal Creatinine | Fatal) * P(Fatal)) / P(Normal Creatinine)
pNormalCreatinine <- nrow(Heart[Heart$creatinineLevel=="Normal",])/nrow(Heart)
pNormalCreatininegivenFatal <- nrow(FatalHeart[FatalHeart$creatinineLevel=="Normal",])/nrow(FatalHeart)
pFatalgivenNormalCreatinine <- (pNormalCreatininegivenFatal * pFatal) / pNormalCreatinine
pFatalgivenNormalCreatinine # P(Fatal | Normal Creatinine) = 0.2342857
# P(Fatal | Abnormal Creatinine) = (P(Abnormal Creatinine | Fatal) * P(Fatal)) / P(Abnormal Creatinine)
pAbnormalCreatinine <- nrow(Heart[Heart$creatinineLevel=="Abnormal",])/nrow(Heart)
pAbnormalCreatininegivenFatal <- nrow(FatalHeart[FatalHeart$creatinineLevel=="Abnormal",])/nrow(FatalHeart)
pFatalgivenAbnormalCreatinine <- (pAbnormalCreatininegivenFatal * pFatal) / pAbnormalCreatinine
pFatalgivenAbnormalCreatinine # P(Fatal | Abnormal Creatinine) = 0.4435484
# Probability of a fatal event is higher given the patient has abnormal creatinine levels.
# Visualizing that
pFatalCreatinine <- c(pFatalgivenNormalCreatinine, pFatalgivenAbnormalCreatinine) # p for probability
cFatalCreatinine <- c("Normal Level of Serum Creatinine", "Abnormal Level of Serum Creatinine") # c for condition
barplot(pFatalCreatinine, names.arg = cFatalCreatinine, col = c("blue", "red"), ylab = "Probability of Fatal Event",
        main = "Probability of Fatal Event by Status of Serum Creatinine Level")

# P(Fatal | Normal Sodium) = (P(Normal Sodium | Fatal) * P(Fatal)) / P(Normal Sodium)
pNormalSodium <- nrow(Heart[Heart$sodiumLevel=="Normal",])/nrow(Heart)
pNormalSodiumgivenFatal <- nrow(FatalHeart[FatalHeart$sodiumLevel=="Normal",])/nrow(FatalHeart)
pFatalgivenNormalSodium <- (pNormalSodiumgivenFatal * pFatal) / pNormalSodium
pFatalgivenNormalSodium # P(Fatal | Normal Sodium) = 0.2476636
# P(Fatal | Abnormal Sodium) = (P(Abnormal Sodium | Fatal) * P(Fatal)) / P(Abnormal Sodium)
pAbnormalSodium <- nrow(Heart[Heart$sodiumLevel=="Abnormal",])/nrow(Heart)
pAbnormalSodiumgivenFatal <- nrow(FatalHeart[FatalHeart$sodiumLevel=="Abnormal",])/nrow(FatalHeart)
pFatalgivenAbnormalSodium <- (pAbnormalSodiumgivenFatal * pFatal) / pAbnormalSodium
pFatalgivenAbnormalSodium # P(Fatal | Abnormal Sodium) = 0.5058824
# Probability of a fatal event is higher given the patient has abnormal sodium levels.
pFatalSodium <- c(pFatalgivenNormalSodium, pFatalgivenAbnormalSodium) # p for probability
cFatalSodium <- c("Normal Level of Serum Creatinine", "Abnormal Level of Serum Creatinine") # c for condition
barplot(pFatalSodium, names.arg = cFatalSodium, col = c("blue", "red"), ylab = "Probability of Fatal Event",
        main = "Probability of Fatal Event by Status of Serum Sodium Level in Blood")

# P(Fatal | Normal Ejection) = (P(Normal Ejection | Fatal) * P(Fatal)) / P(Normal Ejection)
pNormalEjection <- nrow(Heart[Heart$ejectionTest=="Normal",])/nrow(Heart)
pNormalEjectiongivenFatal <- nrow(FatalHeart[FatalHeart$ejectionTest=="Normal",])/nrow(FatalHeart)
pFatalgivenNormalEjection <- (pNormalEjectiongivenFatal * pFatal) / pNormalEjection
pFatalgivenNormalEjection # P(Fatal | Normal Ejection) = 0.2105263
# P(Fatal | Mildly Abnormal Ejection) = (P(Mildly Abnormal Ejection | Fatal) * P(Fatal)) / P(Mildly Abnormal Ejection)
pMildAbEjection <- nrow(Heart[Heart$ejectionTest=="Mildly Abnormal",])/nrow(Heart)
pMildAbEjectiongivenFatal <- nrow(FatalHeart[FatalHeart$ejectionTest=="Mildly Abnormal",])/nrow(FatalHeart)
pFatalgivenMildAbEjection <- (pMildAbEjectiongivenFatal * pFatal) / pMildAbEjection
pFatalgivenMildAbEjection # P(Fatal | Mildly Abnormal Ejection) = 0.2682927
# P(Fatal | Moderately Abnormal Ejection) = (P(Moderately Abnormal Ejection | Fatal) * P(Fatal)) / P(Moderately Abnormal Ejection)
pModAbEjection <- nrow(Heart[Heart$ejectionTest=="Moderately Abnormal",])/nrow(Heart)
pModAbEjectiongivenFatal <- nrow(FatalHeart[FatalHeart$ejectionTest=="Moderately Abnormal",])/nrow(FatalHeart)
pFatalgivenModAbEjection <- (pModAbEjectiongivenFatal * pFatal) / pModAbEjection
pFatalgivenModAbEjection # P(Fatal | Moderately Abnormal Ejection) = 0.2063492
# P(Fatal | Severely Abnormal Ejection) = (P(Severely Abnormal Ejection | Fatal) * P(Fatal)) / P(Severely Abnormal Ejection)
pSevereAbEjection <- nrow(Heart[Heart$ejectionTest=="Severely Abnormal",])/nrow(Heart)
pSevereAbEjectiongivenFatal <- nrow(FatalHeart[FatalHeart$ejectionTest=="Severely Abnormal",])/nrow(FatalHeart)
pFatalgivenSevereAbEjection <- (pSevereAbEjectiongivenFatal * pFatal) / pSevereAbEjection
pFatalgivenSevereAbEjection # P(Fatal | Mildly Abnormal Ejection) = 0.5483871
# Probability of a fatal event is higher given the patient has abnormal ejection fractions. The more severe the abnormality, the higher the probability.
pFatalEjection <- c(pFatalgivenNormalEjection, pFatalgivenMildAbEjection, pFatalgivenSevereAbEjection) # p for probability
cFatalEjection <- c("Normal", "Mildly Abnormal", "Severely Abnormal") # c for condition
barplot(pFatalEjection, names.arg = cFatalEjection, col = c("blue", "red", "red"), ylab = "Probability of Fatal Event",
        main = "Probability of Fatal Event by Status of Ejection Fraction")

# Now we know that the following leads to higher probability of a fatal event: 
# Age, anemia, smoking, hypertension, abnormal CPK, abnormal platelet levels, 
# abnormal creatinine levels, abnormal sodium levels, and abnormal ejection fraction.
# These can be used as predictors in a model which predicts fatal events.




                                                        
