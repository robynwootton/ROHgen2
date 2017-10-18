#Script to extract ALSPAC variables for ROHgen2 analysis
#Robyn Wootton 09/06/2017
#############################################################################################################
#Contents
#1. Load Packages and ROHgen1 data
#2. Search for variables
#3. Extract variables
#4. Merge in necessary Medication status
#5. Merge with ROHgen1 sample
#6. Merge in Principal components
#7. Formatting data as per "traits_definition.pdf"
#8. Saving phenotype data 

#############################################################################################################
#1. Load Packages and ROHgen1 data
#############################################################################################################
#Make sure mac is connected to the R drive before installing below packages
#install.packages("devtools")
library(devtools)
#install_github("explodecomputer/alspac")
library(alspac)

#Load in ROHgen1 data
setwd("/Users/Robyn/Google_Drive/PhD/Research/ROHgen2/ROHgen/extracting_pheno/post_preg")
gen1<-read.csv("phenotypes.csv", header=T)
str(gen1)

#############################################################################################################
#2. Search for additional variables
#############################################################################################################
#load current variables
data(current)
str(current)
setwd("/Volumes/filestore/SSCM ALSPAC/Data/Current")

##### 2) Anthropometric
#Mothers birth weight
vars<-findVars("birth", "weight", "Mums", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Mother")
#dw030 - Mums birth weight known
#dw030a

##### 4) Lung function
#lung function
#NA
#ever smoked
#b650

##### 6) Glycaemic traits
#Fasting plasma glucose 
vars<-findVars("Fasting", "plasma", "glucose", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat4=="Mother")
#glucose_FOM1

#HbA1c 
vars<-findVars("HbA1c", whole.word=TRUE, ignore.case=TRUE)
#NA - only in child

#Diabetes
vars<-findVars("Diabetes", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Mother")
#n1070 - ever had diabetes
#n1071 - only during pregnancy

##### 7) Educational attainment
#years of eductaion 
#C645a

##### 8) Cognition
#g (cognitive ability)
vars<-findVars("g", "cognitive", "cognition", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Mother" | vars$cat3=="Adult")
#NA?

##### 9) Haematological traits
#haemoglobin
vars<-findVars("haemoglobin", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat4=="Mother")
#Hb_FOM1 - fasting levels

#white blood cells
vars<-findVars("white", "blood", logic="all", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat4=="Mother" | vars$cat3=="Adult")
#NA

#lymphocytes
vars<-findVars("lymphocyte", whole.word=TRUE, ignore.case=TRUE)
#NA

#monocytes
vars<-findVars("monocytes", whole.word=TRUE, ignore.case=TRUE)
#NA

#platelet count 
vars<-findVars("platelet", whole.word=TRUE, ignore.case=TRUE)
#NA

##### 10) Electrocardiology
#QRS interval
vars<-findVars("QRS", "interval", "heart", "rate", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Mother" | vars$cat3=="Adult")
#PR interval
#QT interval
#NA

#heart rate
vars<-findVars("heart", "rate", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Mother" | vars$cat3=="Adult")
#ff1bp142 - mean seated pulse rate
#ff1bp145 - mean standing pulse rate

#Myocardial infarction
vars<-findVars("heart", "Myocardial", "attack", "infarction", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Mother" | vars$cat3=="Adult")
#t5000 - ever had heart attack

##### 11) Liver enzymes
#GammaGlutamyl Transferase
vars<-findVars("GammaGlutamyl", "Gamma", "Glutamyl", "Transferase", whole.word=TRUE, ignore.case=TRUE)
#NA

#Alanine transaminase 
vars<-findVars("Alanine", "transaminase", "liver", "enzyme", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat4=="Mother" | vars$cat4=="Adult")
#Ala_FOM1 - Alanine mmol/l

##### 12) Metabolic
#Uric acid
vars<-findVars("Uric", "acid", "serum", "urate",whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat4=="Mother" | vars$cat3=="Mother")
#NA

##### 13) Inflammatory response
#High Sensitive C-Reactive Protein 
vars<-findVars("High", "Sensitive", "C-Reactive", "Protein", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat4=="Mother" | vars$cat3=="Mother")
#crp_FOM1 - fasting levels (mg/l)

#Fibrinogen 
vars<-findVars("Fibrinogen",  whole.word=TRUE, ignore.case=TRUE)
#NA

#Interleukin-6 
vars<-findVars("Interleukin", "6", "serum", "Interleukin-6", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat4=="Mother" | vars$cat3=="Mother")
#NA

#Tumor Necrosis Factor-α 
vars<-findVars("Tumor", "Necrosis", "Factor", "α", "Factor-α", whole.word=TRUE, ignore.case=TRUE)
#NA

##### 14) Renal function
#eGFR
vars<-findVars("eGFR", "Glomerular", "Filtration", "rate", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Adult")
#NA

#Serum Creatinine
vars<-findVars("Serum", "Creatinine", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat4=="Mother")
#Crea_FOM1 - mmol/l

##### 15) Fertility
#Number of children Ever Born
vars<-findVars("Number", "Ever", "children", "Born", "birth", "births", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Mother"| vars$cat4=="Mother")
#j139 - NO. Of Babies Born> Study CH
#m4068 - D4b8: No. pregs where children are still alive
#XB112 - Live births

#Age at first birth
vars<-findVars("Age", "first", "birth", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Mother"| vars$cat4=="Mother")
#m3102a  - Date of birth - child 1 - month  
#m3102b  - Date of birth - child 1 - year

###### 16) Ocular
#Spherical Equivalent Refraction
vars<-findVars("Spherical", "Equivalent", "Refraction", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Mother")
#NA

#Eye surgery 
vars<-findVars("Eye", "surgery", "cataract", "laser", whole.word=TRUE, ignore.case=TRUE)
vars<-subset(vars, vars$cat3=="Mother")
#NA

##### 17) Female reproductive timing
#age_menarche
vars<-findVars("period", whole.word=TRUE, ignore.case=TRUE)
men<-subset(vars, vars$cat3=="Mother")
#d010
#d010a

#age_menopause
meno <- findVars("menopause", whole.word=TRUE, ignore.case=TRUE)
head(meno)
#U1021

#############################################################################################################
#3. Extract variables
#############################################################################################################
library(alspac)
data(current)
setDataDir("/Volumes/filestore/SSCM ALSPAC/Data/")
all.vars <- subset(current, name %in% c("dw032", "b650", "glucose_FOM1", "n1070", "n1071", "c645a", "k9996b", "Hb_FOM1", "ff1bp142",  "ff1bp145",  't5000', "Ala_FOM1", "crp_FOM1", "Crea_FOM1",  "j139",  "m4068", "XB112", "m3102a",  "m3102b",   "d010", "d010a", "U1021", "s4270", "bnf_med"))
results <- extractVars(all.vars)
str(results)
head(results)

#############################################################################################################
#4. Merge in necessary Medication status
#############################################################################################################
setwd("/Volumes/filestore/SSCM ALSPAC/Data/Useful_data/Medications/Mother/FOM1")
library(foreign)
med<-read.dta("FOM1_BPmed_ALN.dta")
str(med)
#0=no 1=yes

#Merge
gen2<-merge(results, med, by="aln", all.x=T)
str(gen2)

#############################################################################################################
#5. Merge with ROHgen1 sample
#############################################################################################################
str(gen1)

#merge
gen<-merge(gen1, gen2, by.x="fid", by.y="aln", all.x=T)
str(gen)

#remove duplicates
gen<-gen[!duplicated(gen[1]),]
str(gen) #N=3326

#############################################################################################################
#6. Merge in Principal components
#############################################################################################################
## performed GCTA PCA, now need to read in and make final 
pcs <- read.table("/Users/Robyn/Google_Drive/PhD/Research/ROHgen2/ROHgen/geno/ALSPAC_mothers.eigenvec")
str(pcs)
names(pcs) <- c("fid", "iid", paste("pc",1:20, sep=""))

dat <- merge(dat, pcs, by="fid")
str(dat)

#############################################################################################################
#7. Formatting data as per "traits_definition.pdf"
#############################################################################################################
library(Hmisc)
#####1 Generic phenotypes
#ids
#need to add the letter M on the end to match up with the genetic data
#30001 and M -> 30001M
dat$qlet<-"M"
dat$ID = paste(dat$fid, dat$qlet, sep="")
str(dat)
sum(duplicated(dat$ID)) #Should be 0

#currently 0=female - recode to 2
table(dat$sex)
dat$sex[dat$sex==0]<-2

##### 2) Anthropometric
#Mothers birth weight
describe(dat$dw032)
#recode -1, -2 and -3 as missing
dat$dw032[dat$dw032==-1]<-NA
dat$dw032[dat$dw032==-2]<-NA
dat$dw032[dat$dw032==-3]<-NA
describe(dat$dw032)
#convert from gs to kgs
dat$dw032<-dat$dw032/1000

##### 4) Lung function
#ever smoked - recode to true and false
table(dat$b650)
levels(dat$b650)[levels(dat$b650)=="Y"] <- TRUE
levels(dat$b650)[levels(dat$b650)=="N"] <- FALSE
levels(dat$b650)[levels(dat$b650)=="DK"] <- NA
levels(dat$b650)[levels(dat$b650)=="Missing"] <- NA

##### 6) Glycaemic traits
#Fasting plasma glucose 
describe(dat$glucose_FOM1)
#convert from whole blood to plasma
dat$glucose_FOM1<-dat$glucose_FOM1*1.13

#Diabetes
#recode to true and false
table(dat$n1070)
levels(dat$n1070)[levels(dat$n1070)=="Yes"] <- TRUE
levels(dat$n1070)[levels(dat$n1070)=="No"] <- FALSE
levels(dat$n1070)[levels(dat$n1070)=="No response"] <- NA
levels(dat$n1070)[levels(dat$n1070)=="Unresolvable"] <- NA
levels(dat$n1070)[levels(dat$n1070)=="Not completed"] <- NA
levels(dat$n1070)[levels(dat$n1070)=="Triplet / quadruplet"] <- NA
#Exclude if they only had gestational diabetes
gest<-subset(dat, select=(dat$n1070==T & dat$n1071=="Yes"))
str(gest) #none overlap

##### 7) Educational attainment
#years of eductaion 
describe(dat$c645a)
#I used this as my conversion guide (http://www.mzes.uni-mannheim.de/publications/misc/isced_97/schn08e_the_application_of_the_isced-97_to_the_uks_educat.pdf)
levels(dat$c645a)[levels(dat$c645a)=="CSE"] <- 13
levels(dat$c645a)[levels(dat$c645a)=="Vocational"] <- 13
levels(dat$c645a)[levels(dat$c645a)=="O level"] <- 13
levels(dat$c645a)[levels(dat$c645a)=="A level"] <- 13
levels(dat$c645a)[levels(dat$c645a)=="Degree"] <- 19
levels(dat$c645a)[levels(dat$c645a)=="Missing"] <- NA

#calculate year of birth
fom1 <- read.csv("/Users/Robyn/Google_Drive/PhD/Research/ROHgen2/ROHgen/extracting_pheno/post_preg/FOM1vars.csv", na.strings="Missing", he=T)
names(fom1) <- c("aln", "year_completion", "age")
fom1$yob <- with(fom1, year_completion - age)
data<-merge(dat, fom1, by.x="fid", by.y="aln", all.x=T)
str(data)

##### 9) Haematological traits
#haemoglobin
summary(data$Hb_FOM1)
#divide by 10 to be in g/dL
data$Hb_FOM1<- data$Hb_FOM1/10
data$Hb_FOM1[data$Hb_FOM1 ==0] <- NA

#heart rate
#ff1bp142 - mean seated pulse rate
#ff1bp145 - mean standing pulse rate
#selected seated because there were more individuals

#Myocardial infarction
#recode 'not completed' and 'no response' 
table(data$t5000)
data$t5000[data$t5000==-10] <- NA
data$t5000[data$t5000==-1] <- NA
data$t5000[data$t5000==1] <- TRUE
data$t5000[data$t5000==2] <- FALSE

##### 13) Inflammatory response
#High Sensitive C-Reactive Protein 
#crp_FOM1 - fasting levels (mg/l)

#Serum Creatinine
#Crea_FOM1 - mmol/l
#multiply by 18 to convert to mg/Dl
data$Crea_FOM1<-data$Crea_FOM1*18

##### 15) Fertility
#Number of children Ever Born
table(data$m4068)
#recode -10 and -1 as NA
data$m4068[data$m4068 ==-10] <- NA
data$m4068[data$m4068 ==-1] <- NA

#Age at first birth
#m3102a  - Date of birth - child 1 - month  
#m3102b  - Date of birth - child 1 - year
table(data$m3102b)
data$afb<-data$m3102b-data$yob
table(data$afb)
data$afb[data$afb < 10] <- NA

##### 17) Female reproductive timing
#age_menarche
table(data$d010)
#recode -1 as missing
data$d010[data$d010 ==-1] <- NA

#age_menopause
table(data$U1021)

#subset all of the required variables 
str(data)
data<-subset(data, select=c("ID", "ID", "sex", "age.x", "pc1.x", "pc2.x", "pc3.x", "pc4", "pc5", "pc6", "pc7", "pc8", 'pc9', "pc10", "pc11", "pc12", "pc13", "pc14", "pc15", "pc16", "pc17", "pc18", 'pc19', "pc20", 
			"height", "bmi", "waist", "hip", "dw032", 
			"tot_chol",    "hdl", "triglyc", "lipid_lowering", 
			"b650", 
			 "bp_sys", 'bp_dia', "bp_med", 
			 "fpg", "diabetic", "fast_ins", 
			 'c645a','yob', 
			 "Hb_FOM1",
			 'ff1bp142', "t5000",
			 "crp_FOM1",
			 "Crea_FOM1",
			 "m4068", "afb",
			 "d010", "U1021"))

#name the variables as specified in "traits_definition.pdf"
names(data) <- c("fid", "iid", "sex", "age", "pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", 'pc9', "pc10", "pc11", "pc12", "pc13", "pc14", "pc15", "pc16", "pc17", "pc18", 'pc19', "pc20", 
			"height", "bmi", "waist", "hip", "birth_weight", 
			"tot_chol",  "hdl", "triglyc", "lipid_lowering", 
			"ever_smoked", 
			 "bp_sys", 'bp_dia', "bp_med", 
			 "fpg", "diabetic", "fast_ins", 
			 'edu','yob', 
			 "hb",
			 'hr', "mi",
			 "hscrp",
			 "creat",
			 "neb", "afb",
			 "age_menarche", "age_menopause")
str(data)
head(data, n=20)
#############################################################################################################
#8. Saving phenotype data 
#############################################################################################################

write.table(data, file="/Users/Robyn/Google_Drive/PhD/Research/ROHgen2/rohgen2/data/alspac_phenotypes.csv", col=T, row=F, qu=F, sep=",")

