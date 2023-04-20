library("mvnormtest")                   #test for multivariate normality
library("car")                          #run the MANOVA test and other assumption testing
library("IDPmisc")                      #remove missing data for Shapiro test
library("multcomp")                     #conducting post hocs for ANCOVAs
library("dplyr")                        #calculate means

#Load Dataset: ASDdata.xslx

#Analysis: MANOVA
#IV = Status; categorical with 2 levels
#IV = Type; categorical with 2 levels
#DV = Subset - Toxic Elements (20 continuous variables)
#DV = Subset - Non-Toxic Elements (19 continuous variables)

#QUESTION 1: How does the urine test differ for kids with Autism (ASD) to kids without (TD)
#QUESTION 2: How does the urine test differ for kids with Autism (ASD) to their mothers
#QUESTION 4: Is there a difference between ASD vs TD Mothers

#Data Wrangling
#Make sure DVs are numeric/integer
#Create a subset containing only your DV in a matrix format
#Normality test max is 5,000 records; limit data if needed [1:5000,]
#Remove Mothers with no kids
#Pair up mother child with same ID
#Data with ASD TD kids


##QUESTION 3: Is there a key difference in toxic and non-toxic essential elements between ASD and TD kids

#Data Wrangling
#Make sure DVs are numeric/integer
#Create a subset containing only your DV in a matrix format
#Normality test max is 5,000 records; limit data if needed [1:5000,]

keepsToxic <- c("'Aluminum'", "'Antimony'", "'Arsenic'", "'Barium'", "'Beryllium'", "'Cadmium'", "'Cesium'", "'Gadolinium'", "'Lead'", "'Mercury'", "'Nickel'", "'Palladium'", "'Platinum'", "'Tellurium'", "'Thallium'", "'Thorium'", "'Tin'", "'Tungsten'", "'Uranium'")
keepsEssential <- c("'Boron'", "'Calcium'", "'Chromium'", "'Cobalt'", "'Iron'", "'Lithium'", "'Magnesium'", "'Manganese'", "'Molybdenum'", "'Phosphorus'", "'Potassium'", "'Selenium'", "'Sodium'", "'Strontium'", "'Sulfur'", "'Vanadium'", "'Zinc'", "'Creatinine'")

#remove missing data (for the M.Shapiro test)
ASDdata2 <- NaRV.omit(ASDdata)

#subset both the Toxic and Non-Toxic Elements
ASDdataToxic3 <- ASDdata2[keepsToxic]
ASDdataToxic4 <- as.matrix(ASDdataToxic3)   #Use this dataset to run the multivariate test

ASDdataEssential3 <- ASDdata2[keepsEssential]
ASDdataEssential4 <- as.matrix(ASDdataEssential3)  #Use this dataset to run the multivariate test

##TEST ASSUMPTIONS
#Sample Size (20 cases per IV; 40 required) - PASSED!

#Test Multivariate Normality - M.Shapiro test
#Read Results: THESE NEEDS TO BE NON-SIGNIFICANT (otherwise don't run a MANOVA)
mshapiro.test(t(ASDdataToxic4))           #ERROR IN SOLVE.DEFAULT
mshapiro.test(t(ASDdataEssential4))       #FAILED - pvalue is significant



#Data Usage
#We no longer the data with data removed OR the matrix. Return to using the original datafile.

#Test Assumption of HofVariance (leventTest("Car") for each DV variable/column)

leveneTest(DV1 ~ IV, data=datafile)
leveneTest(DV2 ~ IV, data=datafile)

#both of these tests MUST be non-significant

#Absence of Multicollinerarity
#correlation between DV's (lower than 0.7)

cor.test(datafile$DV1, datafile$DV2, method="pearson", use="complete.obs")
#Read Results: sample estimates cor (doesn't matter if the cor is +/-), just the strength (proximity to 1)

#Analysis

MANOVA <- manova(cbind(DV1, DV2) ~ IV, data=datafile)
summary(MANOVA)

#Now, we need to run a postHoc in order to determine which variable(s) were significant

#PostHoc
summary.avo(MANOVA, test="wilks")
#Read Results: Look for the DV that has a significant pvalue

#Run the normal ANOVA ttest postHocs + then run the means/raw means if needed. 

#ANCOVA FOR WHEN WE HAVE MET HOFVARIANCE FIRST
#ANCOVA = lm(DV ~ CV + IV*CV, data=filename) #Asterisk creates the interaction term: looking at how the CV and IV work together
#anova(ANCOVA)
ANCOVA = lm(DV~ CV + IV*CV, data=datafile)
anova(ANCOVA)
#Read Results: Having neither an international or vMail Plan influences the number of nighttime minutes. 

#NO NEED TO RUN POSTHOC or MEANS SINCE NOTHING HERE IS SIGNIFICANT

postHocCP <- glht(ANCOVA, linfct=mcp(IV = "Tukey"))
summary(postHocCP)

#DETERMINE Means and Draw Conclusions
#covariants require adjustedMeans calculations
#the adjusted means control for the covariate - use library("effects") for adjusted means
#adjMeans <- effect("IV in quotes", then specify the model ANCOVA)
#adjMeans to call it

