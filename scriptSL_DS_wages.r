######################################
#####STATISTICAL LEARNING PROJECT#####
#####       Based on R 3.5.1     #####
######################################
# Date:    14/02/2019                #
# Authors: Ambrosi Andrea            #
#          Bonaldi Helena            #
#          Munari  Chiara            #
#          Pakler  Marco             #
#          Papa    Bruno             #
######################################

library(ggplot2)
library(faraway)
library(lmtest)
library(corrplot)
library(gpairs)
library(car)
library(coefplot)
library(gridExtra)
library(cowplot)

#Set the directory
setwd("/Users/bru_008/Documents/Corsi/Stat_Learning/ProgettoLinearModel_StatistikaLMaximo/versoLaFine")
#Import the wages file
wages <- read.csv("wages.csv", header = T)

#########################################
#     1.Cleaning and modifying          #
#    dataset for further analysis       #
#########################################

#Re-categorization of the column look
#(1,2) --> 1
#(4,5) --> 3
#For low accuracy in determining marginal values, reduce noise in analysis
wages$reLook[wages$looks == 1 | wages$looks == 2] <- "UnderAvg"
wages$reLook[wages$looks == 3] <- "Average"
wages$reLook[wages$looks == 4 | wages$looks == 5] <- "AboveAvg"

#Build a new column with different interval of hourly wage 
#from 1 (low) to 5 (very high)
binWidth = ( max(wages$wage) - min(wages$wage) )/ 5
wages$wageCat[ wages$wage < 2] <- "1"
wages$wageCat[ 2  <= wages$wage & wages$wage< 4] <- "2"
wages$wageCat[ 4  <= wages$wage & wages$wage< 6] <- "3"
wages$wageCat[  wages$wage >=6] <- "5"

#Create categories for education year under-avg-above
wages$educCat[wages$educ < 11] <- "lowEduc"
wages$educCat[wages$educ < 13.5 & wages$educ> 11.5] <- "avgEduc"
wages$educCat[wages$educ >13.5] <- "highEduc"

#Create categories for exper novice - standard - high - veteran
wages$expCat[wages$exper <= 8] <- "Up to 8 yrs exp"
wages$expCat[wages$exper <= 15 & wages$exper> 8] <- "between 8 and 15 yrs"
wages$expCat[wages$exper <= 27 & wages$exper> 15] <- "between 15 and 27 yrs"
wages$expCat[wages$exper >= 27] <- "above 27 yrs of exp"

#############################
#pre-2 numerical exploration# 
#############################

#information about the dataframe dimension and type of data in columns
str(wages)

#correlation among numerical variables
wagesNumericals <- wages[c("wage" , "educ" , "exper")]
cor(wagesNumericals) #that confirm low correlation |0.2| or lower.. it's ok
corrplot.mixed(cor(wagesNumericals), upper = "ellipse" ) #un modo pi?? carino di vederlo

#visually plot every num variable in respect of every other
gpairs(wagesNumericals)


#function to find the mode of a sample distribution
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(wages$wage)
getmode(wages$educ)



###################################
# 2.DATA EXPLORATION AND PLOTTING #
###################################




#Histogram of the wage distribution
ggplot(data = wages) + geom_histogram(mapping = aes(x = wage , fill = reLook), color = "white")+ xlim(0,30) + theme( text = element_text(size = 30))
ggsave("wageDistroReLookLimX30.png")

#histogram plot for experience distribution
ggplot(data = wages) + geom_histogram(mapping = aes(x = educ) , fill = "blue", color = "white")

#experience years distribution: histogram plot
ggplot(data = wages) + geom_histogram(mapping = aes(x = exper) , binwidth= 5 , fill = "blue", color = "white")

#experience years distribution: histogram plot + density + boxplot
a <- ggplot(wages, aes(x = exper)) + 
  geom_histogram(binwidth = 2, color = "white", fill = "blue" ) 

b <- ggplot(wages, aes(x = "", y = exper)) + 
  geom_boxplot() + 
  coord_flip()

plot_grid(a,b,nrow=2 ,align="v", rel_heights = c(2/3 , 1/3))


#Histogram counts for the different looks categories
ggplot(data = wages) + 
  geom_bar(mapping = aes(x = reLook),fill = "blue", color = "white")+
  theme( text = element_text(size = 20))
ggsave("reLookCount.png")

#histogram plot, counts of respondents in three educational level
ggplot(data = wages) + 
  geom_bar(mapping = aes(x = educCat),fill = "blue", color = "white")+
  theme( text = element_text(size = 20))


###Histogram for educ and exp recathegorized investigating the gender gap####

#male vs female vs condition of being above or below the mean wage threshold
ggplot (data = wages) + geom_bar(mapping = aes(x = gender , fill = wage < mean(wages$wage)), position = "fill")+
  theme( text = element_text(size = 24) , legend.position = "bottom") + scale_fill_brewer(palette = "Paired")
#ggsave("genderVsmeanWageNorm.png")

#mean wage threshold plot for different educational level
ggplot(data = wages, aes =(col = gender), position = "dodge") + 
  geom_bar(mapping = aes(x = educCat  ,fill = wage < mean(wages$wage)))+
  theme( text = element_text(size = 20))

#mean wage threshold plot for different educational level, focus on different gender
ggplot(wages , aes( x = gender)) + geom_bar(aes(fill = wage<mean(wages$wage), color = gender), stroke = 1 , position = "fill") + 
  facet_wrap(~educCat) +   theme( text = element_text(size = 24) , legend.position="bottom")  + scale_fill_brewer(palette = "Paired") 
ggsave("genderVsEducVsWageMean.png")

#mean wage threshold plot for different experience level, focus on different gender
ggplot(wages , aes( x = gender)) + geom_bar(aes(fill = wage<mean(wages$wage), color = gender), stroke = 1 , position = "fill") + 
  facet_wrap(~expCat) +   theme( text = element_text(size = 24) , legend.position="bottom")  + scale_fill_brewer(palette = "Paired") 



#Histogram plus density funciton, both normalized to see the distribution of the wage value among population
ggplot(data = wages , aes(x=wage)) + geom_density() + geom_histogram(aes(y = ..density..), alpha = 0.3)

#Histogram of the educ distribution
ggplot(data = wages) + geom_bar(mapping = aes(x = educ))

#Histogram of years of experience accounting for looks
ggplot(data = wages) + geom_histogram(mapping = aes(x = exper , fill = reLook), binwidth = 5)

#Histogram of years of experience fill colors prop to reLook variable normalized
ggplot(data = wages) + 
  geom_histogram(mapping = aes(x = exper , fill = reLook), position = "fill", color = "white", binwidth = 5)+
  theme( text = element_text(size = 20))
#ggsave("experLookProp.png")


#Histogram that has on the x the reLook columns for both the genders. 
#Each columns is filled by colors based on the wage category.
ggplot(wages , aes( x = reLook)) + geom_bar(aes(fill = wageCat), position = "fill" , color = "white") + 
  facet_wrap(~gender) + scale_fill_brewer(palette = "Set1") 

#Dotted graph for ethnicity and education
ggplot(data = wages) + geom_point( mapping = aes( x = ethnicity , y = educ, color = educ) , position = "jitter")

#Histogram with the education discretized for ethnicity
ggplot(wages) + geom_histogram(aes(x = educ), binwidth = 3, color = "white", fill = "blue") + facet_wrap(~ethnicity) 

#############
# 3. MODELS #
#############

#Clone the dataset replacing the 0 with 1 in the column 
#of the experience in order to perform the analysis with the log function.
wagesL <- wages
wagesL$exper[wagesL$exper == 0] <- 1

# Best model #
modBig <- lm( log(wage) ~ log(exper) + educ + union + educ:ethnicity
              + region + city  + industry:city + reLook*gender, data = wagesL)

# same model but with releveled gender variable
#modBig <- lm( log(wage) ~ log(exper) + educ + union + educ:ethnicity
#              + region + city  + industry:city + reLook*relevel(gender , "male")
#              , data = wagesL)

#We use relevel in order to change the reference level for the relative variable.
#It is used to see the difference in reLook between a nice and a wonderful women.

#With the summary function we can see and verify the patterns of possible discrimination
summary(modBig)


##################################
#       Outliers Test            #
##################################
#find and test for outlier status with Bonferroni critical value from library car
outlierTest(modBig)

outliersDF <- wagesL[c(603),]
wagesNoOut <- wagesL[-c(603),]

modBigNoOut <- lm( log(wage) ~ log(exper) + educ + union + educ:ethnicity
              + region + city  + industry:city + reLook*gender, data = wagesNoOut)
summary(modBigNoOut)




#########################################
# 4.VIOLATIONS OF THE MODEL ASSUMPTIONS #
#########################################

#We assume the normal distribution according to the large numbers law

# 4.1 FULL RANK / COLLINEARITY / MULTICOLLINEARITY #

round(cor(wagesL[, c(1,2,6)]), digits = 3)
#As we can see the correlation between the numeric variables is low.

#Now let's control the VIF (variance inflator factor)
#If it is > 10 we'll have a problem
vif(wages[, c(1,2,6)])
#But the result is less than 10 so there is no high multicollinearity.



aux_reg1 <- lm(log(exper)~ + educ + union + educ:ethnicity
              + region + city  + industry:city + reLook*gender, data = wagesL)

aux_reg2 <- lm( educ ~  log(exper) + union + educ:ethnicity
              + region + city  + industry:city + reLook*gender, data = wagesL) 

summary(aux_reg1)
summary(aux_reg2)

# 4.2 NORMALITY DISTRIBUTION #

qqnorm(modBig$res, ylab = 'Raw Residuals')
qqline(modBig$res)
grid()


# 4.3 HOMOSCEDASTICITY AND NONAUTOCORRELATION #

#Informal method:
#Now we plot the residuals over the fitted values 
#in order to visually detect if there are patterns
plot(modBig$fit, modBig$res, xlab = "Fitted", ylab = "Residuals")
grid()
#and in the plot we can see a pattern on the bottom

#Formal method:
#Let's do the BP-test using the F-test
auxmodBig <- lm(modBig$res^2  ~ log(exper) + educ + union + educ:ethnicity
                + region + city  + industry:city + reLook*gender, data = wagesL) 
summary(auxmodBig)
#Our p-value is: 0.00438 

#Another way to perform the BP-test is with the lmtest library:
bptest(modBig)
#Result 
#BP = 33.029, df = 15, p-value = 0.004651
#Given H0: homoscedasticity
#As result we have that the p-value is small enough so we can reject the null hypothesis.
#It seems there is the heteroscedasticity problem but we can try to correct it with the FWLS.

# 4.4 FWLS AGAINST HETEROSCEDASTICITY #

#Here we take the log of the square of the residuals
logRes2 <- log(modBig$res^2)

#And here we fit the log of the square of the resuduals to the rest of the variables
varMod <- lm( logRes2 ~ log(exper) + educ  + union + educ:ethnicity
              + region + city  + industry:city + reLook*gender, data = wagesL)  

#Then we take the fitted values that we are going to use as weights for the corrected model
w <- exp(varMod$fit)   

#This is the corrected model, like the original one but weighted using the w
modBigFWLS <- lm( log(wage) ~ log(exper) + educ  + union + educ:ethnicity
                  + region + city  + industry:city + reLook*gender, weight = 1/w , data = wagesL)
summary(modBigFWLS)

confint(modBigFWLS)

coefplot(modBigFWLS, intercept=FALSE, outerCI=1.96,
         xlab="Association with hourly wage")

# 4.5 HETEROScEDASTICCTY ROBUST STANDARD ERRORS

coeftest(modBigFWLS , vcoc = hccm)

######## EOF #########
