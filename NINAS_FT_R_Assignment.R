setwd("C:\\Users\\ninas\\R\\RPackages")

.libPaths('C:\\Users\\ninas\\R\\RPackages')
require(foreign)
library(lawstat)
library(nortest)
library(Hmisc)
library(gmodels)
#Question 1 - read file 'salary' and identify its features using function str()
salary <- read.spss("C:\\Users\\ninas\\OneDrive\\Desktop\\SAP_Lab_Assignment3\\salary.sav", to.data.frame = T)
str(salary)
#salary is a dataframe that consists of 474 observations and 11 variables

#the dataset has numeric values in the 'age' & 'work' variables, while they should appear in integer format
salary$age <- as.integer(salary$age)
salary$work <- as.integer(salary$work)

#sexrace variable is unnecessary in the dataset because it consists of 2 other variables that 
#exist in the current dataset. These variables are 'sex' and 'minority'. So, we can drop the variable 'sexrace'
salary <- salary[,-11]
head(salary)


#question 2 - get summary of numeric values in the dataframe and visualize them
df_class <- sapply(salary, class) # find variables that have a numeric type

new_salary_df <- salary[, (df_class == 'integer') | (df_class == 'numeric')] #keep variables with numeric type
new_salary_df <- new_salary_df[,-1] #remove id variable

summary(new_salary_df) #salary of numeric variables
#salbeg (beggining salary) seems to have some outliers. Max value has a great difference from the variable's mean and median

coln <- c('Beggining Salary', 'Time', 'Age', 'Salary Now', 'Education Level', 'Work') #giving proper names to the variables

par(mfrow = c(3,4)) #modify plot window to show all histograms at once
for (i in 1:ncol(new_salary_df)){ #an iteration that goes through all the variables
  hist(new_salary_df[,i], xlab = coln[i], probability = TRUE, col = 'lightblue', main = paste('Histogram of',coln[i]))
  lines(density(new_salary_df[,i])) #lines that depict the actual variables
  index <- seq( min(new_salary_df[,i]), max(new_salary_df[,i]),length.out=100) 
  ynorm <- dnorm(index, mean=mean(new_salary_df[,i]), sd(new_salary_df[,i]) ) 
  #create an index that shows a normal distribution with mean and sd of each variable
  lines( index, ynorm, col='red', lty=3, lwd=3 ) 
  #lines that depict a normal distribution according to the above index for each variable
  qqnorm(rnorm(100,mean = mean(new_salary_df[,i]), sd = sd(new_salary_df[,i]))
         , main = paste('NPP for Normal(', round(mean(new_salary_df[,i]),1),',',round(sd(new_salary_df[,i]),1),') data.'))
  qqline(rnorm(100,mean = new_salary_df[,i], sd = sd(new_salary_df[,i])),col="red",lty=1,lwd=2)
}

#based on the above histograms and lines we can extract the following assumptions:
#only the 'work' variable seems to be normally distributed, because, unlike the rest of the variables,
#it matches with both of its respective normal distributions (on the histogram plot and qqnorm plot).



#Question 3 - examine if the beggining salary can be 1000 dollars. Interpret the results.
#considering that we examine one variable(salbeg), we first must examine its normality using kolmogorov and shapiro tests

shapiro.test(new_salary_df$salbeg)
lillie.test(new_salary_df$salbeg)
#examining the above outcome, we observe that in both cases the p-value is way less than 0.05.
#in other words, there is enough evidence to reject the null hypothesis that the variable salbeg is following a normal distribution

#so the next step is to examine whether the sample is large
length(new_salary_df$salbeg) #the sample has 474 variables, so it is considered a large sample

#we examine the sample's symmetry/skewness
mean(new_salary_df$salbeg)
median(new_salary_df$salbeg) #the difference between the mean and the median is large enough to consider the sample assymetric

#we use the wilcoxon test for one sample
wilcox.test(new_salary_df$salbeg, mu = 1000)
#p-value is less than 0.05, so there is enough evidence to reject the null hypothesis that the median of the population's beggining salary is equal to 1000$

wilcox.test(new_salary_df$salbeg, mu = 1000, alternative = 'less')
wilcox.test(new_salary_df$salbeg, mu = 1000, alternative = 'greater')
#considering the above wilcoxon tests, with a 5% confidence level we assume that the beggining salary of the population is less than 1000$



#Question 4 - consider the difference between salbeg and salnow. test the difference's significance
sal_diff <- new_salary_df$salnow - new_salary_df$salbeg

#we know from question 3 that the variable salbeg doesn't follow a normal distribution, so we examine whether the sample is large
shapiro.test(new_salary_df$salnow)
lillie.test(new_salary_df$salnow)
#the variable salnow, also does not follow a normal distribution - for reference
#so, the difference of these two, also does not a follow a normal distribution

length(sal_diff) #474 observations, so it's a large sample, so we use examine their symmetry

mean(sal_diff)
median(sal_diff) #the difference between mean and median is large, so we use the wilcoxon test to test if the difference is equal to 0

wilcox.test(new_salary_df$salnow, new_salary_df$salbeg, mu = 0, paired = T)
#given that the p-value is less than 0.05, we have enough evidence to reject the null hypothesis that the difference
#of the starting and current salary is 0. So, we can assume that the current salaries are different than the starting ones

wilcox.test(sal_diff, mu = 0, alternative = 'greater')
wilcox.test(sal_diff, mu = 0, alternative = 'less')
#given the above wilcoxon test, with a 5% confidence level we can assume that the median difference in starting and current 
#salaries of the population is more than 0 so the current salaries, in average are greater than the starting ones
par(mfrow = c(1,1))
boxplot(new_salary_df$salbeg, new_salary_df$salnow, names = c('Starting Salary', 'Current Salary')
        ,ylab = 'Salary', col = 'lightblue')
abline(h = median(new_salary_df$salbeg), col = 'red', lty = 2) 
abline(h = median(new_salary_df$salnow), col = 'green', lty = 2)
#by observing the boxplots, we can confirm that the median starting salary of the population is not the 
#same with the median current salary for the population


#Question 5 - test if there is any difference in the starting salary between the two genders
males <- salary[which(salary$sex == 'MALES'),2]
females <- salary[which(salary$sex == 'FEMALES'),2]

n1 <- length(males) 
n2 <- length(females) 
gender_df <- data.frame( salary=c(males, females),  gender=factor( rep(1:2, c(n1,n2)), labels=c('M','F') ) ) 

by(gender_df$salary, gender_df$gender, lillie.test)#we examine normality for both samples
by(gender_df$salary, gender_df$gender, shapiro.test)
#p-value is less than 0.05, so we reject the null hypothesis that the samples are normally distributed (both of them)

#we already examined their size, and both of them are large samples (>50), so in the next step we check their symmetry
mean(males); median(males)
mean(females); median(females)
#while the sample of females' salaries is symmetric, the sample of males isn't. so we use wilcoxon test
#to test whether their starting salaries are equal

wilcox.test(gender_df$salary ~ gender_df$gender, mu = 0)
#p-value < 0.05, so with a confidence level of 5% we reject the null hypothesis that the median starting 
#salary of the population is the same between genders

wilcox.test(gender_df$salary ~ gender_df$gender, mu = 0, alternative = 'less')
wilcox.test(gender_df$salary ~ gender_df$gender, mu = 0, alternative = 'greater')
#given the above p-values, with a confidence level of 5% we can assume that the starting salaries for the males 
#of the population are greater than those of women
par(mfrow = c(1,1))
boxplot(males,females, names = c('Males', 'Females'),xlab = 'Gender', ylab = 'Salary', main = 'Salary per Gender'
        ,col = 'lightblue') 
abline(h = median(males), col = 'green', lty = 2)
abline(h = median(females), col = 'red', lty = 2)
#observing the boxplots we can assume that the median starting salary is not the same 
#for the two genders in the population


#Question 6 - cut the age variable to 3 categories. examine if the beggining salary is the same for all age groups
age_cut <- cut2(salary$age, g = 3)
young_ad <- salary[which(salary$age >= 23 & salary$age<30),2]; len1 <- length(young_ad)
adult_pers <- salary[which(salary$age >= 30 & salary$age<40),2]; len2 <- length(adult_pers)
senior_pers <- salary[which(salary$age >= 40),2]; len3 <- length(senior_pers)

#we examine normality for all 3 of the samples
shapiro.test(young_ad)
lillie.test(young_ad)
shapiro.test(adult_pers)
lillie.test(adult_pers)
shapiro.test(senior_pers)
lillie.test(senior_pers) #regarding that all 3 of the samples have p-value < 0.05, for a 5% significance level we reject the null
#hypothesis that any of them is following normal distribution

age_df <- data.frame(salary = c(young_ad, adult_pers, senior_pers), age_group = factor(rep(1:3, c(len1,len2,len3)), labels=levels(age_cut)))
#we already confirmed that all 3 of the samples are large (>50), so in the next step we examine their symmetry
mean(young_ad); median(young_ad)
mean(adult_pers); median(adult_pers)
mean(senior_pers); median(senior_pers)# 2 of the samples seem to be symmetric, but the 'adult_pers' sample is not

#so we examine if the 3 samples have equal medians using the kruskal-wallis test
kruskal.test(age_df$salary ~ age_df$age_group)
#given that the p-value is (<0.05), with a confidence level of 5% we reject the null hypothesis that the starting 
#salary is the same for each age group. 
boxplot(age_df$salary ~ age_df$age_group, names = c('[23-30)','[30-40)','[40-64]'),ylab = 'Starting Salary'
        ,xlab = 'Age Groups', main = 'Salaries per Age Group', col = 'lightblue')
abline(h = median(young_ad), col = 'green', lty = 2)
abline(h = median(adult_pers), col = 'red', lty = 2)
abline(h = median(senior_pers), col = 'green', lty = 2)
pairwise.t.test(age_df$salary,age_df$age_group)
#observing the boxplots and the outcome of the pairwise.t.test, we can assume that with a confidence level of 1%
#seniors and young adults have a mean equal starting salary (p-value > a = 0.01). adults don't have equal mean salary
#with other age groups

#Question 7 - examine if the proportion of the white males is equal to the proportion of white females
gend_table <- table(salary$minority, salary$sex)
gend_table;
only_white <- rbind(gend_table['WHITE',]); rownames(only_white) <- 'WHITE'
only_white;
prop.table(only_white)
#at first glance, we observe that the proportion of white men is greater than the proportion 
#of white women in the sample 

chisq.test(only_white)
chisq.test(only_white, correct = F)
chisq.test(only_white, simulate.p.value = T)
#through the chisq.test we observe that (p-value > 0.05) so we do not reject the null hypothesis
#that the proportion of white male employees is independent to the proportion of the white female employees

