#Assignment 7

#This assignment cover parameteric and non-parametric statistical testing

#Sofia Arora

#1. Task 1 follows a random uniform distribution between 5 and 9; 
#the completion time for Task 2 follows a random exponential distribution 
#with a rate of 0.1; Task 3 follows a Poisson distribution with a lambda of 4;
#Task 4 follows uniform distribution between 3 and 10. Compute the following.

#a. What are the mean and median times to complete all the tasks?
set.seed(1)
t1=runif(10000,min=5,max=9) #task1 follows uniform distribution
t2=rexp(10000,rate=0.1) #task 2 follows exponential distribution
t3=rpois(10000,lambda=4) # task 3 follows poisson distribution
t4=runif(10000,min=3,max=10) #task 4 follows uniform distribution

tot=ifelse(t1>t2 & t1>t3,t1,ifelse(t2>t3,t2,t3)) #task 1,2 and 3 are parallel
ts=tot+t4 #task 4 is sequential to all the parallel tasks before it
mean(ts) #mean time to complete all tasks
median(ts) #median time to complete all tasks

#b. What is the probability that all the tasks are completed in 15 hours?

tot=ifelse(t1>t2 & t1>t3,t1,ifelse(t2>t3,t2,t3)) #task 1,2 and 3 are parallel
ts=tot+t4 #task 4 is sequential to all the parallel tasks before it
length(ts[ts<15])/length(ts) #calculate probability that total tasks are completed in 15 hours

#c. Create a plot of the density of the total completion time.

plot(density(ts),main="Total Completion Time") #plotting the density function for ts
polygon(density(ts),col="red") #coloring the density function red


#2. Read the file diabetes.csv. There is a variable called Pregnancies, 
#which indicates the number of pregnancies. Assuming this follows a poisson 
#distribution, test the hypothesis that the mean number of pregnancies is 3.7

tstat=mean(diabetes$Pregnancies) #test statistic

pregtest=function() #draw a sample and commpute statistic
{
  s1=rpois(n=length(diabetes$Pregnancies),lambda = 3.7)
  return(mean(s1))
}
pregtest() #run function once

sdist=replicate(10000,pregtest()) #draw a sampling distribution by replicating 10000 times

plot(density(sdist)) #plot sampling distribution
abline(v=tstat,col="red") #plot the tstat value

gap=abs(mean(sdist)-tstat) #compute the gap between mean of sampling distribution and tstat
s2=sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap] #compute values beyond the tstat
pvalue=length(s2)/length(sdist) #computes the pvalue
pvalue
#P-value computed less than the alpha=0.05 and hence hypothesis is rejected.


#3. Read the file diabetes.csv. There is a variable called Insulin. 
#Conduct both a parametric and a non- parametric test for the median value of 80. 
#Are the results from both the tests similar? If not, explain why and which test you would trust more

#Parameteric Test
tstat=median(diabetes$Insulin) #test statistic

insutest=function() #draw a sample and compute statistic, that is median
{
  s1=rnorm(n=length(diabetes$Insulin),mean=80,sd=sd(diabetes$Insulin))
  return(median(s1))
}
insutest()

sdist=replicate(10000,insutest())#draw 10000 samples by replication

plot(density(sdist)) #plot sampling distribution
abline(v=tstat,col="red") #plot tstat
 
gap=abs(mean(sdist)-tstat) #compute the gap between mean of sampling distribution and tstat
s2=sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap] #compute values beyond the tstat
pvalue=length(s2)/length(sdist) #computes the pvalue
pvalue

#Non-parameteric test
tstat=sum(ifelse(diabetes$Insulin>80,1,0)) #define t statistic

insutest1=function() #draw a sample and compute the number of postives, that is 1s
{
  v=c(0,1)
  p=c(0.5,0.5)
  x=sample(x=v,replace=T,prob=p,size=length(diabetes$Insulin))
  return(sum(x))
}

sdist=replicate(10000,insutest1()) #create sampling distribution

plot(density(sdist)) #draw the sampling distribution
abline(v=tstat,col="red") #draw tstat

gap=abs(mean(sdist)-tstat) #compute the gap between mean of sampling distribution and tstat
s1=sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap] #compute values beyond tstat
pvalue=length(s1)/length(sdist) #computes the pvalue
pvalue

#We find both tests are similar as the pvalues for both tests are same.


#4. Read the file diabetes.csv. There are two variables called BMI and Outcome.
#The variable Outcome takes on only two values: 0 and 1. Conduct a 
#non-parametric two sample test for the hypothesis that the standard deviation 
#of BMI is the same for both Outcome values 
diabetes <- as.data.frame(diabetes) 
firstgroup=diabetes[diabetes$Outcome==0,6] #first group contains values of BMI where Outcome is 0
secondgroup=diabetes[diabetes$Outcome==1,6] #second group contains values of BMI where Outcome is 1
tstat=sd(firstgroup)-sd(secondgroup) #compute tstat
n=length(firstgroup) #set n as length of first group
bmitest=function() #function is created that will sample first observations of first group and then second group
{
  x=sample(c(firstgroup,secondgroup))
  m1=sd(x[1:n]) #the metric is computed across first group
  m2=sd(x[(n+1):length(x)])  #the metric is computed across second group
  abs(m1-m2)
  return(m1-m2) #returns absolute values between two groups
}

sdist=replicate(10000,bmitest()) #create sampling distribution

plot(density(sdist)) #draw the sampling distribution
abline(v=tstat,col="red") #draw tstat
abline(v=mean(sdist)-tstat, col="red")
lvalue = mean(sdist>tstat) #compute values beyond tstat
rvalue = mean(sdist<mean(sdist)-tstat) #compute values beyond tstat
pvalue = lvalue+rvalue #computes the pvalue
pvalue

#We find pvalue to be greater than alpha=0.05, hence we fail to reject the hypothesis.

#5. Read the file diabetes.csv. There are two variables called Glucose and BloodPressure. 
#Conduct a non-parametric test for the shapes of the two distributions are 
#identical

diabetes <- as.data.frame(diabetes)

Glucose1=((diabetes$Glucose)-mean(diabetes$Glucose))/sd(diabetes$Glucose) #standardize Glucose variable
BloodPressure1=((diabetes$BloodPressure)-mean(diabetes$BloodPressure))/sd(diabetes$BloodPressure) #standardize Blood Pressure variable

q=c(0.25,0.5,0.75,0.9) #set up quantile points
s1=quantile(Glucose1,probs = q) #s1 is vector that will contain values of  Glucose1  distribution at various quantiles set above
s2=quantile(BloodPressure1,probs = q) #s2 is vector that will contain values of BloodPressure1 distribution at various quantiles set above
tstat=sum(abs(s1-s2)) #compute tstat

shapetest1=function()  #this function will help us boostrap values
{
  q=c(0.25,0.5,0.75,0.9) #quantile values
  x1= quantile(sample(Glucose1,length(diabetes$Glucose),replace=T),q) #creating bootstrap sample and generating quantile for standardized variable Glucose
  x2= quantile(sample(Glucose1,length(diabetes$BloodPressure),replace=T),q) #creating bootstrap sample and generating quantile for standardized variable BloodPressure
  x=sum(abs(x1-x2))
  return(x)
}

sdist=replicate(10000,shapetest1()) #create sampling distribution

plot(density(sdist)) #draw the sampling distribution
abline(v=tstat,col="red") #draw tstat
pvalue=mean(sdist>tstat)
pvalue

#P value is less than alpha and hence we reject the hypothesis, that is, shapes are not identical.

