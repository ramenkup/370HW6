#Spencer Klinge
#Thrusday Sept 25th, 2015
#ISTA370 - Prof. Kumar
#Homework 6

#1

#--- (a) ---

  #i
#Ho: The average height of the track team is not the same as the football team.

  #ii
#two tailed-non directional comparison

  #iii
#lower crit value is -1.645 and the upper is 1.645 where alpah=.05 split among two regions

  #iv
#the  critical value for a one tailed test where alpha=.025 would be 1.96 or -1.96 depending on wether its upper or lower

  #v
#sqrt((SD^2/N1)+(SD^2/N2))
#SD=6 N=36
#sqrt(2.0)
sqrt(2)
#Standerd Error:[1] 1.414214

  #vi
# The null is rejectable at all levels (a=0.05, a=.025, a=.01). the critical values are greater than the Std Err.(SE=1.41)
#---(b)----

  #i
#Ho: Drug A reduces blood pressure less than 5 points compared to drug b.

  #ii
#one-tailed test: directional comparison (>5)

  #iii
#N:100
#SD of A:2
#SD of B:2
#sample difference (xbar-xbar):6
#population difference(u-u):5
#SE: sqrt((SD^2/N1)+(SD^2/N2))
err<-sqrt(4/50)
err
#[1] 0.2828427

  #iv
(6-5)/err
#z=[1] 3.535534

  #v
#No cannot reject the null where z=3.54. (z<5)


#----------2----------
#Ho:the corrilation of GNP and number of people Unemployed is not significantly different from 0
#2
var<-cor(longley$GNP, longley$Unemployed)
var
#cor:[1] 0.6042609
fisher<-.5*log((1+var)/(1-var))
#fisher Z:[1] 0.6998317
denom<-sqrt(((sd(longley$GNP)^2)/16)+(sd(longley$Unemployed)^2/ 16))
#SE:[1] 34.10607
z<- (mean(longley$GNP)- mean(longley$Unemployed))/denom
z
#z=[1] 2.004546

#This is a one tailed test because because the unitilization of natural log.

#The null can be rejected because the Z-score (Z=2.00) falls inside the rejection region where a=.05 crit value=1.645. the null Ho is that GNP and Unemployed is not significantly different from zero, which is rejected.
#----------------3-------------------

#3

A<-c(17, 29, 14, 21, 29, 4, 10)
B<- c(19, 31, 18, 27, 28, 6, 11)

#---(a)---
t.test(A,B,paired=FALSE)#df=11.999

#---(b)---
t.test(A,B,paired=TRUE)#df=6

#---(c)----
# one sampled t test is testing if there is significant difference between a hypothisized popoulation mean and an observed sample mean while a two sample t test is seeing if two sample means are significantly different than the population. not being paired means two data sets counts as two indipendent data sets, while they are combined when they are paired. the means of the sample are compared to the population seperatly in an un paired, while they are compared together to the popmean in a paired comparison.

#---(d)---
# Paired is taking two groups of data and looking at two points in a pair. Unpaired t test has more degrees of freedom because it has twice as many data points/ subjects as the paired. Therefore, unpaired will have more degrees of freedom.

