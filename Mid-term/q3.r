# midterm 2008, Q3
############################
# part a 
############################

males<- c(905.8,1012.9,1202.8,1161.6,1022.2,926.5,768.4,648.0,651.4,622.9,579.0,472.1,417.1,318.2,228.6,142.3,83.9,53.5)
females<-c (860.0,966.5,1149.5,1115.5,1010.5,929.8,737.3,627.0,622.5,621.5,600.1,495.3,437.4,354.6,273.1,201.0,128.5,99.4)

total_males <- sum(males)
total_females <- sum(females)

total_sample_size <- total_males+ total_females

#sample proportion
s_p <- total_males/total_sample_size

s_p

# sample error
se <- sqrt((s_p * (1-s_p))/total_sample_size)

# applying wald test
p_value =2* pnorm( -abs(s_p-0.5)/se)

p_value

# it is 0.93, so H0 is not rejected at 5% level of significance
# we can use pearson chi square test as well.

##################
# part b
##################

# using bonferroni method

# At 5% Level of significance for overall test
alpha_individual <- (.05/length(males))

# counting no of rejections of individual hypothesis
count <- 0

# initializing p-values
p_value <- rep(1,length(males))

for (i in 1: length(males)) {

total_males <- males[i]
total_females <- females[i]

total_sample_size <- total_males+ total_females

#sample proportion
s_p <- total_males/total_sample_size

s_p

# sample error
se <- sqrt((s_p * (1-s_p))/total_sample_size)

# applying wald test
p_value[i] =2* pnorm( -abs(s_p-0.5)/se)

if (p_value[i] < alpha_individual) { 
 cat(males[i],"\n")
 count<- count+1 
}

}

count 

# Reject H0 if count >0; In this case count =3 , so H0 is rejected.

#####################
# part c
#####################
# using B-H method

# Getting the ordered p-values from part(b)
ordered_pvalues <- sort(p_value, decreasing=FALSE)

# assuming all the age groups are independent and so are the corresponding p_values
# FDR (alpha value) =5% or 0.05

p_i <- seq(1:length(males))*(0.05/length(males))

for (i in 1: length(males)) {

if (ordered_pvalues[i] > p_i[i]) {
cat ("i value:",i,"\n")
break
}
}

# cut-off p_value 

cutoff_p_value <- ordered_pvalues[i-1]

cat ("cut-off p_value :",cutoff_p_value,"\n")

# printing age ranges whose hypotheses are rejected.

for (i in 1 : length(males) ) {

if (p_value[i] <= cutoff_p_value) {
cat ("Group no :",i,"rejected\n")
}
}














