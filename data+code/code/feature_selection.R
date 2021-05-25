# ensure the results are repeatable
set.seed(7)

# load the library
library(caret)
library(MASS)
library(glmnet)

# assuming the data is already in the memory

# calculate correlation matrix for the continuous variables
correlationMatrix <- cor(basic.features[,5:7])

# summarize the correlation matrix
print(correlationMatrix)

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# calculate correlation for the categorical variables
tbl = table(basic.features$V2, basic.features$V3) 
print(tbl)
chisq.test(tbl) 

selected = basic.features[basic.features$V3 == "74:da:38:80:79:fc" | basic.features$V3 == "74:da:38:80:7a:08" 
                        | basic.features$V3 == "'b0:c5:54:1c:71:85" | basic.features$V3 == "b0:c5:54:25:5b:0e" 
                        | basic.features$V3 == "5c:cf:7f:07:ae:fb" | basic.features$V3 == "5c:cf:7f:06:d9:02", ]

tbl = table(selected$V1, selected$V2) # is the destination IP independent of the protocol at .05 significance level. 
print(tbl)
ct = chisq.test(tbl) 

# Fit a logistic regression model
fit_glm = glm(selected$V1 ~ selected[,5], selected, family = "binomial")
summary(fit_glm) # generate summary

#No. of cols in data frame
c <- ncol(selected)
#Intializing the vector which will contain the p-values of all variables
pvalues <- numeric(c)
# Getting the p-values
for (i in 1:c) {
  fit <- glm(selected$V1 ~ selected[,i], selected, family = "binomial")
  summ <- summary(fit)
  pvalues[i] <- summ$coefficients[2,4]
}
ord <- order(pvalues)
x10 <- selected[,ord]
names(x10)
# fit_glmnet <- glmnet(selected$V5, selected$V1, alpha=1)
# summary(fit_glmnet) # generate summary
# 
# fit_lm <- lm(selected$V1 ~ selected$V5, data = selected)
# summary(fit_lm) # generate summary
