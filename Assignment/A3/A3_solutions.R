library(readstata13)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggthemes)
library(AER)
library(Hmisc)
library( sjmisc)

#================================================================
# open data 
#================================================================

data("Affairs")
ls()
dat_affairs = Affairs
names(dat_affairs)

#================================================================
# exercise 1
#================================================================

pf = ggplot(dat_affairs,aes(x=affairs)) + geom_bar()
pf

pf_gender = ggplot(dat_affairs,aes(x=affairs,color=as.factor(gender),fill=as.factor(gender))) + geom_histogram(position="dodge") + theme(legend.title = element_blank())
pf_gender

pf_age = ggplot(dat_affairs,aes(x=affairs,color=as.factor(age),fill=as.factor(age))) + geom_histogram(position="dodge") + theme(legend.title = element_blank())
pf_age

pf_relig = ggplot(dat_affairs,aes(x=affairs,color=as.factor(religiousness),fill=as.factor(religiousness))) + geom_histogram(position="dodge") + theme(legend.title = element_blank())
pf_relig

pf_edu = ggplot(dat_affairs,aes(x=affairs,color=as.factor(education),fill=as.factor(education))) + geom_histogram(position="dodge") + theme(legend.title = element_blank())
pf_edu


#================================================================
# exercise 2
#================================================================

# run the regression
reg1 = lm(affairs ~ as.factor(education) + I(gender) + as.factor(occupation) + as.factor(age) +
                   as.factor(children) + as.factor(religiousness) + as.factor(yearsmarried) + as.factor(rating), data = dat_affairs)
summary(reg1)


#================================================================
# exercise 3
#================================================================

# create dummies
dat_dummies = dat_affairs %>% select(education,gender,occupation,age,religiousness,yearsmarried,rating) %>% to_dummy()
names(dat_dummies) = c("grade","high-school","some-college","college","some-graduate","graduate","doctoral",
                       "female","male",
                       paste0("occupation",1:7),
                       "-20","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55+",
                       "anti","not at all","slightly","somewhat","very",
                       "-3m","4-6m","6m1y","1-2y","3-5y","6-8y","9-11y","12y+",
                       "+unhappy","unhappy","average","happy","+happy")


Yvec = as.numeric(dat_affairs$affairs>0)
table(Yvec)

# create matrix of X
Xmat = dat_dummies %>% select("grade","high-school","some-college","some-graduate","graduate","doctoral",
                              "female",
                              paste0("occupation",c(1:3,5:7)),
                              "-20","20-24","25-29","35-39","40-44","45-49","50-54","55+",
                              "anti","not at all","somewhat","very",
                              "-3m","4-6m","6m1y","3-5y","6-8y","9-11y","12y+",
                              "+unhappy","unhappy","happy","+happy")
Xmat = as.matrix(Xmat)

#=========================================
# likelihood function for the probit model
#=========================================

like_probit =function(beta,Xmat,Yvec)
{
  Xbeta             = cbind(1,Xmat)%*%beta  # calculate Xbeta 
  prob              = pnorm(Xbeta);         # calculate probabilities
  prob[prob<0.0001] = 0.0001                # make sure probabilities do not hit bounds
  prob[prob>0.9999] = 0.9999
  loglike           = Yvec*log(prob) + (1-Yvec)*log(1-prob);  
  return(sum(loglike))
}

npar = ncol(Xmat)  + 1
like_probit(runif(npar),Xmat,Yvec)

reg2 = glm(Yvec ~ Xmat, family = binomial(link = "probit"))
summary(reg2)

# extract coefficients 
coef  = reg2$coefficients
  value = logLik(reg2)
  

# testing whether loglikelihood values match
output= cbind(value,like_probit(coef,Xmat,Yvec))
output

#================================================================
# exercise 4
#================================================================
