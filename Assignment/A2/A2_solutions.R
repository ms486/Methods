library(readstata13)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggthemes)

#================================================================
# open data 
#================================================================


setwd("/home/ms486/Dropbox/papers/education/Ghana Structural/experiment/Data")
setwd("/home/ms486/Dropbox/papers/education/Ghana Structural/experiment/Data")

dat = read.dta13("guiide_master_file_anonymized_full.dta")


#================================================================
# Exercise 1 
#================================================================
nrow(dat)
ncol(dat)

listnames = names(dat)                    # 3,332 variables
listnames[grep("id",listnames)]           # 623 variables
str(dat$anon_studentid)                   # individual identifier.. Not unique

sink("summary.txt")
summary(dat)
sink()

table(duplicated(dat$anon_studentid))
dat$id = dat$anon_studentid

#================================================================
# Exercise 2 
#================================================================

dat.surv = dat[,grep("stud_base",listnames)]
dim(dat.surv)

dat.admin = dat[,grep("alladmin",listnames)]
dim(dat.admin)

dat.rest = dat[,-c(grep("stud_base",listnames),grep("alladmin",listnames))]
dim(dat.rest)


listnames = names(dat)                                                          # 3,332 variables
listnames[grep("stud_base",listnames)]           # study base                   # 623 variables
listnames[grep("pg_fu",listnames)]               # parent guardian full??       # 1190 variables
listnames[grep("alladmin",listnames)]            # parent guardian full??       # 148 variables
listnames[grep("pg_bl",listnames)]               # parent guardian bl??       # 1190 variables

#================================================================
# Exercise 3
#================================================================
# verify consistent measure of genders
names(dat.surv)[grep("gender",names(dat.surv),ignore.case = T)]
names(dat.admin)[grep("gender",names(dat.admin),ignore.case = T)]
names(dat.rest)[grep("gender",names(dat.rest),ignore.case = T)]

table(dat.surv$stud_base_GENDER,dat.admin$alladmin_GENDER)

table(dat.surv$stud_base_GENDER,dat.rest$GENDER)

table(dat.admin$alladmin_GENDER,dat.rest$GENDER)


# verify consistency of age
names(dat.surv)[grep("age",names(dat.surv),ignore.case = T)]
names(dat.admin)[grep("age",names(dat.admin),ignore.case = T)]
names(dat.rest)[grep("age",names(dat.rest),ignore.case = T)]

tab1 = table(dat.surv$stud_base_age,dat.admin$alladmin_age)
sum(tab1) - sum(diag(tab1))

tab2 = table(dat.surv$stud_base_age,dat.rest$stud_fu1_age)
sum(tab2) - sum(diag(tab2))

tab3 = table(dat.admin$alladmin_age,dat.rest$stud_fu1_age)
sum(tab3) - sum(diag(tab3))

# underconfidence, overconfidence and inconsistency

table(dat$stud_base_bece_best>dat$stud_base_bece_worst)
table(dat$stud_base_bece_best>dat$stud_base_bece_likely)
table(dat$stud_base_bece_worst>dat$stud_base_bece_likely)


#================================================================
# Exercise 4
#================================================================
listnames[grep("treat",listnames,ignore.case = T)]

datf = dat %>% select(treatgroup,alladmin_GENDER,alladmin_age,SHSregionname) 
names(datf) =  c("treat","gender","age","region")


tab_gender = datf  %>%filter(gender=="M"|gender=="F") %>% group_by(treat,gender) %>% summarize(n=n()) %>% mutate(freq = n / sum(n)) %>%filter(n>100)
tab_gender

tab_age = datf %>% group_by(treat) %>% summarize(n=n(),mage= mean(age,na.rm=T),sage = sd(age,na.rm=T)) %>%filter(n>100)
tab_age

tab_region = datf  %>% group_by(treat,region) %>% summarize(n=n()) %>% mutate(freq = n / sum(n)) %>%filter(n>100)
tab_region

#================================================================
# Exercise 5
#================================================================

datf       = dat  %>% select(stud_base_educ_want,alladmin_GENDER) %>% rename(educ=stud_base_educ_want,gender=alladmin_GENDER) %>% 
                      filter(!is.na(educ)&gender!="") %>% mutate(educ=as.factor(educ))

levels(datf$educ) = c("JHS","TVT","SHS","NTT","Polytechnic","University")

plt = ggplot(datf,aes(educ,fill=gender,color=gender)) + geom_bar(position="dodge")
plt

#================================================================
# Exercise 6
#================================================================

listnames[grep("mychoice|pgm",listnames,ignore.case = T)]
ln = 

datp           = dat  %>% select(id,paste0("stud_base_choice_",1:4,"_pgm")) %>% filter_all(any_vars(!is.na(.)))
names(datp)    = paste("pgm",1:4)

datp_melt      = melt(dat,id.vars="id")
names(datp_melt)

tab_gender = datp_melt %>% group_by(treat,gender) %>% summarize(n=n()) %>% mutate(freq = n / sum(n)) %>%filter(n>100)
tab_gender

# part 3
dats         = dat  %>% select(stud_base_choice_1_bece,stud_base_choice_2_bece,stud_base_choice_3_bece,stud_base_choice_4_bece) %>% filter_all(any_vars(!is.na(.)))
dat_melt      = melt(dats)
levels(dat_melt$variable) = paste0("choice",1:4)

dtw = dat_melt %>%  group_by(variable) %>% summarize(q25 = quantile(value, probs = 0.25,na.rm=T),
                                                     mean_value  = mean(value,na.rm=T),
                                                     sd_value  = sd(value,na.rm=T),
                                                     q75 = quantile(value, probs = 0.75,na.rm=T))



listnames[grep("mychoice|bece",listnames,ignore.case = T)]
dats         = dat  %>% select(stud_base_choice_1_bece,stud_base_choice_2_bece,stud_base_choice_3_bece,stud_base_choice_4_bece) %>% filter_all(any_vars(!is.na(.)))
dat_melt      = melt(dats)
levels(dat_melt$variable) = paste0("choice",1:4)

dtw = dat_melt %>%  group_by(variable) %>% summarize(q25 = quantile(value, probs = 0.25,na.rm=T),
                                                                    mean_value  = mean(value,na.rm=T),
                                                                      sd_value  = sd(value,na.rm=T),
                                                     q75 = quantile(value, probs = 0.75,na.rm=T))


