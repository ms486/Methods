#=========================================================================
# A1
#=========================================================================

#=========================================================================
# Exercise 1: Introduction
#=========================================================================

# 2. list of packages then install
list_packages = c("Hmisc","gdata","boot","MASS","moments","snow","mvtnorm")
install.packages(list_packages)

# 3. set a working directory
setwd("C:/Users/ms486/Dropbox/Teaching/2020/Methods/Assignment/A1")

# 4. Content of the directory and environment
dir()
ls()

# 5. is 678 a multiple of 9
678%%9==0


#=========================================================================
# Exercise 2: Titanic
#=========================================================================

#1 - using sum
#a 
sum(Titanic)
sum(Titanic[,,"Adult",])
sum(Titanic[4,,,])
sum(Titanic["3rd",,"Child",])


# 2 
prop.table(Titanic["1st","Male","Adult",])

