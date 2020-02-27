#=================================
# library
#=================================
library(ggplot2)
library(foreign)


#=======================
# Part 1 
#=======================

crra = function(c,theta)
{
  # tolerance of 0.01
  ifelse(abs(theta-1)<0.01,log(c),c^(1-theta)/(1-theta))
}

# create a grid and evaluate the function
grid   = seq(-2,2,0.0001)
values = crra(10,grid)
qplot(grid,values)


#=======================
# Part 2 
#=======================

choice <- rbind(c(48,48,40,64),c(40,64,32,80),c(32,80,24,96),c(24,96,16,112),c(16,112,8,120),c(48,48,42,66),c(42,66,36,84),c(36,84,30,102),c(30,102,24,120),c(24,120,16,128),c(48,48,38,62),c(38,62,28,76),c(28,76,18,90),c(18,90,8,104),c(8,104,0,112),c(42,42,36,60),c(36,60,30,78),c(30,78,24,96),c(24,96,18,114),c(18,114,10,122),c(54,54,44,68),c(44,68,34,82),c(34,82,24,96),c(24,96,14,110),c(14,110,6,118))

# this function evaluates the difference in expected utility between the two lotteries
expected = function(id,x)
{
  aa=0.5*crra(choice[id,1],x) + 0.5*crra(choice[id,2],x)
  bb=0.5*crra(choice[id,3],x) + 0.5*crra(choice[id,4],x)
  return(cbind(aa,bb))
}

# this function evaluates the difference in expected utility between the two lotteries
findd = function(id,x)
{
  aa=0.5*crra(choice[id,1],x) + 0.5*crra(choice[id,2],x)
  bb=0.5*crra(choice[id,3],x) + 0.5*crra(choice[id,4],x)
  return(aa-bb)
}

bisect = function(fun, a, b, n = 1000, tol = 1e-7, ...) {
  # If the signs of the function at the evaluated points, a and b, stop the function and return message.
  f = function(x)fun(x,...)
  if ((f(a)*f(b) > 0)) {
    stop('signs of f(a) and f(b) do not differ')
  }
  
  for (i in 1:n) {
    c <- (a + b) / 2 # Calculate midpoint
    
    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the 
    # function and return the root.
    if ((f(c) == 0) || ((b - a) / 2) < tol) {
      return(c)
    }
    
    # If another iteration is required, 
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
    ifelse(sign(f(c)) == sign(f(a)), 
           a <- c,
           b <- c)
  }
  # If the max number of iterations is reached and no root has been found, 
  # return message and end function.
  print('Too many iterations')
}

bisect(findd,-6,6,id=1)
bisect(findd,-6,6,id=2)
bisect(findd,-6,6,id=3)
bisect(findd,-6,6,id=4)

#=======================
# Part 3
#=======================

# likelihood 
# initial weath w
w = 20; # initial wealth
nc = 4; # 4 choices
funlike = function(ydata,param)
{
  theta = param[1];
  sigma = param[2];
  pr   = NULL;
  for (iX in 1:nrow(choice))
  {
  expU    = crra(w+choice[,iX],rep(theta,nc))
  pr[iX]  = pnorm((expU[3]+expU[4])/2-(expU[1]+expU[2])/2)
  }
  pr[pr<0.00001] = 0.00001;
  pr[pr>0.99999] = 0.99999;
  logLik = ydata * log(pr) + (1-ydata)*log(1-pr);
  return(sum(logLik));  
}


dat_choices = read.dta("/home/ms486/Dropbox/Teaching/2020/Methods/Assignment/A4/dat_choices.dta")
names(dat_choices)




#=======================
# Part 4
#=======================

values = expand.grid(seq(-5,5,0.01),seq(-5,5,0.01))
values[1:5,]

beale = function(param)
{
  x = param[1]
  y = param[2]
  out = baba;
  return(out)
}

output = apply(values,1,beale);
mV = values[which.max(output),]

# compute the gradient
gradfun = function(fun,xVec)
{
  eps = 0.0001;
  out = NULL
  for (iX in 1:length(xVec))
  {
    z1      = z2 = xVec;
    z1[iX]  = z1[iX] + eps;
    z2[iX]  = z2[iX] - eps;
    out[iX] = fun(z1) - fun(z2)
  }
  return(out/(2*eps));
}

dbeale = function(param)
{
  for (ix in length(param))
  {
  }
}

dbeale(mV)


#===========

# rosenbrock banana function
objfun = function(xVec)
{
  (1- xVec[1])^2 + 5*(xVec[2]-xVec[1]^2)^2
}


objfun(c(0,0))
objfun(c(1,1))

optim(c(0,0),objfun)

# compute the gradient
gradfun = function(fun,xVec)
{
  eps = 0.0001;
  out = NULL
  for (iX in 1:length(xVec))
  {
    z1      = z2 = xVec;
    z1[iX]  = z1[iX] + eps;
    z2[iX]  = z2[iX] - eps;
    out[iX] = fun(z1) - fun(z2)
  }
  return(out/(2*eps));
}

gradfun(c(0,0))
gradfun(c(1,0))

# custom programmed steepest descent
steep = function(x0,tol=0.00001, maxIter = 1000)
{
  diff = 1    # initialize the difference 
  iter = 1    # initialize the iteration
  aVec = c(0.0001, 0.001, 0.01, 0.1, 1);
  while(diff<tol & iter<maxIter)
  {
    fX = objfun(x0);
    gX = gradfun(x0);
    # find the optimal scaling parameter
    fPrime = NULL
    xPrime = NULL
    for (iX in 1:length(aVec))
    {
      
    }
  }
}


























#========================
# part 8
#========================
choice[1:10,]
gridr = seq(0.01,2,by=0.01)
nr    = length(gridr)
nc    = nrow(choice)
wi    = 20;
# 8.1 
# probability matrix
prob = mat.or.vec(nc,nr)
for (iG in 1:nr)
{
  for (iC in 1:nc)
  {
  dif       = crra(20+choice[iC,],rep(gridr[iG],4))
  prob[iC,iG] = pnorm(sum(dif[1:2])/2 - sum(dif[3:4]/2),sd=0.5)
  }
}

  
  pdf("output.pdf")
  for (i in 1:nc)
  {
  print(qplot(gridr,1-prob[i,]))
  }
  dev.off()
  
  


