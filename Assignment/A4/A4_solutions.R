  #=================================
  # library
  #=================================
  library(ggplot2)
  library(foreign)
  library(nloptr)
  library(AER)
  
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
  
  choice = rbind(c(48,48,40,64),c(40,64,32,80),c(32,80,24,96),c(24,96,16,112),c(16,112,8,120),c(48,48,42,66),c(42,66,36,84),c(36,84,30,102),c(30,102,24,120),c(24,120,16,128),c(48,48,38,62),c(38,62,28,76),c(28,76,18,90),c(18,90,8,104),c(8,104,0,112),c(42,42,36,60),c(36,60,30,78),c(30,78,24,96),c(24,96,18,114),c(18,114,10,122),c(54,54,44,68),c(44,68,34,82),c(34,82,24,96),c(24,96,14,110),c(14,110,6,118))
  nq     = nrow(choice); # number of questions
  
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
  
  #============================================
  # alternative bissection function
  #============================================
  bisect2 =function (f, a, b, eps = 1e-05,...) 
  {
    a1 = a
    b1 = b
    if (f(a1,...) * f(b1,...) < 0) {
      while(((b1-a1)/2)>eps){
        x = (a1 + b1)/2
        if (f(a1,...) * f(x,...) < 0) 
          b1 = x
        else a1 = x
      }
    }
    return(a1)
  }
  
  # compute the threesholds
  threshold = NULL
  for (iQ in 1:nq)
  {
  threshold[iQ] = bisect(findd,-6,6,id=iQ)
  }
  
  # this function matches the threeshold to the turning point
  rcreat2 = function(ypref,deb,fin,seuil)
  {
    set = apply(ypref[,deb:fin],1,function(x) which(x==1)[1])
    set[is.na(set)] = 0
    summary(set)
    dd   = round(seuil[deb:fin],2)
    dd    = as.character(dd)
    l1    = c("Inf",dd[1:5])
    l2    = c(dd[1:5],-Inf)
    seu  = paste(l2,l1,sep=":")
    set = as.factor(set)
    table(set)
    levels(set)=seu
    dat = data.frame(set)
    return(dat)
  }
  
  if (dir.exists("/home/ms486")) {
    dat_choices = read.dta("/home/ms486/Dropbox/Teaching/2020/Methods/Assignment/A4/dat_choices.dta")
  }else{
    dat_choices = read.dta("/Users/ms486/Dropbox/Teaching/2020/Methods/Assignment/A4/dat_choices.dta")
  }
  
  datr1 = rcreat2(dat_choices,1,5,threshold)
  datr2 = rcreat2(dat_choices,6,10,threshold)
  datr3 = rcreat2(dat_choices,11,15,threshold)
  datr4 = rcreat2(dat_choices,16,20,threshold)
  datr5 = rcreat2(dat_choices,21,25,threshold)
  
  grapher4 = function(dat,titl)
  {
    m = ggplot(dat, aes(x=set))
    m = m + geom_bar(aes(y = (..count..)/sum(..count..)),colour="blue",fill="grey")
    m = m + xlab("Implied relative risk factor") + ylab("Frequencies") + labs(title=titl)
    m = m+ theme(plot.title = element_text(lineheight=.8))
    m = m + theme(axis.title = element_text(size = rel(1),colour="black"),axis.text = element_text(size = rel(0.80))) 
    return(m)  
  }
  
  mr1=grapher4(datr1,"Lottery 1")
  mr2=grapher4(datr2,"Lottery 2")
  mr3=grapher4(datr3,"Lottery 3")
  mr4=grapher4(datr4,"Lottery 4")
  mr5=grapher4(datr5,"Lottery 5")

  #==============================
  # all graphiques together
  #==============================

  # This function names all possibles outcomes
  ncreat3 = function(deb,fin,seuil)
  {
    # restrict to two digits
    dd   = round(seuil[deb:fin],2)
    dd    = as.character(dd)
    return(paste(c("Inf",dd[1:5]),c(dd[1:5],-Inf),sep=":"))  
  }
  
  # this function identifies the turning point
  rcreat3 = function(ypref,deb,fin,seuil)
  {
    set = apply(ypref[,deb:fin],1,function(x) which(x==1)[1])
    set[is.na(set)] = 0
    summary(set)
    return(set)
  }
  
  datr1m = rcreat3(dat_choices,1,5,threshold)
  datr2m = 6+ rcreat3(dat_choices,6,10,threshold)
  datr3m = 12+rcreat3(dat_choices,6,10,threshold)
  datr4m = 18+rcreat3(dat_choices,6,10,threshold)
  datr5m = 24+rcreat3(dat_choices,6,10,threshold)
  
  n1m    = ncreat3(1,5,threshold)
  n2m    = ncreat3(6,10,threshold)
  n3m    = ncreat3(11,15,threshold)
  n4m    = ncreat3(16,20,threshold)
  n5m    = ncreat3(21,25,threshold)
  
  # associate a name to each individual
  x  = as.factor(c(datr1m,datr2m,datr3m,datr4m,datr5m))
  levels(x) = c(n1m,n2m,n3m,n4m,n5m)
  
  table(x)
  
  datrr = data.frame(x=x,Lotteries = rep(paste0("Lottery",1:5),each=1248))
  
  m = ggplot(datrr, aes(x, fill=Lotteries)) + geom_bar(position="dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  m = m + xlab("Implied relative risk factor") + ylab("Count") 
  m = m+ theme(plot.title = element_text(lineheight=.8)) + theme(legend.position="bottom")
  m = m + theme(axis.title = element_text(size = rel(1.2),colour="black"),axis.text = element_text(face="bold",size = rel(1))) 
  m
  
  #=======================
  # Part 3
  #=======================
  
  # likelihood 
  # initial weath w
  w = 20; # initial wealth
  nc = 4; # 4 choices
  funlike = function(param,ydata)
  {
    theta = param[1];
    sigma = param[2];
    pr   = NULL;
    for (iX in 1:nrow(choice))
    {
    expU    = crra(w+choice[iX,],rep(theta,nc))
    pr[iX]  = pnorm((expU[3]+expU[4])/2-(expU[1]+expU[2])/2,sd=sigma)
    }
    pr[pr<0.00001] = 0.00001;
    pr[pr>0.99999] = 0.99999;
    logLik = ydata * log(pr) + (1-ydata)*log(1-pr);
    return(-sum(logLik));  
  }
  
  dat_choices = read.dta("/home/ms486/Dropbox/Teaching/2020/Methods/Assignment/A4/dat_choices.dta")
  names(dat_choices)
  
  # test the function
  funlike(c(2,2),dat_choices)

   # grid search
    gridt = cbind(seq(-2,5,0.01),1)
    # individual 900
    out   = sapply(1:length(gridt),function(x)funlike(gridt[x,],ypref[900,]))
    gridt[which.max(out),]

    # individual 115
    out   = sapply(1:length(gridt),function(x)funlike(gridt[x,],ypref[115,]))
    gridt[which.max(out),]
    
  
  #=======================
  # Part 4
  #=======================
  
  values = expand.grid(seq(-5,5,0.01),seq(-5,5,0.01))
  values[1:5,]
  
  beale = function(param)
  {
    x = param[1]
    y = param[2]
    out = (1.5 - x + x*y)^2 + (2.25 - x + x*y^2)^2 + (2.625 - x + x*y^3)^2
    return(as.numeric(out))
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
      out[iX] = fun(as.numeric(z1)) - fun(as.numeric(z2))
    }
    return(out/(2*eps));
  }
    
  gradfun(beale,mV)
    
  #============================================
  # rosenbrock banana function
  #============================================
  
  objfun = function(xVec)
  {
    (1- xVec[1])^2 + 5*(xVec[2]-xVec[1]^2)^2
  }
  

  # testing the function  
  objfun(c(0,0))
  objfun(c(1,1))

  optim(c(0,0),objfun)
  
  # compute the gradient
  
  gradfun(objfun,c(0,0))
  gradfun(objfun,c(1,0))
  
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
  # part 6
  #========================
  
  data("SmokeBan")  
  smokeBan = SmokeBan
  
  summary(dat)  
  yVec = as.numeric(SmokeBan$smoker=="yes")
  table(yVec)
  
  xMat = cbind(SmokeBan$ban=="yes",SmokeBan$age/100,
                 smokeBan$education=="hs drop out",
                 smokeBan$education=="some college",
                 smokeBan$education=="college",
                 smokeBan$education=="master",
                 smokeBan$afam=="yes",
                 smokeBan$hispanic=="yes",
                 smokeBan$gender=="male")
 colnames(xMat)  = c("ban","age","drop-out","some-college","college","master","black","hispanic","male")
  
  # likelihood of a probit
  probit = function(beta,xMat,yVec)
  {
    Xbeta             = cbind(1,xMat)%*%beta  # calculate Xbeta 
    prob              = pnorm(Xbeta);         # calculate probabilities
    prob[prob<0.0001] = 0.0001                # make sure probabilities do not hit bounds
    prob[prob>0.9999] = 0.9999
    #print(summary(prob))
    loglike           = yVec*log(prob) + (1-yVec)*log(1-prob);  
    return(-sum(loglike))
  }

  
  fit = function(beta,xMat,yVec)
  {
    Xbeta             = cbind(1,xMat)%*%beta  # calculate Xbeta 
    prob              = pnorm(Xbeta);         # calculate probabilities
    Ysim              = as.numeric(prob>0.5)
    return(Ysim)
  }
  
  npar = ncol(xMat) + 1;
  probit(rnorm(npar),xMat,yVec)

  # optimization step 
  res = optim(runif(npar,0,1), probit, method = "BFGS", xMat = xMat,yVec=yVec,
               control = list(maxit = 30000, trace = TRUE,
                              REPORT = 1),hessian=TRUE)  
  res$par

  ff = fit(res$par,xMat,yVec)
  table(ff,yVec)
      
  
  
  #std     = sqrt(diag(solve(-res$hessian)))
  std     = sqrt(diag(solve(res$hessian)))
  
  tab = cbind(res$par,std)
  tab
  
  
#================================================
# part 7
# documenting the non-monotonicity
#================================================
  
funlike(c(2,0.5),dat_choices[1,])

    
  
  
#================================================
# part 8
# documenting the non-monotonicity
#================================================
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
    
#======================================================
# testing the performance of optimization algorithms
#======================================================

funlike(c(2,2),dat_choices[1,])
funlike(c(2,2),dat_choices)

# nloptr is picky about the argument.. 
# just to be safe 
cgrad = function(param,ydata){
    out = nl.grad(param,funlike,ydata=ydata)
    return(out)
  }

lower  = c(-1,0.0001)
upper  = c(10,5)
funlike(c(2.1,0.8),dat_choices[5,])
funlike(res0$solution,dat_choices[5,])


res0   = nloptr(c(2.1,0.8),eval_f=funlike,lb=lower,ub=upper,opts=list("algorithm"="NLOPT_GN_ISRES","print_level"=2,"xtol_rel"=1.0e-10,"maxeval"=10000),ydata=dat_choices[5,])
res1   = nloptr(c(2.1,0.8),eval_f=funlike,lb=lower,ub=upper,opts=list("algorithm"="NLOPT_LN_BOBYQA","print_level"=2,"xtol_rel"=1.0e-10,"maxeval"=1000),ydata=dat_choices[5,])
res2   = nloptr(c(2.1,0.8),eval_f=funlike,lb=lower,ub=upper,opts=list("algorithm"="NLOPT_LN_SBPLX","print_level"=2,"xtol_rel"=1.0e-10,"maxeval"=1000),ydata=dat_choices[5,])
res3    = nloptr(c(2.1,0.8),eval_f=funlike,eval_grad_f=cgrad,lb=lower,ub=upper,opts=list("algorithm"="NLOPT_LD_LBFGS","print_level"=2,"xtol_rel"=1.0e-10,"maxeval"=1000),ydata=dat_choices[5,])

cbind(res0$objective,res1$objective,res2$objective,res3$objective)
cbind(res0$solution,res1$solution,res2$solution,res3$solution)


#========================
# part 8 
# monte carlo study
#========================

tabparam = expand.grid(c(0.5,0.8,1.5,2.8),c(0.1,0.9))
tabparam

set.seed(123)
nq  = 25
# simulation for monte carlo 
sim = function(param)
{
  ysim = NULL
  for (id in 1:nq)
  {
  ut  = crra(20+choice[id,],rep(param[1],4))
  eps = rnorm(2,0,sd=param[2])
  u1  = eps[1] + (ut[1] + ut[2])/2 
  u2  = eps[2] + (ut[3] + ut[4])/2 
  ysim[id] = as.numeric(u1<u2)
  }
  return(ysim)
}

# optimizer minimizes the function "fn" given starting values x0 and unspecified arguments
optimizer = function(x0,fn,lower,upper,...)
{
  res0       = nloptr(x0,eval_f=fn,lb=lower,ub=upper,opts=list("algorithm"="NLOPT_GN_ISRES","xtol_rel"=1.0e-10,"maxeval"=500),...)
  param0     = res0$solution
  res        = list()
  res[[1]]   = nloptr(param0,eval_f=fn,lb=lower,ub=upper,opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=1000),...)
  res[[2]]   = nloptr(param0,eval_f=fn,lb=lower,ub=upper,opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10,"maxeval"=1000),...)
  vall       = c(res[[1]]$objective,res[[2]]$objective)
  ind        = which.min(vall)
  par        = res[[ind]]$solution
  return(par)
}


nsim = 100;
montecarlo = function(param)
{
  out = matrix(0,nsim,2)
  for (iS in 1:nsim)
  {
    #print(iS)
    ysim = sim(param)
    rr   = nloptr(c(2.1,0.8),eval_f=funlike,lb=c(-2,0.01),ub=c(10,10),opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=1000),ydata=ysim)
    out[iS,] = rr$solution
  }
return(out)
}

outmat = mat.or.vec(nrow(tabparam),4)
for (i in 1:nrow(tabparam))
{
  print(i)
  out        = montecarlo(as.numeric(tabparam[i,]))
  outmat[i,] = c(apply(out,2,mean),apply(out,2,sd))
}

tabf = cbind(tabparam,outmat)
colnames(tabf) = c("theta_true","sigma_true","theta_est","sigma_est","theta_est_sd","sigma_est_sd")
tabf
