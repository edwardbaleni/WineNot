library(Rglpk)

xxx=read.table("D:/2023/SO/ass/newwinedata3.txt",sep=" ")

#LP
#objective is to minimise total cost
my_obj1 <- xxx$Price

my_mat = rbind(xxx$pH,xxx$pH,xxx$Abrasiveness,
               xxx$Hardness,xxx$Dryness,xxx$Dryness,
               xxx$Bitterness,xxx$H,xxx$H,
               c(1,1,1,1,0,0,0),
               c(0,0,0,0,1,1,1))


my_dir=c(">","<",">","<",">","<",">",">","<","==","<=")
my_rhs=c(3.52,3.55,8,10,35,38,7.7,17,18,1,5)

my_types<-c("C","C","C","C","I","I","I")

LP=Rglpk_solve_LP(obj=my_obj1,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=F)

#Simulated Annealing
#set seed to make result reproducible
set.seed(1)
#create a matrix to store all solution by different setting of Simulated Annealing
solu_SA=matrix(0,nrow=7,ncol=5)
#evaluation function
evaluate_x<-function(x)
{ 
  my_obj <- xxx$Price
  eva=x%*%my_obj
  
  if(x%*%xxx$pH<3.52|x%*%xxx$pH>3.55|x%*%xxx$Abrasiveness<8
     |x%*%xxx$Hardness>10|x%*%xxx$Dryness<35|x%*%xxx$Dryness>38
     |x%*%xxx$Bitterness<7.7|x%*%xxx$H<17|x%*%xxx$H>18|sum(x[5:7])>5)
  {
    eva=1000
  }
  return(eva)
}
#function to give a feasible inital solution in order to find optimal
get_initial_x<-function()
{
  x=seq(0,1,length.out=100)
  cur_x=c(0.25,0.25,0.25,0.25,0,0,0)
  found=F
  while(found==F)
  {
    sam=sample(1:4,4,replace = F)
    for(i in 1:100)
    {
      for(j in 1:100)
      {
        for(k in 1:100)
        {
          cur_x[sam[1]]=x[i]
          cur_x[sam[2]]=x[j]
          cur_x[sam[3]]=x[k]
          cur_x[sam[4]]=1-cur_x[sam[1]]-cur_x[sam[2]]-cur_x[sam[3]]
          if(cur_x%*%xxx$pH>3.52&cur_x%*%xxx$pH<3.55&cur_x%*%xxx$Abrasiveness>8
             &cur_x%*%xxx$Hardness<10&cur_x%*%xxx$Dryness>35&cur_x%*%xxx$Dryness<38
             &cur_x%*%xxx$Bitterness>7.7&cur_x%*%xxx$H>17&cur_x%*%xxx$H<18
             &cur_x[sam[4]]>=0)
          {
            cur_xt=cur_x
            found=T
          }
        }
      }
    }
  }
  return(cur_xt)
}

# function to change some value of the solution vector 
# either wine proportion or number of food additive will be change 
perturb_x <- function(cur_x)
{
  sam=sample(1:2,1,replace = F)
  if(sam==1)
  {
    sam=sample(1:4,2,replace = F)
    value=runif(1,-min(cur_x[sam[1]],cur_x[sam[2]])/2,min(cur_x[sam[1]],cur_x[sam[2]])/2)
    cur_x[sam[1]]=cur_x[sam[1]]+value
    cur_x[sam[2]]=cur_x[sam[2]]-value
  }else
  {
    sam=sample(5:7,1,replace = F)
    cur_x[sam]=sample(c(max(cur_x[sam]-1,0),cur_x[sam]+1),1,replace = F)
  }
  return(cur_x)
}

#Geometric with temperature factor=0.995
start_temp <- 1
temp_factor <- 0.995
all_fx=c()
all_x=c()

initx=get_initial_x()
cur_x=initx
cur_fx=evaluate_x(cur_x)

for(i in 1:10000){
  # generate a candidate solution
  prop_x <- perturb_x(cur_x)
  # evaluate the candidate solution
  prop_fx <- evaluate_x(prop_x)
  # calculate the probability of accepting the candidate
  anneal_temp <- start_temp * temp_factor ^ i
  accept_prob <- exp(-(prop_fx - cur_fx) / anneal_temp)
  # accept or reject the candidate
  if(prop_fx < cur_fx){
    cur_x <- prop_x
    cur_fx <- prop_fx
  }
  else{ if(runif(1) < accept_prob){
    cur_x <- prop_x
    cur_fx <- prop_fx
  }}
  
  # store all results
  all_fx <- c(all_fx, cur_fx)
  all_x <- c(all_x,cur_x)
}

all_fx_G1=all_fx
solu_SA[,1]=all_x[((which(all_fx==min(all_fx))[1]-1)*(7)+1):(which(all_fx==min(all_fx))[1]*(7))]


#Geometric with temperature factor =0.95
start_temp <- 0.1
temp_factor <- 0.95
all_fx=c()
all_x=c()
cur_x=initx
cur_fx=evaluate_x(cur_x)

for(i in 1:10000){
  # generate a candidate solution
  prop_x <- perturb_x(cur_x)
  # evaluate the candidate solution
  prop_fx <- evaluate_x(prop_x)
  # calculate the probability of accepting the candidate
  anneal_temp <- start_temp * temp_factor ^ i
  accept_prob <- exp(-(prop_fx - cur_fx) / anneal_temp)
  # accept or reject the candidate
  if(prop_fx < cur_fx){
    cur_x <- prop_x
    cur_fx <- prop_fx
  }
  else{ if(runif(1) < accept_prob){
    cur_x <- prop_x
    cur_fx <- prop_fx
  }}
  
  # store all results
  all_fx <- c(all_fx, cur_fx)
  all_x <- c(all_x,cur_x)
}

all_fx_G5=all_fx
solu_SA[,2]=all_x[((which(all_fx==min(all_fx))[1]-1)*(7)+1):(which(all_fx==min(all_fx))[1]*(7))]

#Logarithmic with starting temp =1 temp_factor=0.995
start_temp <- 1
temp_factor <- 0.995
all_fx=c()
all_x=c()
cur_x=initx
cur_fx=evaluate_x(cur_x)

for(i in 1:10000){
  # generate a candidate solution
  prop_x <- perturb_x(cur_x)
  # evaluate the candidate solution
  prop_fx <- evaluate_x(prop_x)
  # calculate the probability of accepting the candidate
  anneal_temp <- start_temp /(1+temp_factor*log(1+i))
  accept_prob <- exp(-(prop_fx - cur_fx) / anneal_temp)
  # accept or reject the candidate
  if(prop_fx < cur_fx){
    cur_x <- prop_x
    cur_fx <- prop_fx
  }
  else{ if(runif(1) < accept_prob){
    cur_x <- prop_x
    cur_fx <- prop_fx
  }}
  
  # store all results
  all_fx <- c(all_fx, cur_fx)
  all_x <- c(all_x,cur_x)
}

all_fx_L1=all_fx
solu_SA[,3]=all_x[((which(all_fx==min(all_fx))[1]-1)*(7)+1):(which(all_fx==min(all_fx))[1]*(7))]

#Logarithmic with starting temp =0.5,temp_factor=0.995
start_temp <- 0.5
temp_factor <- 0.995
all_fx=c()
all_x=c()
cur_x=initx
cur_fx=evaluate_x(cur_x)

for(i in 1:10000){
  # generate a candidate solution
  prop_x <- perturb_x(cur_x)
  # evaluate the candidate solution
  prop_fx <- evaluate_x(prop_x)
  # calculate the probability of accepting the candidate
  anneal_temp <- start_temp /(1+temp_factor*log(1+i))
  accept_prob <- exp(-(prop_fx - cur_fx) / anneal_temp)
  # accept or reject the candidate
  if(prop_fx < cur_fx){
    cur_x <- prop_x
    cur_fx <- prop_fx
  }
  else{ if(runif(1) < accept_prob){
    cur_x <- prop_x
    cur_fx <- prop_fx
  }}
  
  # store all results
  all_fx <- c(all_fx, cur_fx)
  all_x <- c(all_x,cur_x)
}
all_fx_L5=all_fx
solu_SA[,4]=all_x[((which(all_fx==min(all_fx))[1]-1)*(7)+1):(which(all_fx==min(all_fx))[1]*(7))]

#Logarithmic with starting temp =1 tf=0.8
start_temp <- 1
temp_factor <- 0.8
all_fx=c()
all_x=c()
cur_x=initx
cur_fx=evaluate_x(cur_x)

for(i in 1:10000){
  # generate a candidate solution
  prop_x <- perturb_x(cur_x)
  # evaluate the candidate solution
  prop_fx <- evaluate_x(prop_x)
  # calculate the probability of accepting the candidate
  anneal_temp <- start_temp /(1+temp_factor*log(1+i))
  accept_prob <- exp(-(prop_fx - cur_fx) / anneal_temp)
  # accept or reject the candidate
  if(prop_fx < cur_fx){
    cur_x <- prop_x
    cur_fx <- prop_fx
  }
  else{ if(runif(1) < accept_prob){
    cur_x <- prop_x
    cur_fx <- prop_fx
  }}
  
  # store all results
  all_fx <- c(all_fx, cur_fx)
  all_x <- c(all_x,cur_x)
}

all_fx_L18=all_fx
solu_SA[,5]=all_x[((which(all_fx==min(all_fx))[1]-1)*(7)+1):(which(all_fx==min(all_fx))[1]*(7))]

plot(all_fx_G1,type="l", ylab = "f(x)",lwd=2)
points(all_fx_G5,type="l",col="green",lwd=2)
points(all_fx_L1,type="l",col="red",lwd=2)
points(all_fx_L5,type="l",col="blue",lwd=2)
points(all_fx_L18,type="l",col="brown",lwd=2)
legend("topright", legend=c("Geometric t0=1 tf=0.995","Geometric t0=1 tf=0.95",
                            "Logarithmic T0=1 tf=0.995","Logarithmic T0=0.5 tf=0.995",
                            "Logarithmic T0=1 tf=0.8"),
       col=c("black","green","red", "blue","brown"), lty=1, cex=0.8,lwd=2)



#MOGP
#max each goal to find range of value
my_obj1 <- xxx$Price
my_obj2 <- xxx$Alcohol
my_obj3 <- xxx$C
my_obj4 <- xxx$Sugar
my_obj5 <- xxx$Tannins
my_obj6 <- xxx$Anthocyanins

s1=Rglpk_solve_LP(obj=my_obj1,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=T)
s2=Rglpk_solve_LP(obj=my_obj2,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=T)
s3=Rglpk_solve_LP(obj=my_obj3,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=T)
s4=Rglpk_solve_LP(obj=my_obj4,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=T)
s5=Rglpk_solve_LP(obj=my_obj5,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=T)
s6=Rglpk_solve_LP(obj=my_obj6,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=T)

ss=rbind(s1$solution,s2$solution)
ss=rbind(ss,s3$solution)
ss=rbind(ss,s4$solution)
ss=rbind(ss,s5$solution)
ss=rbind(ss,s6$solution)
#the matrix can be use calculate range of value for each variables
mmm=ss%*%cbind(xxx$Price,xxx$Alcohol,xxx$C,xxx$Sugar,xxx$Tannins,xxx$Anthocyanins)

#use calculated weight to perform MOGP by Rglpk function
#Archimedean approach
my_obj1 <- c(rep(0,7),0.08,1.94,1.94,62.5,62.5,0.42,0.42,0.02,0.02,2.80,2.80)

my_mat = matrix(c(c(xxx$pH,rep(0,11)),
                  c(xxx$pH,rep(0,11)),
                  c(xxx$Abrasiveness,rep(0,11)),
                  c(xxx$Hardness,rep(0,11)),
                  c(xxx$Dryness,rep(0,11)),
                  c(xxx$Dryness,rep(0,11)),
                  c(xxx$Bitterness,rep(0,11)),
                  c(xxx$H,rep(0,11)),
                  c(xxx$H,rep(0,11)),
                  c(1,1,1,1,0,0,0,rep(0,11)),
                  c(0,0,0,0,1,1,1,rep(0,11)),
                  c(xxx$Price,-1,rep(0,10)),
                  c(xxx$Alcohol,0,-1,1,rep(0,8)),
                  c(xxx$C,rep(0,3),-1,1,rep(0,6)),
                  c(xxx$Sugar,rep(0,5),-1,1,rep(0,4)),
                  c(xxx$Tannins,rep(0,7),-1,1,rep(0,2)),
                  c(xxx$Anthocyanins,rep(0,9),-1,1)),ncol=18,byrow=T)

my_dir=c(">","<",">","<",">","<",">",">","<","==","<=","<=","==","==","==","==","==")
my_rhs=c(3.52,3.55,8,10,35,38,7.7,17,18,1,5,0,15,59,2300,1992.2,533.27)


my_types<-c("C","C","C","C","I","I","I","C","C","C","C","C","C","C","C","C","C","C")

ss1=Rglpk_solve_LP(obj=my_obj1,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=F)

#Preemptive approach
#first step
my_obj2 <- c(rep(0,7),0.08,rep(0,10))
my_mat2 = matrix(c(c(xxx$pH,rep(0,11)),
                   c(xxx$pH,rep(0,11)),
                   c(xxx$Abrasiveness,rep(0,11)),
                   c(xxx$Hardness,rep(0,11)),
                   c(xxx$Dryness,rep(0,11)),
                   c(xxx$Dryness,rep(0,11)),
                   c(xxx$Bitterness,rep(0,11)),
                   c(xxx$H,rep(0,11)),
                   c(xxx$H,rep(0,11)),
                   c(1,1,1,1,0,0,0,rep(0,11)),
                   c(0,0,0,0,1,1,1,rep(0,11)),
                   c(xxx$Price,-1,rep(0,10)),
                   c(xxx$Alcohol,0,-1,1,rep(0,8)),
                   c(xxx$C,rep(0,3),-1,1,rep(0,6)),
                   c(xxx$Sugar,rep(0,5),-1,1,rep(0,4)),
                   c(xxx$Tannins,rep(0,7),-1,1,rep(0,2)),
                   c(xxx$Anthocyanins,rep(0,9),-1,1)),ncol=18,byrow=T)

ss2=Rglpk_solve_LP(obj=my_obj2,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=F)

#second step
#use optimal value from last step to constrain
my_obj3 <- c(rep(0,10),62.5,62.5,0,0,0,0,2.80,2.80)
my_mat3 = matrix(c(c(xxx$pH,rep(0,11)),
                   c(xxx$pH,rep(0,11)),
                   c(xxx$Abrasiveness,rep(0,11)),
                   c(xxx$Hardness,rep(0,11)),
                   c(xxx$Dryness,rep(0,11)),
                   c(xxx$Dryness,rep(0,11)),
                   c(xxx$Bitterness,rep(0,11)),
                   c(xxx$H,rep(0,11)),
                   c(xxx$H,rep(0,11)),
                   c(1,1,1,1,0,0,0,rep(0,11)),
                   c(0,0,0,0,1,1,1,rep(0,11)),
                   c(xxx$Price,-1,rep(0,10)),
                   c(xxx$Alcohol,0,-1,1,rep(0,8)),
                   c(xxx$C,rep(0,3),-1,1,rep(0,6)),
                   c(xxx$Sugar,rep(0,5),-1,1,rep(0,4)),
                   c(xxx$Tannins,rep(0,7),-1,1,rep(0,2)),
                   c(xxx$Anthocyanins,rep(0,9),-1,1),
                   c(rep(0,7),0.08,rep(0,10))),ncol=18,byrow=T)
my_dir3=c(">","<",">","<",">","<",">",">","<","==","<=","<=","==","==","==","==","==","<=")
my_rhs3=c(3.52,3.55,8,10,35,38,7.7,17,18,1,5,0,15,59,2300,1992.2,533.27,ss2$optimum)
ss3=Rglpk_solve_LP(obj=my_obj3,mat=my_mat3,dir=my_dir3,rhs=my_rhs3,types=my_types,max=F)

#third step
#use optimal value from previous step to constrain
my_obj4 <- c(rep(0,12),0.42,0.42,rep(0,4))
my_mat4 = matrix(c(c(xxx$pH,rep(0,11)),
                   c(xxx$pH,rep(0,11)),
                   c(xxx$Abrasiveness,rep(0,11)),
                   c(xxx$Hardness,rep(0,11)),
                   c(xxx$Dryness,rep(0,11)),
                   c(xxx$Dryness,rep(0,11)),
                   c(xxx$Bitterness,rep(0,11)),
                   c(xxx$H,rep(0,11)),
                   c(xxx$H,rep(0,11)),
                   c(1,1,1,1,0,0,0,rep(0,11)),
                   c(0,0,0,0,1,1,1,rep(0,11)),
                   c(xxx$Price,-1,rep(0,10)),
                   c(xxx$Alcohol,0,-1,1,rep(0,8)),
                   c(xxx$C,rep(0,3),-1,1,rep(0,6)),
                   c(xxx$Sugar,rep(0,5),-1,1,rep(0,4)),
                   c(xxx$Tannins,rep(0,7),-1,1,rep(0,2)),
                   c(xxx$Anthocyanins,rep(0,9),-1,1),
                   c(rep(0,7),0.08,rep(0,10)),
                   c(rep(0,10),62.5,62.5,0,0,0,0,2.80,2.80)),ncol=18,byrow=T)
my_dir4=c(">","<",">","<",">","<",">",">","<","==","<=","<=","==","==","==","==","==","<=","<=")
my_rhs4=c(3.52,3.55,8,10,35,38,7.7,17,18,1,5,0,15,59,2300,1992.2,533.27,ss2$optimum,ss3$optimum)
ss4=Rglpk_solve_LP(obj=my_obj4,mat=my_mat4,dir=my_dir4,rhs=my_rhs4,types=my_types,max=F)

#last step
#use optimal value from previous step to constrain
my_obj5 <- c(rep(0,8),1.94,1.94,62.5,62.5,rep(0,6))
my_mat5 = matrix(c(c(xxx$pH,rep(0,11)),
                   c(xxx$pH,rep(0,11)),
                   c(xxx$Abrasiveness,rep(0,11)),
                   c(xxx$Hardness,rep(0,11)),
                   c(xxx$Dryness,rep(0,11)),
                   c(xxx$Dryness,rep(0,11)),
                   c(xxx$Bitterness,rep(0,11)),
                   c(xxx$H,rep(0,11)),
                   c(xxx$H,rep(0,11)),
                   c(1,1,1,1,0,0,0,rep(0,11)),
                   c(0,0,0,0,1,1,1,rep(0,11)),
                   c(xxx$Price,-1,rep(0,10)),
                   c(xxx$Alcohol,0,-1,1,rep(0,8)),
                   c(xxx$C,rep(0,3),-1,1,rep(0,6)),
                   c(xxx$Sugar,rep(0,5),-1,1,rep(0,4)),
                   c(xxx$Tannins,rep(0,7),-1,1,rep(0,2)),
                   c(xxx$Anthocyanins,rep(0,9),-1,1),
                   c(rep(0,7),0.08,rep(0,10)),
                   c(rep(0,10),62.5,62.5,0,0,0,0,2.80,2.80),
                   c(rep(0,12),0.42,0.42,rep(0,4))),ncol=18,byrow=T)
my_dir5=c(">","<",">","<",">","<",">",">","<","==","<=","<=","==","==","==","==","==","<=","<=","<=")
my_rhs5=c(3.52,3.55,8,10,35,38,7.7,17,18,1,5,0,15,59,2300,1992.2,533.27,ss2$optimum,ss3$optimum,ss4$optimum)
ss5=Rglpk_solve_LP(obj=my_obj5,mat=my_mat5,dir=my_dir5,rhs=my_rhs5,types=my_types,max=F)
