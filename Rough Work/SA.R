xxx=read.table("D:/2023/SO/ass/newwinedata.txt",sep=" ")
evaluate_x<-function(x)
{ 
  my_obj <- xxx$Price
  eva=x%*%my_obj

  if(x%*%xxx$pH<3.52|x%*%xxx$pH>3.55|x%*%xxx$Abrasiveness<8
        |x%*%xxx$Hardness>10|x%*%xxx$Dryness<35|x%*%xxx$Dryness>38
        |x%*%xxx$Bitterness<7.7|x%*%xxx$H<17|x%*%xxx$H>18)
  {
    eva=1000
  }
  return(eva)
}

get_initial_x<-function()
{
  x=seq(0,1,length.out=100)
  cur_x=c(0.25,0.25,0.25,0.25)
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


#mm=get_initial_x()
#evaluate_x(mm)

perturb_x <- function(cur_x)
{
  sam=sample(1:4,2,replace = F)
  value=runif(1,-min(cur_x[sam[1]],cur_x[sam[2]])/2,min(cur_x[sam[1]],cur_x[sam[2]])/2)
  cur_x[sam[1]]=cur_x[sam[1]]+value
  cur_x[sam[2]]=cur_x[sam[2]]-value
  return(cur_x)
}
#perturb_x(c(0.25,0.25,0.25,0.25))

start_temp <- 1
temp_factor <- 0.995
all_fx=c()
all_x=c()
cur_x=get_initial_x()
#cur_x=get_initial_x()
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

plot(all_fx,type="l", ylab = "f(x)")
min(all_fx)