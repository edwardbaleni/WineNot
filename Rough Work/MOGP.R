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
my_rhs=c(3.52,3.55,8,10,35,38,7.7,17,18,1,5,0,15,59,2300,2458.85,414.91)


my_types<-c("C","C","C","C","I","I","I","C","C","C","C","C","C","C","C","C","C","C")

ss1=Rglpk_solve_LP(obj=my_obj1,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=F)
round(ss1$solution,2)
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
my_rhs3=c(3.52,3.55,8,10,35,38,7.7,17,18,1,5,0,15,59,2300,2458.85,414.91,ss2$optimum)
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
my_rhs4=c(3.52,3.55,8,10,35,38,7.7,17,18,1,5,0,15,59,2300,2458.85,414.91,ss2$optimum,ss3$optimum)
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
my_rhs5=c(3.52,3.55,8,10,35,38,7.7,17,18,1,5,0,15,59,2300,2458.85,414.91,ss2$optimum,ss3$optimum,ss4$optimum)
ss5=Rglpk_solve_LP(obj=my_obj5,mat=my_mat5,dir=my_dir5,rhs=my_rhs5,types=my_types,max=F)
round(ss1$solution,2)