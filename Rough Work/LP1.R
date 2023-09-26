library(Rglpk)

xxx=read.table("newwinedata.txt",sep=" ")
my_obj <- xxx$Price
my_mat = rbind(xxx$pH,xxx$pH,xxx$Abrasiveness,
               xxx$Hardness,xxx$Dryness,xxx$Dryness,
               xxx$Bitterness,xxx$H,xxx$H,c(1,1,1,1))


my_dir=c(">","<",">","<",">","<",">",">","<","==")
my_rhs=c(3.52,3.55,8,10,35,38,7.7,17,18,1)

my_types<-c("C","C","C","C")
Rglpk_solve_LP(obj=my_obj,mat=my_mat,dir=my_dir,rhs=my_rhs,types=my_types,max=F)
