
source("Global_Parameters.r")
source("MainFunctions.R")

#Simulation Setup

iterations = 10
.GlobalEnv$pop_size <- 1000
.GlobalEnv$bad_rate_total = 0.10
output_file = "PSI_Comp_"



#Create random seed vector

set.seed(1236)
rand_seed<-sample.int(999999,iterations)
y_test<-c(rep(0,iterations))
if(exists("cm_transp_psi")){rm(cm_transp_psi)}

#Create standard base table

variable_creator(1357,"base","Variables_Base.R")
model_base<-create_model("base")

start_time = Sys.time()

iter = 1
file_iter = 1
file_iterations = 10
total_iters_per_fi = as.integer(iterations/file_iterations)

while(file_iter <= file_iterations){
  total_pd_dist <- rep(0,total_iters_per_fi)
  iter_counter = 1
  if(exists("cm_transp_psi")){rm(cm_transp_psi)}
  
while(iter <= (total_iters_per_fi*file_iter)){
  variable_creator(rand_seed[iter],"test","Variables_Test.R")
  model_test<-apply_model()
  # print_averages()
  comp_matrix <- run_comparisons_grouped()
  combine_psi()
  y_test[iter]<-mean(model_test[[1]])
  
  cat("Iteration ",iter,"\n")
  iter <- iter + 1
  iter_counter <- iter_counter+1
}


write.csv(cm_transp_psi,paste("Output/",output_file,file_iter,".csv",sep=""), row.names = FALSE)

file_iter=file_iter+1
}
end_time = Sys.time()
difftime(end_time,start_time,units="mins")


# Change made:
#   Existing customer(Distribution)
#     From:prob = c(0.8, 0.2)
#     To:prob = c(0.57,0.43)
#   
#   Number of enquiries (Distribution)
#     From: prob = c(0.3, 0.25, 0.2, 0.15, 0.05, 0.05)
#     To: prob = c(0.1, 0.1, 0.2, 0.5, 0.05, 0.05)  
#   
    
# mean(y_test)
# 
# library(scorecard)
# iv(dt=base_final_dataset,"y_hat")

