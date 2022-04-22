
source("Global_Parameters.r")
source("MainFunctions.R")

#Simulation Setup

iterations = 10
.GlobalEnv$pop_size <- 1000
.GlobalEnv$bad_rate_total = 0.10
output_file = "SimulationOutput_1_"



#Create random seed vector

set.seed(1236)
rand_seed<-sample.int(999999,iterations)


start_time = Sys.time()

iter = 1
file_iter = 1
file_iterations = 1
total_iters_per_fi = as.integer(iterations/file_iterations)

while(file_iter <= file_iterations){
  total_pd_dist <- rep(0,total_iters_per_fi)
  iter_counter = 1
  
while(iter <= (total_iters_per_fi*file_iter)){

  variable_creator(rand_seed[iter],"base","Variables_Base.R")
  create_model_set("base")
  variable_average(data_set=base_model_dataset)
  
 
  cat("Sample 1, Iteration ",iter,"\n")
  iter <- iter + 1
  iter_counter <- iter_counter+1
}


# total_pd_dist <- data.frame(n_1000=total_pd_dist)
write.csv(total_attr_view,paste("Output/",output_file,file_iter,".csv",sep=""), row.names = FALSE)
file_iter=file_iter+1
}
end_time = Sys.time()
difftime(end_time,start_time,units="mins")
