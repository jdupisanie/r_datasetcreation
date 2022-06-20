

source("C:/Users/jdupisanie/Desktop/RCode/MainFunctions.r")

#Dataset Setup

.GlobalEnv$class_size = 10
.GlobalEnv$pop_size <- 10000
.GlobalEnv$bad_rate_total = 0.10
iterations = 10000

.GlobalEnv$var_list = list(
  "Gender",
  "Age",
  "NumEnq",
  "ExistCust",
  "CCother",
  "OutBal",
  "Prov",
  "AppMethod",
  "Income",
  "RecDef"
)

if (exists("total_attr_view")){rm(total_attr_view)}


create_variables_base <- function(base_test) {
  #Gender (1 = Female, 2 = Male)
  group_up_bound <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  bad_ratio <- c(1, 3, 0, 0, 0, 0, 0, 0, 0, 0)
  sample_var = rDiscU(U[, 1], prob = c(0.6, 0.4))
  assign(
    paste(base_test, "_Gender", sep = ""),
    create_data(group_up_bound, bad_ratio, sample_var, base_seed),
    envir = .GlobalEnv
  )
  
  
  #Existing Customer (0 = Yes, 1 = No)
  group_up_bound <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  bad_ratio <- c(1, 2.7, 0, 0, 0, 0, 0, 0, 0, 0)
  sample_var = rDiscU(U[, 2], prob = c(0.8, 0.2), arg = group_up_bound)
  assign(
    paste(base_test, "_ExistCust", sep = ""),
    create_data(group_up_bound, bad_ratio, sample_var, base_seed),
    envir = .GlobalEnv
  )
  
  
  #Number of Enquiries (0,1,2,3,4,5+)
  group_up_bound <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  bad_ratio <- c(1, 1.3, 1.8, 1.9, 2.1, 2.7, 0, 0, 0, 0)
  sample_var = rDiscU(U[, 2],
                      prob = c(0.3, 0.25, 0.2, 0.15, 0.05, 0.05),
                      arg = group_up_bound)
  assign(
    paste(base_test, "_NumEnq", sep = ""),
    create_data(group_up_bound, bad_ratio, sample_var, base_seed),
    envir = .GlobalEnv
  )
  
  
  
  #Age (18 - 75)
  group_up_bound <- c(18, 21, 25, 30, 45, 57, 63, 75, 76, 100)
  bad_ratio <- c(1, 0.85, 0.78, 0.66, 0.5, 0.43 , 0.31, 0, 0, 0)
  sample_var = runif(pop_size, 18, 75)
  assign(
    paste(base_test, "_Age", sep = ""),
    create_data(group_up_bound, bad_ratio, sample_var, base_seed),
    envir = .GlobalEnv
  )
  
  
  
  # CreditCards with other providers (0,1,2,3+)
  group_up_bound <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  # bad_ratio <- c(1, 1.2, 1.7, 2.5, 0, 0, 0, 0, 0, 0)
  bad_ratio <- c(1, 3, 5, 7, 0, 0, 0, 0, 0, 0)
  sample_var = sample(
    x = group_up_bound,
    pop_size,
    replace = T,
    prob = c(0.5, 0.3, 0.15, 0.05, 0, 0, 0, 0, 0, 0)
  )
  assign(
    paste(base_test, "_CCother", sep = ""),
    create_data(group_up_bound, bad_ratio, sample_var, base_seed),
    envir = .GlobalEnv
  )
  
  
  
  # Outstanding balance (0 - 10m)
  group_up_bound <-
    c(0,
      5000,
      10000,
      25000,
      100000,
      500000,
      1000000,
      10000000,
      10000001,
      10000002)
  bad_ratio <- c(1, 1.2, 2, 2.1, 0.8, 0.5, 2.5, 0, 0, 0)
  sample_var <- rlnorm(n = pop_size, mean = 0, sd = 1) * 10000
  assign(
    paste(base_test, "_OutBal", sep = ""),
    create_data(group_up_bound, bad_ratio, sample_var, base_seed),
    envir = .GlobalEnv
  )
  
  
  
  # Province (0-GP, 1-WC, 2-8 other)
  group_up_bound <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  bad_ratio <- c(1, 0.7, 1.8, 1.5, 3, 2.5, 2, 4, 1.2, 0)
  sample_var = sample(
    x = group_up_bound,
    pop_size,
    replace = T,
    prob = c(0.4, 0.3, 0.07, 0.05, 0.05, 0.04, 0.04, 0.03, 0.02, 0)
  )
  assign(
    paste(base_test, "_Prov", sep = ""),
    create_data(group_up_bound, bad_ratio, sample_var, base_seed),
    envir = .GlobalEnv
  )
  
  
  
  # Application Method (Branch, Online, Phone, Marketing Call)
  group_up_bound <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  bad_ratio <- c(1, 0.5, 1.5, 0.4, 0, 0, 0, 0, 0, 0)
  sample_var = sample(
    x = group_up_bound,
    pop_size,
    replace = T,
    prob = c(0.3, 0.4, 0.15, 0.15, 0, 0, 0, 0, 0, 0)
  )
  assign(
    paste(base_test, "_AppMethod", sep = ""),
    create_data(group_up_bound, bad_ratio, sample_var, base_seed),
    envir = .GlobalEnv
  )
  
  
  
  # Income (0 - 300k (clustered at 10k, 25k, 50k and 100k))
  group_up_bound <-
    c(0,
      5000,
      11000,
      20000,
      30000,
      70000,
      1000000,
      1000001,
      1000002,
      1000003)
  bad_ratio <- c(3, 2.5, 2, 1.4, 1.2, 1, 0, 0, 0, 0)
  sample_var = rInc(pop_size)
  assign(
    paste(base_test, "_Income", sep = ""),
    create_data(group_up_bound, bad_ratio, sample_var, base_seed),
    envir = .GlobalEnv
  )
  
  
  
  # Balance of recent defaults (clustered at 0 and 2k)
  group_up_bound <-
    c(0,
      1000,
      3000,
      5000,
      30000,
      1000000,
      1000001,
      1000002,
      1000003,
      1000004)
  bad_ratio <- c(1, 1.1, 2, 2.5, 3, 3.3, 0, 0, 0, 0)
  sample_var = rBal(pop_size) * 1000
  assign(
    paste(base_test, "_RecDef", sep = ""),
    create_data(group_up_bound, bad_ratio, sample_var, base_seed),
    envir = .GlobalEnv
  )
  
  
}

set.seed(1236)
rand_seed<-sample.int(999999,iterations)


iter = 1
file_iter = 1
file_iterations = 10
total_iters_per_fi = as.integer(iterations/file_iterations)

while(file_iter <= file_iterations){
 total_attr_view <- data.frame() 
  while(iter <= (total_iters_per_fi*file_iter)){
  
  base_seed = rand_seed[iter]
  .GlobalEnv$U = rGausCop(pop_size, Sig)
  create_variables_base(base_test = "base")
  add_var_to_set(base_test = "base")
  get_pred_outcome()
  create_model_set("base")
  
  total_attr_temp <-data.frame(mean(base_final_dataset$y_hat))
  
  for(f in 1:length(var_list))
  {
    var_name_gr <-paste(var_list[f], sep = "", "_groupings")
    var_name <- paste(var_list[f], sep = "")
    temp1<-t(aggregate(base_model_dataset$y_hat, list(base_model_dataset[[var_name_gr]]), mean))
    temp1<-data.frame( t(c(temp1[2,])))
    
    temp2<-t(aggregate(base_model_dataset$y_hat, list(base_model_dataset[[var_name_gr]]), length))
    temp2<-temp2/pop_size
    temp2<-data.frame( t(c(temp2[2,])))
    
    a <- length(unique(base_model_dataset[[var_name_gr]]))
    i=1
    name_vec_br <- c()
    name_vec_d <- c()
    while (i<=a){
      name_vec_br<-append(name_vec_br,paste0(var_name,"_br_",i))
      name_vec_d<-append(name_vec_d,paste0(var_name,"_d_",i))
      i <- i + 1
    }
    names(temp1)<-name_vec_br
    names(temp2)<-name_vec_d
    
    
    total_attr_temp<-data.frame(total_attr_temp, temp1,temp2)
    
  }
  
  total_attr_temp[setdiff(names(total_attr_view), names(total_attr_temp))] <- 0
  
  if (exists("total_attr_view")){total_attr_view<-bind_rows(total_attr_view,total_attr_temp)}
  else{total_attr_view<-total_attr_temp}
  
  
   cat("Sample Variables, Iteration ",iter,"\n")
  iter <- iter + 1
}
 
write.csv(total_attr_view,paste("C:/Users/jdupisanie/Desktop/RCode/SimulationOutputVars_",file_iter,".csv",sep=""), row.names = FALSE)
 
 file_iter = file_iter+1
  
}










  


