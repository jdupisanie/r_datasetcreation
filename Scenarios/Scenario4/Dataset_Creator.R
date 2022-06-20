
#Basic creation of a variable set
source("Global_Parameters.r")
source("MainFunctions.R")

variable_creator(1234,"base","Scenarios/Scenario4/Variables_Base.R")
variable_creator(4321,"test","Scenarios/Scenario4/Variables_Test.R")


model_base<-create_model("base")
model_test<-apply_model()

# variable_average(data_set=base_model_dataset)

print_averages()

comp_matrix <- run_comparisons_grouped()

combine_psi()


# write.csv(base_distr_dataset,paste("Output/Distributions.csv",sep=""), row.names = FALSE)
