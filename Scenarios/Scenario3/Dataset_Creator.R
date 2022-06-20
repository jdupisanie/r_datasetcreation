
#Basic creation of a variable set
source("Global_Parameters.r")
source("MainFunctions.R")

variable_creator(1212,"base","Scenarios/Scenario3/Variables_Base.R")
variable_creator(4321,"test","Scenarios/Scenario3/Variables_Test.R")


model_base<-create_model("base")
model_test<-apply_model()

# variable_average(data_set=base_model_dataset)

print_averages()

comp_matrix <- run_comparisons_grouped()

combine_psi()


# write.csv(base_distr_dataset,paste("Output/Distributions.csv",sep=""), row.names = FALSE)
barplot(comp_matrix$dp_index_v2, names.arg = comp_matrix$Attribute_Name, cex.names = 0.5)
