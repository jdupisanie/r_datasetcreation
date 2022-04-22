
#Basic creation of a variable set
source("Global_Parameters.r")
source("MainFunctions.R")

variable_creator(1234,"base","Variables_Base.R")
# variable_creator(1234,"test","Variables_Test.R")


model_base<-create_model("base")
# model_test<-apply_model()

# variable_average(data_set=base_model_dataset)


model_base[[6]]
print(model_base[[6]]/pop_size)


library(scorecard)

info_value = iv(base_final_dataset, y = "y_hat")
