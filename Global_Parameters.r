#Implementation 1 - No shift in the underlying variables.


#source("MainFunctions.R")

#Dataset Setup
.GlobalEnv$pop_size <- 10000
.GlobalEnv$bad_rate_total = 0.10
.GlobalEnv$class_size = 10
.GlobalEnv$base_seed = 1234

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



