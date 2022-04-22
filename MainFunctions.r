# A large change made to the CCOther variable.


# Libaries
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrr)
library(ROCR)
library(InformationValue)
library(ModelMetrics)
library(overlapping)
library(mvtnorm)

set.seed(base_seed)

#Function to create the dataset and bad_ind
create_data = function(group_up_bound,
                       bad_ratio,
                       sample_var,
                       r_seed) {
  # Setup of population
  group_up_edge <- c(group_up_bound, Inf)
  group_dist <- rep(0, 10)
  i <- 1
  while (i < 10) {
    j <- 1
    while (j <= pop_size) {
      if (sample_var[j] >= group_up_edge[i] &&
          sample_var[j] < group_up_edge[i + 1]) {
        group_dist[i] = group_dist[i] + 1
      }
      j = j + 1
    }
    i = i + 1
  }
  group_dist <- group_dist / pop_size
  
  total_exp = pop_size * group_dist
  
  # Optimize the bad rate
  min_perc = 0.01
  max_perc = 0.2
  incr_perc = 0.0005
  bad_rate_var = 0.03
  total_bad = pop_size * bad_rate_total
  mini_bad = Inf
  
  # while (min_perc <= max_perc) {
  #   bad_rate_temp = min_perc / bad_ratio[1] * bad_ratio
  #   bad_temp = total_exp * bad_rate_temp
  #   total_bad_temp = sum(bad_temp)
  #   if (abs(total_bad - total_bad_temp) < mini_bad) {
  #     bad_rate = bad_rate_temp
  #     mini_bad = abs(total_bad - total_bad_temp)
  #   }
  #   min_perc = min_perc + incr_perc
  # }
  
  bad_temp = (bad_ratio*group_dist)/sum(bad_ratio)
  bad_rate = (bad_temp*bad_rate_total)/(sum(bad_temp)*group_dist)
  
  bad_exp = total_exp * bad_rate
  
  set.seed(r_seed)
  sample_rand = runif(pop_size)
  total_act <- rep(0, 10)
  bad_act <- rep(0, 10)
  bad_ind <- rep(0, pop_size)
  group_edges <- c(group_up_bound, Inf)
  
  i <- 1
  while (i < 10) {
    total_act[i] = sum((sample_var >= group_edges[i]) * (sample_var < group_edges[i +
                                                                                    1]))
    bad_act[i] = sum((sample_var >= group_edges[i]) * (sample_var < group_edges[i +
                                                                                  1]) * (sample_rand < bad_rate[i])
    )
    
    j = 1
    while (j < pop_size) {
      if (sample_var[j] >= group_edges[i] &&
          sample_var[j] < group_edges[i + 1] &&
          sample_rand[j] <= bad_rate[i]) {
        bad_ind[j] = 1
      }
      j = j + 1
    }
    i = i + 1
  }
  
  good_distr = (total_act - bad_act) / (pop_size - sum(bad_act,na.rm=TRUE))
  bad_distr = bad_act / sum(bad_act,na.rm=TRUE)
  total_distr = total_act / sum(total_act,na.rm=TRUE)
  inf_value = sum((good_distr - bad_distr) * log(good_distr / bad_distr), na.rm = T)
  
  
  data_summary = data.frame(
    group_up_bound,
    group_dist,
    bad_ratio,
    bad_rate,
    total_exp,
    total_act,
    bad_exp,
    bad_act
  )
  variable_set = data.frame(sample_var, bad_ind)
  
  output_list = list(
    data_summary,
    #1
    variable_set,
    #2
    sum(bad_act),
    #3
    inf_value,
    #4
    group_up_bound,
    #5
    good_distr,
    #6
    bad_distr,
    #7
    group_dist       #8
  )
  
  return (output_list)
}

rInc <-
  function(n,
           probs = c(0.3, 0.4, 0.15, 0.15),
           modes = c(10, 25, 50, 100)) {
    U  = runif(n)
    cp = cumsum(probs)
    n1 = sum(U < cp[1])
    n2 = sum(U < cp[2]) - n1
    n3 = sum(U < cp[3]) - (n1 + n2)
    n4 = n - (n1 + n2 + n3)
    X1 = rnorm(n1, modes[1], modes[1] ^ 0.6)
    X2 = rnorm(n2, modes[2], modes[2] ^ 0.6)
    X3 = rnorm(n3, modes[3], modes[3] ^ 0.6)
    X4 = rnorm(n4, modes[4], modes[4] ^ 0.6)
    X  = c(X1, X2, X3, X4)
    X  = abs(X) * 1000
    return(X)
  }

rBal <- function(n,
                 p = 0.6,
                 lambda = 1 / 40,
                 start = 2,
                 stop = 100) {
  U  = runif(n)
  n1 = sum(U > p)
  if ((n1 < n) & (n1 > 0)) {
    X0 = rep(0, n - n1)
    U  = runif(n1, pexp(start, lambda), pexp(stop, lambda))
    X1 = -log(1 - U) / lambda
    X  = c(X0, X1)
  }
  if (n1 == 0) {
    X = rep(0, n)
  }
  if (n1 == n) {
    U = runif(n1, pexp(start, lambda), pexp(stop, lambda))
    X = -log(1 - U) / lambda
  }
  return(X)
}


# Generating copula to influence the correlation between three variables
corr12 = 0.1
corr13 = 0.2
corr23 = 0.1

Sig = matrix(c(1, corr12, corr13, corr12, 1, corr23, corr13, corr23, 1), 3, 3)

rGausCop <- function(n, Sig) {
  d  = dim(Sig)[1]
  mu = rep(0, d)
  N  = rmvnorm(n, mu, Sig)
  X  = pnorm(N)
  return(X)
}



rDiscU <- function(U, probs, arg = seq(0, length(probs) - 1, 1)) {
  cp = cumsum(probs)
  X  = rep(arg[1], pop_size)
  for (k in 2:length(probs)) {
    for (j in 1:pop_size) {
      if (U[j] < cp[k] && U[j] > cp[k - 1]) {
        X[j] = arg[k]
      }
    }
  }
  return(X)
}

add_var_to_set = function(base_test) {
  x = as.name(paste(base_test, "_final_dataset", sep = ""))
  assign(as.character(x), data.frame(overall_bad_ind = (c(
    rep(1, pop_size * bad_rate_total), rep(0, pop_size * (1 - bad_rate_total))
  ))), envir = .GlobalEnv)
  # final_dataset <<- data.frame(overall_bad_ind = (c(rep(1,pop_size*bad_rate_total),rep(0,pop_size*(1-bad_rate_total)))))
  
  i <- 1
  while (i <= 10) {
    var_table = eval(parse(text = paste(base_test, "_", var_list[i], sep = "")))
    var_name <- deparse(substitute(var_table))
    
    temp_table_a <- var_table[[2]]
    temp_table_b <- temp_table_a[order(-temp_table_a$bad_ind),]
    names(temp_table_b)[1] <-
      # paste(base_test, "_", var_list[i], sep = "")
      paste(var_list[i], sep = "")
    assign(as.character(x),
           data.frame(eval(as.name(x)), var_name = temp_table_b[1]),
           envir = .GlobalEnv)
    # names(eval(as.name(x)))[names(eval(as.name(x))) == "sample_var"] <<- var_name
    i = i + 1
  }
}

# x_model_dataset = eval(parse(text = paste(base_test, "_model_dataset", sep = "")))

get_pred_outcome <- function() {
  temp_model <- glm(
    base_final_dataset$overall_bad_ind ~
      factor(eval(parse(text = var_list[1]))) +
      eval(parse(text = var_list[2])) +
      eval(parse(text = var_list[3])) +
      factor(eval(parse(text = var_list[4]))) +
      eval(parse(text = var_list[5])) +
      eval(parse(text = var_list[6])) +
      factor(eval(parse(text = var_list[7]))) +
      factor(eval(parse(text = var_list[8]))) +
      eval(parse(text = var_list[9])) +
      eval(parse(text = var_list[10]))
    ,
    data = base_final_dataset,
    family = binomial(link = "logit")
  )
  
  temp_model_summary <- summary(temp_model)
  
  temp_probabilities <- predict(temp_model, type = "response")
  
  y_hat = rbinom(pop_size, size = 1, prob = temp_probabilities)
  
  temp_set = data.frame(base_final_dataset, y_hat)
  
  .GlobalEnv$base_final_dataset <-
    data.frame(.GlobalEnv$base_final_dataset, y_hat)
  
}

create_model_set <- function(base_test) {
  x_model_dataset = as.name(paste(base_test, "_model_dataset", sep = ""))
  x_distr_dataset = as.name(paste(base_test, "_distr_dataset", sep = ""))
  
  x_final_dataset = eval(parse(text = paste(base_test, "_final_dataset", sep =
                                              "")))
  assign(as.character(x_distr_dataset),
         data.frame(group_num <- c(1:10)),
         envir = .GlobalEnv)
  if (base_test == "base") {
    assign(as.character(x_model_dataset),
           data.frame(y_hat = x_final_dataset[["y_hat"]],overall_bad_ind = x_final_dataset[["overall_bad_ind"]]),
           envir = .GlobalEnv)
    
  }
  
  if (base_test == "test") {
    assign(as.character(x_model_dataset),
           data.frame(c(pop_size)),
           envir = .GlobalEnv)
   
  }
  
  
  m <- 1
  while (m <= 10) {
    var_table <-
      eval(parse(text = paste(base_test, "_", var_list[m], sep = "")))
    var_name <- paste(var_list[m], sep = "")
    # eval(parse(text = paste(base_test, "_", var_list[m], sep = "")))
    # var_name <- paste(base_test, "_", var_list[m], sep = "")
    
    group_up_bound <- c(-Inf, var_table[[5]], Inf)
    sample_var <- x_final_dataset[[var_name]]
    bad_ind <- x_final_dataset[["overall_bad_ind"]]
    groupings <- as.numeric(cut(sample_var, group_up_bound))
    bad_act <- rep(0, 10)
    total_act <- rep(0, 10)
    good_act <- rep(0, 10)
    
    i <- 1
    while (i < 10) {
      total_act[i] = sum(groupings == i)
      bad_act[i] = sum((groupings == i) * (bad_ind))
      good_act[i] = total_act[i] - bad_act[i]
      i = i + 1
    }
    
    good_distr = (total_act - bad_act) / (pop_size - sum(bad_act))
    bad_distr = bad_act / sum(bad_act)
    total_distr = total_act / sum(total_act)
    
    if (base_test == "base") {
      woe_v = log(good_distr / bad_distr)
      woe_v[woe_v > 10] <- 10
      woe <- rep(0, pop_size)
      
      i <- 0
      while (i < 10) {
        j <- 1
        while (j <= pop_size) {
          if (isTRUE(groupings[j] == i)) {
            woe[j] <- woe_v[i]
          }
          j = j + 1
        }
        i = i + 1
      }
    }
    
    if (base_test == "test") {
      var_name_woe <- paste(var_list[m], "_woe", sep = "")
      var_name_groupings <-
        paste(var_list[m], "_groupings", sep = "")
      woe_v <-
        unique(base_model_dataset[c(var_name_groupings, var_name_woe)])
      woe <- rep(0, pop_size)
      
      i <- 0
      while (i < 10) {
        j <- 1
        while (j <= pop_size) {
          if (isTRUE(groupings[j] == woe_v[i, 1])) {
            woe[j] <- woe_v[i, 2]
          }
          j = j + 1
        }
        i = i + 1
      }
      
    }
    
    
    
    temp_woe_group <- data.frame(woe, groupings)
    names(temp_woe_group)[1] <- paste(var_name, "_woe", sep = "")
    names(temp_woe_group)[2] <-
      paste(var_name, "_groupings", sep = "")
    
    assign(as.character(x_model_dataset),
           data.frame(eval(as.name(x_model_dataset)), temp_woe_group),
           envir = .GlobalEnv)
    
    
    temp_distr_group <-
      data.frame(total_distr, good_distr, bad_distr)
    names(temp_distr_group)[1] <-
      paste(var_name, "_distr_total", sep = "")
    names(temp_distr_group)[2] <-
      paste(var_name, "_distr_good", sep = "")
    names(temp_distr_group)[3] <-
      paste(var_name, "_distr_bad", sep = "")
    
    assign(as.character(x_distr_dataset),
           data.frame(eval(as.name(x_distr_dataset)), temp_distr_group),
           envir = .GlobalEnv)
    
    m = m + 1
    
  }
}

create_test_set <- function(base_test) {
  base_test <- 'test'
  x_model_dataset = as.name(paste(base_test, "_model_dataset", sep = ""))
  x_distr_dataset = as.name(paste(base_test, "_distr_dataset", sep = ""))
  
  x_final_dataset = eval(parse(text = paste(base_test, "_final_dataset", sep =
                                              "")))
  assign(
    as.character(x_model_dataset),
    data.frame(overall_bad_ind = x_final_dataset[["overall_bad_ind"]]),
    envir = .GlobalEnv
  )
  
  assign(as.character(x_distr_dataset),
         data.frame(group_num <- c(1:10)),
         envir = .GlobalEnv)
  m <- 1
  while (m <= 10) {
    var_table <-
      eval(parse(text = paste(base_test, "_", var_list[m], sep = "")))
    var_name <- paste(var_list[m], sep = "")
    # eval(parse(text = paste(base_test, "_", var_list[m], sep = "")))
    # var_name <- paste(base_test, "_", var_list[m], sep = "")
    
    group_up_bound <- c(-Inf, var_table[[5]], Inf)
    sample_var <- x_final_dataset[[var_name]]
    bad_ind <- x_final_dataset[["overall_bad_ind"]]
    groupings <- as.numeric(cut(sample_var, group_up_bound))
    bad_act <- rep(0, 10)
    total_act <- rep(0, 10)
    good_act <- rep(0, 10)
    
    i <- 0
    while (i < 10) {
      total_act[i] = sum(groupings == i)
      bad_act[i] = sum((groupings == i) * (bad_ind))
      good_act[i] = total_act[i] - bad_act[i]
      i = i + 1
    }
    
    good_distr = (total_act - bad_act) / (pop_size - sum(bad_act))
    bad_distr = bad_act / sum(bad_act)
    total_distr = total_act / sum(total_act)
    
    
    var_name_woe <- paste(var_list[m], "_woe", sep = "")
    var_name_groupings <- paste(var_list[m], "_groupings", sep = "")
    woe_v <-
      unique(base_model_dataset[c(var_name_groupings, var_name_woe)])
    woe <- rep(0, pop_size)
    
    i <- 0
    while (i < 10) {
      j <- 1
      while (j <= pop_size) {
        if (isTRUE(groupings[j] == woe_v[i, 1])) {
          woe[j] <- woe_v[i, 2]
        }
        j = j + 1
      }
      i = i + 1
    }
    
    temp_woe_group <- data.frame(woe, groupings)
    names(temp_woe_group)[1] <- paste(var_name, "_woe", sep = "")
    names(temp_woe_group)[2] <-
      paste(var_name, "_groupings", sep = "")
    
    assign(as.character(x_model_dataset),
           data.frame(eval(as.name(
             x_model_dataset
           )), temp_woe_group),
           envir = .GlobalEnv)
    
    
    
    temp_distr_group <- data.frame(total_distr, good_distr, bad_distr)
    names(temp_distr_group)[1] <-
      paste(var_name, "_distr_total", sep = "")
    names(temp_distr_group)[2] <-
      paste(var_name, "_distr_good", sep = "")
    names(temp_distr_group)[3] <-
      paste(var_name, "_distr_bad", sep = "")
    
    assign(as.character(x_distr_dataset),
           data.frame(eval(as.name(
             x_distr_dataset
           )), temp_distr_group),
           envir = .GlobalEnv)
    
    m = m + 1
    
  }
}

create_model <- function(base_test) {
  x_model_dataset = eval(parse(text = paste(base_test, "_model_dataset", sep = "")))
  
  model <- glm(x_model_dataset$y_hat ~
               +factor(eval(parse(text = paste(var_list[1], "_woe", sep = ""))))
               +eval(parse(text = paste(var_list[2], "_woe", sep = "")))
               +eval(parse(text = paste(var_list[3], "_woe", sep = "")))
               +factor(eval(parse(text = paste(var_list[4], "_woe", sep = ""))))
               +eval(parse(text = paste(var_list[5], "_woe", sep = "")))
               +eval(parse(text = paste(var_list[6], "_woe", sep = "")))
               +factor(eval(parse(text = paste(var_list[7], "_woe", sep = ""))))
               +factor(eval(parse(text = paste(var_list[8], "_woe", sep = ""))))
               +eval(parse(text = paste(var_list[9], "_woe", sep = "")))
               +eval(parse(text = paste(var_list[10], "_woe", sep = "")))
               
    ,
    data = x_model_dataset,
    family = binomial(link = "logit")
  )
  model_summary <- summary(model)

  probabilities <-  predict(model, type = "response")
  optCutOff <- optimalCutoff(x_model_dataset$y_hat, probabilities)#[1]
  predicted <- ifelse(probabilities > optCutOff, 1, 0)
  
  ROCRpred <- prediction(predicted, x_model_dataset$y_hat)
  ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
  plot(ROCRperf)
  #      colorize = TRUE,
  #      text.adj = c(-0.2, 1.7))
  output_table <- table(predicted, x_model_dataset$y_hat)
  gini_coeff <- gini(model)
  
  if (base_test == "base") {
    base_prob_cut_offs <<- c(0, quantile(probabilities, c(1:9) / 10), 1)
  }
  
  pd_v <- rep(0, 10)
  k <- 1
  while (k <= 10) {
    pd_v[k] = sum((probabilities >= base_prob_cut_offs[k]) * (probabilities < base_prob_cut_offs[k +
                                                                                                   1])) / pop_size
    k = k + 1
  }
  
  
  .GlobalEnv$model_output <-
    list(
      model,
      model_summary,
      probabilities,
      optCutOff,
      predicted,
      output_table,
      gini_coeff,
      pd_v
    )
}

apply_model <- function() {
  probabilities <-
    predict(model_base[[1]], newdata = test_model_dataset, type = "response")
  predicted <- ifelse(probabilities > model_base[4], 1, 0)
  
  pd_v <- rep(0, 10)
  k <- 1
  while (k <= 10) {
    pd_v[k] = sum((probabilities >= base_prob_cut_offs[k]) * (probabilities < base_prob_cut_offs[k +
                                                                                                   1])) / pop_size
    k = k + 1
  }
  #output_table <- table(predicted, test_model_dataset$overall_bad_ind)
  output_table <- data.frame(predicted, test_model_dataset)
  
  output_list <- list(probabilities, predicted, pd_v, output_table)
  
  return(output_list)
}

run_comparisons_grouped <- function() {
  #PSI Metric
  psi_v <- rep(0, class_size + 1)
  dpv_v <- rep(0, class_size + 1)
  taplinhunt_v <- rep(0, class_size + 1)
  ks_v <- rep(0, class_size + 1)
  overlap_v <- rep(0, class_size + 1)
  dp_index_v <- rep(0, class_size + 1)
  dp_index_v2 <- rep(0, class_size + 1)
  
  DP_Index_Base <- rep(0, class_size)
  DP_Index_Test <- rep(0, class_size)
  
  i <- 1
  while (i <= 10) {
    base_var_name <- paste(var_list[i], "_distr_total", sep = "")
    test_var_name <- paste(var_list[i], "_distr_total", sep = "")
    
    base_distr <- base_distr_dataset[[base_var_name]]
    test_distr <- test_distr_dataset[[test_var_name]]
    t1 = 0
    t2 = 0
    j <- 1
    while (j <= 10) {
      psi_v[i]  = psi_v[i] +
        ifelse(test_distr[j] == 0, 0, ((base_distr[j] - test_distr[j]) * log(base_distr[j] / test_distr[j])
        ))
      dpv_v[i] = ifelse(abs(base_distr[j] - test_distr[j]) > dpv_v[j],
                        abs(base_distr[j] - test_distr[j]),
                        dpv_v[i])
      t1 = t1 + ifelse(test_distr[j] == 0, 0, (test_distr[j] - mean(base_distr)) ^
                         2)
      t2 = t2 + ifelse(base_distr[j] == 0, 0, (base_distr[j] - mean(base_distr)) ^
                         2)
      overlap_v[i] = overlap_v[i] + min(base_distr[j], test_distr[j])
      
      #ks_v[i] = ks.test(base_distr[j],test_distr[j])
      
      j = j + 1
    }
    taplinhunt_v[i] = 0.5 * (1 + t1 / t2)
    
    #ks_v[i] = ks.test(ecdf(base_distr),ecdf(test_distr))
    
    groupings_var <- paste(var_list[i], "_groupings", sep = "")
    woe_var <- paste(var_list[i], "_woe", sep = "")
    groupings_var1 <- as.name(groupings_var)
    woe_var1 <- as.name(woe_var)
    
    c <- unique(base_model_dataset[c(groupings_var, woe_var)])
    d <- c[c[[woe_var1]] != 0, ]
    d <- d[order(d[[groupings_var1]]), ]
    e <- data.frame(seq(1:10), base_distr)
    f <- data.frame(seq(1:10), test_distr)
    g <- merge(
      x = e,
      y = f,
      by = c(1, 1),
      all = TRUE
    )
    h <- merge(x = g, y = d , by = c(1, 1))
    model_coef <- model_base[[1]][["coefficients"]][[i + 1]]
    
    DP_Index_Base[i] = sum(h$base_distr * h[[woe_var1]] * model_coef)
    #print(h$base_distr)
    #print(h[[woe_var1]])
    #print(model_coef)
    #print(DP_Index_Base[i])
    DP_Index_Test[i] = sum(h$test_distr * h[[woe_var1]] * model_coef)
    
    i = i + 1
  }
  
  
  # DP_Index_total<-sum(model_base[[1]][["coefficients"]][[1]],DP_Index_Base)
  temp_total <- mean(model_base[[3]])
  DP_Index_total = log(1 / temp_total - 1)
  #print("DP INDEX TOTAL")
  #print(DP_Index_total)
  i_dp1 = 1
  while (i_dp1 <= 10) {
    dp_index_v[i_dp1] = DP_Index_total - DP_Index_Base[i_dp1] + DP_Index_Test[i_dp1]
    dp_index_v2[i_dp1] = (1 / (exp(dp_index_v[i_dp1]) + 1)) - (1 / (exp(DP_Index_total) +
                                                                      1))
    i_dp1 = i_dp1 + 1
  }
  
  total_dpv = sum(dp_index_v2)
  avg_base = mean(model_base[[3]])
  avg_test = mean(model_test[[1]])
  dp_index_v2 <- dp_index_v2*(avg_test - avg_base)/total_dpv
  
  
  base_pd_distr <- model_base[[8]]
  test_pd_distr <- model_test[[3]]
  
  j <- 1
  while (j <= 10) {
    psi_v[11]  = psi_v[11] +
      ifelse(test_distr[j] == 0, 0, ((base_pd_distr[j] - test_pd_distr[j]) * log(base_pd_distr[j] / test_pd_distr[j])
      ))
    dpv_v[11] = ifelse(
      abs(base_pd_distr[j] - test_pd_distr[j]) > dpv_v[11],
      abs(base_pd_distr[j] - test_pd_distr[j]),
      dpv_v[11]
    )
    overlap_v[11] = overlap_v[11] + min(base_pd_distr[j], test_pd_distr[j])
    j = j + 1
  }
  
  
  dp_index_v[11] = sum(dp_index_v)
  dp_index_v2[11] = sum(dp_index_v2[1:10])
  
  #ks_v[11] = ks.test(base_distr[11],test_distr[11])
  
  
  
  
  psi_v <- round(psi_v, digits = 4)
  dpv_v <- round(dpv_v, digits = 4)
  taplinhunt_v <- round(taplinhunt_v, digits = 4)
  ks_v <- round(ks_v, digits = 4)
  overlap_v <- round(overlap_v, digits = 4)
  dp_index_v <- round(dp_index_v, digits = 4)
  dp_index_v2 <- round(dp_index_v2, digits = 8)
  
  all_comparisons <-
    data.frame(
      c(as.vector(unlist(var_list)), "PD_groups"),
      psi_v,
      dpv_v,
      taplinhunt_v,
      #ks_v,      *** to be added ***
      overlap_v,
      #dp_index_v,
      dp_index_v2
    )
  
  return(all_comparisons)
}

variable_creator = function(seed,base_test_select,variables_select){
  .GlobalEnv$base_seed=seed
  source(variables_select)
  add_var_to_set(base_test = base_test_select)
  if(base_test_select == "base"){
    get_pred_outcome()
  }
  create_model_set(base_test_select)
}

variable_average <- function(data_set, all_Variable){
  
  if (exists("total_attr_view")){}  else{.GlobalEnv$total_attr_view <- data.frame()}
  
  total_attr_temp <-data.frame(mean(data_set$y_hat))
  
  for(f in 1:length(var_list))
  {
    var_name_gr <-paste(var_list[f], sep = "", "_groupings")
    var_name <- paste(var_list[f], sep = "")
    temp1<-t(aggregate(data_set$y_hat, list(data_set[[var_name_gr]]), mean))
    temp1<-data.frame( t(c(temp1[2,])))
    
    temp2<-t(aggregate(data_set$y_hat, list(data_set[[var_name_gr]]), length))
    temp2<-temp2/pop_size
    temp2<-data.frame( t(c(temp2[2,])))
    
    a <- length(unique(data_set[[var_name_gr]]))
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
  
  if (exists("total_attr_view")){.GlobalEnv$total_attr_view<-bind_rows(.GlobalEnv$total_attr_view,total_attr_temp)}
  else{.GlobalEnv$total_attr_view<-total_attr_temp}
  
  #return(total_attr_temp)
}
