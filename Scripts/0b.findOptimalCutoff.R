# ============================ STATISTICAL DECISION THEORY ==============================
# FUNCTION DEFINITION: findOptimalCutoff()
# ---------------------------------------------------------------------------------------
# SCRIPT AUTHOR(S):  Dr Arno Botha
# DESCRIPTION: A custom function that implements various functional calls from the 
#   "optimalCutpoints"-package, towards finding a probability cut-off for a given binary
#   probabilisic classifier model.
# =======================================================================================



# -- Custom function to evaluate a given probabilistic classifier [predict.Var] against actual 
#   labels [label.Var] across several measures and criteria towards finding an optimal cut-off.
# - Cost-sensitive & Cost-insensitive varieties, see the "OptimalCutpoints"-package for details.
# - 8 measures are considered: Generalized Youden's Index; Minimised Misclassification Cost; Weighted Max Kappa Index;
#   Maximised Sensitivity; Maximised Specificity; Maximised Product between Sensitivity and Specificity;
#   Equalizing Sensitivity and Specificity; Prevalence-matching.
# - Each measure is weighted by a specifiable weight vector, thereby culminating in a weighted cut-off as output
findOptimalCutoff <- function(predict.Var, label.Var, neg.Val = 0, data, cost_fp=1, cost_fn=1,
                              Importance_Weight=c(0.75, 0.75, 0.75, 0, 0, 0.25, 0.25, 0.25)) {
  
  # - Testing conditions
  # predict.Var <- "Prob0"; label.Var <- "target"; data <- datValid; neg.Val <- 0
  
  require(data.table)
  require(OptimalCutpoints)
  # See https://cran.r-project.org/web/packages/OptimalCutpoints/OptimalCutpoints.pdf
  
  # Youden Index method | Youden1950, Greiner2000
  # Based on finding the c-point where both TPR and FPR is optimized | Y = max_c {Sens(c) + spec(c) - 1}
  optimal.cutpoint.Youden <- optimal.cutpoints(X = predict.Var, status = label.Var, tag.healthy = neg.Val, # these are the negatives
                                               methods = "Youden", data = data, ci.fit = FALSE, conf.level = 0.95, trace = FALSE,
                                               control = control.cutpoints())
  #summary(optimal.cutpoint.Youden)
  # plot(optimal.cutpoint.Youden)
  
  # Generalized Youden Index method | Youden1950
  # Based on finding the c-point where both TPR and FPR is optimized | GY = max_c {Sens(c) + r.spec(c) - 1}
  # where r = (1-p)/p . (C_FNB/C_FP)
  optimal.cutpoint.GenYouden <- optimal.cutpoints(X = predict.Var, status = label.Var, tag.healthy = neg.Val, # these are the negatives
                                                  methods = "Youden", data = data, ci.fit = FALSE, conf.level = 0.95, trace = FALSE,
                                                  control = control.cutpoints(CFP=cost_fp, CFN=cost_fn, generalized.Youden=T))
  #summary(optimal.cutpoint.GenYouden)
  # plot(optimal.cutpoint.Youden)
  
  # Minimise Misclassification Cost (MC) | Smith1991, Greiner (1995,1996)
  # Based on minimising MC | MCT(c) = C_FN / C_FP . p(1 - Sens(c)) + (1-p)(1 - Spec(c))
  optimal.cutpoint.MCT <- optimal.cutpoints(X = predict.Var, status = label.Var, tag.healthy = neg.Val, # these are the negatives
                                            methods = "MCT", data = data, ci.fit = FALSE, conf.level = 0.95, trace = FALSE,
                                            control = control.cutpoints(CFP=cost_fp, CFN=cost_fn))
  #summary(optimal.cutpoint.MCT)
  # plot(optimal.cutpoint.MCT)
  
  # Maximise Weighted kappa Index | Cohen1960, Greiner2000, Kraemer2002
  # Based on minimising MC | MCT(c) = C_FN / C_FP . p(1 - Sens(c)) + (1-p)(1 - Spec(c))
  optimal.cutpoint.WKappa <- optimal.cutpoints(X = predict.Var, status = label.Var, tag.healthy = neg.Val, # these are the negatives
                                               methods = "MaxKappa", data = data, ci.fit = FALSE, conf.level = 0.95, trace = FALSE,
                                               control = control.cutpoints(CFP=cost_fp, CFN=cost_fn, weighted.Kappa=T))
  ##summary(optimal.cutpoint.WKappa)
  # plot(optimal.cutpoint.WKappa)
  
  # Max-Sensitivity (TPR)| Filella1995, Hoffman2000
  # Based on finding c that maximises TPR
  optimal.cutpoint.MaxSens <- optimal.cutpoints(X = predict.Var, status = label.Var, tag.healthy = neg.Val, # these are the negatives
                                                methods = "MaxSe", data = data, ci.fit = FALSE, conf.level = 0.95, trace = FALSE,
                                                control = control.cutpoints())
  #summary(optimal.cutpoint.MaxSens)
  # plot(optimal.cutpoint.MaxSens)
  
  # Max-Specificity (FPR)| Bortheiry1994, Hoffman2000
  # Based on finding c that maximises FPR
  optimal.cutpoint.MaxSpec <- optimal.cutpoints(X = predict.Var, status = label.Var, tag.healthy = neg.Val, # these are the negatives
                                                methods = "MaxSp", data = data, ci.fit = FALSE, conf.level = 0.95, trace = FALSE,
                                                control = control.cutpoints())
  #summary(optimal.cutpoint.MaxSpec)
  # plot(optimal.cutpoint.MaxSpec)
  
  # Max- Product of Sens & Spec (FPR)| Lewis2008
  # Based on finding c that maximises the product of TPR and FPR
  optimal.cutpoint.MaxProdSensSpec <- optimal.cutpoints(X = predict.Var, status = label.Var, tag.healthy = neg.Val, # these are the negatives
                                                        methods = "MaxProdSpSe", data = data, ci.fit = FALSE, conf.level = 0.95, trace = FALSE,
                                                        control = control.cutpoints())
  #summary(optimal.cutpoint.MaxProdSensSpec)
  # plot(optimal.cutpoint.MaxProdSensSpec)
  
  # Equality between Sensitivity and Specificity | Greiner1995, Hosmer2000
  # Based on selecting c such that Sensitivity and Specificity is roughly equal
  optimal.cutpoint.EqualSensSpec <- optimal.cutpoints(X = predict.Var, status = label.Var, tag.healthy = neg.Val, # these are the negatives
                                                      methods = "SpEqualSe", data = data, ci.fit = FALSE, conf.level = 0.95, trace = FALSE,
                                                      control = control.cutpoints())
  #summary(optimal.cutpoint.EqualSensSpec)
  # plot(optimal.cutpoint.EqualSensSpec)  
  
  # Prevalence-matching | Manel2001, Kelly2008
  # Based on selecting c that matches sample prevalence (p) of event | c : p.Sense(c) + (1-p).(1-Spec(c))
  optimal.cutpoint.PrevMatch <- optimal.cutpoints(X = predict.Var, status = label.Var, tag.healthy = neg.Val, # these are the negatives
                                                  methods = "PrevalenceMatching", data = data, ci.fit = FALSE, conf.level = 0.95, trace = FALSE,
                                                  control = control.cutpoints())
  #summary(optimal.cutpoint.PrevMatch)
  # plot(optimal.cutpoint.PrevMatch)
  
  ### NOTE: Methods tried but discarded: Cost-Benefit (CB) method; gave multiple (3) c-values, ranging from 0.47 to 0.6,
  # which correspond to other methods, e.g., Youden gave 0.6, but cost-sensitive Generalized Youden gave 0.47.
  # MaxSpSe-method; gave multiple answers and deemed inferior to MaxProdSpSe
  
  # - Collect results together into a data.table object
  datResults <- data.table(Method_Name = c("GenYouden", "MCT", "WMaxKappa", "MaxSpec", "MaxSens", "MaxProdSensSpec", 
                                           "EqualSensSpec", "PrevMatch"),
                           Method_Description = c("Generalized Youden's Index (Max c)", "Min of Misclassification Cost (Min c)", "Weighted Max Kappa Index",
                                                  "Max Sensitivity", "Max Specificity", "Max of Prod of Sensitivity and Specificity",
                                                  "Sensitivity=Specificity",  "Prevalence Matching"),
                           CostType = c("Cost-sensitive", "Cost-sensitive", "Cost-sensitive", 
                                        "Cost-insensitive", "Cost-insensitive", "Cost-insensitive",
                                        "Cost-insensitive", "Cost-insensitive"),
                           CutOff = c(max(optimal.cutpoint.GenYouden$Youden$Global$optimal.cutoff$cutoff),
                                      min(optimal.cutpoint.MCT$MCT$Global$optimal.cutoff$cutoff),
                                      optimal.cutpoint.WKappa$MaxKappa$Global$optimal.cutoff$cutoff,
                                      optimal.cutpoint.MaxSpec$MaxSp$Global$optimal.cutoff$cutoff,
                                      optimal.cutpoint.MaxSens$MaxSe$Global$optimal.cutoff$cutoff,
                                      optimal.cutpoint.MaxProdSensSpec$MaxProdSpSe$Global$optimal.cutoff$cutoff,
                                      optimal.cutpoint.EqualSensSpec$SpEqualSe$Global$optimal.cutoff$cutoff,
                                      optimal.cutpoint.PrevMatch$PrevalenceMatching$Global$optimal.cutoff$cutoff),
                           Importance_Weight=Importance_Weight)
  
  # - Calculate weighted mean cut-off across various methods
  wMeanCutOff <- sum(datResults$Importance_Weight * datResults$CutOff) / sum(datResults$Importance_Weight)
  
  return(list(Results=datResults, WeightedMean_Cutoff = wMeanCutOff))
}

