# ============================== CUSTOM FUNCTIONS ==============================
# Defining custom functions used across various projects
# ------------------------------------------------------------------------------
# PROJECT TITLE: Dynamic SICR-research
# SCRIPT AUTHOR(S): Dr Arno Botha, Esmerelda Oberholzer

# DESCRIPTION:
# This script defines various functions that are used elsewhere in this project
# or, indeed, used across other projects. Functions are grouped thematically.
# ==============================================================================



# --------------------------- TERNARY CONDITION OPERATORS ----------------------
# See https://stackoverflow.com/questions/8790143/does-the-ternary-operator-exist-in-r
`%?%` <- function(x, y) list(x = x, y = y)
`%:%` <- function(xy, z) if(xy$x) xy$y else z



# -------------------------------- UTILITY FUNCTIONS --------------------------
# - Mode function (R doesn't have a built-int one)
getmode <- function(v) {
  uniqv <- unique(v);
  # discard any missingness
  uniqv <- uniqv[complete.cases(uniqv)]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# - Memory function using 'gdata' package
getMemUsage <- function(limit=1000){
  require(gdata); require(scales)
  # - Get list of significant object sizes occupied in memory, order ascendingly
  totUsage <- ll()
  memSize <- subset(totUsage, KB >= limit)
  memSize$MB <- memSize$KB/1000
  gc(verbose=F)
  cat("Total memory used: ", comma(sum(totUsage$KB)/1000), "MB\n")
  cat("Big objects size: ", comma(sum(memSize$MB)), "MB\n\n")
  return(  memSize[order(memSize$KB), c(1,3)])
}



# -------------------------- INTERLEAVING FUNCTION ------------------------------
# - Coalescing function to facilitate data fusion between two given vectors
# Input: two scalar values (x & y) that may have selective missingness in either side (left: x; right: y)
# Output: Returns the non-missing side. If both are non-missing, then returns the (given) preference.
interleave <- function(x,y, na.value = as.integer(NA), pref='X') {
  # ensure require(dplyr)
  case_when(!is.na(x) & is.na(y) ~ x,
            is.na(x) & !is.na(y) ~ y,
            is.na(x) & is.na(y) ~ na.value,
            x == y ~ x,
            x != y & pref=='X' ~ x,
            x != y & pref=='Y' ~ y,
  )
}



# ------------------------- INTERPOLATION FUNCTION -----------------------------
# - Missing value Treatment: Interpolate the values between two known non-missing points
# Assumes all missingness are 'encased' between two known points.
# Input: [given]: a time series possibly with some missing values for which we like to interpolate;
#     [shouldRollBackward]: If the first element is missing, should we try to fix this by 'back-interpolating'
#       from the first non-missing point found?;
#     [SilenceWarnings]: Self-explanatory;
#     [shouldRollForward]: When there is only a single non-missing element, should we simply copy that value forward?
# Output: Linearly interpolated vector
interPol <- function(given, shouldRollForward=T, shouldRollBackward=T, SilenceWarnings=T) {
  
  # -- Testing conditions
  #given <- macro_data_hist$Inflation # for testing
  #given <- as.vector(subset(macro_data, Scenario=="Historic")[order(Date_T), RealGDP_Growth_yoy])
  #unique(macro_data$Scenario)
  #given <- as.vector(subset(macro_data, Scenario=="Baseline")[order(Date_T), rbqn_rb5339q])
  # (given <- as.vector(subset(macro_data, Scenario=="SevereStress")[order(Date_T), Consumption_Level_1q]))
  
  
  # first, check if there are any non-missing element
  if (all(is.na(given))) {
    # yes, there is, so just return the same input values and throw a warning (if allowed)
    if (SilenceWarnings==F) {
      warning("All data is missing, returning NA throughout..") 
    }
    return(given)
  }
  
  # second, check if there is any missing value; if so, then exit the function
  if (all(!is.na(given))) {
    return(given)
  }
  
  # third, check if first value is missing, which can hamper our interpolation procedure
  if (is.na(given[1])) {
    # yup, so should we try to fix this by 'back-interpolating' based on the first set of 2 non-missing values in the series?
    if (shouldRollBackward == T) {
      
      start.point <- 1 # starting point for filling in interpolated vaues at the end of this procedure

      # find first non-missing value in the series, which will be our 'ending value' for interpolating backwards
      end.point <- which(!is.na(given))[1]-1 # position before first non-missing element
      end.val <- given[end.point+1] # first non-missing element
      
      # we need to find second non-missing value and perform an 'interim' interpolation so that we have a one-period value
      # by which to change [end.val] backwards to [start.point] at the 'same speed' (as an assumption)
      start.point2 <- which(!is.na(given))[1]+1 # position after first non-missing element
      start.val2 <- given[start.point2-1] # first non-missing element
      end.point2 <- which(!is.na(given))[2]-1 # position before second non-missing element
      end.val2 <- given[end.point2+1] # second non-missing element
      
      # interpolate across this range, including the two values as outer bounds (therefore add 2 to the interpolation length)
      # note that (end.point - start.point + 1) denotes the length of this missingness-episode
      inter.vals <- seq(from=start.val2, to=end.val2, length.out = end.point2 - start.point2 + 1 + 2)
      
      # - might as well linearly interpolate here (saving a computing cycle of the while loop later on) ..
      # delete the first and last observation (they are the outer values outside of the missingness range)
      # and assign these interpolated values to the given vector
      given[start.point2:end.point2] <- inter.vals[2:(end.point2 - start.point2 + 2)]
      
      # check if we have non-zero elements at both sides
      if (start.val2 == 0 & end.val2 == 0) {
        # yes, so by-pass this treatment and just fill with 0s
        given[start.point:end.point] <- rep(0, end.point-start.point + 1)
      } else {
        
        # get interpolation 'speed'
        speed <- diff(given[start.point2:(start.point2+1)]) / given[start.point2]
        # given[start.point2]*(1+speed) # test
        
        # 'discount' the value backwards from the first non-missing value, using the previously calculated speed as the 'discount rate'
        for (i in end.point:start.point ) {
          given[i] <- given[i+1]*(1+speed)^(-1)
        } 
      }
      
    } else {
      # no we cannot. So throw error and exit
      stop("Error: Base assumption violated - First observation is missing, cannot interpolate. Exiting ..") 
    }
  }
  
  # repeat until no more missingness in given vector
  while ( any(is.na(given)) ) {
    
    # -- testing conditions
    #given <- c(2,NA,NA,5,NA,NA,8) # works
    #given <- c(2,NA,NA,5,6, NA,NA, 9) # works
    #given <- c(2,NA,NA,5,6, NA, NA, NA)
    
    # find the indices of all missing observations
    miss.ind <- which(is.na(given))
    
    # find "episodes" of missingness in these indices, since there may be more than 1 episode in the general case,
    # for which we need to repeat this procedure.
    # 1. Do this by first isolating the cases where the lagged differences are greater than 1
    # 2. Add 1 to these found positions to move to the "initial starting points" of the next episode in succession
    # 3. Pre-fix this vector with '1' to re-include the first 'episode' that was deselected previously
    # 4. Given this vector of indices (of indices), return starting positions again
    episode.starting.times <- miss.ind[c(1, which(diff(miss.ind) > 1) + 1)]
    
    # - check if we have data points outside of the first episode from which to interpolate
    # get staring point of first episode of missingness
    start.point <- episode.starting.times[1]
    # get ending point of first episode (got to test first if we have multiple episodes and diverge logic from there)
    if (length(episode.starting.times) > 1) {
      # we have multiple episodes. Therefore, scan the series from missingness's start up to the first non-missing element, then minus 1
      # add this to the starting point, minus 1 to exclude the first missing value (otherwise we are double-counting it when adding this range)
      end.point <- start.point + (Position(function(x) {!is.na(x)}, x=given[start.point:(episode.starting.times[2]-1)] ) - 1) - 1
    } else {
      # we don't have multiple episodes. Therefore, take last known missingness index
      end.point <- miss.ind[length(miss.ind)]
    }
    
    # given the starting and ending points for the actual interpolation, test for non-missing data outside of this range from 
    # which we need to interpolate
    if (!is.na(given[start.point-1]) & !is.na(given[end.point+1])) {# returns true if we can interpolate (no missingness outside of range)
      start.val <- given[start.point-1]
      end.val <- given[end.point+1]
      # interpolate across this range, including the two values as outer bounds (therefore add 2 to the interpolation length)
      # note that (end.point - start.point + 1) denotes the length of this missingness episode
      inter.vals <- seq(from=start.val, to=end.val, length.out = (end.point - start.point + 1) + 2)
      # delete the first and last observation (they are the outer values outside of the missingness range)
      # and assign these interpolated values to the given vector
      given[start.point:end.point] <- inter.vals[2:(end.point - start.point + 2)]
      
    } else {
      # assumption violated or episode's length = 1. Check if we can simply replace NAs with last known value in either case?
      if (shouldRollForward == T){
        if (SilenceWarnings==F) {
          warning("Base assumption violated - no available data outside of missingness range from which to interpolate. Rolling values forward instead ..")
        }
        # by definition, we must have a non-missing first element (should start.point >= 2)
        start.val <- given[start.point-1]
        given[start.point:end.point] <- rep(start.val, (end.point - start.point + 1)) # just repeat for the length of the missingness episode
        
      } else {
        # no we cannot. So throw error and exit
        stop("Error: Base assumption violated - no available data outside of missingness range from which to interpolate. Exiting ..") 
      }
    }
    
  }
  
  return(given)
}



# --------------------------- SCALING FUNCTIONS --------------------------------
# - two scaling functions to standardize given vectors unto a uniform scale
# Input: [given]: a real-valued vector
# Output: standardized vector

# 1) Range-based scaler | vectors will have equal ranges (min-max)
scaler <- function(given){
  output <- (given - min(given,na.rm=T)) / (max(given, na.rm=T) - min(given, na.rm=T))
  return(output)
}
# 2) Z-score/normalized scaler | vectors should roughly be N(0,1) distributed
scaler.norm <- function(given){
  # (given <- as.vector(subset(macro_data_hist1, Scenario=="Baseline")$DebtToIncome_Rate)) # for testing
  output <- (given - mean(given,na.rm=T)) / (sqrt(var(given,na.rm=T)))
  # check for NaN values (which can result if there is 0 variance)
  if (all(is.na(output))) {
    # just assign the central value, in this case, 0
    output <- rep(0, length(output))
  }
  return(output)
}



# --------------------------- IMPUTATION FUNCTIONS -----------------------------
# - Curate a main vector [x] to equal the previous/most-recent non-
# missing element in a given vector
imputeLastKnown <- function (x) {
  # -- Testing purposes
  # x <- Lookup$ZeroBal_Remain_Ind; x_lead <- Lookup$ZeroBal_Remain_Ind_lead
  # x <- c(0,0,0,1,1,1,0,1)
  # x <- c(0,0,0,1,1,1,0,NA)
  # x <- c(0,0,0,1,1,1,1,NA)
  # x <- c(0,0,0,1,NA,1,0,NA)
  # x <- c(0,NA)
  
  firstOne <- which(is.na(x))[1]
  if (!is.na(firstOne) & firstOne > 1) {
    x[firstOne] <- x[firstOne-1]
    # call function recursively to fix earlier missing-cases
    return( imputeLastKnown(x))
  } else { # no missing value found, return original vector
    return(x)
  }
}

# - Curate a main vector [x] where x[1] is missing.
# This is achieve by finding the first non-missing element and back-filling that value
imputeFirstKnown <- function(x) {
  # -- Testing purposes
  # x <- c(NA, NA, 2,3,4)
  firstOne <- which(!is.na(x))[1]
  if (!is.na(firstOne) & firstOne > 1) {
    x[1:(firstOne-1)] <- x[firstOne]
    return(x)
  } else { # no non-missing value found, return original vector
    return(x)
  }
}



# -------------------- FEATURE ENGINEERING FUNCTIONS ---------------------------

# - Adjusting for inflation
# Assumes a monthly macroeconomic dataset [macro_data_hist] to exist with [Date_T] and [Inflation] fields
adjInflation <- function(g_start, g_stop) {
  compFact <- macro_data_hist[Date_T >= g_start & Date_T <= g_stop, list(Factor = prod(1 + (Inflation/100)/12))]
  return(compFact)
}

# -  Adjusting for inflation (Robust version that accepts a macroeconomic dataset)
# Generating an inflation factor for a given series of yearly inflation growth rates
# Input:  [datMacro]: The dataset containing the yearly inflation growth rate
#         [time]: Name of the time/date variable in [datMacro]
#         [g_start]:  The starting date for the series of inflation growth rates
#         [g_stop]:   The ending date for the series of inflation growth rates
# Output: A factor indicating the cumulative inflation over the period starting at [g_start] and ending [g_stop]
# --- Define custom function for computing inflation/deflation factors
adjInflation_MV <- function(datMacro, time, Inflation_Growth, g_start, g_stop) {
  # datMacro=datMV; time="Date"; g_start<-date("2015-02-28"); g_stop<-date("2022-12-31"); Inflation_Growth<-"M_Inflation_Growth"
  compFact <- as.numeric(datMacro[get(time) >= g_start & get(time) <= g_stop, list(Factor = prod(1 + (get(Inflation_Growth))/12))])
  return(compFact)
  # rm(datMacro, time, g_start, g_stop, Inflation_Growth); gc()
}
# - Unit test
# if (!exists('datMV')) unpack.ffdf(paste0(genPath,"datMV"), tempPath)
# (test <- adjInflation(datMacro=datMV, time="Date", g_start=date("2015-02-28"), g_stop=date("2022-12-31"), Inflation_Growth="M_Inflation_Growth"))
# rm(datMV, test); gc()


# - Dummy-encoding by given formula
# Converts found categorical/logical fields into numerical dummy variables given a formula
# Inputs: [datGiven]: given data.table object, [modelForm]: given model formula to investigate
#   [returnType]: "data": returns dummified data.talbe object, "modelForm": returns modified formula
dummify <- function(datGiven, modelForm, returnType="data", verbose=T) {
  
  # - necessary libraries
  require(stringr)
  
  # - Subset relevant fields only
  datSubset <- subset(datGiven, select=all.vars(modelForm))
  modelForm_refined <- modelForm
  
  # - Iterate across fields and treat as necessary
  flds <- colnames(datSubset)[-1]
  for (fld in flds ) {
    # testing conditions:
    # fld <- colnames(datSubset)[-1][5]
    
    # Check for categorical/logical fields
    if (!is.numeric(datSubset[, get(fld)])) {
      
      # get bins, order decreasingly by frequency
      fldBins <- unique(datSubset[, list(Freq=.N), by=list(Var=get(fld))][order(-Freq), Var])
      
      # count bins
      n_fldBins <- length( fldBins )
      
      # re-encode bins into (n-1) dummy variable names
      colNames <- c( sapply(1:(n_fldBins-1), function(j) { paste0(fld, "_", str_replace_all(fldBins[j], pattern=regex("\\s*"), repl=""))}) )
      
      # message
      if (verbose) cat("\nFOUND: [", fld, "]. Bins (", n_fldBins, "): ", fldBins, ". Recoded as new dummy fields: ", colNames)
      
      # create dummy variables
      datSubset[, (colNames) := lapply(1:(n_fldBins-1), function(i,dat,colNames,fld,fldBins) {
        # testing conditions
        # i <- 1; dat <- datSubset
        ifelse(dat[, get(fld)]==fldBins[i], 1, 0)
      }, dat=.SD, colNames=colNames, fld=fld, fldBins=fldBins)]
      
      # remove categorical variable and add new fields
      datSubset[, (fld) := NULL]
      flds <- colnames(datSubset)[-1]
      
      # adjust formula
      modelForm_refined <- reformulate( termlabels = flds[which(fld!=flds)], response = all.vars(modelForm_refined)[attr(terms(modelForm_refined), "response")] )
    }
  }
  
  if (returnType=="data") return(datSubset) else return(modelForm_refined)
}



# ------------------------- SICR-DEFINITION FUNCTION ---------------------------
# Function that defines a SICR-event for a given loan's history
# Input: [delinq]: g1-measured delinqeuncy vector (number of payments in arrears) at every time t
#     [d]: threshold for g1-mesaure beyond which a SICR-event is said to have occured at t
#     [s]: "stickiness" of the delinquency test, or the number of consecutive periods for which
#         g1(t) >= d must hold before a SICR-event is said to occur
SICR_flag <- function(delinq, d, s) {
  
  # Prepare vectors into a mini data.table for easier wrangling
  dat <- data.table(delinq)
  
  # Main delinquency test at every time period
  dat[, Test0 := ifelse(delinq >= d, 1, 0)]
  
  # Second condition: assessing whether this delinquency threshold was met for (s-1) lagged periods
  varList <- c('Test0')
  if (s > 1) {
    for (l in 1:(s-1)) {
      dat[, paste0('Test',l) := shift(Test0, n=l, type="lag")]
      
      # add newly created variable to a list
      varList <- c(varList, paste0('Test',l))
    }
  }
  
  # Sum the number of lagged flags per row, used for final logic test
  dat[, Test_Sum := rowSums(.SD, na.rm = T), .SDcols=varList]
  
  # Finally, test whether g_0(t) >= d for at least s number of periods
  # This is achieved by equating summed lagged flags and evaluating against s >=1
  dat[, Sticky := ifelse(Test_Sum == s, 1, 0)]  
  
  # return SICR-flag vector
  return(dat$Sticky)
}


# ------------------------- INSET GRAPHING FUNCTION ---------------------------
# - Create custom function so that we can specify which facet to annotate with inset graph
# inspired from: blopig.com/blog/2019/08/combining-inset-plots-with-facets-using-ggplot2/
annotation_custom2 <- function(grob, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, data) {
  layer(data=data, stat=StatIdentity, position=PositionIdentity,
        geom=ggplot2:::GeomCustomAnn,
        inherit.aes=T, params=list(grob=grob, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))
}




# ------------------------- SICR-EVALUATION FUNCTION ---------------------------
# - Evaluation function for given model (SVM/logit) using ROC-analysis and a time graph of 
#   actual vs expected predictions (discrete/probabilistic)
# Inputs include: model_assess: fitted model object; dataSet: evaluation dataset in format required by model_assess;
# graphSet: additional data that complements dataSet for graphing purposes (must have columns "LoanID", "Date", "SICR_def")
# target: given target variable within dataSet; modelType: "SVM" or "logit"; cutOFF: probability score cut-off for logit models
evalModel <- function(model_assess, dataSet, graphSet, target, modelType="SVM", cutOFF=0.5) {
  
  # --- 0. Package setup
  require(pROC)
  require(ggplot2)
  require(scales)
  require(ggthemes)
  require(extrafont)
  require(RColorBrewer)
  
  
  # --- 1. ROC-analysis
  
  # - Score using fitted model (dependent on model type) into both probabilistic and discrete output
  if (modelType == "SVM") {
    
    # - score output using SVM-specific version of predict()
    dataSet_scored <- predict(model_assess, newdata = dataSet, probability = TRUE, decision.values=T)
    
    # - Discrete & probabilistic output (Platt-scaling)
    expDisc <- as.numeric(levels(dataSet_scored))[dataSet_scored]
    expProb <- attr(dataSet_scored, "probabilities")[,"1"]
  } else {
    expProb <- predict(model_assess, newdata = dataSet, type="response") # logit
    expDisc <-  ifelse(expProb >= cutOFF, 1, 0)
  }
  
  # AUC: Discrete & Probabilistic
  cat("Discrete AUC: ", sprintf("%.2f",auc(dataSet[, get(target)], expDisc, quiet=T)*100))
  cat("\nProbabilistic AUC: ", sprintf("%.2f",auc(dataSet[, get(target)], expProb, quiet=T)*100))
  
  
  # --- 2. Time graph: SICR-rates between actual and expected output
  
  # - Create graphing dataset by stacking 3 target types of the same given datasets
  datSICR_graph <- rbind(data.table(graphSet, SICR_events=dataSet[, get(target)] , Type="a_Actual"),
                         data.table(graphSet, SICR_events=expDisc, Type="b_Modelled_disc"),
                         data.table(graphSet, SICR_events=expProb , Type="c_Modelled_prob"))
  
  # - Transform factor back to numeric variables for aggregation purposes
  datSICR_graph[, SICR_events := as.numeric(levels(SICR_events))[SICR_events]]
  
  
  ### TEMP Start
  #test <- datSICR_graph[SICR_def==0 & Type=="c_Modelled_prob", list(EventRate = sum(SICR_events, na.mr=T)/.N), by=list(Date)]
  #ggplot(test, aes(x=Date, y=EventRate)) + geom_line(size=0.3) + geom_point(size=1) + theme_bw()
  ### TEMP END
  
  # - Aggregate to monthly level and observe up to given point
  SICR_StartDte <- min(graphSet$Date, na.rm=T)
  SICR_EndDte <- max(graphSet$Date, na.rm=T)
  port.aggr <- datSICR_graph[SICR_def==0, list(EventRate = sum(SICR_events, na.rm=T)/.N, AtRisk = .N),
                             by=list(Type, Date)][Date >= SICR_StartDte & Date <= SICR_EndDte,] %>% setkey(Type,Date)
  
  # - Aesthetics engineering
  port.aggr[, Facet_label := paste0("SICR-definition ", SICR_label)]
  
  # - Calculate MAE over time by line graph type in summarising differences amongst line graphs
  port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date, Type), names_from = c(Type), values_from = c(EventRate))
  (diag.Act_ExpDisc <- mean(abs(port.aggr2$a_Actual - port.aggr2$b_Modelled_disc)) * 100)
  (diag.Act_ExpProb <- mean(abs(port.aggr2$a_Actual - port.aggr2$c_Modelled_prob)) * 100)
  
  # - Calculate standard deviation of these processes
  stdev_SICR_Act <- sd(port.aggr2$a_Actual, na.rm=T)
  stdev_SICR_ExpDisc <- sd(port.aggr2$b_Modelled_disc, na.rm=T)
  stdev_SICR_ExpProb <- sd(port.aggr2$c_Modelled_prob, na.rm=T)
  
  # - Graphing parameters
  col.v <- brewer.pal(5, "Dark2")
  label.v <- c("a_Actual"=bquote(italic(A[t])*": Actual SICR-rate in "*italic(D[V])),
               "b_Modelled_disc"=bquote(italic(B[t])*": Expected SICR-rate: discrete"),
               "c_Modelled_prob"=bquote(italic(C[t])*": Expected SICR-rate: probabilistic"))
  chosenFont <- "Cambria"
  
  # - Create graph
  (g <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=Type)) + theme_bw() + 
      labs(x="Reporting date (ccyymm)", y="Conditional SICR-Incidence rate (%) given Stage 1") + 
      theme(text=element_text(family=chosenFont),legend.position = "bottom",
            axis.text.x=element_text(angle=90),
            strip.background=element_rect(fill="snow2", colour="snow2"),
            strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
      # main line graph with overlaid points
      geom_line(aes(colour=Type, linetype=Type), size=0.3) + 
      geom_point(aes(colour=Type, shape=Type), size=1) + 
      #annotations
      annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual", mean(EventRate)]*0.6,
               label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.2f", diag.Act_ExpDisc),"%'"),
               family=chosenFont, size=3, parse=T) + 
      annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date >= "2012-12-31" & Type=="a_Actual", mean(EventRate)]*0.45,
               label=paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*': ", sprintf("%.2f", diag.Act_ExpProb),"%'"),
               family=chosenFont, size=3, parse=T) +     
      # facets & scale options
      facet_grid(Facet_label ~ .) + 
      scale_colour_manual(name="", values=col.v, labels=label.v) + 
      scale_shape_discrete(name="", labels=label.v) + scale_linetype_discrete(name="", labels=label.v) + 
      scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
      scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))
  
  return(g)
}

#rm(datSICR_graph, label.v, port.aggr, port.aggr2, g, chosenFont, SICR_StartDte, SICR_EndDte,
#stdev_SICR_Act, stdev_SICR_ExpDisc, stdev_SICR_ExpProb, col.v, label.v,
#expDisc, expProb, dataSet_scored); gc()
#rm(svm_model_assess,dataSet,graphSet,target,modelType,cutOFF)



# ------------------------- JOIN MACROECONOMIC DATA FUNCTION ---------------------------

merge_macro_info <- function (input_dat){
  
  # - Preliminaries
  require(data.table)
  require(ETLUtils)
  require(ffbase)
  
  if (!exists('dat_SICR_MVs')) unpack.ffdf(paste0(genPath,"datSICR_MVs"), tempPath)
  
  # - Find intersection between fields in the credit dataset and the macroeconomic dataset
  (overlap_flds <- intersect(colnames(input_dat), colnames(dat_SICR_MVs))) 
  
  # - Merge on Date by performing a left-join
  output_dat <- as.data.table(merge(input_dat, dat_SICR_MVs, by="Date", all.x=T)); gc()
  
  # - Validate merging success by checking for missingness (should be zero)
  list_merge_variables <- list(colnames(dat_SICR_MVs))
  results_missingness <- list()
  for (i in 1:length(list_merge_variables)){
    output <- sum(is.na(output_dat$list_merge_variables[i]))
    results_missingness[[i]] <- output
  }
  cat((length(which(results_missingness > 0)) == 0) %?% "SAFE: No missingness, fusion with macroeconomic data is successful.\n" %:%
        "WARNING: Missingness in certain macroecnomic fields detected, fusion compromised.\n")
  
  return(output_dat)
  
}




# ------------------------- DIAGNOSTIC FUNCTIONS FOR LOGIT MODELS ---------------------------


# --- Pseudo R^2 measures for classifiers
# Calculate a pseudo coefficient of determination (R^2) \in [0,1] for glm assuming binary
# logistic regression as default, based on the "null deviance" in likelihoods
# between the candidate model and the intercept-only (or "empty/worst/null") model.
# NOTE: This generic R^2 is NOT equal to the typical R^2 used in linear regression, i.e., it does
# NOT explain the % of variance explained by the model; but rather it denotes the %-valued degree
# to which the candidate's fit can be deemed as "perfect".
# Implements McFadden's pseudo R^2, Cox-Snell generalised R^2, Nagelkerke's improvement upon Cox-Snell's R^2
# see https://bookdown.org/egarpor/SSS2-UC3M/logreg-deviance.html ; https://web.pdx.edu/~newsomj/cdaclass/ho_logistic.pdf; 
# https://statisticalhorizons.com/r2logistic/
# https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression
coefDeter_glm <- function(model) {
  
  # - Safety check
  if (!any(class(model) == "glm")) stop("Specified model object is not of class 'glm' or 'lm'. Exiting .. ")
  
  
  # -- Preliminaries
  require(scales) # for formatting of results
  L_full <- logLik(model) # log-likelihood of fitted model, ln(L_M)
  nobs <- attr(L_full, "nobs") # sample size, same as NROW(model$model)
  orig_formula <- deparse(unlist(list(model$formula, formula(model), model$call$formula))[[1]]) # model formula
  orig_call <- model$call; calltype.char <- as.character(orig_call[1]) # original model fitting call specification, used merely for "plumbing"
  data <- model$model # data matrix used in fitting the model
  # get weight matrix corresponding to each observation, if applicable/specified, otherwise, this defaults to just the 0/1-valued observations (Y)
  if (!is.null(model$prior.weights) & length(model$prior.weights) > 0) {
    weights <- model$prior.weights
  } else if (!is.null(data$`(weights)`) & length(data$`(weights)` > 0)) {
    weights <- data$`(weights)`
  } else weights <- NULL
  data <- data[, 1, drop=F]; names(data) <- "y"
  nullCall <- call(calltype.char, formula = as.formula("y ~ 1"), data = data, weights = weights, family = model$family, 
                   method = model$method, control = model$control, offset = model$offset)
  L_base <- logLik(eval(nullCall)) # log-likelihood of the null model, ln(L_0)
  
  # -- Implement the McFadden pseudo R^2 measure from McFadden1974, R^2 = 1 - log(L_M)/log(L_0)
  # NOTE: null deviance L_0 plays an analogous role to the residual sum of squares in linear regression, therefore
  # McFadden's R^2 corresponds to a proportional reduction in "error variance", according to Allison2013 (https://statisticalhorizons.com/r2logistic/)
  # NOTE2: deviance (L_M) and null deviance (L_0) within a GLM-object is already the log-likelihood since deviance = -2*ln(L_M) by definition
  # https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression
  coef_McFadden <- 1 - model$deviance / model$null.deviance
  if ( !all.equal(coef_McFadden, as.numeric(1 - (-2*L_full)/(-2*L_base) ) ) ) stop("ERROR: Internal function error in calculating & verifying McFadden's pseudo R^2-measure")
  
  
  # -- Implement Cox-Snell R^2 measure from Cox1983, which according to Allison2013 is more a "generalized" R^2 measure than pseudo,
  # given that its definition is an "identity" in normal-theory linear regression. Can therefore be used to other regression settings using MLE,
  # E.g., negative binomial regression for count data or Weibull regression for survival data
  # Definition: R^2 = 1 - (L_0/L_F)^(2/nobs), but equivalent to below given that L_base = ln(L0) and L_full = ln(L_full)
  # Why? Since (L_0/L_F)^(2/nobs) can be rewritten as exp[ ln( (L_0/L_F)^(2/nobs) )] which simplifies to exp[ (2/nobs) . ln( L_0/L_F )] given property ln(a^b) = b.ln(a),
  # finally becoming exp[ (2/nobs) . ( ln( L_0 ) - ln( L_F)) ]  given property ln(a/b) = ln(a) - ln(b).
  # The below is numerically expedient in avoiding "underflow" memory issues when dealing with large negative log-likelihood values that should rather not be exponentiated.
  # Source: DescTools::PseudoR2 function in DescTools package
  coef_CoxSnell <- as.numeric( 1 - exp(2/nobs * (L_base - L_full)) )
  
  
  # -- Implement Nagelkerke R^2 from Nagelkerke1991, which according to Allison2013 improves upon Cox-Snell R^2 by ensuring an upper bound of 1
  # NOTE: Cox-Snell R^2 has an upper bound of 1 - (L_0)^(2/n), which can be considerably less than 1.
  # This comes at the cost of reducing the attractive theoretical properties of the Cox-Snell R^2 
  coef_Nagelkerke <- (1 - exp((model$deviance - model$null.deviance)/nobs))/(1 - exp(-model$null.deviance/nobs))
  
  
  # -- Report results
  return( data.frame(McFadden=percent(coef_McFadden, accuracy=0.01), CoxSnell=percent(coef_CoxSnell, accuracy=0.01), Nagelkerke=percent(coef_Nagelkerke, accuracy=0.01)) )
  
  ### NOTE: All of the above were tested and confirmed to equal the results produced below:
  # DescTools::PseudoR2(model, c("McFadden", "CoxSnell", "Nagelkerke"))
  
  # - cleanup (only relevant whilst debugging this function)
  rm(model, L_full, L_base, nobs, data, nullCall, orig_formula, orig_call, weights, coef_McFadden, coef_CoxSnell, coef_Nagelkerke)
}
# - Unit test
# install.packages("ISLR"); require(ISLR)
# datTrain_simp <- data.table(ISLR::Default); datTrain[, `:=`(default=as.factor(default), student=as.factor(student))]
# logit_model <- glm(default ~ student + balance + income, data=datTrain_simp, family="binomial")
# coefDeter_glm(logit_model)
### RESULTS: candidate is 46% (McFadden) better than null-model in terms of its deviance



# --- Variable importance measures
# A function for measuring and rank-ordering the variable "importance" given a logit model
# Three such measures are implemented:
# 1) standardised coefficients via refitting on Z-scored input space [stdCoef_ZScores]
# 2) absolute coefficients [absCoef]
# 3) partial dependence (an explanable AI measure; see https://arxiv.org/pdf/1904.03959.pdf) [partDep]
# Regarding measures 1-2, the size of coefficients (or transforms thereof) are used in ranking the 
# variables from most to least "important". A larger/ smaller measure-value indicates a more/ less important
# Input:  [logit_model]: A logistic regression model trained using glm()
#         [method]:      "stdCoef"; "absCoef", "partDep" using the "FIRM"-technique from the vip::vi_firm() function
#         [sig_level]:   Significance level or threshold under which the variables are considered as statistically significant using p-values from the Wald-statistic
#         [impPlot]:     Switch for producing a bar chart that shows the variable importance according to the specified measure
#         [pd_plot]:     Should a partial dependence plot be created for each variable
# Output: A data table containing the variable importance information
varImport_logit <- function(logit_model, method="stdCoef_ZScores", sig_level=0.05, impPlot=F, pd_plot=F, chosenFont="Cambria", 
                            colPalette="BrBG", colPaletteDir=1, plotVersionName="", plotName=paste0(genFigPath, "VariableImportance_", method,"_", plotVersionName,".png"), 
                            limitVars=10, dpi=180){
  
  # - Unit testing conditions:
  # logit_model <- glm(default ~ student + balance + income, data=datTrain1, family="binomial")
  # method <- "stdCoef_ZScores"; sig_level<-0.05; impPlot<-T; pd_plot<-T; chosenFont="Cambria"; colPalette="BrBG"; colPaletteDir=1
  # plotName=paste0(genFigPath, "VariableImportance_", method,".png"); limitVars=10
  
  # - Safety check
  if (!any(class(logit_model) %in% c("glm", "lm"))) stop("Specified model object is not of class 'glm' or 'lm'. Exiting .. ")
  
  # --- 0. Setup
  # - Get the data the model was trained on
  datTrain1 <- subset(logit_model$data, select = names(logit_model$data)[names(logit_model$data) %in% names(model.frame(logit_model))])
  # Getting the names of the original training dataset
  datTrain1_names <- names(datTrain1)
  
  # - Filtering for significant variables
  coefficients_summary <- data.table(names=names(summary(logit_model)$coefficients[,4][-1]), sig=summary(logit_model)$coefficients[,4][-1],
                                     coefficient=summary(logit_model)$coefficients[,1][-1], se=summary(logit_model)$coefficients[,2][-1]) %>% arrange(names) # Names of variables in the model
  coefficients_sig_model_level <- as.list(rep(0,coefficients_summary[,.N])) # Corresponding level name of the categorical variable; NULL in the case of a numeric or integer variable
  coefficients_data <-  data.table(names=names(datTrain1)[-which(names(datTrain1) %in% names(model.frame(logit_model))[1])]) %>% arrange(names) # Names of variables training dataset
  coefficients_sig_data_index <- rep(0,coefficients_summary[,.N]) # Index showing if the variable in the model is significant or not
  coefficients_sig_data <- rep(0,coefficients_summary[,.N]) # Names of the significant variable's associated column name in the training dataset
  sig_level <- ifelse(is.na(sig_level),1,sig_level) # The significance level against which each variable must be tested
  k <- 1 # Counter
  for (i in 1:length(coefficients_data$names)){ # Main loop - looping through all the relevant variables in the training dataset
    if(class(datTrain1[,get(coefficients_data$names[i])]) %in% c("numeric", "integer")){ # Do the following if variable i is numeric
      coefficients_sig_data_index[k] <- coefficients_summary$sig[k]<=sig_level
      coefficients_sig_data[k] <- as.character(coefficients_data[i,])
      coefficients_sig_model_level[[k]] <- NA 
      k<-k+1
    } else { # Do the following if variable i is numeric
      levels_n <- length(unique(datTrain1[,get(coefficients_data$names[i])]))-1
      coefficients_sig_data_index[k:(k+levels_n-1)] <- rep(ifelse(any(coefficients_summary$sig[k:(k+levels_n-2)]<=sig_level),T,F),levels_n) # Checking if any levels of this variable is significant
      coefficients_sig_data[k:(k+levels_n-1)] <- as.character(coefficients_data[i,])
      for(j in 1:levels_n){
        coefficients_sig_model_level[[k+j-1]] <- unique(datTrain1[,get(coefficients_data$names[i])])[order(unique(datTrain1[,get(coefficients_data$names[i])]))][-1][j]
      }
      k<-k+levels_n
    }
  }
  
  coefficients_sig_model <- coefficients_summary$names[coefficients_sig_data_index==1] # Names of variables in model (may be more than the number of variables in the training dataset due to hot one encoding)
  coefficients_sig_model_level <- coefficients_sig_model_level[!coefficients_sig_data_index==0] # The chain ensures that only levels of the significant categorical variables are chosen
  coefficients_sig_data <- coefficients_sig_data[coefficients_sig_data_index==1] # The chaining ensures that only the columns pertaining to the significant variables are chosen
  
  # - Stopping the function if there are no significant variables
  if (is.null(coefficients_data)) stop("ERROR: Variable importance not conducted since there are no significant variables.") 
  
  # - Initiating the dataset to be returned (results dataset)
  results <- list(data = data.table(Variable = coefficients_sig_model,Value = 0,Rank = 0))
  
  # --- 2. Calculating variable importance based on specified method
  
  if (method=='stdCoef_ZScores'){
    # -- Standardizing the input space using Z-scores, followed by refitting the logit model
    # B = \beta - mean(X) / sd(x)
    # NOTE: The resulting coefficients are therefore "standardized", as per Menard2011 (http://www.jstor.org/stable/41290135)
    
    # Scaling the variables
    datTrain2 <- copy(datTrain1)
    for (i in 1:length(unique(coefficients_sig_data))){
      # Checking if the variable is numeric so that the underlying training data can be scaled
      if (class(datTrain2[, get(unique(coefficients_sig_data)[i])]) %in% c("numeric","integer")){ ### Can have a "binary" as well as "factor" type variables - check how these types of variables are handled - check in an additional script to confirm; factorise numeric variables (in model call)P and fit model - check funcitonality
        datTrain2[, (unique(coefficients_sig_data)[i]) := (get(unique(coefficients_sig_data)[i])-mean(get(unique(coefficients_sig_data)[i]),na.rm=T))/sd(get(unique(coefficients_sig_data)[i]), na.rm=T)]
      } # if
    }
    # Re-training the model on the scaled data
    suppressWarnings( logit_model <- glm(logit_model$formula, data=datTrain2, family="binomial") )
    
    # Calculating importance measure and preparing result set
    results$data[,Value := data.table(names=names(logit_model$coefficients[which(names(logit_model$coefficients) %in% coefficients_sig_model)]),
                                      Std_Coefficient=logit_model$coefficients[which(names(logit_model$coefficients) %in% coefficients_sig_model)]) %>% arrange(names) %>% subset(select="Std_Coefficient")]
    results$Method <- "Standardised coefficients using Z-scores"
    
  } else if (method=="stdCoef_Goodman") { 
    # -- Variable importance based on Goodman's standardised coefficients
    # See Menard2011; https://www.jstor.org/stable/41290135)
    # B = \beta / sd(\beta)
    
    # Calculating importance measure and preparing result set
    results$data <- copy(coefficients_summary)[names %in% coefficients_sig_model]
    results$data[,Value:=coefficient/se] # Compute the importance measure
    results$data[,`:=`(coefficient=NULL,se=NULL, sig=NULL)]; colnames(results$data) <- c("Variable", "Value")
    results$Method <- "Standardised coefficients: Goodman"
    
  } else if (method=="stdCoef_Menard"){ 
    # -- Variable importance based on Menard's standardised coefficients from Menard2011; https://www.jstor.org/stable/41290135)
    # B = \beta . s_x . R / s_{logit( \hat{Y} )}, where s_x is the standard deviation of X, R is the Pearson correlation between observed values Y and 
    # predicted class probabilities \hat{Y}, and s_\hat{Y} is the standard deviation of logit(\hat{Y})
    
    # Computing the standard deviations for each x (this requires a loop to ensure that categorical variables are correctly accounted for)
    ### AB: how to compute exactly for categorical? Google suggests standard error of bin proportion, Harrell suggests using "binconf" function from Hmisc-package to calculate Wilson's confidence interval
    # see (https://stats.stackexchange.com/questions/51248/how-can-i-find-the-standard-deviation-in-categorical-distribution#:~:text=There%20is%20no%20standard%20deviation,a%20binomial%20or%20multinomial%20proportion.)
    sd_x <- rep(0, length(coefficients_sig_model))
    for (i in 1:length(coefficients_sig_model)){
      x <- datTrain1[,get(coefficients_sig_data[i])]
      if (class(x) %in% c("numeric", "integer")){
        sd_x[i] <- sd(x, na.rm=T)
      } else {
        levels_n <- length(unique(datTrain1[,get(coefficients_sig_data[i])]))
        if (levels_n==2){
          x <- as.numeric(x == coefficients_sig_model_level[[i]])
        } else {
          x <- as.numeric(x == coefficients_sig_model_level[[i]])
        }
        sd_x[i] <- sd(x, na.rm=T)
      } # else
    } # for
    
    # Computing the standard deviation for each y
    y_prob <- na.omit(predict(logit_model, newdata = datTrain1, type="response")); y_logit <- log(y_prob/(1-y_prob)) # Standard deviation of predictions
    sd_y <- sd(y_logit) 
    # Computing the coefficient of determination
    r2 <- coefDeter_glm(logit_model)
    # Computing the variable importance
    results$data$Value <- coefficients_summary$coefficient[coefficients_sig_data_index==1] * r2 * (sd_x/sd_y)
    
    # Calculating importance measure and preparing result set
    results$data <- copy(coefficients_summary)[names %in% coefficients_sig_model]
    results$data[,Value:=abs(coefficient/se)] # Compute the importance measure
    results$data[,`:=`(coefficient=NULL,se=NULL, sig=NULL)]; colnames(results$data) <- c("Variable", "Value")
    results$Method <- "Standardised Coefficients: Menard"
    
  } else if (method=="partDep") { # - Variable importance as determined by feature importance rank measure (FIRM) (explainable AI technique)
    
    # Calculating importance measure and preparing result set
    results$data <- vip::vi_firm(logit_model, feature_names=coefficients_sig_data, method="firm", train=datTrain1)
    
    # Calculating (and) the partial dependence
    if (pd_plot==T){
      for (i in seq_along(coefficients_sig_model)){
        datPlot_pd <- data.table(attr(results$data, which = "effects")[[i]])
        names(datPlot_pd) <- c("x","y")
        (results$plots[[paste0("pd_",coefficients_sig_model[i])]] <- ggplot(datPlot_pd) + geom_point(aes(x=x,y=y)) + theme_minimal() +
            labs(x=coefficients_sig_data[i],y="yhat") + theme(plot.title = element_text(hjust=0.5)))
      } # for
    } # if
    
    # Adding a column to indicate the rank order of the variables' importance and getting the data in the desired format
    results$data <- data.table(results$data) %>% rename(Value = Importance)
    results$Method <- "Partial Dependence (FIRM)"
    
  } else {stop(paste0('"', method,'" is not supported. Please use either "stcCoef_ZScores", "stdCoef_Goodman", "stdCoef_Menard", or "pd" as method.'))}# if else (method)
  
  # - Ranking the variables according to their associated importance measure values
  results$data[,Value_Abs:=abs(Value)]
  results$data <- results$data %>% arrange(desc(Value_Abs)) %>% mutate(Rank=row_number()) %>% as.data.table()
  
  # - Calculate contribution degrees to sum of importance measure across all variables
  # NOTE: These contributions are merely ancillary and for graphing purposes.
  # They should not considered too seriously, unless studied more extensively.
  sumVarImport <- sum(results$data$Value_Abs, na.rm=T)
  results$data[, Contribution := Value_Abs / sumVarImport]
  
  # - Post results to console
  print(results$data)
  
  # --- 3. Creating a general plot of the variable importance (if desired)
  if (impPlot==T){
    # Generic variable importance plot
    (results$plots[["Ranking"]] <- ggplot(results$data, aes(x=reorder(Variable, Value_Abs))) + geom_col(aes(y=Value_Abs, fill=Value_Abs)) +
       coord_flip() + theme_minimal() + theme(text=element_text(family=chosenFont)) +
       labs(x="Variable name", y=results$Method))
    
    # - create graphing object based on the top [limitVars]-number of variables
    datGraph <- results$data[1:min(.N, limitVars), ]
    
    # - cull away lengthy names that will otherwise ruin the graph
    datGraph[, Variable_Short := ifelse(str_length(Variable) >= 18, paste0(substr(Variable, 1, 18),".."), Variable)]
    
    # Generic variable importance plot
    results$plots[["Ranking"]] <- ggplot(datGraph, aes(x=reorder(Variable_Short, Value_Abs))) + theme_minimal() + theme(text=element_text(family=chosenFont)) + 
      geom_col(aes(y=Value_Abs, fill=Value_Abs)) + geom_label(aes(y=sumVarImport*0.05, label=paste(percent(Contribution, accuracy=0.1)), fill=Value_Abs), family=chosenFont) + 
      annotate(geom="text", x=datGraph[.N, Variable_Short], y=sumVarImport*0.3, label=paste0("Variable Importance (sum): ", comma(sumVarImport, accuracy=0.1)), family=chosenFont, size=3) + 
      coord_flip() + scale_fill_distiller(palette=colPalette, name="Absolute value", direction=colPaletteDir) +
      scale_colour_distiller(palette=colPalette, name="Absolute value", direction=colPaletteDir) + 
      labs(x="Variable name", y=results$Method)
    
    # Show plot to current display device
    print(results$plots[["Ranking"]])
    
    cat("Saving plot of variable importance at: ", plotName, "\n")
    # - Save graph
    ggsave(results$plots[["Ranking"]] , file=plotName, width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
    
  }
  
  # - Return results
  return(results)
  # rm(logit_model, datTrain1, datTrain2, method, impPlot, coefficients_sig_model, coefficients_sig_data, sumVarImport, limitVars, coefficients_data, coefficients_sig_data, coefficients_summary, coefficients_sig_model_level)
}



# --- Residual Deviance measures
# Perform residual analysis for a glm-model using deviances (difference between predicted probabilities and observed proportions of success)
# A standard normal distribution approximates the residual deviance distribution for a well-fitted model (assuming logistic regression)
# Accordingly, min/max residuals should lie within [-3,3], median should be close to 0, and 1st/3rd quantiles 
# should be similarly in their absolute value.
# Deviations from these principles indicate strain in the underlying fit of the model
# see https://library.virginia.edu/data/articles/understanding-deviance-residuals ; https://web.pdx.edu/~newsomj/cdaclass/ho_diagnostics.pdf
resid_deviance_glm <- function(model, err_Median = 0.025, err_quantiles = 0.05, plotResid=T, inf_degree_significance=0.01) {
  
  # - Preliminaries
  require(scales) # formatting numbers
  require(dplyr)
  
  # - Safety check
  if (!any(class(model) == "glm")) stop("Specified model object is not of class 'glm' or 'lm'. Exiting .. ")
  
  # - testing conditions
  # model <- logit_model
  
  # -- 1a. Using built-in functionality to calculate deviance residuals and summarise them accordingly
  d_aggr1 <- quantile(residuals(model))
  
  # -- 1b. Manual calculation of the above (for verification purposes)
  # NOTE: this process uses several types of residuals, which we'll illustrate here assuming logistic regression
  
  # 0) get predictions and observations (y)
  p_hat <- predict(model, type = "response"); y <- model$y
  
  # 1) raw residuals: the difference between observed values {0,1} and predicted probabilities of belonging to a binary-valued class
  e <- residuals(model, type = "response") # or simply e = y - p_hat where y is the observed binary-valued outcome \in {0,1} and e \in [-1,1]
  
  # 2) Pearson residuals: rescaled version of raw residuals by dividing it with the standard deviation of a binomial distribution (if using logistic regression)
  r <- e / sqrt(p_hat * (1 - p_hat)) # or simply r <- residuals(model, type = "pearson")
  
  # 3) Calculate "hatvalues", which is the "degree to which observation y_i influences fitted/predicted/class-probability \hat{y}_i
  # NOTE: hatvalue h \in [0,1] is the leverage score for ith observation (related to Mahalanobis distance),defined as
  # h = x_i^T . (X^T . X)^{-1} . x_i  , where x_i is the ith observation and X is the n x p design matrix whose rows correspond to observations (input space) and columns are variables
  # see https://en.wikipedia.org/wiki/Leverage_(statistics)#Definition_and_interpretations ; https://stats.stackexchange.com/questions/551302/developing-leverage-statistics-manually-in-r
  # see https://pj.freefaculty.org/guides/stat/Regression/RegressionDiagnostics/OlsHatMatrix.pdf
  # High hat-values indicate greater leverage/influence of the associated observation relative to the mean
  # NOTE: Calculating hatvalues is conceptually easy, e.g., X <- model.matrix(model); h <- diag(X %*% solve(t(X) %*% X) %*% t(X))
  # However, singular matrices (non-invertible, i.e., where det(cov(X)) == 0) can hinder its calculation in practice, particulalry when trying to invert "t(X) %*% X" using "solve()"
  # This can be mitigated by applying a small adjustment within the "solve()"-part using ridge regression, or by the LASSO;
  # However, this is exhaustive and we rather rely on R's built-in functionality that already caters exhaustively for these issues.
  h <- hatvalues(model)
  # Influential observations can be highlighted for those cases 
  # see https://stackoverflow.com/questions/9476475/how-to-produce-leverage-stats
  inf_degree <- NROW(h[h> (3 * mean(h))]) / NROW(h)
  inf_degree_max <- max(h) / mean(h)
  
  # 4) standardised/studentized Pearson residuals: adjusting the Pearson residual for leverage (or "hat values")
  # NOTE: These residuals are usually standard normally distributed, which can be a useful diagnostic in and of itself; see Agresti2002
  rs <- r / sqrt(1 - h) # or simply rs <- rstandard(m, type = "pearson") 
  
  # 5) deviance residuals (finally): derived from the likelihood ratio test when comparing a candidate to a saturated/full/perfect model (such that p coefficients = n observations)
  d <- sign(e)*sqrt(-2*(y*log(p_hat) + (1 - y)*log(1 - p_hat)) ) # or simply as residuals(model)
  d_aggr <- quantile(d)
  # [SANITY CHECK] Distribution summary of residual deviances should agree with each other, respective to both methods by which they are calculated.
  cat( all.equal(d_aggr1, d_aggr) %?% 'SAFE: Both methods by which residual deviances are calculated agree with each other in result.\n' %:% 
         'WARNING: The methods by which residual deviances are calculated yield different results.\n')
  
  # -- 2. Reporting results
  cat( (inf_degree <= inf_degree_significance) %?% 
         paste0('SAFE: The degree of influential cases is low at ', percent(inf_degree, accuracy=0.1), ' where hatvalues h > (3*mean(h)) is true.\n---------\n') %:%
         paste0('WARNING: High degree of influential cases at ', percent(inf_degree, accuracy=0.1), ' where hatvalues h > (3*mean(h)) is true. \n\tAlso, max(hatvalue) / mean(hatvalue) = ', 
                comma(inf_degree_max), '; whereas rule-of-thumb is 3.\n---------\n') )
  cat("Residual deviance (difference between observed and predicted; smaller = better):", comma(sum(d^2)), '\n---------\n')
  # [DIAGNOSTIC] Absolute values of min and max percentiles <= 3 ?
  cat( (abs(d_aggr[1]) <= 3 & abs(d_aggr[5]) <= 3) %?% 'SAFE: Min/max residual deviances are within expected bounds (<=3 in absolute value); model fit is adequate.\n' %:%
         'WARNING: Min/max residual deviances are outside expected bounds (<=3 in absolute value); model fit is somewhat strained.\n')
  # [DIAGNOSTIC] median residual deviance close to 0 ?
  cat( (abs(d_aggr[3]) <= err_Median) %?% 
         paste0('SAFE: Median residual deviance (', comma(abs(d_aggr[3]), accuracy=0.01), ') is sufficiently close to zero; model fit is adequate.\n') %:%
         paste0('WARNING: Median residual deviance (', comma(abs(d_aggr[3]), accuracy=0.01), ') is not zero; model fit is somewhat strained.\n') )
  # [DIAGNOSTIC] 1st and 3rd percentile is relatively close to one another, indicating a symmetric distribution ?
  cat( (abs(d_aggr[2]) - abs(d_aggr[4]) <= err_quantiles) %?% 'SAFE: 1st/3rd quantiles of residual deviances are sufficiently close to each in absolute value; model fit is adequate.\n---------\n' %:%
         'WARNING: 1st/3rd quantiles of residual deviances differ substantially from each other in absolute value; model fit is somewhat strained.\n---------\n' )
  # [DIAGNOSTIC] Degree to which hat values (leverage scores) exceed a common rule of thumb (> 3 x mean(h))
  
  # -- 3. Graph distritubion of residuals
  if (plotResid) { hist(d, breaks="FD") }
  
  return(d_aggr)
  
  # -- cleanup (only relevant whilst debugging this function)
  rm(e,d,p_hat,y,r,rs,err_Median,err_quantiles,model,X,h,inf_degree_significance)
}
# - Unit test
# install.packages("ISLR"); require(ISLR)
# datTrain <- data.table(ISLR::Default); datTrain[, `:=`(default=as.factor(default), student=as.factor(student))]
# logit_model <- glm(default ~ student + balance + income, data=datTrain, family="binomial")
# summary(logit_model)
# resid_deviance_glm(logit_model)
### RESULTS: candidate's max residual > 3, which indicates some strain.
# distributional shape somewhat skew since abs(1st) != abs(3rd) quantiles



# ------------------------- Generalised Youden Index Function ---------------------------
# - This function runs an optimisation procedure to find the Generalised Youden Index for a trained model.
### INPUT:
# - Trained_Model: the trained classifier for which you want to obtain the optimal cutoff p_c
# - Train_DataSet: The training dataset (in datatable format) which will be used to find p_c
# - Target: Character string containing the name of the target variable (target variable should be numeric 0/1)
# - a: The cost multiple (or ratio) of a false negative relative to a false positive
### OUTPUT: 
# - The output of the optimisation procedure; i.e., the optimal cut-off p_c and other information detailing whether the 
#   algorithm converged.

Gen_Youd_Ind<-function(Trained_Model, Train_DataSet, Target, a){
  # Trained_Model<-logitMod_Adv
  # Train_DataSet<-datCredit_train[960000:965000,]
  # Target<-"DefaultStatus1_lead_12_max"
  # a<-4
  
  require(data.table, DEoptimR)
  
  # - ensure given target name does not coincide with the intended name used internally in this function
  # If not, then ensure the field doesn't already exist
  if (Target != "Target" & "Target" %in% colnames(Train_DataSet)){
    Train_DataSet[, Target := NULL]
  }
  
  # - ensure target variable is numeric (and not factor)
  if (class(Train_DataSet[,get(Target)]) == "factor") {
    Train_DataSet[, Target := as.numeric(levels(get(Target)))[get(Target)]]
  } else Train_DataSet[, Target := get(Target)]
  
  # - Calculate Prevalence Rate q1
  q1 <- mean(Train_DataSet$Target,na.rm=TRUE)
  
  # - Objective Function to be minimized (negative the function to be maximized)
  GYI_a <- function(pc){
    Train_DataSet[, prob_vals := predict(Trained_Model, Train_DataSet, type="response")] # Obtain predicted probabilities for the model
    Train_DataSet[, class_vals := ifelse(prob_vals<=pc,0,1)] # Dichotomise the probability scores according to the cutoff pc
    
    # - Safety Check for missingness in predictions
    if(anyNA(Train_DataSet[,list(prob_vals,class_vals)])){
      stop("Missingness in predicted probabilities, Exit function...")
    }
    
    # - Calculate the True Positive Rate & True Negative Rate given pc
    TPR<-sum(Train_DataSet[,class_vals]==1 & Train_DataSet[,Target==1], na.rm=TRUE)/Train_DataSet[Target==1,.N]
    TNR<-sum(Train_DataSet[,class_vals]==0 & Train_DataSet[,Target==0], na.rm=TRUE)/Train_DataSet[Target==0,.N]
    
    # - Clean Up the Created Input Fields
    Train_DataSet[, prob_vals := NULL]
    Train_DataSet[, class_vals := NULL]
    
    # - The function to minimize
    -(TPR + (1-q1)/(a*q1)*TNR - 1)
  }
  # - Run Optimisation via a Differential Evolution algorithm
  results <- JDEoptim(lower=0, upper=1, fn=GYI_a)
  #optim(par=c(0,1), fn=GYI_a, lower=0, upper=1)
  
  return(list(cutoff=results$par, value=results$value, iterations=results$iter))
}
# # - Unit test
# require(ISLR); require(OptimalCutpoints); require(DEoptimR) # Robust Optimisation Tool		
# datTrain <- data.table(ISLR::Default); datTrain[, `:=`(default=ifelse(default=="No",0,1), student=as.factor(student))]
# datTrain[, default_fac := as.factor(default)]
# logit_model <- glm(default ~ student + balance + income, data=datTrain, family="binomial")
# # - optimal.cutpoints function from OptimalCutpoints
# datTrain[, prob_vals := predict(logit_model, type="response")]
# opti<-optimal.cutpoints(X = "prob_vals", status = "default", tag.healthy = 0, methods = "Youden", data = datTrain, ci.fit = FALSE, trace = FALSE, control = control.cutpoints(CFP=1, CFN=4, generalized.Youden=T))
# summary(opti) # Optimal Cut-off = 0.2127908; Optimal Criterion = 6.6734234
# # - Custom Gen_Youd_Ind function
#Gen_Youd_Ind(logit_model,datTrain,"default",4) # Optimal Cut-off = 0.2120438; Optimal Criterion = -6.673423
#Gen_Youd_Ind(logit_model,datTrain,"default_fac",4) # Optimal Cut-off = 0.2120438; Optimal Criterion = -6.673423
