# ================================= DELINQUENCY MEASURES ================================
# Function definitions of various delinquency measures
# ---------------------------------------------------------------------------------------
# SCRIPT AUTHOR(S): Dr Arno Botha
# VERSION: 1.4 (Aug-2021)
# DESCRIPTION: 
# This script contains function definitions for constructing the g_1, g_2, and g_3 
# delinquency measures, as well as k-based curing measurement
# See Botha, A., Beyers, C., De Villiers, P. (2021). Simulation-based optimisation of 
#   the timing of loan recovery across different portfolios. Expert Systems with Applications.
#   https://doi.org/10.1016/j.eswa.2021.114878
# =======================================================================================

 


# ========= Function Declarations

# ==== Function: Calculate CD (g1: Contractual Delinquency)
# - Inputs the following:
# 1) vec.Instal: a vector of fixed instalments, one for each loan account within a loan portfolio
# 2) mat.Receipt: a matrix of time-indexed cashflows/receipts (starting at t=0), each column corresponding to the history of each account
# 3) sc.Thres: a scalar threshold above which the repayment ratio is considered current and beneath which it is considered delinquent
# 4) period: a scalar indicating the contractual period of all loans
# 5) n: the number of loans within the portfolio
# 6) method: arguments include either 'base' or 'simple':
#    'base' -> construct the robust g_1 measure
#    'simple' -> construct the simpler arrears / instalment ratio and take the ceiling thereof (not used)
calculate.CD <- function(vec.Instal, mat.Receipt, sc.Thres, period, n, method="base") {
  
  # - prepare various working matrices, to be filled later
  mat.CD <- matrix(-1, nrow=period+1, ncol=n); #include time 0
  mat.CD.c <- mat.CD;
  mat.CD.d <- mat.CD;
  mat.CD.m <- mat.CD;
  
  # - two methods are implemented: 'base' (default) and 'simple'
  if (method == "base") {
    
    # - calculate repayment ratio h_t at each time point for each loan
    mat.RepayRatio <- sapply(1:n, function(i,R,I) {
      val <- c(0,R[1:period,i] / I[i])
      return(val)
    }, R=mat.Receipt, I=vec.Instal)
    
    # - create a matrix containing Boolean-values of the decision function d_1(t,i), at every period t of each loan i
    mat.CD.d <- ifelse(mat.RepayRatio < sc.Thres, 1, 0)
    
    # - create a matrix containing integer-values of the m(t,i) function, i.e., the reduction in accrued delinquency at every period t of each loan i
    mat.CD.m <- floor(mat.RepayRatio / sc.Thres)*(1-mat.CD.d) - 1
    
    # - let g_1(t) at t=0 (row=1) be equal to zero, by mathematical design
    mat.CD[1,] <- 0
    
    # - finally, create the g_1 measure at each period t simultaneously across all loans (vectorised approach)
    for (ii in 2:(period+1)) {
      # - fill in at period t=ii the Boolean-values of the decision function d_2, across all loans
      mat.CD.c[ii,] <- ifelse(mat.CD[ii-1,] == 0, 1, 0)
      # - now construct g_1 at period t=11 simultaneously across all loans
      mat.CD[ii,] <-  pmax(0, mat.CD.d[ii,]*mat.CD.c[ii,] + (1-mat.CD.c[ii,])*(mat.CD[ii-1,] - mat.CD.m[ii,]))
    }
    
  } else if (method == "simple") {
    
    # - convert the previous instalment vector into a time-indexed matrix of instalments (repeat the value across all periods)
    mat.Instal <- sapply(1:n, function(i,I) {
      return(c(0,rep(I[i],period)))
    },I=vec.Instal);
    
    # - compute the differences between instalments and receipts at each period for each loan
    mat.Diff <- mat.Instal - rbind(rep(0,n),mat.Receipt);
    
    # - create the cumulative sum of these differences, i.e., the arrears balance
    mat.Arrears <- sapply(1:n, function(i,y) {
      bal <- cumsum(y[,i])
      return(bal)
    }, y=mat.Diff)
    
    # - simply divide the accumulated arrears with the fixed instalment, and take the ceiling hereof
    # as the number of payments in arrears
    mat.CD <- ceiling(mat.Arrears %*% diag(1/vec.Instal))
  }
  
  return (mat.CD)
}





# ==== Function: Calculate CD (g1: Contractual Delinquency)
# - Inputs the following:
# 1) ins: instalment
# 2) rec: cash flow/receipt
# 3) prev.cd: g1-delinquency measurement at previous period
# 4) t: current time period
# 5) sc.Thres: a scalar threshold above which the repayment ratio is considered current and beneath which it is considered delinquent
calculate.CD_t <- function(ins, rec, prev.CD=0, t=1, sc.Thres=0.9) {
  
  if (t == 0 ){
    g_1 <- 0
  } else {
    
    # - calculate repayment ratio h_t
    repayRatio <- rec / ins
    
    # - Boolean-values of the decision function d_1(t,i)
    g_1.d <- ifelse(repayRatio < sc.Thres, 1, 0)
    
    # - the m(t,i) function, i.e., the reduction in accrued delinquency
    g_1.m <- floor(repayRatio / sc.Thres)*(1-g_1.d) - 1
      
    # - the decision function d_2(t,i)
    g_1.c <- ifelse(prev.CD == 0, 1, 0)
    
    # - finally construct g_1
    g_1 <- max(0, g_1.d*g_1.c + (1-g_1.c)*(prev.CD - g_1.m))
  }
  
  return (g_1)
}




# ==== Function: Calculate CD (g1: Contractual Delinquency), assuming variable instalments from to real data
# - Inputs the following:
# 1) mat.Instal: a matrix of time-indexed variable instalments, each column corresponding to each account within a wider loan portfolio
# 2) mat.Receipt: a matrix of time-indexed cashflows/receipts, each column corresponding to each account
# 3) sc.Thres: a scalar threshold above which the repayment ratio is considered current and beneath which it is considered delinquent
# 4) period: a scalar indicating the contractual period of all loans
# 5) n: the number of loans within the portfolio
# 6) method: arguments include either 'base' or 'simple':
#    'base' -> construct the robust g_1 measure
#    'simple' -> construct the simpler arrears / instalment ratio and take the ceiling thereof (not used)
calculate.CD.forData <- function(mat.Instal, mat.Receipt, sc.Thres, period, n, method="base") {
  
  # - prepare various working matrices, to be filled later
  mat.CD <- matrix(-1, nrow=period+1, ncol=n); #include time 0
  mat.CD.c <- mat.CD;
  mat.CD.d <- mat.CD;
  mat.CD.m <- mat.CD;
  
  # - two methods are implemented: 'base' (default) and 'simple'
  if (method == "base") {
    
    # - calculate repayment ratio h_t at each time point for each loan
    mat.RepayRatio <- sapply(1:n, function(i,R,I) {
      
      ratios <- ifelse(I[1:period,i] > 0,  R[1:period,i] / I[1:period,i], 
                       ifelse(R[1:period,i] > 0, R[1:period,i] / 0.0001, sc.Thres)
      );
      val <- c(0,ratios) # pad with zeros at origination (t=0)
      return(val)
    }, R=mat.Receipt, I=mat.Instal)
    
    # - create a matrix containing Boolean-values of the decision function d_1(t,i), at every period t of each loan i
    mat.CD.d <- ifelse(mat.RepayRatio < sc.Thres, 1, 0)
    
    # - create a matrix containing integer-values of the m(t,i) function, i.e., the reduction in accrued delinquency at every period t of each loan i
    mat.CD.m <- floor(mat.RepayRatio / sc.Thres)*(1-mat.CD.d) - 1
    
    # - let g_1(t) at t=0 (row=1) be equal to zero, by mathematical design
    mat.CD[1,] <- 0
    
    # - finally, create the g_1 measure at each period t simultaneously across all loans (vectorised approach)
    for (ii in 2:(period+1)) {
      # - fill in at period t=ii the Boolean-values of the decision function d_2, across all loans
      mat.CD.c[ii,] <- ifelse(mat.CD[ii-1,] == 0, 1, 0)
      # - now construct g_1 at period t=11 simultaneously across all loans
      mat.CD[ii,] <-  pmax(0, mat.CD.d[ii,]*mat.CD.c[ii,] + (1-mat.CD.c[ii,])*(mat.CD[ii-1,] - mat.CD.m[ii,]))
    }
    
  } else if (method == "simple") {
    
    # - compute the differences between instalments and receipts at each period for each loan
    mat.Diff <- mat.Instal - rbind(rep(0,n),mat.Receipt);
    
    # - create the cumulative sum of these differences, i.e., the arrears balance
    mat.Arrears <- sapply(1:n, function(i,y) {
      bal <- cumsum(y[,i])
      return(bal)
    }, y=mat.Diff)
    
    # - simply divide the accumulated arrears with the fixed instalment, and take the ceiling hereof as the number of payments in arrears
    mat.CD <- ceiling(mat.Arrears %*% diag(1/vec.Instal))
  }
  
  return (mat.CD)
}






# ==== Function: Calculates g2: Macaulay Duration Index (MD-measure), g3: Degree of Delinquency (DoD-measure)
# - Inputs the following:
# 1) vec.Instal: a vector of fixed instalments, one for each loan account within a loan portfolio
# 2) mat.Receipt: a matrix of time-indexed cashflows/receipts (starting at t=0), each column corresponding to the history of each account
# 3) vec.Principal: a vector of loan amounts/principals, constituting individual accounts within a loan portfolio
# 4) period: a scalar indicating the contractual period of all loans
# 5) n: the number of loans within the portfolio
# 6) i.rate: either a single effective rate (to be repeated across the portfolio) or a vector of effective rates per annum, each element corresponding to the rate of a loan account
# 7) vec.DoD.lambda: a vector of account-level multipliers by which to stress delinquency in g3. This is the lambda-function defined in equation 22
calculate.MDoD <- function(vec.Instal, mat.Receipt, vec.Principal, period, n, i.rate, vec.DoD.lambda) {

  # - if a vector of interest rates are given, then use that, otherwise repeat the given interest rate across the portfolio
  if (NROW(i.rate) == 1) {
    vec.i <- rep(i.rate, n)
  } else {
    vec.i <- i.rate
  }
  
  # - convert given effective rates per annum to nominal rate per period (convertibly monthly per annum, in this case)
  i_p.rate <- ((1+vec.i)^(1/12) - 1)*12
  
  # - transform the given instalment vector to a matrix by repeating the level instalment across the history of each loan account, which 
  # is mapped to each column in the matrix (loan term, and therefore number of rows, is the same for each account by design)
  # Also, include time 0 as the first row
  mat.Instal <- sapply(1:n, function(i,I) {
    return(c(0,rep(I[i],period))) # instalment at time t=0 is 0 by design
  },I=vec.Instal);
  
  # - calculate the present value (at time 0) of every instalment of every loan account: row=period, column=account
  # Also, include time 0 as the first row
  mat.InstalPV <- sapply(1:n, function(i,I) {
    return(c(0, (1+i_p.rate[i]/12)^(-1*(1:period)) * I[i]))
  },I=vec.Instal);
  
  # - calculate \delta_t as the difference between I_t and R_t for each loan account at each period t=0,...,T
  mat.Diff <- mat.Instal - rbind(rep(0,n), mat.Receipt);
  
  # - Calculate expected duration: Part 1 (standard Macaulay Duration, but without summing)
  # 1) Weigh each discounted instalment of an account by the loan principal, at each period
  # 2) Multiply this with time elapsed (loan age) per annual period
  MDoD.ExpWeightTime <- (mat.InstalPV %*% diag(1/vec.Principal)) * (0:period / 12);
  
  # - create various matrices to be populated later
  MDoD.ActWeightTime <- MDoD.ExpWeightTime # to be modified recursively later, based on actual experience (receipts)
  MDoD.CashPV <- mat.InstalPV
  vec.term <- rep(period,n) # assume all accounts have the same contractual term
  
  # - create a copy of (undiscounted) instalments, such that the last instalment of each account
  # can be updated with arrears recursively later
  MDoD.CashFlow.Star <- mat.Instal
  
  # - create matrices containing the eventual expected and actual duration values that are recursively calculated later
  MDoD.ExpDuration <- matrix(-1.00, nrow=period+1, ncol=n)
  MDoD.ActDuration <- MDoD.ExpDuration # basically serves line 9's purpose in g3's algorithm
  
  # - create a vew n-sized vectors with one value per account within a portfolio
  vec.Term.Star <- vec.term # a vector of the behavioural term, set to the contractual term, only to be incremented gradually once an account becomes out-of-contract
  vec.InContract <- rep(1,n) # a vector of Boolean flags indicating whether an account is still within its contractual tenure (1) or not (0)
  vec.saved.Arrears <- rep(0.00, n) # a vector of accumulated arrears to be added to the last contractually expected instalment
  
  # - main loop: iterate by period, progressing through the history of a loan
  for (i in 1:(period+1)) {
    
    # -- g3-specific
    # - retrieve the last instalment (previously modified or not) of each account for modification (if necessary)
    # This is line 4 in g3's algorithm, calculating \alpha
    vec.saved.Arrears <- MDoD.CashFlow.Star[matrix(c(vec.Term.Star+1,1:n),nrow=n)]
    
    # -- g3-specific
    # - update the behaviour term of each loan (if necessary)
    # If an account is out-of-contract, then increment the behavioural term, otherwise, simply return the contractual term
    # This is line 5 in g3's algorithm, calculating \Tau
    vec.Term.Star <- sapply(1:n, function(k,y) { 
      val <- period
      if (y[k] == 0) val <- i -1
      return(val) },y=vec.InContract)
    
    # -- g3-specific
    # - determine if an account is out-of-contract: if yes, return 0, otherwise return 1
    # The current period i is compared to the contractual term, whilst adjusting for t=0 included in all matrices as row 1
    # This is implementing the decision function \delta_3() used in constructing g3
    vec.InContract <- ifelse(i > (vec.term + 1), 0, 1)
    
    # -- g3-specific
    # - only execute for t>=1 (ignore origination time point)
    if (i > 1) {
      
      # - If in-contract, then add arrears (if any) to last contractually expected instalment per account, after accumulating these arrears for one period with interest.
      # - If out-of-contract, then accumulate the previously-modified last instalment for one period with interest 
      # This is line 6 in g3's algorithm, calculating I'_(\Tau).
      MDoD.CashFlow.Star[matrix(c(vec.Term.Star+1,1:n),nrow=n)] <- mat.Diff[i,] * (1+vec.delta_pp)^(vec.Term.Star-i+1) +
        MDoD.CashFlow.Star[matrix(c(vec.Term.Star+1,1:n),nrow=n)] * vec.InContract + 
        vec.saved.Arrears * (1-vec.InContract) * (1+vec.delta_pp)
      
    }
    
    # - iterate for each account (across columns), at this particular time period i
    for (j in 1:n) {  
      
      # - only execute for t>=1 (ignore origination time point)
      if (i>1) {
        
        # - calculate remaining discounting periods for jth loan as at time i, based on in-contract Boolean flags, whilst adjusting for t=0 included in all matrices as row 1
        # This is line 7 in g3's algorithm, calculating \beta(m)
        periods <- (0:(vec.Term.Star[j]-i+1)) + (1-vec.InContract[j])
        
        # - calculate discount factors for jth loan, corresponding to the remaining discounting periods, as at time i
        discount <- (1+i_p.rate[j]/12)^(-periods)
        
        # - Recalculate the expected Macaulay Duration as at time i for the jth's loan remaining expected (and unmodified) instalments
        # This is line 8 in g3's algorithm, calculating f_{ED}(t=i) - before summation
        MDoD.ExpWeightTime[i:(vec.Term.Star[j]+1),j] <- mat.Instal[i:(vec.Term.Star[j]+1),j] * discount / vec.Principal[j] * (periods)/12
        
        # - Recalculate the present value of each element within the jth loan's remaining and future cash flows, as at time i
        # Note that this may include the arrears-modified last instalment (i.e., line 6's effect in g3's algorithm)
        MDoD.CashPV[i:(vec.Term.Star[j]+1),j] <- discount * MDoD.CashFlow.Star[i:(vec.Term.Star[j]+1),j]
        
        # - Recalcualte the actual Macaualy Duration as at time i for the jth's loan remaining future cash flows
        # This is line 10 in g3's algorithm, calculating f_{AD}(t=i) - before summation
        MDoD.ActWeightTime[i:(vec.Term.Star[j]+1),j] <- MDoD.CashPV[i:(vec.Term.Star[j]+1),j] / vec.Principal[j] * (periods)/12
      }
      
      # -- Calculate the Expected/Actual Duration vectors for kth loan
      # - sum across remaining periods as at time i, for final step in calculating Macaulay Duration
      MDoD.ExpDuration[i,j] <- sum(MDoD.ExpWeightTime[i:(vec.Term.Star[j]+1),j])
      MDoD.ActDuration[i,j] <- sum(MDoD.ActWeightTime[i:(vec.Term.Star[j]+1),j])
      
    } # exit inner loop
  } # exit main loop
  
  # - create a final matrix containing measurements from the g2 function: (row=period, column=loan)
  mat.MD <- MDoD.ActDuration / MDoD.ExpDuration
  
  # - create Boolean-valued decision matrix, filled with values from the decision function d4
  MDoD.ExceedsExp <- ifelse(MDoD.ActDuration > MDoD.ExpDuration, 1, 0)
  
  # - create a final matrix containing measurements from the g3 function: (row-period, column=loan)
  mat.DoD <- MDoD.ActDuration / MDoD.ExpDuration * (MDoD.ExceedsExp %*% diag(vec.DoD.lambda) + 1)
  
  # - Failsafe: eliminate negative values in the MD-matrix and DoD-matrix
  # This may happen due to bad data input (e.g., negative instalments)
  mat.MD <- ifelse(mat.MD >= 0, 1, 0)*mat.MD
  mat.DoD <- ifelse(mat.DoD >= 0, 1, 0)*mat.DoD
  
  # - Failsafe: eliminate NaNs in the MD-matrix and DoD-matrix by replacing them with NAs (missing values)
  mat.MD[is.na(mat.MD)] <- NA
  mat.DoD[is.na(mat.DoD)] <- NA
  
  return(list(MD=mat.MD,DoD=mat.DoD, ActDur=MDoD.ActDuration, ExpDur=MDoD.ExpDuration))
}





# ==== Function: Calculates various various delinquency- and curing-based flags and counters given g-measurable delinquency
# Operates at loan-level
# Inputs: [d]: delinquency threshold (decision parameter); [k]: probation period in months (decision parameter); 
# [delinq]: an account's g-measurable delinquency over time; [loanParams]: named list containing the label and value for the account;
# [age]: loan age vector to facilitate merging later on the account-age level
# Outputs: an account-level dataset containing periodic flags and counters to track default, k-curability, and k-cured stat
DelinqCure <- function(delinq, age, loanParams=list(Label="LoanID",Value=1, DelinqLabel="g1_Delinq", AgeLabel="Age"), d=3, k=6) {
  
  # - Testing purposes:
  # d <- 3; k <- 6; loanParams <- list(Label="LoanID",Value=1, DelinqLabel="g1_Delinq", AgeLabel="Age")
  # delinq <- c(0,0,0,1,1,2,2,3,4,3,3,3,2,2,1,2,1,0,1,2,3,3,4,2,2,2,1,2,1,1,2,3,3,2,2,3,4,2,3,2,2,1,0,0,0,0,1,2,3); #Test1
  # delinq <- c(0,0,0,1,1,2,2,3,4,3,3,3,2,2,1,2,1,0,1,2,3); #Test2
  # delinq <- c(0,0,0,1,1,2,2,2,1,2,0,1,2,2,1,2,1,0,1,2,1); #Test3
  # age <- 1:length(delinq)
  
  require(data.table)
  
  # - Prepare vector and given loan parameters into a mini data.table for easier wrangling
  dat <- data.table(delinq, age); acc_period <- length(delinq)
  dat[, (loanParams[['Label']]) := (loanParams[['Value']])] # assign loan-identifier to facilitate merging outside of function call
  
  # - First condition: test delinquency at every available period against default threshold d
  dat[, DefaultTest0 := ifelse(delinq >= d, 1, 0)]
  
  # - Get the first default time (not affected by k's value)
  dat[, EnterDefault_Raw := ifelse(delinq == d & shift(delinq, n=1, type="lag") < d, 1, 0)] # intermediary calculation
  DefTime1 <- which(dat$EnterDefault_Raw == 1)[1]
  dat[, EnterDefault_Raw := NULL]
  
  # - Create various empty vectors to be filled hereafter
  dat[, TimeInDefault := rep(0, acc_period)]
  dat[, TimeinCurable := rep(0, acc_period)]
  dat[, DefEpisode_Num := rep(0, acc_period)]
  dat[, Status := rep("", acc_period)]
  dat[, DefaultStatus := rep(0, acc_period)]
  dat[, DelinqState := rep("", acc_period)]
  dat[, EnteredDefault := rep(NA, acc_period)]
  dat[, EnteredCurable := rep(NA, acc_period)]
  dat[, EnteredCured := rep(NA, acc_period)]
  
  # - Parameters and boolean switches
  bool_Default <- FALSE; bool_curable <- FALSE; bool_cured <- FALSE
  
  # - Main loop across all time periods
  for (tt in 1:acc_period) {
    
    if (tt < DefTime1 | is.na(DefTime1)){ # time leading up to first default episode (if it exists)
      
      # set main vectors accordingly
      dat[tt, TimeInDefault := NA]
      dat[tt, TimeinCurable := NA]
      dat[tt, DefEpisode_Num := 0]
      dat[tt, Status := case_when(delinq == 0 ~ "Current", delinq < d  ~ "Delinquent")]           
      dat[tt, DefaultStatus := 0]
      dat[tt, DelinqState := "S_P"] # (g,d)-performing state
      
    }
    else if (tt == DefTime1) { # just entered first (g,d)-default state S_D
      
      dat[tt, EnteredDefault := 1] # set event flag
      
      # reset counters
      DefaultCounter <- 1
      timeInDef <- 0
      timeInCurable <- 0
      
    }
    
    if (dat[tt, DefaultTest0] == 1) { # applicable while in a (g,d)-default state S_D
      
      # check if previously k-cured
      if (bool_cured) { # just entered (g,d)-default state S_D again from a k-cured state S_P
        
        dat[tt, EnteredDefault := 1] # set event flag
        DefaultCounter <- DefaultCounter + 1 # register newest default episode
        
        # reset period counters
        timeInDef <- 0
        timeInCurable <- 0
        
      } else if (bool_curable) { # re-entered (g,d)-default state S_D from a k-curable state S_C, but remained in overall default
        
        # accrue overall time in default state and assign to first variable, and reset first variable
        timeInDef <- timeInDef + timeInCurable
        timeInCurable <- 0
        
      }
      
      timeInDef <- timeInDef + 1 # accrue time counter
      
      # set main vectors accordingly
      dat[tt, TimeInDefault := timeInDef]
      dat[tt, TimeinCurable := NA]
      dat[tt, DefEpisode_Num := DefaultCounter]
      dat[tt, Status := "Defaulted"]
      dat[tt, DefaultStatus := 1]
      dat[tt, DelinqState := "S_D"] # (g,d)-default state
      
      # set switches
      bool_Default <- TRUE; bool_curable <- FALSE; bool_cured <- FALSE
      
    } else if (bool_Default) { # just entered into k-curable state S_C from (g,d)-default state S_D
      
      dat[tt, EnteredCurable := 1] # set event flag
      timeInCurable <- 0 # reset k-curable time counter
      
      # set switches
      bool_Default <- FALSE; bool_curable <- TRUE; bool_cured <- FALSE
      
    }
    
    
    if (bool_curable) { # applicable while in k-curable state S_C
      
      timeInCurable <- timeInCurable + 1 # accrue time counter
      
      # test k-curability
      if (timeInCurable<=k) { # still in k-curable state S_C
        
        # set main vectors accordingly
        dat[tt, TimeInDefault := timeInDef + timeInCurable]
        dat[tt, TimeinCurable := timeInCurable]
        dat[tt, DefEpisode_Num := DefaultCounter]
        dat[tt, Status := "k-curable"]
        dat[tt, DefaultStatus := 1]
        dat[tt, DelinqState := "S_C"] # k-curable state
        
      } else { # just exited k-curable state and entered (g,d)-performing state S_P
        
        dat[tt, EnteredCured := 1] # set event flag
        
        # set switches
        bool_Default <- FALSE; bool_curable <- FALSE; bool_cured <- TRUE  
        
      }
    }
    
    if (bool_cured) { # applicable while in cured / (g,d)-performing state S_P
      
      # set main vectors accordingly
      dat[tt, TimeInDefault := NA]
      dat[tt, TimeinCurable := NA]
      dat[tt, DefEpisode_Num := DefaultCounter]
      dat[tt, Status := case_when(delinq == 0 ~ "Cured: current", delinq < d  ~ "Cured: delinquent")]
      dat[tt, DefaultStatus := 0]
      dat[tt, DelinqState := "S_P"] # (g,d)-performing state | k-cured
      
    }
  }
  
  # relabel delinquency and age vectors to given
  setnames(dat, "delinq", loanParams[['DelinqLabel']])
  setnames(dat, "age", loanParams[['AgeLabel']])
  
  return(dat)# return prepared dataset
}



# - ancillary function that returns the first starting point of the first default episode within a given loan's history,
# and each loans's term (to restrict search space)
default.start.first <- function(i, thres.d, del.mat, t) {
  # testing purposes
  # i <- 116; del.mat <- matCD.Use; thres.d <- d; t<-advance_adjterm_v
  
  # ------ tests:
  #--times-------------------------------0,1,2, 3,4, 5,6,7,8,9,  10,11,12,13,  14,15,16,17,18,19,20
  # test case (assume CD): test.del <- c(3,3,2, 3,2, 4,5,2,1,2,   3, 4, 3, 2,   4, 5, 6, 5, 4, 3, 2)
  #   with d=3, first default episode ought to be time=0
  #--times---------------------------------0,1,2,3,4,5, 6,7,8
  # test case 2 (assume CD): test.del <- c(0,0,1,2,1,0, 0,1,0)
  #   with d=1, first default episode ought to be time=2
  
  # find positions (times) in delinquency matrix where threshold is/was reached to be (g,d)-defaulting at t
  #   - this should include from time t=0 (at origination)
  vec.found <- which(del.mat[1:(t[i]+1),i] >= thres.d)
  # test case: vec.found <- which(test.del >= 3)
  # test case 2: vec.found <- which(test.del >= 1)
  
  if(length(vec.found) == 1) {
    # only one index found
    episodes.start <- vec.found
  } else {
    # 1. Find positions in these positions where the lagged difference is greater than 1.
    #   - these indicate 'breaks' between episodes 
    # 2. Add 1 to these found positions to move to the 'initial starting points' of the next episode in succession
    # 3. Pre-fix this vector with '1' to re-include the first 'episode' that was deselected previously
    # 4. Given this vector of indices, return starting positions again
    episodes.start <-  vec.found[c(1, which(diff(vec.found) > 1) + 1 )]
  }
  
  # return starting period of first episode (if it exists)
  #   - if it doesn't exist, return -1
  #   - subtract 1 to account for a skewed index that includes in its range t=0 as index=1, 
  #     as a result of using the entire range of {del.mat}
  first.start <- ifelse(length(vec.found) == 0, -1, episodes.start[1] - 1)
  return(first.start)
  #rm(del.mat);
}



# ==== Function: Calculates various various delinquency- and curing-based flags and counters given g-measurable delinquency
# Operates at portfolio-level [vectorised]
# Inputs: [d]: delinquency threshold (decision parameter); [k]: probation period in months (decision parameter); 
# [delinq]: an account's g-measurable delinquency over time; [loanParams]: named list containing the label and value for the account;
# [age]: loan age vector to facilitate merging later on the account-age level
# Outputs: various matrices: k-redefault (including 1st default), k-curability, and k-cured
DelinqCure_m <- function(d=3, k=6, matCD.Use, n.given, advance_adjterm_v) {
  
  # - Testing purposes:
  # k<-3
  # matCD.Use <- matrix(c(0,0,1, 2,3,3, 2,3,2, 2,2,3, 2,2,2, 1,0,0,     0,1,2, 3,3,3, 2,2,3, 3,2,2, 1,0,2, 3,3,3), nrow=18) # two hypothetical loans
  # advance_adjterm_v <- c(17,17); n.given <- NCOL(matCD.Use)
    
  # - get default start times of first episode (if multiple exist), given threshold d, otherwise return -1 to indicate a performing loan
  # Assuming t >= 0 to include origination, e.g., if default is at t=12, then position in delinquency matrix corresponds to 13
  default_firststart_v <- sapply(1:n.given, default.start.first, thres.d=d, del.mat=matCD.Use, t=advance_adjterm_v)

  
  # create empty state space matrix: 1: S_D (default), 2: S_C (k-curable), 3: S_P (Performing)
  matState <- matrix(0, nrow=NROW(matCD.Use), ncol=n.given)
  
  # create empty counter matrix for tracking time spent in k-curable state [S_C]
  matTimeCurable <- matrix(NA, nrow=NROW(matCD.Use), ncol=n.given)
  
  # create helper vectors
  TimeKCurable_v <- rep(NA,n.given) # Time spent in the k-curable state [S_C]
  KCurable_v <- rep(FALSE,n.given) # Boolean vector indicating whether a loan is in the k-curable state [S_C] at a particular point in the main loop (or period)

  # - Main loop, iterating across time (row), vectorised across loans (columns)
  for (tt in 1:NROW(matCD.Use)) {
    
    # - testing conditions
    # tt <- 6
    
    # -- Pre-default (1st episode)
    # Handle time leading up to first default episode (if it exists)
    # if the current period is less than the first default time, or if default has never been breached,
    # then assign state S_P (3), otherwise assign pending 'state' (0)
    matState[tt,] <- ifelse(tt < (default_firststart_v+1) | default_firststart_v<0, 3, 0)
    
    # -- First condition: test delinquency at every available period against default threshold d
    DefaultTest_v <- ifelse(matCD.Use[tt,] >= d, 1, 0)
    # Check default test and assign state S_D (1) accordingly
    matState[tt, which(DefaultTest_v == 1)] <- 1
    
    # -- Cull away elements beyond given event times (adjusted term) such that
    # a 'state' 0 is determinable in the latter part of this loop, otherwise NA
    terminated <- which(tt > (advance_adjterm_v+1))
    matState[tt, terminated] <- NA
    # also ensure those terminated are no longer tracked for k-curability
    KCurable_v[terminated] <- FALSE
    
    if (tt > 1) {
      
      # -- Second condition: Check k-curability across following steps
      
      # 1) INFLOW
      # 1a) Flag those loans that have newly progressed into the k-curable state [S_C], provided they were in the default state [S_D]
      k.new_curables <- which(matState[tt,] == 0 & matState[tt-1,] == 1)
      # 1b) reset time counters for these loans to 0
      TimeKCurable_v[k.new_curables] <- 0 # will be increased along with all other pre-existing k-curables later in this loop-iteration
      # 1c) set their switches accordingly to start tracking the number of periods/time
      KCurable_v[k.new_curables] <- TRUE
      
      # 2) OUTFLOW
      # 2a) Flag those loans that have regressed back into the default state [S_D] that were previously k-curable [S_C]
      redefaults <- which(matState[tt,] == 1 & matState[tt-1,] == 2)
      # 2b) reset time counters for these loans
      TimeKCurable_v[redefaults] <- NA
      # 2c) set their switches accordingly to stop tracking the number of periods/time
      KCurable_v[redefaults] <- FALSE
      
      # 3) Increase time counter for all loans in the k-curable state [S_C]
      TimeKCurable_v[KCurable_v] <- TimeKCurable_v[KCurable_v] + 1
      matTimeCurable[tt, KCurable_v] <- TimeKCurable_v[KCurable_v] # stamp history away into this matrix
      
      # 4) Decide k-curability
      # 4a) Evaluate against current k-value
      k.curables <- which(TimeKCurable_v <= k & KCurable_v) # still in k-curable state [S_C]
      k.cured <- which(TimeKCurable_v > k & KCurable_v) # newly exited the k-curable state into performing state [S_P]
      # 4b) Set states accordingly
      matState[tt, k.curables] <- 2
      matState[tt, k.cured] <- 3
      # 4c) Reset counters and switches for those newly k-cured loans
      TimeKCurable_v[k.cured] <- NA
      KCurable_v[k.cured] <- FALSE
      matTimeCurable[tt, k.cured] <- -1 # stamping away the moment of curing, to facilitate future lookups using this matrix
      
      
      # -- Third condition: set k-cured state [S_P]
      # get qualifying loans
      oldCures <- which(matState[tt,] == 0 & matState[tt-1,] == 3)
      # set state accordingly [S_P]
      matState[tt, oldCures] <- 3
      
    }
    
  }
  
 return(list(State=matState, TimeCurable = matTimeCurable))

}



# ==== Function: Find starting points of a specified delinquency state for a specific episode number
# Delinquency states include: (default [S_D], k-curable [S_C], "default episode" [S_D] + [S_C]).
# Function operates at loan-level.
# -- Inputs: [state_m]: Delinquency matrix as output by DelinqCure_m(); [state]: string denoting type of state requested;
#   [N_epi]: the j-th (g1,d)-default episode requested
# -- Outputs: applicable starting time(s), where available
startTime <- function(i, state_m, state, N_epi=0) {
  
  # -- Testing conditions
  # i <- 916; state <- "S_D:S_C"; state_m <- matState; N_epi <- 1
  # state_m <- matrix(c(3,3,3, 1,1,2, 2,2,3, 3,1,2, 2,1,2, 2,2,3,
  #                     3,1,1, 2,2,1, 2,2,3, 1,2,2, 3,3,1, 1,2,2), nrow=18)

  # Differentiate lookup-logic based on given state for which want to determine a starting time
  if (state == "S_D") { lookup <- 1 } else 
  if (state == "S_C") { lookup <- 2 } else 
  if (state == "S_P") { lookup <- 3 } else    
  if (state == "S_D:S_C") { # span a default episode (c_i + k + 1) - \tau_i
    
    lookup <- c(1,2)
    
  } else {
    return (NA) # unknown state
  }
  
  
  # find indices where the state is equal to lookup-value
  start.t <- which(state_m[,i] %in% lookup)
  
  # continue only if there is a first starting point
  if (!is.na(start.t[1])) {
    
    # 1. Find positions in these positions where the lagged difference is greater than 1.
    #   - these indicate 'breaks' between episodes 
    # 2. Add 1 to these found positions to move to the 'initial starting points' of the next episode in succession
    # 3. Pre-fix this vector with '1' to re-include the first episode
    # 4. Given this vector of indices, return as starting times of various (g,d)-default episodes [S_D], or k-curable episodes [S_C]
    episodes.start <- start.t[c(1, which(diff(start.t) > 1) + 1)]
    
    # Evaluate episode number against available episodes
    if (N_epi <= length(episodes.start)){
      
      if (N_epi > 0) { # return the starting time of a specified episode number
        
        return (episodes.start[N_epi])  
        
      } else { # otherwise, just return all starting times
        
        return (episodes.start)
        
      }
      
      
    } else {
      return(-1) # specified episode doesn't exist, return specific error code
    }
    
  } else {
    return(-1) # no first starting point found, return specific error code
  }
}



# ==== Function: Calculate number of episodes for a given state
# Delinquency states include: (default [S_D], k-curable [S_C], "default episode" [S_D] + [S_C]).
# Each episode refers to the number of consecutive periods during which a loan resides in a specified state
# Function operates at loan-level.
# -- Inputs: [state_m]: Delinquency matrix as output by DelinqCure_m(); [state]: string denoting type of state requested.
# -- Outputs: episode count
episodeCount <- function(i, state_m, state) {
  
  # -- Testing conditions
  # i <- 3; state <- "S_D"; state_m <- matState;
  # state_m <- matrix(c(3,3,3, 1,1,2, 2,2,3, 3,1,2, 2,1,2, 2,2,3,
  #                     3,1,1, 2,2,1, 2,2,3, 1,2,2, 3,3,1, 1,2,2,
  #                     3,3,3, 3,3,3, 3,3,3, 3,3,3, 3,3,3, 3,3,3), nrow=18)
  
  # Differentiate lookup-logic based on given state for which want to determine a starting time
  if (state == "S_D") { lookup <- 1 } else 
  if (state == "S_C") { lookup <- 2 } else 
  if (state == "S_P") { lookup <- 3 } else
  if (state == "S_D:S_C") { # span a default episode (c_i + k + 1) - \tau_i
    
    lookup <- c(1,2)
        
  } else {
    return (NA) # unknown state
  }
  
  # find indices where the state is equal to lookup-value
  start.t <- which(state_m[,i] %in% lookup)
  
  # continue only if there is a first starting point
  if (!is.na(start.t[1])) {
    
    # 1. Find positions in these positions where the lagged difference is greater than 1.
    #   - these indicate 'breaks' between episodes 
    # 2. Add 1 to these found positions to move to the 'initial starting points' of the next episode in succession
    # 3. Pre-fix this vector with '1' to re-include the first episode
    # 4. Given this vector of indices, return as starting times of various (g,d)-default episodes [S_D], or k-curable episodes [S_C]
    episodes.start <- start.t[c(1, which(diff(start.t) > 1) + 1)]
    
    # Length of these episode starting points vector is the number of episodes (assuming it's not NA)
    if (!(any(is.na(episodes.start)))) {
      
      return (length(episodes.start)) 
      
    } else { return (0)}
    
  } else {
    return (0) # no first starting point found, so 0 episodes
  }
}

