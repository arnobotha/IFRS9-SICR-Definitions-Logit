# ================================== ANCILLARY 1 =============================== 
# Conducting some graphical data analysis as part of data preparation 
# ------------------------------------------------------------------------------

# Project title: Dynamic SICR-research
# Script authors: Arno Botha, Esmerelda Oberholzer

# Description:
# Graphical data analysis form part of data preparation.
# Some plots are drawn to get a better understanding of the distribution of
# the variables.
# Objects are created for the plots of the variables if one wants to save the
# plots at a later stage.

# ------------------------------------------------------------------------------

# -- Script dependencies:
# - 0.Setup.R
# - 1.Data_Import.R
# - 2.Data_Prepare.R

# -- Inputs:
# - dat_SICR_merge

# -- Outputs:
# - Various graphs

# ------------------------------------------------------------------------------

# -- Set the DPI

dpi <- 150

# ------------------------------------------------------------------------------

# -- Loan types histograms

# Extract the data relating to the loan types
dat_lntype_hist <- subset(dat_SICR_merge, 
                          select = c(LN_TPE))

# Create an object for the plot
lntype.obj1 <- ggplot(dat_lntype_hist, 
                      aes(x=factor(LN_TPE), 
                          fill = LN_TPE)) + 
                      geom_bar() + 
                      theme_minimal() +
                      labs(y="Count", 
                           x= "Loan types", 
                           fill = "Loan types") +
                      ggtitle("Histogram of the loan types") +
                      theme(plot.title = element_text(hjust = 0.5), 
                            panel.background = element_blank())

# ------------------------------------------------------------------------------

# -- Interest rate/prime margin histogram

# Extract the data relating to the prime margin (interest rate variable used)
dat_int_rate_hist <- subset(dat_SICR_merge, 
                            select = c(Prime_Margin,
                                       Prime_Margin_Exp))

# Create an object for the plot: Normal prime margin variable
int_rate.obj1 <- ggplot(dat_int_rate_hist, 
                        aes(x=Prime_Margin)) + 
                        geom_histogram(aes(y = stat(density)), 
                                       fill = "hotpink4") + 
                        geom_density(alpha = .2, 
                                     col = "hotpink4") +
                        theme_minimal() +
                        labs(y="Density", 
                             x= "Prime margin", 
                             fill = "Prime margin") +
                        ggtitle("Histogram with kernel density function of 
                                the prime margin") +
                        theme(plot.title = element_text(hjust = 0.5), 
                              panel.background = element_blank())

# Create an object for the plot: Squared prime margin variable
int_rate.obj2 <- ggplot(dat_int_rate_hist, 
                        aes(x=Prime_Margin_Exp)) + 
                        geom_histogram(aes(y = stat(density)), 
                                       fill = "violetred4") + 
                        geom_density(alpha = .2, 
                                     col = "violetred4") +
                        theme_minimal() +
                        labs(y="Density", 
                             x= "Squared prime margin", 
                             fill = "Squared prime margin") +
                        ggtitle("Histogram with kernel density function of the 
                                squared prime margin") +
                        theme(plot.title = element_text(hjust = 0.5), 
                              panel.background = element_blank())

# ------------------------------------------------------------------------------

# -- Exposure variable histogram

# Only display up to the 99.9th percentile since the distribution is skewed
dat_exposure_hist <- subset(dat_SICR_merge,
                            EXPOSURE < quantile(dat_SICR_merge$EXPOSURE, 
                                                0.999),
                            select = c(EXPOSURE,
                                       Exposure_log_transform))

# Create an object for the plot of the exposure variable
exp.obj1 <- ggplot(dat_exposure_hist, 
                   aes(x=EXPOSURE)) + 
                   geom_histogram(aes(y = stat(density)), 
                                  fill = "steelblue4") + 
                   geom_density(alpha = .2, 
                                col = "steelblue4") +
                   theme_minimal() +
                   labs(y="Density", 
                        x= "Exposure (in Rand)", 
                        fill = "Exposure (in Rand)") +
                   ggtitle("Histogram with a kernel density function of the 
                            loan exposures (capped at the 99th percentile)") +
                   theme(plot.title = element_text(hjust = 0.5), 
                         panel.background = element_blank())


# Create an object for the plot of the log-transformed exposure variable
exp.obj2 <- ggplot(dat_exposure_hist, 
                   aes(x=Exposure_log_transform)) + 
                   geom_histogram(aes(y = stat(density)), 
                                  fill = "royalblue4") + 
                   geom_density(alpha = .2, 
                                col = "royalblue4") +
                   theme_minimal() +
                   labs(y="Density", 
                        x= "Log-transformed exposure (in Rand)", 
                        fill = "Log-transformed exposure (in Rand)") +
                   ggtitle("Histogram with a kernel density function of the 
                           log-transformed loan exposures") +
                   theme(plot.title = element_text(hjust = 0.5), 
                         panel.background = element_blank())

# ------------------------------------------------------------------------------

# -- Loan age histogram

# Create a data set with just the loan ages
dat_loan_age_hist <- subset(dat_SICR_merge, 
                            select = c(LOAN_AGE))

# Create an object for the plot
loan_age.obj1 <- ggplot(dat_loan_age_hist, 
                        aes(x=LOAN_AGE)) + 
                        geom_histogram(aes(y = stat(density)), 
                                       fill = "lightskyblue4") + 
                        geom_density(alpha = .2, 
                                     col = "lightskyblue4") +
                        theme_minimal() +
                        labs(y="Density", 
                             x= "Loan age", 
                             fill = "Loan age") +
                        ggtitle("Histogram with a kernel density function of 
                                the loan ages") +
                        theme(plot.title = element_text(hjust = 0.5), 
                              panel.background = element_blank())

# ------------------------------------------------------------------------------

# -- New loan term histogram

# Create a data set with the loan term variable
dat_new_loan_term_hist <- subset(dat_SICR_merge, 
                                 select = c(NEW_LOAN_TERM,
                                            Loan_Term_Binned))

# Ensure that the binned loan term is a factor
dat_new_loan_term_hist[, Loan_Term_Binned := as.factor(Loan_Term_Binned)]

# Create an object for the loan term plot
new_loan_term.obj1 <- ggplot(dat_new_loan_term_hist, 
                             aes(x=NEW_LOAN_TERM)) + 
                             geom_histogram(aes(y = stat(density)), 
                                            fill = "lightcoral") + 
                             geom_density(alpha = .2, 
                                          col = "lightcoral") +
                             theme_minimal() +
                             labs(y="Density", 
                                  x= "Loan term", 
                                  fill = "Loan term") +
                             ggtitle("Histogram with a kernel density function 
                                      of the loan terms") +
                             theme(plot.title = element_text(hjust = 0.5), 
                                   panel.background = element_blank())

# - Binned loan term

# Create an object for the plot
new_loan_term.obj2 <- ggplot(dat_new_loan_term_hist, 
                             aes(x=(Loan_Term_Binned), 
                                 fill = Loan_Term_Binned)) + 
                             geom_bar() + 
                             theme_minimal() +
                             labs(y="Count", 
                                  x= "Binned loan terms", 
                                  fill = "Binned loan terms") +
                             ggtitle("Histogram of the binned loan terms") +
                             theme(plot.title = element_text(hjust = 0.5), 
                                   panel.background = element_blank())

# ------------------------------------------------------------------------------

# -- Past due amount histogram

# - Only display up to the 99.9th percentile since the distribution is skewed
past_due_amt_hist <- subset(dat_SICR_merge,
                            PAST_DUE_AMT < quantile(dat_SICR_merge$PAST_DUE_AMT, 
                                                    0.999), 
                            select = c(PAST_DUE_AMT))

# Create an object for the plot
past_due_amt.obj1 <- ggplot(past_due_amt_hist, 
                            aes(x=PAST_DUE_AMT)) + 
                            geom_histogram(binwidth = 100000, 
                                           colour = "plum4", 
                                           fill = "plum4") + 
                            theme_minimal() +
                            labs(y="Count", 
                                 x= "Past due amount (in Rand)", 
                                 fill = "Past due amount (in Rand)") +
                            ggtitle("Histogram of past due amounts 
                                    (capped at the 99th percentile)") +
                            theme(plot.title = element_text(hjust = 0.5), 
                                  panel.background = element_blank())

# ------------------------------------------------------------------------------

# -- Receipts histogram

# - Cap at -500000 and 500000 for presentation purposes

# Create a data set for the receipts variable
receipts_hist <- subset(dat_SICR_merge, 
                        receipts < 500000 & receipts > -500000, 
                        select = c(receipts))

# Create an object for the plot
receipts.obj1 <- ggplot(receipts_hist, 
                        aes(x=receipts)) + 
                 geom_histogram(aes(y = stat(density)), 
                                fill = "palegreen4") +
                 geom_density(alpha = .2, 
                              col = "palegreen4") +
                 theme_minimal() +
                 labs(y="Density", 
                      x= "Receipts (in Rand)", 
                      fill = "Receipts (in Rand)") +
                 ggtitle("Histogram with a kernel density function of monthly 
                         loan cash receipts") +
                 theme(plot.title = element_text(hjust = 0.5), 
                       panel.background = element_blank())

# ------------------------------------------------------------------------------

# -- g0-measure histogram (months in arrears)

# - Only display up to the 99.9th percentile since the distribution is skewed

# Create a data set with the g0-measure
mnths_arrears_hist <- subset(dat_SICR_merge,
                             g_0 < quantile(dat_SICR_merge$g_0, 
                                            0.99),
                             select = c(g_0))

# Create an object for the plot 
mnths_arrears.obj1 <- ggplot(mnths_arrears_hist, 
                             aes(x=g_0)) + 
                             geom_histogram(aes(y = stat(density)), 
                                            fill = "cyan4") + 
                             geom_density(alpha = .2, 
                                          col = "cyan4") +
                             theme_minimal() +
                             labs(y="Density", 
                                  x= "g_0-measure", 
                                  fill = "g_0-measure") +
                             ggtitle("Histogram with a kernel density function 
                                     of the g_0-measure") +
                             theme(plot.title = element_text(hjust = 0.5), 
                                   panel.background = element_blank())

# ------------------------------------------------------------------------------
