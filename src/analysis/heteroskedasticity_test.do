* N-SSATS 

* Import the data
import delimited using "..\..\data\cleaned\nssats_eq_lags_complete.csv", clear

* Check missing 
count if !missing(lgbtq_perc_actual, state_policy, l_state_policy, govt_fund, l_govt_fund, year)

* Regression
regress lgbtq_perc_actual state_policy l_state_policy govt_fund l_govt_fund year

* Breusch-Pagan Test
hettest

********************************************************************************

* N-MHSS

* Import the data
import delimited using "..\..\data\cleaned\nmhss_eq_lags_complete.csv", clear

* Check missing 
count if !missing(lgbtq_perc, state_policy, l_state_policy, fd_govt_fund, fd_l_govt_fund, year)

* Regression
regress lgbtq_perc state_policy l_state_policy fd_govt_fund fd_l_govt_fund year

* Breusch-Pagan Test
hettest