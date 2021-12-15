* Import the data
import delimited using "..\..\data\cleaned\nssats_eq_lags_complete.csv", clear

* Check missing 
count if !missing(lgbtq_actual, state_policy, l_state_policy, l2_state_policy, govt_fund, l_govt_fund, l2_govt_fund, year)

* Regression
regress lgbtq_actual state_policy l_state_policy l2_state_policy govt_fund l_govt_fund l2_govt_fund year

* Breusch-Pagan Test
hettest