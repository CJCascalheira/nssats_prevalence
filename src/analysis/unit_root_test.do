* Import the data
import delimited using "..\..\data\cleaned\nssats_eq_lags.csv", clear

* Define case and time parameters
xtset case_state year 

*** UNIT ROOT TESTS: ORIGINAL ***

* Fisher-type Philips-Perron unit-root test
xtunitroot fisher lgbtq_total, demean trend pperron lags(1)
xtunitroot fisher lgbtq_actual, demean trend pperron lags(1)
xtunitroot fisher state_policy, demean trend pperron lags(1)
xtunitroot fisher govt_fund, demean trend pperron lags(1)

* Levin-Lin-Chu unit-root test
xtunitroot llc lgbtq_total, trend
xtunitroot llc lgbtq_actual, trend
xtunitroot llc state_policy, trend
xtunitroot llc govt_fund, trend