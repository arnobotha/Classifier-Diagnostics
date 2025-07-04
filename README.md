# Classifier-Diagnostics
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13799785.svg)](https://doi.org/10.5281/zenodo.13799785)

 R-codebase for the Modelling-standard titled "Diagnostics for Binary Classification" (Std-ClassifierDiagnostics), developed using retail mortgage data.


## Structure
This R-codebase can be run sequentially using the file numbering itself as a structure. **Delinquency measures** are algorithmically defined in **DelinqM.R** as data-driven functions, which may be valuable to the practitioner outside of the study's current scope. These delinquency measures were formulated and empirically tested in [Botha2022](https://www.researchgate.net/publication/358329458_The_loss_optimization_of_loan_recovery_decision_times_using_forecast_cashflows), as part of a loss optimisation exercise of recovery decision times, as implemented in the corresponding [R-codebase](https://github.com/arnobotha/The-loss-optimisation-of-loan-recovery-decision-times-using-forecast-cash-flows). A simulation study from [Botha2021](https://www.researchgate.net/publication/350169758_Simulation-based_optimisation_of_the_timing_of_loan_recovery_across_different_portfolios) also demonstrated these delinquency measures at length, with its corresponding [R-codebase](https://github.com/arnobotha/Simulation-based-optimisation-of-the-timing-of-loan-recovery-across-different-portfolios). Similarly, the **TruEnd-procedure** from [Botha2024](https://www.researchgate.net/publication/380214432_The_TruEnd-procedure_Treating_trailing_zero-valued_balances_in_credit_data) and its corresponding [R-codebase](https://github.com/arnobotha/TruEnd-Procedure)  is implemented in the **TruEnd.R** script, which includes a small variety of functions related to running the TruEnd-procedure practically.

## Data
This R-codebase assumes that monthly loan performance data is available. Naturally, the data itself can't be made publically available given its sensitive nature, as well as various data privacy laws, particularly the _Protection of Personal Information (POPI)_ Act of 2013 in South Africa. However, the structure and type of data that is required for reproducing this study, is sufficiently described in the commentary within the scripts. This should enable the practitioner to extract and prepare data accordingly. Moreover, this codebase assumes South African macroeconomic data is available, as sourced and collated by internal staff of the bank in question.

## Copyright
All code and scripts are hereby released under an [MIT](https://opensource.org/licenses/MIT) license. Similarly, all graphs produced by relevant scripts as well as those published here, are hereby released under a Creative Commons Attribution ([CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/)) licence.
