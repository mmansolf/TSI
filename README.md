# `TSI`
True score imputation: Custom imputation function for `mice` in R

## Credit line
This package includes code Adapted with permission from Wolters Kluwer Health, Inc.:
* Guo, Y., Little, R. J., & McConnell, D. S. (2012). On using summary statistics from an external calibration sample to correct for covariate measurement error. Epidemiology, 165-174.
* Full text link: https://journals.lww.com/epidem/Fulltext/2012/01000/On_Using_Summary_Statistics_From_an_External.25.aspx

# Description
Generates data sets using true score imputation to account for psychometric measurement error. Imputation is conducted by a call to the `mice` function from the eponymous package, using `method='truescore'` to call the custom imputation function `mice.impute.truescore` included in this package. Reliability or standard error of measurement are incorporated through the `blots` argument to `mice`.

# Installing from source
We recommend installing the latest development version of this package from Github to ensure use of the latest and greatest version. To do so:
1. Install the `devtools` package if you haven't. In R, this can be done using the following code:
`install.packages('devtools')`
2. Load the `devtools` package:
`library(devtools)`
3. Install the `TSI` package using `install_github()`:
`install_github('TSI')`

# Additional license information
LICENSE: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
* For more information on this license type, see https://creativecommons.org/licenses/by-nc/4.0/
* For more information on licensing for this package in particular, see the included LICENSE.md file.

# Bugs and questions
Bug reports and feedback are always welcome. For reporting bugs and requesting minor improvements, please use the "Issues" functionality in Github. For larger requests for improvement or jolly scholarly co-operation in expanding the true score imputation framework, please email me directly at maxwell.mansolf@northwestern.edu. When in doubt, either contact method is fine.
