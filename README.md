# `TSI`
True score imputation: Custom imputation function for `mice` in R

## Credit line
This package includes code Adapted with permission from Wolters Kluwer Health, Inc.:
* Guo, Y., Little, R. J., & McConnell, D. S. (2012). On using summary statistics from an external calibration sample to correct for covariate measurement error. Epidemiology, 165-174.
* Full text link: https://journals.lww.com/epidem/Fulltext/2012/01000/On_Using_Summary_Statistics_From_an_External.25.aspx

# Description
Generates data sets using true score imputation to account for psychometric measurement error. Imputation is conducted by a call to the `mice` function from the eponymous package, using `method='truescore'` to call the custom imputation function `mice.impute.truescore` included in this package. Reliability or standard error of measurement are incorporated through the `blots` argument to `mice`. Because these function calls can be complicated, a convenience function `TSI` is included which creates and executes the `mice` call for the user.

# Example
The following code uses an included data set, `data_eap`, containing two variables containing EAP-generated T scores, `Fx` and `Fy`, and associated standard errors, `SE.Fx` and `SE.Fy`, respectively. This code conducts five imputations with five burn-in iterations per imputation, generating imputed true score variables `Tx` and `Ty`. More details on this example can be found in the vignette, obtained by running `vignette('TSI')` after installing the package.
```
library(TSI)
set.seed(0)
mice.data=TSI(data_eap,
              os_names=c('Fx','Fy'),
              se_names=c('SE.Fx','SE.Fy'),
              metrics='T',
              score_types='EAP',
              separated=T,
              ts_names=c('Tx','Ty'),
              mice_args=c(m=5,maxit=5,printFlag=F))
mice.data
```

The resulting `mids` object can be analyzed using convenience functions available for the `mice` package:

```
pool(with(mice.data,lm(Ty~Tx+m)))
```

# Directly calling the `mice` package
The above code uses a convenience function to generate a call to `mice` to generate the imputations. Instead, one can call `mice` directly, although the function call is more complicated when constructed by hand. As above, see `vignette('TSI')` for more details.

```
library(TSI)
set.seed(0)
mice.data=mice(data_eap_2,m=5,maxit=5,
  method=c('pmm','pmm','pmm','pmm','pmm',
           'truescore','truescore'),
  blocks=list(Fx="Fx",Fy="Fy",SE.Fx="SE.Fx",SE.Fy="SE.Fy",m="m",
              Tx='Tx',Ty='Ty'),
  blots=list(Tx=list(calibration=list(os_name='Fx',
                                      se_name='SE.Fx',
                                      score_type='EAP',
                                      mean=50,
                                      var_ts=100,
                                      separated=T)),
             Ty=list(calibration=list(os_name='Fy',
                                      se_name="SE.Fy",
                                      score_type='EAP',
                                      mean=50,
                                      var_ts=100,
                                      separated=T))),
  predictorMatrix=matrix(c(0,1,1,1,1,0,0,
                           1,0,1,1,1,0,0,
                           1,1,0,1,1,0,0,
                           1,1,1,0,1,0,0,
                           1,1,1,1,0,0,0,
                           1,1,1,1,1,0,0,
                           1,1,1,1,1,0,0),7,7,byrow=T),
  printFlag=F,
  remove.constant=F)
mice.data

pool(with(mice.data,lm(Ty~Tx+m)))
```

# Installing from source
We recommend installing the latest development version of this package from Github to ensure use of the latest and greatest version. To do so:
1. Install the `devtools` package if you haven't. In R, this can be done using the following code:
`install.packages('devtools')`
2. Load the `devtools` package using the following code:
`library(devtools)`
3. Install the `TSI` package using `install_github()` using the following code:
```
devtools::install_github('TSI',build_vignettes=T)
```
The `,build_vignettes=T` is optional but recommended for viewing the vignette accompanying this package.
The `TSI` package is not yet on CRAN, so trying to install it with `install.packages()` will not work. This is coming soon!

## Other installation options
While the above method should work for most users, there are alternatives:
* The `ghit` library has an analogous `ghit::install_github()` function to that in the `devtools` package
* Download the package as a .zip file, then run the following code and interactively select the .zip file to install:
`install.packages(file.choose(), repos = NULL, type = "source")`

# Additional license information
LICENSE: Creative Commons - Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
* For more information on this license type, see https://creativecommons.org/licenses/by-nc/4.0/
* For more information on licensing for this package in particular, see the included LICENSE.md file.

# Bugs and questions
Bug reports and feedback are always welcome. For reporting bugs and requesting minor improvements, please use the "Issues" functionality in Github. For larger requests for improvement or jolly scholarly co-operation in expanding the true score imputation framework, please email me directly at maxwell.mansolf@northwestern.edu. When in doubt, either contact method is fine.
