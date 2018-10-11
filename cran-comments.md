## Resubmission
This is a resubmission. In this version I have:

## Attended to a CRAN error:
We see such failures occasionally: for the record this time it is

 SEMinR Model succesfully bootstrapped── 1. Failure: Seminr evaluates the factor discriminant validity p_values correc
 diff[1, 2] is not strictly less than 0.1. Difference: 0.000314

Seems you failed to heed the warnings in §1.6 of 'Writing R Extensions': please re-read it and correct ASAP (CRAN is shut until Sep 10) and before Sep 21 to safely retain the package on CRAN.

Also try a spell checker --- it is 'successfully'.

-- 
Brian D. Ripley,                  ripley@stats.ox.ac.uk
Emeritus Professor of Applied Statistics, University of Oxford

Specifically, we updated the bootstrap method to take a seed for reproducability and added a tolerance of 0.0001 for floating point calculation tests. All tests including random processes such as bootstrapping were made reproducible.

## Test environments
* macOS Sierra 10.12.6 (on travis-ci), R 3.5.0
* Ubuntu 14.04.5 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 
