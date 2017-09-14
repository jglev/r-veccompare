## Rebusmission

This is a resubmission. In this version I have:

* Changed the DESCRIPTION to replace the redundant "The veccompare package contains functions for automating" with "Automates..."

* Single quoted software names such as 'RMarkdown' and 'RStudio'.

* Edited the LICENSE file to contain the third template field (ORGANIZATION). 
  Since there is no organization sponsoring the package, I set the ORGANIZATION field to "copyright holder" so that the CRAN template for the license will read "Neither the name of the copyright holder nor the names of its contributors..."

* Fixed an error in an example for the `render.venn.diagram` function.

## Test environments

* local ubuntu 17.04 install, R 3.3.2 (2016-10-31) (platform x86_64-pc-linux-gnu)
* windows 
    * local windows 10 install, R 3.3.0 (2016-05-03) (platform x86-w64-mingw32)
    * via win-builder, R Under development (unstable) (2017-09-12 r73242) (platform x86_64-w64-mingw32)

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE:

* checking dependencies in R code ... NOTE
    Namespace in Imports field not imported from: ‘pander’
    All declared Imports should be used.
    
    * Explanation: 
      The pander package is used in the RMarkdown template
      that ships with this package. Thus, it seems useful to list it
      in the Imports field. I would prefer not to list it in the Suggests
      field, since doing so would require making the RMarkdown template
      potentially more confusing for users by requiring that I add a
      check to it that pander is installed.

## Downstream dependencies

There are currently no downstream dependencies of this package.
