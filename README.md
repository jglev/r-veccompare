# veccompare: Automatically Generate All n-Wise Set Comparisons on Vectors

## Overview

`veccompare` contains functions for automating [set operations](https://en.wikipedia.org/wiki/Set_(mathematics)#Basic_operations "Wikipedia: Set (Mathematics): Basic Operations"). Given a named list of 5 vectors, for example, `veccompare` can calculate all 2-, 3-, 4-, and 5-way comparisons between those vectors, recording information for each comparison about the set "union" (combined elements), "intersection" (overlap / shared elements), and compliments (which elements are unique to each vector involved in the comparison). It can also create Venn diagrams for up to 5-way comparisons, using the `VennDiagram` package; and network graphs for all 2-way comparisons, using the `qgraph` package.

The package also contains a function for automating reporting in [RMarkdown](http://rmarkdown.rstudio.com/lesson-1.html "RStudio: RMarkdown Introduction"), by generating markdown output for easy analysis, as well as an RMarkdown template for use with RStudio.

## Authors

- Jacob Gerard Levernier (<jlevern@upenn.edu>) (**Current Maintainer**)  
Designed and authored the package source code and documentation.    
Roles: author, creator, designer, engineer, programmer
- Heather Gaile Wacha (<wacha2@wisc.edu>)  
Provided intellectual overview and consultation during development for use with medieval cartographic datasets.  
Roles: conceptor, consultant, data contributor

## Installation

You can download the latest stable version from [CRAN](https://cran.r-project.org/web/packages/veccompare/index.html "CRAN page for veccompare"):

```{r}
install.packages('veccompare')
```

You can download the latest development version from [GitHub](https://github.com/publicus/r-veccompare "GitHub repository for veccompare"):

```{r}
# You can install the development version of veccompare from GitHub:
# install.packages("devtools")
devtools::install_github("publicus/r-veccompare")
```

## Usage

The primary function from `veccompare` is `compare.vectors()`. Complementarily, `compare.vectors.and.return.text.analysis.of.overlap()` will call `compare.vectors()` and generate Markdown-style output from it (for example, for use within an RMarkdown file). `veccompare` also provides a function, `summarize.two.way.comparisons.percentage.overlap()`, that can create correlation-plot-style images and network graphs for all two-way comparisons between vectors.

An RMarkdown template illustrating several of `veccompare`'s features can be viewed and used from within RStudio by clicking `File -> New File -> R Markdown... -> From Template -> Veccompare Overlap Report`.
