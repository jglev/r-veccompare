Following https://github.com/STAT545-UBC/Discussion/issues/240, I was trying to run `R CMD Rd2pdf veccompare --no-clean`, but was getting an error,

```
Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
  Running 'texi2dvi' on 'Rd2.tex' failed.
Messages:
sh: 1: /usr/bin/texi2dvi: not found
```

Following https://stackoverflow.com/a/17652742, I fixed this by installing `texinfo`: "You need to install texinfo:" `sudo apt-get install texinfo`







I was getting an error re: PDF builds failing when using Ctrl + Shift + E in RStudio. This turned out to be from an unescaped '%' in an ROxygen comment.

From https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html: "Note that \ and % are special characters. To insert literals, escape with a backslash: \\, \%."
See the Note under `reformat_code` in https://cran.r-project.org/web/packages/Rd2roxygen/Rd2roxygen.pdf (p. 6):

> One possible situation is the percent symbol %, which should be escaped even in the examples code (cf Writing R Extensions), and this can make the code syntactically incorrect, e.g. a %in% b should be a \%in\% b but the latter is not valid R code.

