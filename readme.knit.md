---
title: "The four seasons of commodity futures"
author: "Olivier"
date: "2018-12-31"

output: github_document
---



## Correlations matrices

Hi Pawel, no file size exceeds 6gb which I thought would be ok as most notebooks run with RAM >= 8gb nowadays. One solution is to read sequetially, only loading one chunk of a particular file at a time in memory. The matrices are 24x24 (24 commodities) so ideally you would choose chunk size = 24x. Each file is organised as follow:  


```
## # A tibble: 24 x 29
##    variable periods subperiods subperiod ticker `BOA Comdty` `C A Comdty`
##    <chr>    <chr>   <chr>      <chr>     <chr>         <dbl>        <dbl>
##  1 CHP      period~ 1997-07-0~ 1997-07-~ BOA C~       1            0.366 
##  2 CHP      period~ 1997-07-0~ 1997-07-~ C A C~       0.366        1     
##  3 CHP      period~ 1997-07-0~ 1997-07-~ CCA C~      -0.311       -0.129 
##  4 CHP      period~ 1997-07-0~ 1997-07-~ CLA C~       0.133        0.102 
##  5 CHP      period~ 1997-07-0~ 1997-07-~ CTA C~      -0.184       -0.507 
##  6 CHP      period~ 1997-07-0~ 1997-07-~ FCA C~       0.0390      -0.621 
##  7 CHP      period~ 1997-07-0~ 1997-07-~ GCA C~       0.612       -0.0591
##  8 CHP      period~ 1997-07-0~ 1997-07-~ HGA C~      -0.293       -0.353 
##  9 CHP      period~ 1997-07-0~ 1997-07-~ HOA C~      -0.0716       0.439 
## 10 CHP      period~ 1997-07-0~ 1997-07-~ JOA C~      -0.312        0.398 
## # ... with 14 more rows, and 22 more variables: `CCA Comdty` <dbl>, `CLA
## #   Comdty` <dbl>, `CTA Comdty` <dbl>, `FCA Comdty` <dbl>, `GCA
## #   Comdty` <dbl>, `HGA Comdty` <dbl>, `HOA Comdty` <dbl>, `JOA
## #   Comdty` <dbl>, `KCA Comdty` <dbl>, `LBA Comdty` <dbl>, `LCA
## #   Comdty` <dbl>, `LHA Comdty` <dbl>, `NGA Comdty` <dbl>, `O A
## #   Comdty` <dbl>, `PAA Comdty` <dbl>, `PLA Comdty` <dbl>, `S A
## #   Comdty` <dbl>, `SBA Comdty` <dbl>, `SIA Comdty` <dbl>, `SMA
## #   Comdty` <dbl>, `W A Comdty` <dbl>, `XBWA Comdty` <dbl>
```

  
  
Another solution, not incompatible with the one above actually, would be to explain me in plain english what you do with each matrix so I could perform the task myself which I would happily do. Meanwhile you could focus on the other part of the analysis which I encourage you to also explain to me, all these emails are confusing me more than anything else!

Speak soon,

Olivier.
