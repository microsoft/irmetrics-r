# IR metrics for R

This is a small library for implementing several standard "test collection" or "offline" evaluation measures
for search systems. It uses the C/W/L framework described by Moffat et al. and implements:

* Precision at _k_ (P)
* Average precision (AP)
* Reciprocal rank (RR)
* Expected reciprocal rank (ERR)
* Rank-biased precision (RBP)
* Scaled discounted cumulative gain (SDCG)
* INST

Example:

```r
# a gain or relevance vector: how good each retrieved thing is, in the order they appear
gains <- c(1, 0, 0, 0.5, 1)

# precision at 1 is perfect
P(gains, k=1) #> 1
# precision at 3 is one-third
P(gains, k=3) #> 0.33

# rank-biased precision, with persistence 0.7
RBP(gains, p=0.7) #> 0.42

# Investigate the weight vector, see the metric accumulate, ...
str(RBP(gains, p=0.7))
#> List of 8
#>  $ metric    : num 0.423
#>  $ C         : num [1:5] 0.7 0.7 0.7 0.7 0.7
#>  $ W         : num [1:5] 0.3 0.21 0.147 0.103 0.072
#>  $ gain      : num [1:5] 1 0 0 0.5 1
#>  $ cum.metric: num [1:5] 0.3 0.3 0.3 0.351 0.423
#>  $ i         : int [1:5] 1 2 3 4 5
#>  $ residual  : num 0.168
```

# Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.microsoft.com.

When you submit a pull request, a CLA-bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., label, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.
