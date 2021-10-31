---
output: html_document
editor_options:
  chunk_output_type: console
---
# References and Session Info

## Session Info




```r
sessionInfo()
```

```
#> R version 4.0.3 (2020-10-10)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: Ubuntu 20.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/local/lib/R/lib/libRblas.so
#> LAPACK: /usr/local/lib/R/lib/libRlapack.so
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=de_DE.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#>  [1] brms_2.16.1          Rcpp_1.0.6           EnvStats_2.4.0      
#>  [4] ggraph_2.0.5.9000    tidygraph_1.2.0      glue_1.4.2          
#>  [7] patchwork_1.1.0.9000 prismatic_1.0.0.9000 forcats_0.5.1       
#> [10] stringr_1.4.0        dplyr_1.0.6          purrr_0.3.4         
#> [13] readr_1.4.0          tidyr_1.1.3          tibble_3.1.2        
#> [16] ggplot2_3.3.4.9000   tidyverse_1.3.0.9000
#> 
#> loaded via a namespace (and not attached):
#>   [1] readxl_1.3.1         backports_1.2.1      plyr_1.8.6          
#>   [4] igraph_1.2.6         splines_4.0.3        crosstalk_1.1.1     
#>   [7] inline_0.3.17        rstantools_2.1.1     digest_0.6.27       
#>  [10] htmltools_0.5.1.1    viridis_0.5.1        rsconnect_0.8.24    
#>  [13] fansi_0.5.0          magrittr_2.0.1       checkmate_2.0.0     
#>  [16] graphlayouts_0.7.1   modelr_0.1.8         RcppParallel_5.0.2  
#>  [19] matrixStats_0.56.0   xts_0.12.1           prettyunits_1.1.1   
#>  [22] colorspace_2.0-2     rvest_1.0.0          ggrepel_0.9.1       
#>  [25] haven_2.3.1          xfun_0.24            callr_3.6.0         
#>  [28] crayon_1.4.1         jsonlite_1.7.2       lme4_1.1-26         
#>  [31] zoo_1.8-8            polyclip_1.10-0      gtable_0.3.0        
#>  [34] V8_3.4.0             distributional_0.2.2 pkgbuild_1.2.0      
#>  [37] rstan_2.21.2         abind_1.4-5          scales_1.1.1        
#>  [40] mvtnorm_1.1-2        DBI_1.1.1            miniUI_0.1.1.1      
#>  [43] viridisLite_0.4.0    xtable_1.8-4         StanHeaders_2.21.0-7
#>  [46] stats4_4.0.3         DT_0.17              htmlwidgets_1.5.3   
#>  [49] httr_1.4.2           threejs_0.3.3        posterior_1.1.0     
#>  [52] ellipsis_0.3.2       pkgconfig_2.0.3      loo_2.4.1           
#>  [55] farver_2.1.0         sass_0.4.0.9000      dbplyr_2.1.1        
#>  [58] utf8_1.2.1           tidyselect_1.1.1     rlang_0.4.12        
#>  [61] reshape2_1.4.4       later_1.1.0.1        munsell_0.5.0       
#>  [64] cellranger_1.1.0     tools_4.0.3          cli_2.5.0           
#>  [67] generics_0.1.0       broom_0.7.6          ggridges_0.5.2      
#>  [70] evaluate_0.14        fastmap_1.1.0        yaml_2.2.1.99       
#>  [73] processx_3.5.1       knitr_1.33           fs_1.5.0            
#>  [76] nlme_3.1-149         mime_0.11            projpred_2.0.2      
#>  [79] xml2_1.3.2           compiler_4.0.3       bayesplot_1.8.1     
#>  [82] shinythemes_1.2.0    rstudioapi_0.13      curl_4.3            
#>  [85] gamm4_0.2-6          reprex_2.0.0         statmod_1.4.35      
#>  [88] tweenr_1.0.2         bslib_0.2.4          stringi_1.7.3       
#>  [91] ps_1.6.0             Brobdingnag_1.2-6    lattice_0.20-41     
#>  [94] Matrix_1.2-18        nloptr_1.2.2.2       markdown_1.1        
#>  [97] shinyjs_2.0.0        tensorA_0.36.1       vctrs_0.3.8         
#> [100] pillar_1.6.1         lifecycle_1.0.0      jquerylib_0.1.3     
#> [103] bridgesampling_1.1-2 httpuv_1.5.5         R6_2.5.0            
#> [106] bookdown_0.19        promises_1.2.0.1     gridExtra_2.3       
#> [109] codetools_0.2-16     boot_1.3-25          colourpicker_1.1.1  
#> [112] MASS_7.3-53          gtools_3.8.2         assertthat_0.2.1    
#> [115] withr_2.4.2          shinystan_2.5.0      mgcv_1.8-33         
#> [118] parallel_4.0.3       hms_1.1.0            grid_4.0.3          
#> [121] coda_0.19-4          minqa_1.2.4          rmarkdown_2.9.6     
#> [124] ggforce_0.3.2.9000   shiny_1.6.0          lubridate_1.7.10    
#> [127] base64enc_0.1-3      dygraphs_1.1.1.6
```

## References

<div id="refs"></div>


---
