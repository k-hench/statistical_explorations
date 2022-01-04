---
output: html_document
editor_options:
  chunk_output_type: console
---
# References and Session Info

## Session Info



### R settings


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
#> [1] parallel  stats     graphics  grDevices utils     datasets  methods  
#> [8] base     
#> 
#> other attached packages:
#>  [1] reticulate_1.22      bayesplot_1.8.1      mixedup_0.3.9       
#>  [4] tidybayes_3.0.1      rethinking_2.21      cmdstanr_0.4.0.9000 
#>  [7] rstan_2.21.2         StanHeaders_2.21.0-7 ggdag_0.2.3.9000    
#> [10] GGally_1.5.0         ggtext_0.1.1         brms_2.16.1         
#> [13] Rcpp_1.0.6           EnvStats_2.4.0       ggraph_2.0.5.9000   
#> [16] tidygraph_1.2.0      glue_1.4.2           patchwork_1.1.0.9000
#> [19] prismatic_1.0.0.9000 forcats_0.5.1        stringr_1.4.0       
#> [22] dplyr_1.0.6          purrr_0.3.4          readr_1.4.0         
#> [25] tidyr_1.1.3          tibble_3.1.2         ggplot2_3.3.5       
#> [28] tidyverse_1.3.0.9000
#> 
#> loaded via a namespace (and not attached):
#>   [1] readxl_1.3.1         backports_1.2.1      plyr_1.8.6          
#>   [4] igraph_1.2.6         svUnit_1.0.6         splines_4.0.3       
#>   [7] crosstalk_1.1.1      inline_0.3.17        rstantools_2.1.1    
#>  [10] digest_0.6.27        htmltools_0.5.1.1    viridis_0.5.1       
#>  [13] rsconnect_0.8.24     fansi_0.5.0          magrittr_2.0.1      
#>  [16] checkmate_2.0.0      graphlayouts_0.7.1   modelr_0.1.8        
#>  [19] RcppParallel_5.0.2   matrixStats_0.56.0   xts_0.12.1          
#>  [22] prettyunits_1.1.1    colorspace_2.0-2     rvest_1.0.0         
#>  [25] ggrepel_0.9.1        ggdist_3.0.1         haven_2.3.1         
#>  [28] xfun_0.24            callr_3.6.0          crayon_1.4.1        
#>  [31] jsonlite_1.7.2       lme4_1.1-26          zoo_1.8-8           
#>  [34] polyclip_1.10-0      gtable_0.3.0         V8_3.4.0            
#>  [37] distributional_0.2.2 pkgbuild_1.2.0       shape_1.4.5         
#>  [40] abind_1.4-5          scales_1.1.1         mvtnorm_1.1-2       
#>  [43] DBI_1.1.1            miniUI_0.1.1.1       gridtext_0.1.4      
#>  [46] viridisLite_0.4.0    xtable_1.8-4         stats4_4.0.3        
#>  [49] DT_0.17              htmlwidgets_1.5.3    httr_1.4.2          
#>  [52] threejs_0.3.3        arrayhelpers_1.1-0   RColorBrewer_1.1-2  
#>  [55] posterior_1.1.0      ellipsis_0.3.2       reshape_0.8.8       
#>  [58] pkgconfig_2.0.3      loo_2.4.1            farver_2.1.0        
#>  [61] sass_0.4.0.9000      dbplyr_2.1.1         utf8_1.2.1          
#>  [64] tidyselect_1.1.1     rlang_0.4.12         reshape2_1.4.4      
#>  [67] later_1.1.0.1        munsell_0.5.0        cellranger_1.1.0    
#>  [70] tools_4.0.3          cli_3.1.0            generics_0.1.0      
#>  [73] broom_0.7.6          ggridges_0.5.2       evaluate_0.14       
#>  [76] fastmap_1.1.0        yaml_2.2.1.99        processx_3.5.1      
#>  [79] knitr_1.33           fs_1.5.0             nlme_3.1-149        
#>  [82] mime_0.11            projpred_2.0.2       xml2_1.3.2          
#>  [85] compiler_4.0.3       shinythemes_1.2.0    rstudioapi_0.13     
#>  [88] png_0.1-7            curl_4.3             gamm4_0.2-6         
#>  [91] reprex_2.0.0         statmod_1.4.35       tweenr_1.0.2        
#>  [94] bslib_0.2.4          stringi_1.7.3        ps_1.6.0            
#>  [97] Brobdingnag_1.2-6    lattice_0.20-41      Matrix_1.2-18       
#> [100] nloptr_1.2.2.2       markdown_1.1         shinyjs_2.0.0       
#> [103] tensorA_0.36.1       vctrs_0.3.8          pillar_1.6.1        
#> [106] lifecycle_1.0.0      jquerylib_0.1.3      bridgesampling_1.1-2
#> [109] httpuv_1.5.5         R6_2.5.0             bookdown_0.19       
#> [112] promises_1.2.0.1     gridExtra_2.3        codetools_0.2-16    
#> [115] boot_1.3-25          colourpicker_1.1.1   MASS_7.3-53         
#> [118] gtools_3.8.2         assertthat_0.2.1     withr_2.4.2         
#> [121] shinystan_2.5.0      mgcv_1.8-33          hms_1.1.0           
#> [124] grid_4.0.3           coda_0.19-4          minqa_1.2.4         
#> [127] rmarkdown_2.9.6      ggforce_0.3.2.9000   shiny_1.6.0         
#> [130] lubridate_1.7.10     base64enc_0.1-3      dygraphs_1.1.1.6
```

<center>
<img src='img/Rlogo.svg' width='139' />
</center>

### Python settings


```python
import arviz as az
import matplotlib.pyplot as plt
import numpy as np
import pymc3 as pm
import scipy.stats as stats
import seaborn as sns
import matplotlib
from matplotlib.colors import ListedColormap
import matplotlib.font_manager
import session_info

session_info.show(html = False)
```

```
#> -----
#> arviz               0.11.4
#> matplotlib          3.5.1
#> numpy               1.21.4
#> pymc3               3.11.4
#> scipy               1.7.3
#> seaborn             0.11.2
#> session_info        1.0.0
#> -----
#> IPython             7.29.0
#> jupyter_client      6.1.2
#> jupyter_core        4.6.3
#> notebook            6.0.3
#> -----
#> Python 3.8.10 (default, Nov 26 2021, 20:14:08) [GCC 9.3.0]
#> Linux-5.11.0-43-generic-x86_64-with-glibc2.29
#> -----
#> Session information updated at 2022-01-05 00:41
```

<center>
<img src='img/python_logo.svg' align='center' width='139' />
</center>

## References

<div id="refs"></div>


---
