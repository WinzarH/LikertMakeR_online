---
title: "Step 1: Define a correlation matrix"
---
  
<!-- #### Step 1: Define a correlation matrix -->

### Options:

#### (1) Define correlations based on _Cronbach's alpha_

Recommended for creating individual items (questions) that apply to a scale with the desired Cronbach's alpha.

Required parameters are:

  -  **_scales_**: number items in the scale
  
  - **_alpha_**: a target Cronbach's alpha. 

  - **_variance_**: amount of variation in the correlations generated 
  (default = '0.5'):  
  
     * '0' causes all correlations to be equal; 
     
     * '1' will make some correlations high and some quite low or negative; 
     
     * '1.5' risks generating an infeasible non-positive-definite matrix


#### (2) Upload your own correlation matrix as a CSV file

Recommended for generating several multi-item scales that are correlated in a predefined way.

A **template** can be downloaded.

#####  Correlation matrix example:


```
var,Extraversion,Agreeableness,Conscientiousness,Neuroticism,Openness,Humility
Extraversion,1,0.213,0.016,-0.088,0.264,0.106
Agreeableness,0.213,1,0.14,-0.033,-0.233,0.156
Conscientiousness,0.016,0.14,1,-0.112,0.0021,0.072
Neuroticism,-0.088,-0.033,-0.112,1,-0.01,-0.192
Openness,0.264,-0.233,0.0021,-0.01,1,0.018
Humility,0.106,0.156,0.072,-0.192,0.018,1
```

###### matrix source:

Milojev, P., D. Osborne, L.M. Greaves, F.K. Barlow, C.G. Sibley, (2013),
"The Mini-IPIP6: Tiny yet highly stable markers of Big Six personality",
_Journal of Research in Personality_, 47/6:936-944. 
<https://doi.org/10.1016/j.jrp.2013.09.004>
  
