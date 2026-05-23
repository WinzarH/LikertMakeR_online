---
title: "Generate correlated multi-item scales"
---

<!--
## Generate correlated multi-item scales
-->

### Step 1: Establish a correlation matrix

  1. In the **Correlation Matrix** tab,
  
  Hit the **Upload CSV file** radio button
  
Upload a correlation matrix in CSV file format.

The correlation matrix must be a square matrix with the same dimensions as the number of scales (columns) you want in your dataframe.

A **template** can be downloaded.

####  Correlation matrix example :

Big 6 personality protocol  (Milojev et al., 2013)[^1]

```
var, Extraversion, Agreeableness, Conscientiousness, Neuroticism, Openness, Humility
Extraversion, 1, 0.213, 0.016, -0.088, 0.264, 0.106
Agreeableness, 0.213, 1, 0.14, -0.033, -0.233, 0.156
Conscientiousness, 0.016, 0.14, 1, -0.112, 0.0021, 0.072
Neuroticism, -0.088, -0.033, -0.112, 1, -0.01, -0.192
Openness, 0.264, -0.233, 0.0021, -0.01, 1, 0.018
Humility, 0.106, 0.156, 0.072, -0.192, 0.018, 1
```

**Note:** Technically, data points in a CSV file are separated with a "comma" 
only - no spaces - but I have separated them here for ease-of-reading. 
_The program will quietly repair most deviations from strict CSV formatting._


### Step 2: Establish scale properties

  2. hit the **Data Generation** tab

Either, complete the online form, or
upload a CSV file

#### 2a: Complete the online form:

Required parameters are:

  - **_n_**: number of observations.  Acceptable range is '4' to '1024'

  - **_mean_**: mean value of each variable. Default = '3'. 

  - **_sd_**: standard deviation value of each variable. Default = '1'. 
  
   - **_lowerbound_**: a value for the lower bound of each variable _(e.g., '1' in a 1&nbsp;to&nbsp;5 rating scale)_. Default = '1'.

  - **_upperbound_**: a value for the upper bound of each variable _(e.g., '7' in a 1&nbsp;to&nbsp;7 rating scale)_. Default = '5'.

  - **_nItems_**: number of items in the scale. Default = '1'. 

Default, starter, values are given in the input array.


#### 2b: Upload a CSV file:

The **Comma-separated Values** (CSV) file should have a single row with column names, followed by 'k' rows, where 'k' = the number of scales you want to produce. 

Column names must be labelled:

  - scale (scale name)
  - mean (scale mean)
  - sd (scale standard deviation)
  - lower (scale lowerbound (e.g., '1' in a 1-5 scale))
  - upper (scale upperbound (e.g., '5' in a 1-5 scale))
  - nItems (number of items (questions) in the scale)

#####  Example parameters CSV file

Six constructs, each measured with four 1-7 rating-scale items (Milojev et al., 2013)

```
scale,             mean,  sd,   lower, upper, nItems
Extraversion,      4,     1.16, 1,     7,     4
Agreeableness,     5.27,  0.98, 1,     7,     4
Conscientiousness, 5.17,  1.05, 1,     7,     4
Neuroticism,       3.39,  1.1,  1,     7,     4
Openness,          4.76,  1.12, 1,     7,     4
Humility,          5.15,  1.32, 1,     7,     4

```

### Step 3: Generate the dataframe

When you are satisfied with the correlations and the scale properties, then:

  - Enter the desired sample size, and

  - hit the **Generate my data** button
  
Your new dataframe will appear a moment later.

_(This may take time if you have many variables and a large sample size)_

  - Hit the **Download my data** button to retrieve your data as a CSV file 

### Step 3: Data validation

  3. Hit the **Data Validation** tab
  
You should see summary statistics and a graphic summary of your dataframe.



[^1]: Milojev, P., D. Osborne, L.M. Greaves, F.K. Barlow, C.G. Sibley, (2013),
"The Mini-IPIP6: Tiny yet highly stable markers of Big Six personality",
_Journal of Research in Personality_, 47/6:936-944. 
<https://doi.org/10.1016/j.jrp.2013.09.004>
