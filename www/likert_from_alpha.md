---
title: "Likert-scale items from Cronbach's Alpha and predefined moments"
---

<!-- 
## Generate Likert-scale items from Cronbach's Alpha and predefined moments
-->

### Step 1: Correlation matrix from Cronbach's alpha

  1. In the **Correlation Matrix** tab,
   
  Click on the **"Generate from Cronbach's alpha"** button (the default)

   - Enter the _Number of items_ in your scale
   - set the _Target alpha_ value
   - for most purposes, leave _"Variance"_ at the default (`0.5`)
     
When ready, hit the **"Generate correlation matrix"** button 

You will see a correlation matrix that will deliver a dataframe with your desired alpha.

### Step 2: Generate your data

   2. hit the **Data Generation** tab
 
   Most users prefer **"Manual entry"**

   - Complete the online form to enter for each item:
   
     * Scale name,
     * Scale mean,
     * Scale standard deviation
     * Lower bound (e.g., `1` on a `1-5` scale)
     * Upper bound (e.g., `5` on a `1-5` scale)
     * nItems should remain at `1`

   - Enter the **Number of Observations**
     
  Hit the **Generate my data** button.
  
  You should see a dataframe with columns to match your specifications.
  

### Step 3: Validate your data

  3. Hit the **Data Validation** tab

  A visual summary of your data will appear
  
  Additional panels reveal data moments (means and standard deviations), 
  calculated Cronbach's alpha and more.

