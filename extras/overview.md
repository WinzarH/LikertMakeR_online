---
title: "Start Here!"
subtitle: "Make synthetic rating-scale data in three steps"
---


### Path A

I have Cronbach's alpha and I want to generate a dataframe of items that 
create a multi-item scale

  1. In the **Correlation Matrix** tab,
   
  Click on the **"Generate from Cronbach's alpha"** button (the default)

   - Enter the _Number of items_ in your scale
   - set the _Target alpha_ value
   - for most purposes, leave _"Variance"_ at the default (`0.5`)
     
When ready, hit the **"Generate correlation matrix"** button 

You will see a correlation matrix that will deliver a dataframe with your desired alpha.

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
  
  

#### This is a two-step process ... plus validation:
  
  1. Define a correlation matrix 
     - Specify Cronbach's alpha for a multi-item scale, or
     - Upload your own correlation matrix as a CSV file (suggested for correlating multi-item scales)
  
  2. Define rating scale parameters: sample-size, name, mean, SD, number of 
  items in each scale
     - Input parameters manually, by editing the on-screen table, or
     - Upload a CSV file of parameters. Download a template for editing if needed.
  
  3. Validate the resulting dataframe
     - Visualise correlated scales and see summary statistics
  
  
