---
layout: project
title: "Bayesian Modeling of Hydroformylation Reaction Data"
permalink: /portfolio/st558-hydroformylation/
date: 2023-04-26
tags:
  - North Carolina State University
  - ST558
  - Bayesian Analysis
  - LASSO Regression
  - R Programming
  - MCMC
  - Data Visualization
  - Chemical Modeling
---

<section id="overview">
  <h2>Project Overview</h2>
  <p>
    This project, completed as part of the <strong>ST558: Data Science for Statisticians</strong> course at 
    <strong>North Carolina State University</strong>, applied advanced <em>Bayesian modeling</em> and 
    <em>LASSO regression</em> techniques to experimental chemical data from a hydroformylation process.  
    The analysis aimed to predict the composition of two aldehyde products (linear vs. branched) 
    given varying temperature, pressure, and reactant ratios.
  </p>

  <p>
    Using a dataset of 106 experimental runs, I explored relationships among seven predictor variables, 
    performed <strong>principal component analysis (PCA)</strong>, and implemented multiple 
    <strong>Bayesian linear and hierarchical models</strong> to estimate coefficients and model fit.  
    Convergence diagnostics such as Gelman-Rubin, Geweke, and Effective Sample Size (ESS) were used 
    to assess model performance across chains.
  </p>

  <p><strong>Submitted Analysis:</strong> You can view the full report here: 
    <a href="{{ '/_projects/NCSU_Courses/ST558/project_v3_20230426.pdf' | relative_url }}" target="_blank">Final Report (PDF)</a>.
  </p>

  <img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST558/PROJ04/MainTile.png' | relative_url }}" alt="Bayesian Hydroformylation Modeling Visualization" class="image fit" />
</section>

<section id="deliverables">
  <h2>Key Deliverables</h2>
  <ul>
    <li>Exploratory data analysis with correlation and PCA visualizations</li>
    <li>Bayesian linear model implementation using Gibbs sampling</li>
    <li>Bayesian LASSO regression with and without random effects</li>
    <li>Expanded quadratic and interaction terms for model flexibility</li>
    <li>Model convergence assessment via Geweke, Gelman-Rubin, and ESS metrics</li>
    <li>Comparison of WAIC and DIC model fit criteria</li>
  </ul>
</section>
