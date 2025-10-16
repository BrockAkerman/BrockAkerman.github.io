---
layout: project
title: "Framingham Heart Study: Blood Pressure Mean Comparison"
permalink: /portfolio/st502-project/
date: 2023-03-15
tags:
  - North Carolina State University
  - ST502
  - Inferential Statistics
  - Hypothesis Testing
  - t-Test
  - Power Analysis
  - Confidence Intervals
  - R Programming
---

<section id="overview">
  <h2>Project Overview</h2>
  <p>
    This project, completed as part of the <strong>ST502: Fundamentals of Statistical Inference II</strong> course at 
    <strong>North Carolina State University</strong>, investigates whether there is a statistically significant 
    difference in the mean systolic blood pressure between smokers and non-smokers in the 
    <em>Framingham Heart Study</em> dataset.
  </p>

  <p>
    The analysis compares group means using both the <strong>pooled-variance t-test</strong> (assuming equal variances) 
    and the <strong>Welch–Satterthwaite t-test</strong> (for unequal variances), supported by a detailed 
    exploratory data analysis to verify test assumptions such as normality and homogeneity of variance.
  </p>

  <p>
    In addition, <strong>bootstrap resampling</strong> methods were employed to assess the robustness of the 
    estimated mean difference and its confidence interval, while <strong>power analysis</strong> was used to 
    evaluate the sensitivity of the study design under varying sample sizes and effect magnitudes.
  </p>

  <p><strong>Submitted Analysis:</strong> You can view the full report here:
    <a href="{{ '/_projects/NCSU_Courses/ST502/R_Project/ST502_R_Project/ST502_R_Project_Complete.pdf' | relative_url }}" target="_blank">Final Report (PDF)</a>.
  </p>

  <img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST502/MainTile.png' | relative_url }}" alt="Framingham Heart Study Visualization" class="image fit" />
</section>

<section id="deliverables">
  <h2>Key Deliverables</h2>
  <ul>
    <li>Exploratory analysis of systolic blood pressure by smoking status</li>
    <li>Two-sample t-tests: pooled and Welch–Satterthwaite approaches</li>
    <li>95% confidence intervals for mean differences</li>
    <li>Bootstrap resampling for interval validation</li>
    <li>Power analysis to assess effect detection capability</li>
    <li>Visualization of distributions and test results using <code>ggplot2</code></li>
    <li>Reproducible reporting via R Markdown and knitr</li>
  </ul>
</section>
