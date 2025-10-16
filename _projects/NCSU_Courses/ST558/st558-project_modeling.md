---
layout: project # Use a specific layout for a project (you may need to create this layout)
title: "Diabetes Health Indicators Modeling"
permalink: /portfolio/st558-project/
date: 2023-05-01
tags: 
  - North Carolina State University
  - ST558
  - R Programming
  - Data Visualization
  - Modeling
  - Logistic Regression
  - Log Loss
  - Classification Trees
  - Best Sets
  - Epidemiology
---

<!-- Project Overview -->
<section id="overview">
  <h2>Project Overview</h2>
  <p>
    This project, completed as part of the <strong>ST558: Data Science for Statisticians</strong> course at 
    <strong>North Carolina State University</strong>, focused on developing predictive models to understand 
    and classify diabetes risk factors using the <em>Behavioral Risk Factor Surveillance System (BRFSS 2015)</em> dataset.
  </p>

  <p>
    The analysis involved cleaning and transforming health indicator data, splitting into training and test sets, 
    and comparing the performance of multiple models — including <strong>logistic regression</strong>, 
    <strong>classification trees</strong>, and <strong>random forests</strong>. Model performance was evaluated 
    using metrics such as log loss, accuracy, precision, recall, and F1-score.
  </p>

  <p>
    The final model identified the most effective predictors of diabetes status, highlighting relationships 
    between key health factors such as blood pressure, cholesterol, BMI, and general health.
  </p>

  <p><strong>Submitted Analysis:</strong> You can view the full report here:
    <a href="{{ '/_projects/NCSU_Courses/ST558/Modeling.pdf' | relative_url }}" target="_blank">Final Report (PDF)</a>.
  </p>

  <img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST558/PROJ03/MainTile.png' | relative_url }}" alt="Diabetes Modeling Visualization" class="image fit" />
</section>

<!-- Key Deliverables -->
<section id="deliverables">
  <h2>Key Deliverables</h2>
  <ul>
    <li>Data cleaning and preparation using <code>dplyr</code> and <code>tidyverse</code></li>
    <li>Model development with <code>caret</code>, <code>rpart</code>, and <code>randomForest</code></li>
    <li>Evaluation using <code>log loss</code>, <code>accuracy</code>, <code>precision</code>, <code>recall</code>, and <code>F1-score</code></li>
    <li>Comparison of model types to identify best performance</li>
    <li>Reproducible workflow using R Markdown and knitr</li>
  </ul>
</section>
