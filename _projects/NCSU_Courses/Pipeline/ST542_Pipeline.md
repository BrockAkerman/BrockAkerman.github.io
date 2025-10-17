---
layout: project
title: "Machine Learning Pipeline for Power Grid Optimization"
permalink: /portfolio/st542_Pipeline/
date: 2024-05-01
tags:
  - North Carolina State University
  - ST542
  - PySpark
  - Machine Learning
  - Data Engineering
  - Predictive Modeling
---

<section id="overview">
  <h2>Project Overview</h2>
  <p>
    This project, completed as part of the <strong>ST542: Statistical Learning and Data Mining</strong> course at 
    <strong>North Carolina State University</strong>, demonstrates the development of a scalable 
    <em>machine learning pipeline</em> using <strong>PySpark</strong> for optimizing energy distribution in 
    renewable power grids.
  </p>

  <p>
    The analysis integrates exploratory data analysis (EDA), feature engineering, model training, and evaluation 
    within an automated Spark pipeline. The workflow simulates continuous data ingestion and retraining to 
    mimic real-world streaming behavior for renewable energy forecasting.
  </p>

  <p><strong>Submitted Analysis:</strong> You can view the full report here:
    <a href="{{ '/assets/literature/Model_and_Stream.pdf' | relative_url }}" target="_blank">Final Report (PDF)</a>.
  </p>

  <img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST542/Heatmap.png' | relative_url }}" 
       alt="PySpark Power Grid Pipeline Visualization" class="image fit" />
</section>

<section id="deliverables">
  <h2>Key Deliverables</h2>
  <ul>
    <li>Exploratory Data Analysis (EDA) of energy generation and weather datasets</li>
    <li>Feature selection and transformation using PySpark’s MLlib</li>
    <li>Implementation of a scalable <code>Pipeline()</code> and <code>CrossValidator()</code></li>
    <li>Model comparison and tuning for regression and classification tasks</li>
    <li>Demonstration of continuous data streaming and retraining simulation</li>
    <li>Full reproducibility through Jupyter Notebook and Binder integration</li>
  </ul>
</section>
