---
layout: project
title: "Interactive Data Science Model Decision Tree"
permalink: /portfolio/ds-model-decisioner/
date: 2026-03-27
tags:
  - React
  - Vite
  - Machine Learning Strategy
  - Data Science Lifecycle
  - Tool Development
  - Frontend Engineering
---

### Project Overview

The **Data Science Decisioner** is an interactive web application designed to streamline the model selection process. Built with **React** and **Vite**, this tool serves as a functional "Command Center" for data scientists, guiding them through a logical flow—from identifying high-level tasks to selecting specific algorithms based on data characteristics.

Rather than relying on static cheat sheets, this tool provides a dynamic interface that covers Supervised Learning (Classification/Regression), Unsupervised Learning (Clustering/Anomaly Detection), Deep Learning, and Reinforcement Learning. It was developed to bridge the gap between theoretical knowledge and practical model deployment.

<img src="/assets/media/thumbs/ProjectAssets/Freelance_Assets/decisioner.PNG" alt="Data Science Decisioner Interface" style="width:100%; max-width:800px; display:block; margin:auto; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.1);">

### Key Outcomes

- **Automated Logical Workflows:** Created a nested decision-tree structure that handles complex ML logic, such as class imbalance, target dimensionality, and data modality.
- **Modern Web Architecture:** Utilized Vite for lightning-fast Hot Module Replacement (HMR) and optimized production builds.
- **Comprehensive Scoping:** Built out "v2" logic encompassing 40+ distinct model endpoints, including specialized branches for Dimensionality Reduction and Association Rule Mining.
- **Reference Integration:** Integrated "DS Toolkit" references into the UI, linking specific decision paths to pre-existing code libraries and notebooks.

### Technical Implementation

- **React State Management:** Handles user navigation through the tree nodes while maintaining a history for "back" and "restart" functionality.
- **Vite Build Pipeline:** Deployed via GitHub Actions for continuous integration and automated hosting.
- **Modular Component Design:** Structured to allow for easy expansion, such as the upcoming integration with local NLP models (Ollama).

### Key Deliverables

- **Live Interactive Tool:** Explore the decision tree in real-time:  
  [Launch Data Science Decisioner](https://brockakerman.github.io/stats-decision-tree/)

- **Source Code Repository:** View the React implementation and logic:  
  [GitHub Project Repository](https://github.com/BrockAkerman/stats-decision-tree)