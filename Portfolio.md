


Claim offer
You said:

Portfolio.md
File
Next project to work on is my website.  Attached is the file that I believe creates the framework for the portfolio part of my website.  

I need to create a separate section that is distinguised from the academic stuff.  For example, I just completed a project for bank lending risk pipeline.  It was not done while I was enrolled as a student but it was not done while I was employed with anyone either.  What should I title this section and can you update this .md file to incorporate this new section?


ChatGPT said:
Good — this is actually a very common portfolio problem once you move from “student” → “professional statistician.” You now have work that is:

✅ not coursework

✅ not employer IP

✅ still serious, real-world work

✅ demonstrates initiative and applied skill

That deserves its own category, because recruiters interpret sections differently.

✅ What to Title the New Section
You want a title that signals independent professional capability, not hobby work.

Here are strong options ranked by how hiring managers interpret them:

⭐ Best choice (recommended)
Independent Projects

Why this works:

Sounds professional and intentional

Clearly separate from academics

Common in data science portfolios

Implies self-directed applied work

Strong alternatives
Applied Projects → emphasizes real-world application

Professional Projects → good, but can confuse if not employer-based

Independent Research & Development

Personal Applied Analytics

❌ Avoid
Personal Projects (sounds beginner/junior)

Side Projects (sounds casual)

Miscellaneous

👉 I recommend:

Independent Projects

It positions your bank lending risk pipeline as self-directed professional work, which is exactly what it is.

✅ Structural Change Needed
Right now your page has one single grid containing everything.

Instead, we’ll create:

Independent Projects
    (bank lending risk pipeline, etc.)

Academic Projects
    (current portfolio items)
This keeps visual consistency while creating a narrative hierarchy.

✅ Updated .md File (Drop-In Replacement)
Below is your updated file with:

New Independent Projects section added

Existing projects moved under Academic Projects

Grid styling preserved

Easy place to add future independent work

You can paste this directly over your current file.

✅ Updated Portfolio.md
---
layout: portfolio
title: "Portfolio Gallery"
permalink: /portfolio/
---

<section id="portfolio">

  <!-- ============================= -->
  <!-- Independent Projects Section -->
  <!-- ============================= -->

  <h2>Independent Projects</h2>

  <div class="portfolio-container">
    <div class="portfolio-grid">

      <!-- Independent Project Example -->
      <!-- Replace link + image when ready -->
      <article class="work-item">
        <a href="{{ '/portfolio/bank_lending_risk_pipeline/' | relative_url }}" 
           class="image fit thumb" target="_blank" rel="noopener">
          <img src="{{ '/assets/media/thumbs/ProjectAssets/Independent/bank_risk_pipeline.png' | relative_url }}" 
               alt="Bank Lending Risk Pipeline" />
        </a>
        <h3>Bank Lending Risk Pipeline</h3>
        <p>End-to-end statistical pipeline for credit risk modeling and lending decision support</p>
      </article>

      <!-- Add future independent projects here -->

    </div>
  </div>


  <!-- ============================= -->
  <!-- Academic Projects Section -->
  <!-- ============================= -->

  <h2>Academic Projects</h2>

  <div class="portfolio-container">
    <div class="portfolio-grid">

	<!-- Project 9 -->
	<article class="work-item">
	  <a href="{{ '/portfolio/st542_Pipeline/' | relative_url }}" 
         class="image fit thumb" target="_blank" rel="noopener">
		<img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST542/Heatmap.png' | relative_url }}" 
			 alt="ML Pipeline" />
	  </a>
	  <h3>Power Grid Machine Learning Pipeline</h3>
	  <p>Optimizing resources in South America</p>
	</article>

	<!-- Project 8 -->
	<article class="work-item">
	  <a href="{{ '/_projects/NCSU_Courses/ST542/Bridging_the_Gap_Dental_Veterinary_Training_Analysis.pdf' | relative_url }}" 
		 class="image fit thumb" target="_blank" rel="noopener">
		<img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST542/DVM_Density.PNG' | relative_url }}" 
			 alt="Bridging the Gap" />
	  </a>
	  <h3>"Bridging the Gap"</h3>
	  <p>Perception Gaps in Veterinary Dental Training and Preparedness</p>
	</article>

    <!-- Project Feature1 -->
    <article class="work-item">
      <a href="/portfolio/st558_Shiny/" class="image fit thumb">
        <img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST558/MainTile.png' | relative_url }}" 
             alt="Airport JSON ShinyDash" />
      </a>
      <h3>Shiny Dashboard</h3>
      <p>Visualization of real time JSON weather data at airports</p>
    </article>
  
	<!-- Project Feature2 -->
	<article class="work-item">
	  <a href="{{ '/portfolio/st518-project/' | relative_url }}" class="image fit thumb">
		<img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST518/MainTile.png' | relative_url }}" 
             alt="Solubility Analysis" />
	  </a>
	  <h3>Effervescence and Solubility Analysis</h3>
	  <p>Modeling over-the-count medications</p>
	</article>

	<!-- Project 7 -->
	<article class="work-item">
	  <a href="{{ '/portfolio/math4950-project/' | relative_url }}" class="image fit thumb">
		<img src="{{ '/assets/media/thumbs/ProjectAssets/UNG_Assets/Oxygen_Cover2.png' | relative_url }}" 
             alt="Research Paper Cover" />
	  </a>
	  <h3>Biological Research Capstone</h3>
	  <p>Deoxygenated River Water effects on Rainbow Trout Propagation</p>
	</article>

	<!-- Project 6 -->
	<article class="work-item">
	  <a href="{{ '/portfolio/math3390-project/' | relative_url }}" class="image fit thumb">
		<img src="{{ '/assets/media/thumbs/ProjectAssets/UNG_Assets/Sweet_Potato_Cover.PNG' | relative_url }}" 
             alt="MATH3390 Project Cover" />
	  </a>
	  <h3>Agricultural-Economic Analysis</h3>
	  <p>Organic vs Conventional Produce Pricing</p>
	</article>

	<!-- Project 5 -->
	<article class="work-item">
	  <a href="{{ '/portfolio/st501-project/' | relative_url }}" class="image fit thumb">
		<img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST501/MainTile.png' | relative_url }}" 
             alt="Convergence Theory Visualization" />
	  </a>
	  <h3>Visualizing Statistical Convergence</h3>
	  <p>Poisson, Exponential & Monte Carlo in R</p>
	</article>

	<!-- Project 4 -->
	<article class="work-item">
	  <a href="{{ '/portfolio/st502-project/' | relative_url }}" class="image fit thumb">
		<img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST502/MainTile.png' | relative_url }}" 
             alt="Framingham Heart Study Analysis" />
	  </a>
	  <h3>Longitudinal Framingham Heart Study</h3>
	  <p>Hypothesis Testing Deltas in Blood Pressures</p>
	</article>

	<!-- Project 3 -->
	<article class="work-item">
	  <a href="{{ '/portfolio/st558-hydroformylation/' | relative_url }}" class="image fit thumb">
		<img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST531/MainTile.png' | relative_url }}" 
             alt="Bayesian Modeling" />
	  </a>
	  <h3>Bayesian Modeling of Hydroformylation Reaction</h3>
	  <p>Exploring Bayesian Linear and LASSO Regression Models</p>
	</article>

	<!-- Project 2 -->
	<article class="work-item">
	  <a href="{{ '/portfolio/st558-project01/' | relative_url }}" class="image fit thumb">
		<img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST558/PROJ01/MainTile.png' | relative_url }}" 
             alt="Census Bureau Statistical Compendia" />
	  </a>
	  <h3>Census Bureau Statistical Compendia</h3>
	  <p>Demonstration of the R Tidyverse</p>
	</article>

	<!-- Project 1 -->
	<article class="work-item">
	  <a href="{{ '/portfolio/st558-project/' | relative_url }}" class="image fit thumb">
		<img src="{{ '/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST558/PROJ03/MainTile.png' | relative_url }}" 
             alt="Diabetes Health Indicators Modeling" />
	  </a>
	  <h3>Diabetes Health Indicators</h3>
	  <p>Leveraging Statistical Models to Find Best Models</p>
	</article>

    </div>
  </div>

</section>
🔥 Strategic Advice (Important)
You just crossed an invisible career boundary.

Recruiters interpret sections like this as:

Section	Signal
Academic Projects	“Trained capability”
Independent Projects	“Can operate without supervision”
Work Experience	“Delivered business value”
Your lending risk pipeline belongs closer psychologically to work experience than coursework — and this change communicates that immediately.

If you want, next I can show you a high-impact ordering strategy that statistically increases recruiter engagement (hint: your strongest project should appear in the first 3 tiles, not chronologically).




