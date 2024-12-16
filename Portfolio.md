---
layout: portfolio
title: "Portfolio Gallery"
permalink: /portfolio/
---

<section id="portfolio">
  <h2>Featured Projects</h2>
  <div class="row">
    <!-- Project 1 -->
    <article class="6u 12u$(xsmall) work-item">
      <a href="/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST558/MainTile.png" class="image fit thumb">
        <img src="/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST558/MainTile.png" alt="Airport JSON ShinyDash" />
      </a>
      <h3>Airport JSON ShinyDash</h3>
      <p>A Shiny Dash project analyzing JSON data for ST558.</p>
    </article>

    <!-- Project 2 -->
    <article class="6u 12u$(xsmall) work-item">
      <a href="/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST518/MainTile.png" class="image fit thumb">
        <img src="/assets/media/thumbs/ProjectAssets/NCSU_Assets/ST518/MainTile.png" alt="Solubility Analysis" />
      </a>
      <h3>Solubility Analysis of Cold Brand Medicines</h3>
      <p>Effervescence analysis project for ST518.</p>
    </article>

    <!-- Add more projects here -->
  </div>
</section>


<!-- 
<section id="portfolio">
  <h2>Featured Projects</h2>
  <div class="row">
    {% for project in site.projects %}
    <article class="6u 12u$(xsmall) work-item">
      <a href="{{ project.url }}" class="image fit thumb">
        <img src="{{ project.image }}" alt="{{ project.title }}" />
      </a>
      <h3>{{ project.title }}</h3>
      <p>{{ project.description }}</p>
    </article>
    {% endfor %}
  </div>
</section>

ASK ABOUT USING LIQUID IN _PROJECTS FOLDER
-->
