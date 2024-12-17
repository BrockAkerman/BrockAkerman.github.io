---
layout: post_archive
title: "The Math Path: A Statistics Enthusiast's Journal"
permalink: /blogs-and-interests/
---

<div class="blogs-and-interests">
  <div class="blog-cards-container">
  {% for post in site.posts %}
    <a href="{{ post.url | relative_url }}" class="blog-card">
      <div class="blog-image" style="background-image: url('{{ post.image | default: '/assets/default-image.jpg' | relative_url }}');"></div>
      <div class="blog-content">
        <h2>{{ post.title }}</h2>
        <p class="blog-meta">{{ post.date | date: "%B %d, %Y" }}</p>
        <p>{{ post.excerpt }}</p>
      </div>
    </a>
  {% endfor %}
</div>
<!-- 
  <div class="pagination">
    {% if paginator.previous_page %}
      <a href="{{ paginator.previous_page_path | relative_url }}">&laquo; Previous</a>
    {% endif %}
    {% for page in (1..paginator.total_pages) %}
      <a href="{{ paginator.paginate_path | replace: ':num', page | relative_url }}"
         class="{% if page == paginator.page %}active{% endif %}">
        {{ page }}
      </a>
    {% endfor %}
    {% if paginator.next_page %}
      <a href="{{ paginator.next_page_path | relative_url }}">Next &raquo;</a>
    {% endif %}
  </div>
-->  
