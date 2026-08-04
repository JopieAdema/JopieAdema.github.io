---
title: Living Research Notes
permalink: /living-research/
full-width: true
css:
  - "/assets/css/living-research.css"
---

<div class="lr-page">

<section class="lr-intro" aria-labelledby="living-research-introduction">
  <h2 id="living-research-introduction">A faster feedback loop for policy evaluation</h2>
  <p>Social science research is slow: we observe a policy change, wait till data arrives, and try to publish it, leading to a long lag until policy makers receive feedback. However, this does not need to be (it's 2026). Here, I post real-time research notes that automatically update as new data becomes available. The first is on EU migration policy.</p>
</section>

<section class="lr-projects" aria-label="Living research projects">
  <div class="lr-status-row">
    <span class="lr-status"><span aria-hidden="true"></span> Updating monthly</span>
  </div>

  {% if site.data.living_research and site.data.living_research.size > 0 %}
  {% for note in site.data.living_research %}
  <article class="lr-card">
    <div class="lr-card-meta">
      <span class="lr-pill">{{ note.status }}</span>
      <span>Data through {{ note.data_vintage }}</span>
    </div>
    <h3><a href="/assets/living-research/{{ note.slug }}/">{{ note.title }}</a></h3>
    <p>{{ note.summary }}</p>
    <div class="lr-embed">
      <iframe src="/assets/living-research/{{ note.slug }}/" title="{{ note.title }}" loading="lazy"></iframe>
      <div class="lr-embed-fade"></div>
    </div>
    <a class="lr-card-link" href="/assets/living-research/{{ note.slug }}/">Read the full living note <span aria-hidden="true">→</span></a>
  </article>
  {% endfor %}
  {% else %}
  <article class="lr-card lr-card-pending">
    <div class="lr-card-meta">
      <span class="lr-pill">First note</span>
      <span>EU migration policy</span>
    </div>
    <h3>Research question in development</h3>
    <p>The reproducible publication pipeline is ready. The policy reform, data sources, and research design for this first note will be defined next.</p>
    <div class="lr-card-grid" aria-label="Planned research note sections">
      <span><strong>01</strong> Setting &amp; idea</span>
      <span><strong>02</strong> Research design</span>
      <span><strong>03</strong> Preliminary results</span>
      <span><strong>04</strong> Conclusion</span>
    </div>
  </article>
  {% endif %}
</section>

<aside class="lr-method" aria-labelledby="how-living-research-works">
  <h2 id="how-living-research-works">How Living Research works</h2>
  <div class="lr-method-grid">
    <div><span>1</span><h3>New data arrive</h3><p>Public source data are checked on a monthly schedule.</p></div>
    <div><span>2</span><h3>Analysis reruns</h3><p>Versioned R code validates and analyzes the latest release.</p></div>
    <div><span>3</span><h3>Results update</h3><p>Only validated outputs replace the last successful publication.</p></div>
  </div>
</aside>

</div>
