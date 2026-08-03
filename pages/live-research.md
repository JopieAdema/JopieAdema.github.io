---
title: Live Research
permalink: /live-research/
css:
  - "/assets/css/live-research.css"
---

<section class="lr-intro" aria-labelledby="live-research-introduction">
  <h2 id="live-research-introduction">A faster feedback loop for policy</h2>
  <p>Social science research is slower than it can be: we observe a policy change, wait until the data is there, send it to a journal, leading to a long lag until the relevant policy makers receive feedback. However, this does not need to be (it's 2026). On this page, I post real-time research notes that automatically update as new data becomes available. The first, on EU migration policy, can be found below.</p>
  <p>If you're interested, shoot me an email at <a href="mailto:j.a.h.adema94@gmail.com">j.a.h.adema94@gmail.com</a>!</p>
</section>

<section class="lr-projects" aria-labelledby="research-notes">
  <div class="lr-section-heading">
    <div>
      <h2 id="research-notes">Research notes</h2>
    </div>
    <span class="lr-status"><span aria-hidden="true"></span> Updating monthly</span>
  </div>

  {% if site.data.living_research and site.data.living_research.size > 0 %}
  {% for note in site.data.living_research %}
  <article class="lr-card">
    <div class="lr-card-meta">
      <span class="lr-pill">{{ note.status }}</span>
      <span>Data through {{ note.data_vintage }}</span>
    </div>
    <h3><a href="/assets/live-research/{{ note.slug }}/">{{ note.title }}</a></h3>
    <p>{{ note.summary }}</p>
    <a class="lr-card-link" href="/assets/live-research/{{ note.slug }}/">Read the live note <span aria-hidden="true">→</span></a>
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

<aside class="lr-method" aria-labelledby="how-live-research-works">
  <h2 id="how-live-research-works">How Live Research works</h2>
  <div class="lr-method-grid">
    <div><span>1</span><h3>New data arrive</h3><p>Public source data are checked on a monthly schedule.</p></div>
    <div><span>2</span><h3>Analysis reruns</h3><p>Versioned R code validates and analyzes the latest release.</p></div>
    <div><span>3</span><h3>Results update</h3><p>Only validated outputs replace the last successful publication.</p></div>
  </div>
</aside>
