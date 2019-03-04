---
layout: news
title: News
ghproj: "http://github.com/justinethier/cyclone/tree/master/"
---

{% for post in site.posts %}
  [{{ post.title }}](./{{ post.url }})
  <br />
  <span class="date-labels">{{post.date | date: "%B %e, %Y" }}</span>
  <br />
  {{ post.excerpt }}
  <br />
  <br />

{% endfor %}

