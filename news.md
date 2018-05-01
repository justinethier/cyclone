---
layout: main
title: News
ghproj: "http://github.com/justinethier/cyclone/tree/master/"
---

{% for post in site.posts %}
  <p style="font-size: xx-large;">
  [{{ post.title }}](./{{ post.url }})
  </p>
  <span class="date-labels">{{post.date | date: "%b %d %Y" }}</span>
  <br />
  {{ post.excerpt }}

{% endfor %}

