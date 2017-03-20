---
layout: main
title: News
ghproj: "http://github.com/justinethier/cyclone/tree/master/"
---

{% for post in site.posts %}
  <span class="date-labels">{{post.date | date: "%b %d %Y" }}</span>
  »
  [{{ post.title }}](./{{ post.url }})
  <br />
  {{ post.excerpt }}

{% endfor %}

