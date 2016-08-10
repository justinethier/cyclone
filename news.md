---
layout: main
title: News
ghproj: "http://github.com/justinethier/cyclone/tree/master/"
---

{% for post in site.posts %}
  <span class="test">{{post.date | date: "%b %d %Y" }}</span>
  »
  [{{ post.title }}](./{{ post.url }})

{% endfor %}

