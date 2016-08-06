---
layout: main
title: News
ghproj: "http://github.com/justinethier/cyclone/tree/master/"
---

{% for post in site.posts %}
  {{post.date | date: "%b %d %Y" }}
  [{{ post.title }}]({{ site.url }}/{{ post.url }})
  {{post.excerpt}}

{% endfor %}

