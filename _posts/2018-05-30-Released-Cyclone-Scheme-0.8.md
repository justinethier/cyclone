---
layout: post
title: Released Cyclone Scheme 0.8
excerpt: Support for complex numbers has finally been added to Cyclone in this release.
---

Features

- Added support for complex numbers.
- When printing intermediate forms during debugging via `-t` Cyclone will now emit less verbose S-expressions for code in CPS form. To support this effort and make debugging easier, added helper functions `ast:ast->sexp`, `ast:sexp->ast`, and `ast:ast->pp-sexp` to `(scheme cyclone ast)`.
- Avoid inlining function calls into named let loops.

