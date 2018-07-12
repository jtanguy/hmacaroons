Macaroons: Pure haskell implementation of macaroons
===================================================

![Maintenance Status](https://img.shields.io/badge/maintenance%20status-needs%20love-ff69b4.svg)
[![Build Status](https://travis-ci.org/jtanguy/hmacaroons.svg?branch=master)](https://travis-ci.org/jtanguy/hmacaroons)
[![Coverage Status](https://coveralls.io/repos/github/jtanguy/hmacaroons/badge.svg?branch=master)](https://coveralls.io/github/jtanguy/hmacaroons?branch=master)
[![Join the chat at https://gitter.im/jtanguy/hmacaroons](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jtanguy/hmacaroons?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Macaroons is a pure haskell implementation of macaroons. It aims to provide
compatibility at a serialized level with the [reference implementation](https://github.com/rescrv/libmacaroons)
and the [python implementation](https://github.com/ecordell/pymacaroons)

**WARNING: This library has not been audited by security experts.**
**There is no error handling at the moment, everyhting is silently accepted**

It is developed in the purpose of exploration purposes, and would need much
more attention if it were to be used in production.

References
==========

Papers and articles
-------------------

- [Google paper on macaroons](http://research.google.com/pubs/pub41892.html)
- [Macaroons at Mozilla](https://air.mozilla.org/macaroons-cookies-with-contextual-caveats-for-decentralized-authorization-in-the-cloud/)
- [Time for better security in NoSQL](http://hackingdistributed.com/2014/11/23/macaroons-in-hyperdex/)

Implementations
---------------

- [C](https://github.com/rescrv/libmacaroons)
- [Java](https://github.com/nitram509/jmacaroons)
- [Node.js](https://github.com/nitram509/macaroons.js)
- [Python](https://github.com/ecordell/pymacaroons)
- [Rust](https://github.com/cryptosphere/rust-macaroons.git)
