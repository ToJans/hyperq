[![Build Status](https://secure.travis-ci.org/hyperq/hyperq.png)](http://travis-ci.org/hyperq/hyperq)

http://hyperq.github.io/assets/hyperq-blue.png

[Main Site](http://hyperq.github.io)

hyperq is a small project with a big ambition. We aim to build the worlds best
algorithmic trading platform using the best off-the-shelf open source
technology stack to be found.

## Build

```sh
$ # build the library and tests
$ cabal configure --enable-tests
$ cabal build
$ # run the benchmarks
$ cabal test
```

Beyond building the haskell code, the easiest way to
get up to speed is to read the project [blog](http://hyperq.github.io/blog). If
you're interested in contributing to development or find a logic bug, then
fork me with:

```
$ git clone https://github.com/hyperq/hyperq.git
$ git submodule init
$ git submodule update
```

## News

12 June, 2013

Moved to haskell-centric design

25 April, 2013

Happy Birthday, hyperq!  Zero today!

23 April, 2013

A discussion group has been formed at [hft-discuss](https://groups.google.com/forum/?hl=en&fromgroups#!forum/hft-discuss)

Some near-term goals include:
- moving the project to an organization basis (which will involve a name
  change (watch this space).
- installing a modern communication toolkit
- exploring automated deployment and build options
- discussing the relative strengths and weaknesses of python versus R, as they
  apply to the hft problem domain.

## Bug tracker

Have a bug or a feature request? [Please open a new issue](https://github.com/hyperq/hyperq/issues). 

## Community

Join the discussions at [hft-discuss](https://groups.google.com/forum/?hl=en&fromgroups#!forum/hft-discuss)

Hyperq is sponsored by [Scarce Capital](http://scarcecapital.com). Follow
[@scarcecapital on Twitter](http://twitter.com/scarcecapital).

Read, subscribe (and contribute!) to the [The hyperq Blog](http://hyperq.github.io).

## Contributing

Please submit all pull requests against the master branch.

Thanks!

## Core Contributors

**Tony Day**

+ [http://twitter.com/tonyday567](http://twitter.com/tonyday567)
+ [http://github.com/tonyday567](http://github.com/tonyday567)


## Copyright and license

Copyright 2013 hyperq.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this work except in compliance with the License.
You may obtain a copy of the License in the LICENSE file, or at:

  [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
