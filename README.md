# Hetero Finder

This is the repository which implements the community detection algorithms used in the article [Community Detection in Multi-Partite Multi-Relational Networks Based on Information Compression](http://link.springer.com/article/10.1007/s00354-016-0206-1).

The paper is co-authored by [Dr. Xin Liu](https://www.nii.ac.jp/faculty/list/project-profs/) and [Weichu Liu](https://github.com/weichuliu), under supervision of [Prof. Tsuyoshi Murata](http://www.ai.cs.titech.ac.jp/murata.html). The code is written and published by Weichu Liu.

The code is published under BSD license. Feel free to use it for research and for fun (yes, community detection is FUN! And scala is FUN!). It'd be appreciated if you contact [Murata Lab](http://www.ai.cs.titech.ac.jp/) if you are doing related research.

## Usage
This is a scala project. It is built with [sbt](http://www.scala-sbt.org).

Suppose you have `java` and `sbt` on your system.

- To compile project into a single jar file:

```
$ cd /path/to/hetero_scala
hetero_scala $ sbt assembly
... (scala magics)
hetero_scala $ ls target/scala-2.10/hetcom.jar
```

- To run the algorithm using jar file:

```
# Get a help doc by calling without arguments
$ java -jar /path/to/hetcom.jar
usage:
  ...
  ...

$ java -jar /path/to/hetcom.jar ...(arguments)
# or
$ java -cp /path/to/hetcom.jar {package}.{object} ...(arguments)
```

- To use it in REPL:

```
hetero_scala $ sbt console
# when memory is not enough
hetero_scala $ sbt -mem 4096 console
```

## Notes

- In sbt REPL (sbt console), most packages have FastUnfolding/Fast* algorithm to detect community from uni/k/hetero networks. Check unit tests.
- Composite modularity was named combiner because it was called combined modularity.
- Info compression method is also called hetero-finder, for that the k-partite ver (proposed by Xin Liu) was called BiNetFinder/TriNetFinder
- There was a python version first. But it is in the deep deep dark.
- If you don't know scala:
    - *Programming in Scala* (2nd!!!!!) by Martin Odersky is THE book for scala. A MUST READ.
    - Twitter's *Effective Scala* worths a read.
    - [This SO post](http://stackoverflow.com/questions/21116100) is convenience.

## Files
This is the description of files

```
.
├── README.md                     # this
├── assembly.sbt                  # assembly settings
├── build.sbt
├── nets                          # all (uni|bi|tri)-partite / heterogeneous sample networks for test.
│   ├── bi-sw.net
│   ├── bi.net
│   ├── hetero.net
│   ├── tri.net
│   ├── truth
│   ├── uni-karate.net
│   ├── uni-large.net
│   └── uni.net
├── project                       # settings for assembly (sbt plugin to compile scala into a .jar)
│   └── assembly.sbt
├── schedule                      # A simple TODO list, where a young and naïve me lives
└── src
    ├── main
    │   └── scala
    │       ├── combiner.scala    # implementation of composite modularity
    │       ├── common.scala      # common functions
    │       ├── counter.scala     # a Python collections.Counter like counter for counting objects
    │       ├── hetcom.scala      # main
    │       ├── hfcommon.scala    # common functions used in info-comp
    │       ├── hfinder2.scala    # implementation of info compression
    │       ├── kfinder.scala     # implementation of Xin Liu's KNetFinder
    │       ├── merger.scala      # Louvain-C method in the thesis. Community detection on each subgraph -> Consensus Clustering -> Community detection again.
    │       ├── muratabi.scala    # implementation of murata's bipartite modularity
    │       ├── muratatri.scala   # implementation of murata's tripartite modularity
    │       ├── newman.scala      # implementation of newman's modularity
    │       ├── oldhfinder.scala  # myth...
    │       └── ufinder.scala     # implementation of rosvall's paper
    └── test                      # Unit tests
        └── scala
            ├── combinertest.scala
            ├── commontest.scala
            ├── countertest.scala
            ├── findertest.scala
            ├── muratabitest.scala
            ├── muratatritest.scala
            └── newmantest.scala
```