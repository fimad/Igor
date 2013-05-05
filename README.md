Senior Project
==============

All of the files related to my senior project. The `paper/` directory contains
the draft of my final paper. The `src/` directory contains the implementation of
a gadget-based metamorphic engine inspired by return-oriented programming
techniques and the Frankenstein metamorphic system [1]. To quote from the
abstract of my paper:

> This paper presents the design and implementation of a
> metamorphic engine intended to be capable of evading cur-
> rent commercial classification methods, as well as several
> promising techniques from current anti-malware research.
> The resulting system is able to achieve high levels of vari-
> ation among the instances generated while at the same time
> creating programs that share key statistical properties with
> benign programs. The engine achieves this by reframing mal-
> ware obfuscation as a combined compiler and search prob-
> lem and is heavily influenced by developments in the field of
> return-oriented programming. Given a semantic blue print of
> a program the engine pieces together blocks of random bytes
> to create the final executable. By controlling the distribution
> of the random bytes used to generate the executable, the en-
> gine is able to influence statistical properties of the resulting
> executables. To achieve the high degree of variation neces-
> sary to evade commercial detection techniques, the process
> of piecing together and choosing random bytes is performed
> nondeterministically.

The engine is written in Haskell and contains fairly decent Haddock
documentation throughout.

[1] https://www.usenix.org/conference/woot12/frankenstein-stitching-malware-benign-binaries
