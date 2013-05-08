Igor
====

The implementation of a gadget-based metamorphic engine inspired by
return-oriented programming techniques and the Frankenstein metamorphic system [1].
To quote from the abstract of my paper:

>    This paper presents the design and implementation of a metamorphic engine
>    intended to be capable of evading current commercial classification methods,
>    as well as several promising techniques from current anti-malware research.
>    The resulting system is able to achieve high levels of variation among the
>    instances generated while at the same time creating programs that share key
>    statistical properties with benign programs. The engine achieves this by
>    reframing malware obfuscation as a combined compiler and search problem and
>    is heavily influenced by developments in the field of return-oriented
>    programming.  Given a semantic blue print of a program the engine pieces
>    together blocks of random bytes to create the final executable. By
>    controlling the distribution of the random bytes used to generate the
>    executable, the engine is able to influence statistical properties of the
>    resulting executables. To achieve the high degree of variation necessary to
>    evade commercial detection techniques, the process of piecing together and
>    choosing random bytes is performed nondeterministically.  

The engine is written in Haskell and contains fairly decent Haddock
documentation throughout.

[1] https://www.usenix.org/conference/woot12/frankenstein-stitching-malware-benign-binaries
