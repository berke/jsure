Installation instructions
=========================

* Pre-compiled binaries for Linux are available in bin/
* To compile Jsure:
  - Get, build and install Aurochs from http://aurochs.fr/
  - make
  - PREFIX=/usr/local make install

Important note
==============
I have recently discovered that the Javascript grammar allows you to omit
semicolons in most circumstances.  Actually, it is more on the order
of tolerating forgotten semicolons than "allowing" their omission, because the
parser described in the ECMA specification is supposed to backtrack and retry
when it doesn't find an expected semicolon.  Of course this (1) supposes
a particular parsing method and (2) makes the suggested parser run slowly.

Unfortunately I would need to completely rewrite my Javascript grammar
(and possibly make hacks to Aurochs) to support the omission of semicolons.

I think omitting semicolons is very bad practice.  For one thing, it
makes the source more ambiguous and harder to read.  For another thing,
it departs from common C and Java syntactic conventions.  Fortunately,
it seems that only a handful of people systematically omit semicolons.

I'm saying a handful but I have only seen one such case:  someone whose
Javascript looked like

  function GetLen_(G) { var K, L, C = ""	// Get good margin
    G.Len.value = K = L = Math.max(G.Len.value, 1)
    while (--K) C += " " ; G.Result.value = C + "|"
    return L }

I guess people who'll use Jsure are not in the business of obfuscating
their source (there are tools for that), so I'll leave semicolons mandatory.
