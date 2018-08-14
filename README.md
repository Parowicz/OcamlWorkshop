# Repository with random Ocaml stuff

I enjoyed using Ocaml/F#/Haskell in various hackerranks. 
Thous i decide to learn more about Ocaml build chain, unit test framework (OUnit),
documentation generator (ocamldoc) etc.

# Build 

Build is as simple as `make`. Obviously you have to be in project root. 
Directory /_build with all artifacts will be created. Tests can be runned by `make test` command.

Type `make doc` to run ocamldoc. *.docdir with documentation (in html) should appear. 

Builded with:
* Debian 9.5 
* Ocaml 4.02.3
* Oasis 0.4.11
