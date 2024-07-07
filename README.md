# Fortran Scraping

## Fortran Standards Documents and Resources
* Distilled from https://gcc.gnu.org/wiki/GFortranStandards
* All drafts in PDFs contain LHS margin with line numberings.

## Fortran 90
* https://wg5-fortran.org/N001-N1100/N692.pdf

## Fortran 95
* https://wg5-fortran.org/N1151-N1200/N1191.pdf

## Fortran 2003
* https://j3-fortran.org/doc/year/04/04-007.pdf

## Fortran 2008
* https://j3-fortran.org/doc/year/10/10-007.pdf

## Fortran 2018
* https://j3-fortran.org/doc/year/18/18-007r1.pdf

## Fortran 2023
* https://j3-fortran.org/doc/year/23/23-007r1.pdf

## Official ISO Spec
* https://www.iso.org/standard/82170.html

## Testsuites

* https://github.com/fujitsu/compiler-test-suite/tree/main/Fortran
* https://fortran-lang.discourse.group/t/fortran-compiler-testing-framework/1573
* https://github.com/scivision/fortran2018-examples
* https://github.com/fortran-lang/test-drive
* https://github.com/llvm/llvm-test-suite/tree/main/Fortran/gfortran
* https://github.com/OpenFortranProject/open-fortran-parser

## Notes on scraping
* From ISO/IEC 1539-1:2023: `The syntax rules are not a complete and accurate syntax description of Fortran, and cannot be used to
generate a Fortran parser automatically; where a syntax rule is incomplete, it is restricted by corresponding
constraints and text.`
* The main scraping script is [extract.sh](https://github.com/kaby76/fortran/blob/main/extract.sh). It calls
[tritext](https://github.com/kaby76/Trash/tree/10666b6d74bc7154008512912d409a4c2b81ebae/src/tritext)
to yank out text from a PDF file. Then, it calls a custom-written program just for
extracting the EBNF from the text, [extraction](https://github.com/kaby76/fortran/tree/main/extraction).
The script also uses an [ENBF in Antlr](https://github.com/kaby76/fortran/tree/main/ebnf) to parse and modify the scraped Fortran EBNF.
