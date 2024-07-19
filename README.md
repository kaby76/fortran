# Scraping the EBNF grammar from the ISO Spec

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
* There are many rules that end in `-list`. These are currently enumerated in the extract.sh script, but the rules
should be generated.
* The grammar generated is based on https://github.com/AkhilAkkapelli/Fortran2023Grammar. But,
there are several issues that need explanation.
    * [NAME](https://github.com/AkhilAkkapelli/Fortran2023Grammar/blob/553123a023f70e9a524e2a4036be128978834c42/Fortran2023Lexer.g4#L502)
      is an identifer. [PROGRAM](https://github.com/AkhilAkkapelli/Fortran2023Grammar/blob/553123a023f70e9a524e2a4036be128978834c42/Fortran2023Lexer.g4#L14)
      is a keyword. There is no distinguishment between keywords
      and non-keywords, but there should be. In other words, NAME can remain NAME because it is
      that way in the spec, but PROGRAM should be KW_PROGRAM in order to distinguish the two
      uses.
    * NAME is defined here: https://github.com/AkhilAkkapelli/Fortran2023Grammar/blob/553123a023f70e9a524e2a4036be128978834c42/Fortran2023Lexer.g4#L502.
      But, keyword "NAME" is define using NAAM here. https://github.com/AkhilAkkapelli/Fortran2023Grammar/blob/553123a023f70e9a524e2a4036be128978834c42/Fortran2023Lexer.g4#L152.
      It needs standardization.
    * Beyond camel-case naming, `program : program_unit ( program_unit )*;` contains useless parentheses.
      These need to be removed.
    * The order of the parser rules does not correspond to the spec.
    * `mult_op :  ASTERIK |  SLASH  ;`. Terminal symbols should use the established Unicode
      character name. ['*'](https://www.compart.com/en/unicode/U+002A)
      is not named ASTERIK, but Asterisk.
