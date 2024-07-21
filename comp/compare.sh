#!/bin/sh

# Sort parser rules and reformat. Assumes useless parentheses already
# removed.
cp ../FortranParser.g4 .
trparse -t ANTLRv4 FortranParser.g4 | trsort | trsponge -c
antlr-format -c ../../repo_coding_style.json FortranParser.g4

# Remove useless parentheses, rename to Snake case, delete comments,
# and reformat.
cp ../../Fortran2023Grammar/Fortran2023Parser.g4 .
trparse -t ANTLRv4 FortranParser.g4 | \
	trquery grep ' //parserRuleSpec/RULE_REF' | trtext | sort -u > after.txt
sed 's/_\([a-z]\)/\U\1/g' after.txt > before.txt
paste -d "," before.txt after.txt > rename.txt
bash /c/Users/Kenne/Documents/Github/g4-scripts/delete-useless-parentheses.sh Fortran2023Parser.g4
trparse -t ANTLRv4 Fortran2023Parser.g4 | \
	trrename -R rename.txt | \
	trquery delete ' //(@DOC_COMMENT | @BLOCK_COMMENT | @LINE_COMMENT)' | \
	trsponge -c
antlr-format -c ../../repo_coding_style.json Fortran2023Parser.g4
trparse -t ANTLRv4 Fortran2023Parser.g4 | trsort | trsponge -c
antlr-format -c ../../repo_coding_style.json Fortran2023Parser.g4

diff FortranParser.g4 Fortran2023Parser.g4
