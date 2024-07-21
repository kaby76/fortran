#!/bin/sh

# Copy from source.
cp ../FortranParser.g4 .
cp ../FortranLexer.g4 .
cp ../../Fortran2023Grammar/Fortran2023Parser.g4 .

# Work around antlr-reformat issue of inconsistent spacing.
trparse -t ANTLRv4 *.g4 | trquery insert after ' //LPAREN' '" "' | trsponge -c

# Remove comments in all grammars.
trparse -t ANTLRv4 *.g4 | \
	trrename -R rename.txt | \
	trquery delete ' //(@DOC_COMMENT | @BLOCK_COMMENT | @LINE_COMMENT)' | \
	trsponge -c

# Sort parser rules. Assumes useless parentheses already removed.
trparse -t ANTLRv4 FortranParser.g4 | trsort | trsponge -c
trparse -t ANTLRv4 FortranLexer.g4 FortranParser.g4 | trfoldlit | trsponge -c

# Remove useless parentheses, rename to Snake case,
# and reformat.
trparse -t ANTLRv4 FortranParser.g4 | \
	trquery grep ' //parserRuleSpec/RULE_REF' | trtext | sort -u > after.txt
sed 's/_\([a-z0-9]\)/\U\1/g' after.txt > before.txt
paste -d "," before.txt after.txt > rename.txt
# Add in non-standard names into renaming.
cat >> rename.txt <<EOF
upperCoBound,upper_cobound
BINARYCONSTANT,BINARY_CONSTANT
OCTALCONSTANT,OCTAL_CONSTANT
HEXCONSTANT,HEX_CONSTANT
bindAttr,binding_attr
bindAttrList,binding_attr_list
APOSTROPHEREPCHAR,SQUOTE_REP_CHAR
QUOTEREPCHAR,DQUOTE_REP_CHAR
deferredCoShapeSpecList,deferred_coshape_spec_list
deferredCoShapeSpec,deferred_coshape_spec
explicitCoShapeSpec,explicit_coshape_spec
definedIOGenericSpec,defined_io_generic_spec
lowerCoBound,lower_cobound
LETTERSPEC,LETTER_SPEC
EOF
bash /c/Users/Kenne/Documents/Github/g4-scripts/delete-useless-parentheses.sh Fortran2023Parser.g4
trparse -t ANTLRv4 Fortran2023Parser.g4 | trrename -R rename.txt | trsponge -c
antlr-format -c ../../repo_coding_style.json Fortran2023Parser.g4
trparse -t ANTLRv4 Fortran2023Parser.g4 | trsort | trsponge -c

# Reformat all grammars.
antlr-format -c ../../repo_coding_style.json *.g4

# Diff grammars.
diff FortranParser.g4 Fortran2023Parser.g4
