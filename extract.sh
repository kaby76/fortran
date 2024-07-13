#
#set -x
set -e
echo '==========================================================='
echo 'Extracting text from ISO Fortran spec and extracting rules.'
echo '==========================================================='

echo "Extracting all text from PDF $@. This uses tritext."
echo "Note, care is taken to filter out margins and add in markup for symbol identification."
to="${@%%.*}".txt
echo $to
if [ ! -f $to ]
then
	tritext.exe --filter 's0<50 || e1>780 || e1<68' --markup $@ > $to
	if [ $? -ne 0 ]
	then
		echo problem.
		exit
	fi
fi

echo "Making the program that extracts EBNF rules from the text."
if [ ! -f extraction/bin/Debug/net8.0/RuleExtraction.exe ]
then
	pushd extraction
	dotnet build
	popd
fi

echo "Extracting rules from text $to."
cat $to | ./extraction/bin/Debug/net8.0/RuleExtraction.exe > ebnf.ebnf

echo "Change lines with meta characters, which interfere with parsing simplified EBNF."
sed -i "s%R779 <i>lbracket</i> <b>is</b> \[%R779 <i>lbracket</i> <b>is</b> '['%" ebnf.ebnf
sed -i "s%R780 <i>rbracket</i> <b>is</b> \]%R780 <i>rbracket</i> <b>is</b> ']'%" ebnf.ebnf
sed -i 's%_ \] '"'"'%_ ] <i>SQUOTE</i>%' ebnf.ebnf
sed -i 's%\] [.][.][.] '"'"'%] ... <i>SQUOTE</i>%' ebnf.ebnf
sed -i 's%_ \] "%_ ] <i>DQUOTE</i>%' ebnf.ebnf
sed -i 's%\] [.][.][.] "%] ... <i>DQUOTE</i>%' ebnf.ebnf
dos2unix ebnf.ebnf
cp ebnf.ebnf backup.ebnf

echo "Adding rules that spec does not define."
trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery 'insert //EOF "

<i>ac-value-list</i> <b>is</b> <i>ac-value</i> [ , <i>ac-value</i> ] ...
<i>access-id-list</i> <b>is</b> <i>access-id</i> [ , <i>access-id</i> ] ...
<i>actual-arg-spec-list</i> <b>is</b> <i>actual-arg-spec</i> [ , <i>actual-arg-spec</i> ] ...
<i>alloc-opt-list</i> <b>is</b> <i>alloc-opt</i> [ , <i>alloc-opt</i> ] ...
<i>allocatable-decl-list</i> <b>is</b> <i>allocatable-decl</i> [ , <i>allocatable-decl</i> ] ...
<i>allocate-coshape-spec-list</i> <b>is</b> <i>allocate-coshape-spec</i> [ , <i>allocate-coshape-spec</i> ] ...
<i>allocate-object-list</i> <b>is</b> <i>allocate-object</i> [ , <i>allocate-object</i> ] ...
<i>allocate-shape-spec-list</i> <b>is</b> <i>allocate-shape-spec</i> [ , <i>allocate-shape-spec</i> ] ...
<i>allocation-list</i> <b>is</b> <i>allocation</i> [ , <i>allocation</i> ] ...
<i>association-list</i> <b>is</b> <i>association</i> [ , <i>association</i> ] ...
<i>assumed-implied-spec-list</i> <b>is</b> <i>assumed-implied-spec</i> [ , <i>assumed-implied-spec</i> ] ...
<i>assumed-shape-spec-list</i> <b>is</b> <i>assumed-shape-spec</i> [ , <i>assumed-shape-spec</i> ] ...
<i>bind-entity-list</i> <b>is</b> <i>bind-entity</i> [ , <i>bind-entity</i> ] ...
<i>binding-attr-list</i> <b>is</b> <i>binding-attr</i> [ , <i>binding-attr</i> ] ...
<i>binding-name-list</i> <b>is</b> <i>binding-name</i> [ , <i>binding-name</i> ] ...
<i>bounds-remapping-list</i> <b>is</b> <i>bounds-remapping</i> [ , <i>bounds-remapping</i> ] ...
<i>bounds-spec-list</i> <b>is</b> <i>bounds-spec</i> [ , <i>bounds-spec</i> ] ...
<i>case-value-range-list</i> <b>is</b> <i>case-value-range</i> [ , <i>case-value-range</i> ] ...
<i>close-spec-list</i> <b>is</b> <i>close-spec</i> [ , <i>close-spec</i> ] ...
<i>coarray-association-list</i> <b>is</b> <i>coarray-association</i> [ , <i>coarray-association</i> ] ...
<i>codimension-decl-list</i> <b>is</b> <i>codimension-decl</i> [ , <i>codimension-decl</i> ] ...
<i>common-block-object-list</i> <b>is</b> <i>common-block-object</i> [ , <i>common-block-object</i> ] ...
<i>component-attr-spec-list</i> <b>is</b> <i>component-attr-spec</i> [ , <i>component-attr-spec</i> ] ...
<i>component-decl-list</i> <b>is</b> <i>component-decl</i> [ , <i>component-decl</i> ] ...
<i>component-spec-list</i> <b>is</b> <i>component-spec</i> [ , <i>component-spec</i> ] ...
<i>concurrent-control-list</i> <b>is</b> <i>concurrent-control</i> [ , <i>concurrent-control</i> ] ...
<i>connect-spec-list</i> <b>is</b> <i>connect-spec</i> [ , <i>connect-spec</i> ] ...
<i>cosubscript-list</i> <b>is</b> <i>cosubscript</i> [ , <i>cosubscript</i> ] ...
<i>data-i-do-object-list</i> <b>is</b> <i>data-i-do-object</i> [ , <i>data-i-do-object</i> ] ...
<i>data-stmt-object-list</i> <b>is</b> <i>data-stmt-object</i> [ , <i>data-stmt-object</i> ] ...
<i>data-stmt-value-list</i> <b>is</b> <i>data-stmt-value</i> [ , <i>data-stmt-value</i> ] ...
<i>dealloc-opt-list</i> <b>is</b> <i>dealloc-opt</i> [ , <i>dealloc-opt</i> ] ...
<i>deferred-coshape-spec-list</i> <b>is</b> <i>deferred-coshape-spec</i> [ , <i>deferred-coshape-spec</i> ] ...
<i>deferred-shape-spec-list</i> <b>is</b> <i>deferred-shape-spec</i> [ , <i>deferred-shape-spec</i> ] ...
<i>dummy-arg-list</i> <b>is</b> <i>dummy-arg</i> [ , <i>dummy-arg</i> ] ...
<i>dummy-arg-name-list</i> <b>is</b> <i>dummy-arg-name</i> [ , <i>dummy-arg-name</i> ] ...
<i>entity-decl-list</i> <b>is</b> <i>entity-decl</i> [ , <i>entity-decl</i> ] ...
<i>entity-name-list</i> <b>is</b> <i>entity-name</i> [ , <i>entity-name</i> ] ...
<i>enumerator-list</i> <b>is</b> <i>enumerator</i> [ , <i>enumerator</i> ] ...
<i>enumerator-name-list</i> <b>is</b> <i>enumerator-name</i> [ , <i>enumerator-name</i> ] ...
<i>equivalence-object-list</i> <b>is</b> <i>equivalence-object</i> [ , <i>equivalence-object</i> ] ...
<i>equivalence-set-list</i> <b>is</b> <i>equivalence-set</i> [ , <i>equivalence-set</i> ] ...
<i>event-wait-spec-list</i> <b>is</b> <i>event-wait-spec</i> [ , <i>event-wait-spec</i> ] ...
<i>explicit-shape-spec-list</i> <b>is</b> <i>explicit-shape-spec</i> [ , <i>explicit-shape-spec</i> ] ...
<i>external-name-list</i> <b>is</b> <i>external-name</i> [ , <i>external-name</i> ] ...
<i>final-subroutine-name-list</i> <b>is</b> <i>final-subroutine-name</i> [ , <i>final-subroutine-name</i> ] ...
<i>flush-spec-list</i> <b>is</b> <i>flush-spec</i> [ , <i>flush-spec</i> ] ...
<i>form-team-spec-list</i> <b>is</b> <i>form-team-spec</i> [ , <i>form-team-spec</i> ] ...
<i>image-selector-spec-list</i> <b>is</b> <i>image-selector-spec</i> [ , <i>image-selector-spec</i> ] ...
<i>implicit-none-spec-list</i> <b>is</b> <i>implicit-none-spec</i> [ , <i>implicit-none-spec</i> ] ...
<i>implicit-spec-list</i> <b>is</b> <i>implicit-spec</i> [ , <i>implicit-spec</i> ] ...
<i>import-name-list</i> <b>is</b> <i>import-name</i> [ , <i>import-name</i> ] ...
<i>input-item-list</i> <b>is</b> <i>input-item</i> [ , <i>input-item</i> ] ...
<i>inquire-spec-list</i> <b>is</b> <i>inquire-spec</i> [ , <i>inquire-spec</i> ] ...
<i>intrinsic-procedure-name-list</i> <b>is</b> <i>intrinsic-procedure-name</i> [ , <i>intrinsic-procedure-name</i> ] ...
<i>io-control-spec-list</i> <b>is</b> <i>io-control-spec</i> [ , <i>io-control-spec</i> ] ...
<i>io-implied-do-object-list</i> <b>is</b> <i>io-implied-do-object</i> [ , <i>io-implied-do</i> ] ...
<i>label-list</i> <b>is</b> <i>label</i> [ , <i>label</i> ] ...
<i>letter-spec-list</i> <b>is</b> <i>LETTERSPEC</i> [ , <i>LETTERSPEC</i> ] ...
<i>lock-stat-list</i> <b>is</b> <i>lock-stat</i> [ , <i>lock-stat</i> ] ...
<i>named-constant-def-list</i> <b>is</b> <i>named-constant-def</i> [ , <i>named-constant-def</i> ] ...
<i>namelist-group-object-list</i> <b>is</b> <i>namelist-group-object</i> [ , <i>namelist-group-object</i> ] ...
<i>object-name-list</i> <b>is</b> <i>object-name</i> [ , <i>object-name</i> ] ...
<i>only-list</i> <b>is</b> <i>only</i> [ , <i>only</i> ] ...
<i>output-item-list</i> <b>is</b> <i>output-item</i> [ , <i>output-item</i> ] ...
<i>pointer-decl-list</i> <b>is</b> <i>pointer-decl</i> [ , <i>pointer-decl</i> ] ...
<i>pointer-object-list</i> <b>is</b> <i>pointer-object</i> [ , <i>pointer-object</i> ] ...
<i>position-spec-list</i> <b>is</b> <i>position-spec</i> [ , <i>position-spec</i> ] ...
<i>proc-component-attr-spec-list</i> <b>is</b> <i>proc-component-attr-spec</i> [ , <i>proc-component-attr-spec</i> ] ...
<i>proc-decl-list</i> <b>is</b> <i>proc-decl</i> [ , <i>proc-decl</i> ] ...
<i>rename-list</i> <b>is</b> <i>rename</i> [ , <i>rename</i> ] ...
<i>saved-entity-list</i> <b>is</b> <i>saved-entity</i> [ , <i>saved-entity</i> ] ...
<i>section-subscript-list</i> <b>is</b> <i>section-subscript</i> [ , <i>section-subscript</i> ] ...
<i>specific-procedure-list</i> <b>is</b> <i>specific-procedure</i> [ , <i>specific-procedure</i> ] ...
<i>sync-stat-list</i> <b>is</b> <i>sync-stat</i> [ , <i>sync-stat</i> ] ...
<i>target-decl-list</i> <b>is</b> <i>target-decl</i> [ , <i>target-decl</i> ] ...
<i>type-attr-spec-list</i> <b>is</b> <i>type-param-attr-spec</i> [ , <i>type-param-attr-spec</i> ] ...
<i>type-bound-proc-decl-list</i> <b>is</b> <i>type-bound-proc-decl</i> [ , <i>type-bound-proc-decl</i> ] ...
<i>type-param-decl-list</i> <b>is</b> <i>type-param-decl</i> [ , <i>type-param-decl</i> ] ...
<i>type-param-name-list</i> <b>is</b> <i>type-param-name</i> [ , <i>type-param-name</i> ] ...
<i>type-param-spec-list</i> <b>is</b> <i>type-param-spec</i> [ , <i>type-param-spec</i> ] ...
<i>v-list</i> <b>is</b> <i>v</i> [ , <i>v</i> ] ...
<i>variable-name-list</i> <b>is</b> <i>variable-name</i> [ , <i>variable-name</i> ] ...
<i>wait-spec-list</i> <b>is</b> <i>wait-spec</i> [ , <i>wait-spec</i> ] ...

<i>access-name</i> <b>is</b> <i>name</i>
<i>ancestor-module-name</i> <b>is</b> <i>name</i>
<i>arg-name</i> <b>is</b> <i>name</i>
<i>array-name</i> <b>is</b> <i>name</i>
<i>associate-construct-name</i> <b>is</b> <i>name</i>
<i>associate-name</i> <b>is</b> <i>name</i>
<i>binding-name</i> <b>is</b> <i>name</i>
<i>block-construct-name</i> <b>is</b> <i>name</i>
<i>block-data-name</i> <b>is</b> <i>name</i>
<i>case-construct-name</i> <b>is</b> <i>name</i>
<i>coarray-name</i> <b>is</b> <i>name</i>
<i>common-block-name</i> <b>is</b> <i>name</i>
<i>component-name</i> <b>is</b> <i>name</i>
<i>construct-name</i> <b>is</b> <i>name</i>
<i>critical-construct-name</i> <b>is</b> <i>name</i>
<i>data-pointer-component-name</i> <b>is</b> <i>name</i>
<i>do-construct-name</i> <b>is</b> <i>name</i>
<i>entity-name</i> <b>is</b> <i>name</i>
<i>entry-name</i> <b>is</b> <i>name</i>
<i>enum-type-name</i> <b>is</b> <i>name</i>
<i>enumeration-type-name</i> <b>is</b> <i>name</i>
<i>enumerator-name</i> <b>is</b> <i>name</i>
<i>external-name</i> <b>is</b> <i>name</i>
<i>final-subroutine-name</i> <b>is</b> <i>name</i>
<i>forall-construct-name</i> <b>is</b> <i>name</i>
<i>function-name</i> <b>is</b> <i>name</i>
<i>function-reduction-name</i> <b>is</b> <i>name</i>
<i>generic-name</i> <b>is</b> <i>name</i>
<i>if-construct-name</i> <b>is</b> <i>name</i>
<i>import-name</i> <b>is</b> <i>name</i>
<i>index-name</i> <b>is</b> <i>name</i>
<i>int-constant-name</i> <b>is</b> <i>name</i>
<i>int-variable-name</i> <b>is</b> <i>name</i>
<i>intrinsic-procedure-name</i> <b>is</b> <i>name</i>
<i>local-name</i> <b>is</b> <i>name</i>
<i>module-name</i> <b>is</b> <i>name</i>
<i>namelist-group-name</i> <b>is</b> <i>name</i>
<i>parent-submodule-name</i> <b>is</b> <i>name</i>
<i>part-name</i> <b>is</b> <i>name</i>
<i>procedure-component-name</i> <b>is</b> <i>name</i>
<i>procedure-entity-name</i> <b>is</b> <i>name</i>
<i>procedure-name</i> <b>is</b> <i>name</i>
<i>procptr-entity-name</i> <b>is</b> <i>name</i>
<i>program-name</i> <b>is</b> <i>name</i>
<i>result-name</i> <b>is</b> <i>name</i>
<i>scalar-constant-expr</i> <b>is</b> <i>constant-expr</i>
<i>scalar-constant-subobject</i> <b>is</b> <i>constant-subobject</i>
<i>scalar-constant</i> <b>is</b> <i>constant</i>
<i>scalar-constant</i> <b>is</b> <i>constant</i>
<i>scalar-default-char-constant-expr</i> <b>is</b> <i>default-char-constant-expr</i>
<i>scalar-default-char-expr</i> <b>is</b> <i>default-char-expr</i>
<i>scalar-default-char-variable</i> <b>is</b> <i>default-char-variable</i>
<i>scalar-expr</i> <b>is</b> <i>expr</i>
<i>scalar-int-constant-expr</i> <b>is</b> <i>int-constant-expr</i>
<i>scalar-int-constant-expr</i> <b>is</b> <i>int-constant-expr</i>
<i>scalar-int-constant-name</i> <b>is</b> <i>int-constant-name</i>
<i>scalar-int-constant-subobject</i> <b>is</b> <i>int-constant-subobject</i>
<i>scalar-int-constant</i> <b>is</b> <i>int-constant</i>
<i>scalar-int-expr</i> <b>is</b> <i>int-expr</i>
<i>scalar-int-variable-name</i> <b>is</b> <i>int-variable-name</i>
<i>scalar-int-variable</i> <b>is</b> <i>int-variable</i>
<i>scalar-logical-expr</i> <b>is</b> <i>logical-expr</i>
<i>scalar-logical-variable</i> <b>is</b> <i>logical-variable</i>
<i>scalar-mask-expr</i> <b>is</b> <i>mask-expr</i>
<i>scalar-structure-component</i> <b>is</b> <i>structure-component</i>
<i>scalar-variable-name</i> <b>is</b> <i>variable-name</i>
<i>scalar-variable</i> <b>is</b> <i>variable</i>
<i>select-construct-name</i> <b>is</b> <i>name</i>
<i>submodule-name</i> <b>is</b> <i>name</i>
<i>subroutine-name</i> <b>is</b> <i>name</i>
<i>team-construct-name</i> <b>is</b> <i>name</i>
<i>type-name</i> <b>is</b> <i>name</i>
<i>type-param-name</i> <b>is</b> <i>name</i>
<i>use-name</i> <b>is</b> <i>name</i>
<i>where-construct-name</i> <b>is</b> <i>name</i>

";' | trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf

echo "Replace certain rules that straddle parser/lexer boundary."
trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery 'replace //rule_[lhs/id_/ID/text() = "<i>char-literal-constant</i>"]
		"
<i>char-literal-constant</i> <b>is</b>
	[ <i>kind-param</i> _ ] <i>SQUOTE-REP-CHAR</i>
	<b>or</b> [ <i>kind-param</i> _ ] <i>DQUOTE-REP-CHAR</i>
";
		' | \
	trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf

echo "Deleting several rules involving 'xyz', which should not be in the EBNF."
trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery '
		delete //rule_[lhs/id_/ID/text() = "<i>xyz</i>"];
		delete //rule_[lhs/id_/ID/text() = "<i>xyz-list</i>"];
		delete //rule_[lhs/id_/ID/text() = "<i>xyz-name</i>"];
		delete //rule_[lhs/id_/ID/text() = "<i>scalar-xyz</i>"];
		' | \
	trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf
		
echo "Get names of symbols."
trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery 'grep  //lhs/id_/ID' | trtext | sort -u > ids.txt
cat ids.txt | sed 's/[<]i[>]\(.*\)[<][/]i[>]/\1/' > updated_ids.txt
sed -i 's/-/_/g' updated_ids.txt
dos2unix ids.txt updated_ids.txt
paste -d "%" ids.txt updated_ids.txt > rename.txt
dos2unix rename.txt
sed -i 's/^/s%/' rename.txt
dos2unix rename.txt
sed -i 's/\(.*\)/\1%g/' rename.txt
dos2unix rename.txt

echo "Fix ups."
cat > rename2.txt <<EOF
s%[<]i[>][<][<]/i[>]%<%g
s%[<]i[>][>][<]/i[>]%>%g
EOF
sed -f rename2.txt -i ebnf.ebnf


echo "Delete rule numbers from rules, and add semi-colon rule terminators."
echo "Add function-name, which is missing from Spec EBNF."
echo "Change syntax around for ASSIGN, ALT, zero_or_one, zero_or_more, and any."
trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery '
		delete //RULE_NUMBER;
		replace //ASSIGN ":";
		replace //ALT "|";
		replace //zero_or_one/LEFT_SQUARE_BRACKET "(";
		replace //zero_or_one/RIGHT_SQUARE_BRACKET ")?";
		delete //zero_or_more/DOT_DOT_DOT;
		replace //zero_or_more/LEFT_SQUARE_BRACKET "(";
		replace //zero_or_more/RIGHT_SQUARE_BRACKET ")*";
		move //any/(@WS | @NL) ..;
		insert before //any "'\''";
		insert after //any "'\''";
		insert after //rule_ " ;";
		' | \
	trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf


echo "Rename."
sed -f rename.txt -i ebnf.ebnf

echo "More renames."
cat > rename2.txt <<EOF
s%[<]i[>]letter[<]/i[>]%letter%g
s%[<]i[>]digit[<]/i[>]%digit%g
EOF
sed -f rename2.txt -i ebnf.ebnf

rm -f rename.txt rename2.txt ids.txt updated_ids.txt