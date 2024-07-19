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
<i>letter-spec-list</i> <b>is</b> <i>LETTER-SPEC</i> [ , <i>LETTER-SPEC</i> ] ...
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
<i>type-attr-spec-list</i> <b>is</b> <i>type-attr-spec</i> [ , <i>type-attr-spec</i> ] ...
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
<i>parent-type-name</i> <b>is</b> <i>name</i>
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

trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery 'replace //rule_[lhs/id_/ID/text() = "<i>boz-literal-constant</i>"]
		"
<i>boz-literal-constant</i> <b>is</b> <i>BINARY-CONSTANT</i> <b>or</b> <i>OCTAL-CONSTANT</i> <b>or</b> <i>HEX-CONSTANT</i>

";
		' | \
	trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf

trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery 'delete //rule_[lhs/id_/ID/text() = "<i>alphanumeric-character</i>"];
		' | \
	trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf


trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery 'delete //rule_[lhs/id_/ID/text() = "<i>binary-constant</i>"];' | \
	trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf

trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery 'delete //rule_[lhs/id_/ID/text() = "<i>octal-constant</i>"];' | \
	trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf

trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery 'delete //rule_[lhs/id_/ID/text() = "<i>hex-constant</i>"];' | \
	trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf


trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery '
	replace //rule_[lhs/id_/ID/text() = "<i>defined-unary-op</i>"]
"
<i>defined-unary-op</i> <b>is</b> <i>DEFINEDUNARYBINARYOP</i>
";
	replace //rule_[lhs/id_/ID/text() = "<i>defined-binary-op</i>"]
"
<i>defined-binary-op</i> <b>is</b> <i>DEFINEDUNARYBINARYOP</i>
";
	replace //rule_[lhs/id_/ID/text() = "<i>letter-spec</i>"]
"
<i>letter-spec</i> <b>is</b> <i>LETTER-SPEC</i>
";
	replace //rule_[lhs/id_/ID/text() = "<i>label</i>"]
"
<i>label</i> <b>is</b> <i>DIGITSTRING</i>
";
	replace //rule_[lhs/id_/ID/text() = "<i>digit-string</i>"]
"
<i>digit-string</i> <b>is</b> <i>DIGITSTRING</i>
";
	delete //rule_[lhs/id_/ID/text() = "<i>hex-digit</i>"];
	delete //rule_[lhs/id_/ID/text() = "<i>hex-digit-string</i>"];
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

echo "Delete duplicates."
trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery '
	delete //rule_[lhs/id_/ID/text() = "<i>scalar-constant</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>scalar-int-constant</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>defined-unary-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>power-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>module</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>module-subprogram</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>separate-module-subprogram</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>function-subprogram</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>subroutine-subprogram</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>module-subprogram-part</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>submodule</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>block-data</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>mult-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>add-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>concat-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>rel-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>not-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>and-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>or-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>equal-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>defined-binary-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>equiv-op</i>"][2];
	delete //rule_[lhs/id_/ID/text() = "<i>main-program</i>"][2];
	' | \
	trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf

echo "Delete rule numbers from rules, and add semi-colon rule terminators."
echo "Add function-name, which is missing from Spec EBNF."
echo "Change syntax around for ASSIGN, ALT, zero_or_one, zero_or_more, and any."
trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery '
		delete //rule_[lhs/id_/ID/text() = "<i>name</i>"];
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

echo "Renaming remaining symbols, and adding lexer rules."
cat > rename3.txt <<EOF
s%[<]i[>]SQUOTE-REP-CHAR[<]/i[>]%SQUOTE_REP_CHAR%g
s%[<]i[>]DQUOTE-REP-CHAR[<]/i[>]%DQUOTE_REP_CHAR%g
s%[<]i[>]BINARY-CONSTANT[<]/i[>]%BINARY_CONSTANT%g
s%[<]i[>]OCTAL-CONSTANT[<]/i[>]%OCTAL_CONSTANT%g
s%[<]i[>]HEX-CONSTANT[<]/i[>]%HEX_CONSTANT%g
s%[<]i[>]LETTER-SPEC[<]/i[>]%LETTER_SPEC%g
s%[<]i[>]DEFINEDUNARYBINARYOP[<]/i[>]%DEFINEDUNARYBINARYOP%g
s%[<]i[>]DIGITSTRING[<]/i[>]%DIGITSTRING%g
EOF
sed -f rename3.txt -i ebnf.ebnf

cat > FortranLexer.g4 <<EOF
lexer grammar FortranLexer;

options { caseInsensitive=true; }

LINE_COMMENT: '!' .*? '\r'? '\n' -> skip;

BLOCK_COMMENT: '/*' .*? '*/' -> skip;

SPACE: [ ] -> skip;

WS: [\t\r\n]+ -> skip;

EXTENDS: 'EXTENDS' ;

PROGRAM: 'PROGRAM';

END: 'END';

COMMA: ',';

FUNCTION: 'FUNCTION';

LPAREN: '(';

RPAREN: ')';

ASTERIK: '*';

SUBROUTINE: 'SUBROUTINE';

MODULE: 'MODULE';

SUBMODULE: 'SUBMODULE';

BLOCK: 'BLOCK';

DATA: 'DATA';

COLON : ':';

INTRINSIC: 'INTRINSIC';

NONINTRINSIC: 'NON_INTRINSIC';

IMPLIES: '=>';

OPERATOR: 'OPERATOR';

POWER: '**';

SLASH: '/';

PLUS: '+';

MINUS: '-';

CONCAT: '//';

EQ: '.EQ.';

NE: '.NE.';

LT: '.LT.';

LE: '.LE.';

GT: '.GT.';

GE: '.GE.';

EQUAL: '==';

NOTEQUAL: '/=';

LESSTHAN: '<';

LESSEQUAL: '<=';

GREATERTHAN: '>';

GREATEREQUAL: '>=';

NOT: '.NOT.';

AND: '.AND.';

OR: '.OR.';

EQV: '.EQV.';

NEQV: '.NEQV.';

READ: 'READ';

FORMATTED: 'FORMATTED';

UNFORMATTED: 'UNFORMATTED';

WRITE: 'WRITE';

ASSIGNMENT: 'ASSIGNMENT';

ASSIGN: '=';

USE: 'USE';

DOUBLECOLON: '::';

ONLY: 'ONLY';

IMPORT : 'IMPORT';

NONE: 'NONE';

ALL: 'ALL';

KIND: 'KIND';

INTEGER: 'INTEGER';

LEN: 'LEN';

REAL: 'REAL';

DOUBLE: 'DOUBLE';

PRECISION: 'PRECISION';

COMPLEX: 'COMPLEX';

CHARACTER: 'CHARACTER';

LOGICAL: 'LOGICAL';

TYPE: 'TYPE';

CLASS: 'CLASS';

EXTERNAL: 'EXTERNAL';

IMPLICIT: 'IMPLICIT';

PARAMETER: 'PARAMETER';

FORMATIN : FORMAT (SPACE)* LPAREN -> pushMode(FORMAT_MODE);

FORMAT: 'FORMAT';

BINDC: BIND SPACE* LPAREN SPACE* C;

BIND: 'BIND';

NAAM: 'NAME';

RESULT: 'RESULT';

ENTRY: 'ENTRY';

DOT: '.';

TRUE: '.TRUE.';

FALSE: '.FALSE.';

SQUOTE: '\'';

DQUOTE: '"';

STAT: 'STAT';

TEAM: 'TEAM';

TEAMNUMBER: 'TEAM_NUMBER';

LBRACKET: '[';

RBRACKET: ']';

RE: 'RE';

IM: 'IM';

PERCENT: '%';

LPARENSLASH: '(/';

RPARENSLASH: '/)';

// R602 UNDERSCORE -> _
UNDERSCORE: '_';

SEQUENCE: 'SEQUENCE';

PRIVATE: 'PRIVATE';

PROCEDURE: 'PROCEDURE';

NOPASS: 'NOPASS';

PASS: 'PASS';

POINTER: 'POINTER';

ALLOCATABLE: 'ALLOCATABLE';

CODIMENSION: 'CODIMENSION';

CONTIGUOUS: 'CONTIGUOUS';

DIMENSION: 'DIMENSION';

PUBLIC: 'PUBLIC';

CONTAINS: 'CONTAINS';

FINAL: 'FINAL';

GENERIC: 'GENERIC';

DEFERRED: 'DEFERRED';

NONOVERRIDABLE: 'NON_OVERRIDABLE';

INTENT: 'INTENT';

OPTIONAL: 'OPTIONAL';

PROTECTED: 'PROTECTED';

SAVE: 'SAVE';

IN: 'IN';

OUT: 'OUT';

INOUT: 'INOUT';

INTERFACE: 'INTERFACE';

ABSTRACT: 'ABSTRACT';

ENUM: 'ENUM';

ENUMERATOR: 'ENUMERATOR';

ASYNCHRONOUS: 'ASYNCHRONOUS';

TARGET: 'TARGET';

VALUE: 'VALUE';

VOLATILE: 'VOLATILE';

EQUIVALENCE: 'EQUIVALENCE';

COMMON: 'COMMON';

NAMELIST: 'NAMELIST';

DOUBLEDOT: '..';

EVENT: 'EVENT';

WAIT: 'WAIT';

UNTILCOUNT: 'UNTIL_COUNT';

POST: 'POST';

ERRMSG: 'ERRMSG';

ERROR: 'ERROR';

STOP: 'STOP';

QUIET: 'QUIET';

ENDFILE: 'ENDFILE';

DEALLOCATE: 'DEALLOCATE';

CYCLE: 'CYCLE';

CONTINUE: 'CONTINUE';

CLOSE: 'CLOSE';

UNIT: 'UNIT';

IOSTAT: 'IOSTAT';

IOMSG: 'IOMSG';

ERR: 'ERR';

STATUS: 'STATUS';

CALL: 'CALL';

BACKSPACE: 'BACKSPACE';

ALLOCATE: 'ALLOCATE';

MOLD: 'MOLD';

SOURCE: 'SOURCE';

OPEN: 'OPEN';

ACCESS: 'ACCESS';

ACTION: 'ACTION';

BLANK: 'BLANK';

DECIMAL: 'DECIMAL';

DELIM: 'DELIM';

ENCODING: 'ENCODING';

FILE: 'FILE';

FORM: 'FORM';

NEWUNIT: 'NEWUNIT';

PAD: 'PAD';

POSITION: 'POSITION';

RECL: 'RECL';

ROUND: 'ROUND';

SIGN: 'SIGN';

NULLIFY: 'NULLIFY';

LOCK: 'LOCK';

ACQUIREDLOCK: 'ACQUIRED_LOCK';

INQUIRE: 'INQUIRE';

IOLENGTH: 'IOLENGTH';

EXIST: 'EXIST';

ID: 'ID';

NAMED: 'NAMED';

NEXTREC: 'NEXTREC';

NUMBER: 'NUMBER';

OPENED: 'OPENED';

PENDING: 'PENDING';

POS: 'POS';

READWRITE: 'READWRITE';

SEQUENTIAL: 'SEQUENTIAL';

SIZE: 'SIZE';

STREAM: 'STREAM';

IF: 'IF';

GO: 'GO';
TO: 'TO';

NEWINDEX: 'NEW_INDEX';

FLUSH: 'FLUSH';

FAIL: 'FAIL';

IMAGE: 'IMAGE';

EXIT: 'EXIT';

FORALL: 'FORALL';

WHERE: 'WHERE';

EOR: 'EOR';

UNLOCK: 'UNLOCK';

SYNC: 'SYNC';

MEMORY: 'MEMORY';

IMAGES: 'IMAGES';

REWIND: 'REWIND';

RETURN: 'RETURN';

FMT: 'FMT';

NML: 'NML';

ADVANCE: 'ADVANCE';

REC: 'REC';

PRINT: 'PRINT';

CRITICAL: 'CRITICAL';

CHANGE: 'CHANGE';

SELECT: 'SELECT';

CASE: 'CASE';

DEFAULT: 'DEFAULT';

ASSOCIATE: 'ASSOCIATE';

ELSEWHERE: 'ELSEWHERE';

IS: 'IS';

RANK: 'RANK';

ELSE: 'ELSE';

THEN: 'THEN';

DO: 'DO';

CONCURRENT: 'CONCURRENT';

WHILE: 'WHILE';

SHARED: 'SHARED';

LOCAL: 'LOCAL';

LOCALINIT: 'LOCAL_INIT';

RECURSIVE: 'RECURSIVE';

PURE: 'PURE';

NONRECURSIVE: 'NON_RECURSIVE';

IMPURE: 'IMPURE';

ELEMENTAL: 'ELEMENTAL';

ATSYMBOL: '@';

NIL: '.NIL';

QUESTION: '?';

NOTIFY: 'NOTIFY';

TYPEOF: 'TYPEOF';

CLASSOF: 'CLASSOF';

ENUMERATION: 'ENUMERATION';

DIRECT: 'DIRECT';

LEADINGZERO: 'LEADING_ZERO';

REDUCE: 'REDUCE';

SIMPLE: 'SIMPLE';

DEFINEDUNARYBINARYOP: DOT LETTER+? DOT;

// R865 letter-spec -> letter [- letter]
///LEXER RULE: LetterSpec should only be defined as LETTER MINUS LETTER form and not in only LETTER form due to conflict with NAME
LETTER_SPEC: LETTER MINUS LETTER;

// R765 binary-constant -> B ' digit [digit]... ' | B " digit [digit]... "
BINARY_CONSTANT: B SQUOTE DIGIT+? SQUOTE | B DQUOTE DIGIT+? DQUOTE;

// R766 octal-constant -> O ' digit [digit]... ' | O " digit [digit]... "
OCTAL_CONSTANT: O SQUOTE DIGIT+? SQUOTE | O DQUOTE DIGIT+? DQUOTE;

// R767 hex-constant -> Z ' hex-digit [hex-digit]... ' | Z " hex-digit [hex-digit]... "
HEX_CONSTANT: Z SQUOTE HEXDIGIT+? SQUOTE | Z DQUOTE HEXDIGIT+? DQUOTE;

//R0003 RepChar
SQUOTE_REP_CHAR: SQUOTE (~[\u0000-\u001F])*?  SQUOTE;

DQUOTE_REP_CHAR: DQUOTE (~[\u0000-\u001F])*?  DQUOTE;

REALEXPONENTLETTER: DIGITSTRING DOT DIGITSTRING? EXPONENTLETTER | DOT DIGITSTRING EXPONENTLETTER | DIGITSTRING EXPONENTLETTER;

// R603 name -> letter [alphanumeric-character]...
NAME: LETTER (ALPHANUMERICCHARACTER)*;

// R0002 Letter ->
//         A | B | C | D | E | F | G | H | I | J | K | L | M |
//         N | O | P | Q | R | S | T | U | V | W | X | Y | Z
LETTER: 'A'..'Z'; 

// R711 digit-string -> digit [digit]...
DIGITSTRING: DIGIT+; 

// R0001 Digit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
DIGIT: '0'..'9';

// R601 alphanumeric-character -> letter | digit | underscore
ALPHANUMERICCHARACTER: LETTER | DIGIT | UNDERSCORE;

C: 'C';

E: 'E';

D: 'D';

// R716 exponent-letter -> E | D
EXPONENTLETTER: E | D;

B: 'B';

O: 'O';

Z: 'Z';

A: 'A';

F: 'F';

// R768 hex-digit -> digit | A | B | C | D | E | F
HEXDIGIT: DIGIT | A | B | C | D | E | F;


///FORMAT MODE

mode FORMAT_MODE;

FORMAT_LPAREN: '(' -> pushMode(FORMAT_MODE), type(LPAREN);

FORMAT_RPAREN: ')' -> popMode, type(RPAREN);

FORMAT_SPACE: ' ' -> skip;

FORMAT_COMMA: ',' -> type(COMMA);

FORMAT_ASTERIK: '*' -> type(ASTERIK);

FORMAT_UNDERSCORE: '_' -> type(UNDERSCORE);

FORMAT_DIGITSTRING: FORMAT_DIGIT+  -> type(DIGITSTRING);

FORMAT_DIGIT: '0'..'9' -> type(DIGIT);

FORMAT_APOSTROPHE: '\'' -> type(SQUOTE);

FORMAT_QUOTE: '"' -> type(DQUOTE);

FORMAT_APOSTROPHEREPCHAR: FORMAT_APOSTROPHE (~[\u0000-\u001F\u0027])*?  FORMAT_APOSTROPHE -> type(SQUOTE_REP_CHAR);

FORMAT_QUOTEREPCHAR: FORMAT_QUOTE (~[\u0000-\u001F\u0022])*?  FORMAT_QUOTE -> type(DQUOTE_REP_CHAR);

P: 'P';

DC: 'DC';

DP: 'DP';

LZS: 'LZS';

LZP: 'LZP';

LZ: 'LZ';

RU: 'RU';

RD: 'RD';

RZ: 'RZ';

RN: 'RN';

RC: 'RC';

RP: 'RP';

BN: 'BN';

BZ: 'BZ';

SS: 'SS';

SP: 'SP';

S: 'S';

T: 'T';

TL: 'TL';

TR: 'TR';

X: 'X';

I: 'I';

FORMAT_B: 'B' -> type(B);

FORMAT_O: 'O' -> type(O);

FORMAT_Z: 'Z' -> type(Z);

FORMAT_F: 'F' -> type(F);

FORMAT_E: 'E' -> type(E);

EN: 'EN';

ES: 'ES';

EX: 'EX';

G: 'G';

L: 'L';

FORMAT_A: 'A' -> type(A);

AT: 'AT';

FORMAT_D: 'D' -> type(D);

DT: 'DT';

FORMAT_NAME: FORMAT_LETTER (FORMAT_ALPHANUMERICCHARACTER)* -> type(NAME);

FORMAT_LETTER: 'A'..'Z' -> type(LETTER); 

FORMAT_ALPHANUMERICCHARACTER: (FORMAT_LETTER | FORMAT_DIGIT | FORMAT_UNDERSCORE) -> type(ALPHANUMERICCHARACTER);

FORMAT_SLASH: '/' -> type(SLASH);

FORMAT_COLON: ':' -> type(COLON);

FORMAT_PLUS: '+' -> type(PLUS);

FORMAT_MINUS: '-' -> type(MINUS);

FORMAT_DOT: '.' -> type(DOT);
EOF

cat > FortranParser.g4 <<EOF
parser grammar FortranParser;

options { tokenVocab=FortranLexer; }

name: NAME | PROGRAM | END| FUNCTION | SUBROUTINE | MODULE
	 | SUBMODULE | BLOCK | DATA | INTRINSIC | NONINTRINSIC | OPERATOR
	 | READ | FORMATTED | UNFORMATTED | WRITE | ASSIGNMENT | USE | ONLY | IMPORT |  NONE | ALL
	 | KIND | INTEGER | LEN | REAL | DOUBLE | PRECISION | COMPLEX | CHARACTER | LOGICAL | TYPE | CLASS 
	 | EXTERNAL | IMPLICIT | PARAMETER | FORMAT | BIND | NAME | RESULT | ENTRY | STAT | TEAM | TEAMNUMBER | RE | IM 
	 | SEQUENCE | PRIVATE | PROCEDURE | NOPASS | PASS | POINTER | ALLOCATABLE | CODIMENSION | CONTIGUOUS | DIMENSION 
	 | PUBLIC | CONTAINS | FINAL | GENERIC | DEFERRED | NONOVERRIDABLE | INTENT | OPTIONAL | PROTECTED | SAVE | IN | OUT | INOUT 
	 | INTERFACE | ABSTRACT | ENUM | ENUMERATOR | ASYNCHRONOUS | TARGET | VALUE | VOLATILE | EQUIVALENCE | COMMON | NAMELIST | EVENT
	 | WAIT | UNTILCOUNT | POST | ERRMSG | ERROR | STOP | QUIET | ENDFILE | DEALLOCATE | CYCLE | CONTINUE | CLOSE | UNIT | IOSTAT 
	 | IOMSG | ERR | STATUS | CALL | BACKSPACE | ALLOCATE | MOLD | SOURCE | OPEN | ACCESS | ACTION | BLANK | DECIMAL | DELIM 
	 | ENCODING | FILE | FORM | NEWUNIT | PAD | POSITION | RECL | ROUND | SIGN | NULLIFY | LOCK | ACQUIREDLOCK | INQUIRE | IOLENGTH
	 | EXIST | ID | NAMED | NEXTREC | NUMBER | OPENED | PENDING | POS | READWRITE | SEQUENTIAL | SIZE | STREAM | IF | GO | TO | NEWINDEX
	 | FLUSH | FAIL | IMAGE | EXIT | FORALL | WHERE | EOR | UNLOCK | SYNC | MEMORY | IMAGES | REWIND | RETURN | FMT | NML | ADVANCE | REC
	 | PRINT | CRITICAL | CHANGE | SELECT | CASE | DEFAULT | ASSOCIATE | ELSEWHERE | IS | RANK | ELSE | THEN | DO | CONCURRENT | WHILE
	 | SHARED | LOCAL | LOCALINIT | RECURSIVE | PURE | NONRECURSIVE | IMPURE | ELEMENTAL | NOTIFY | TYPEOF | CLASSOF | ENUMERATION
	 | DIRECT | LEADINGZERO | REDUCE | SIMPLE;

EOF
cat ebnf.ebnf >> FortranParser.g4

rm -f rename.txt rename2.txt rename3.txt ids.txt updated_ids.txt ebnf1.ebnf

echo "Fixing string literals in parser grammar."
trparse -t ANTLRv4 FortranParser.g4 | \
	trquery '
	replace //STRING_LITERAL[text() = "'"','"'" ]' '" COMMA ";
	replace //STRING_LITERAL[text() = "'"'_'"'" ]' '" UNDERSCORE ";
	replace //STRING_LITERAL[text() = "'"')'"'" ]' '" RPAREN ";
	replace //STRING_LITERAL[text() = "'"'('"'" ]' '" LPAREN ";
	replace //STRING_LITERAL[text() = "'"':'"'" ]' '" COLON ";
	replace //STRING_LITERAL[text() = "'"'+'"'" ]' '" PLUS ";
	replace //STRING_LITERAL[text() = "'"'/'"'" ]' '" SLASH ";
	replace //STRING_LITERAL[text() = "'"'*'"'" ]' '" ASTERIK ";
	replace //STRING_LITERAL[text() = "'"'-'"'" ]' '" MINUS ";
	replace //STRING_LITERAL[text() = "'"'.'"'" ]' '" DOT ";
	replace //STRING_LITERAL[text() = "'"'GO'"'" ]' '" GO ";
	replace //STRING_LITERAL[text() = "'"'TO'"'" ]' '" TO ";
	replace //STRING_LITERAL[text() = "'"'A'"'" ]' '" A ";
	replace //STRING_LITERAL[text() = "'"'B'"'" ]' '" B ";
	replace //STRING_LITERAL[text() = "'"'C'"'" ]' '" C ";
	replace //STRING_LITERAL[text() = "'"'D'"'" ]' '" D ";
	replace //STRING_LITERAL[text() = "'"'E'"'" ]' '" E ";
	replace //STRING_LITERAL[text() = "'"'F'"'" ]' '" F ";
	replace //STRING_LITERAL[text() = "'"'IMPORT,'"'" ]' '" IMPORT COMMA ";
	replace //STRING_LITERAL[text() = "'"'(C)'"'" ]' '" LPAREN C RPAREN ";
	replace //STRING_LITERAL[text() = "'"'(C'"'" ]' '" LPAREN C ";
	replace //STRING_LITERAL[text() = "'"'),'"'" ]' '" RPAREN COMMA ";
	replace //STRING_LITERAL[text() = "'"'ENUM,'"'" ]' '" ENUM COMMA ";
	replace //STRING_LITERAL[text() = "'"'BIND(C)'"'" ]' '" BIND LPAREN C RPAREN ";
	replace //STRING_LITERAL[text() = "'"'TYPE('"'" ]' '" TYPE LPAREN ";
	replace //STRING_LITERAL[text() = "'"'CLASS('"'" ]' '" CLASS LPAREN ";
	replace //STRING_LITERAL[text() = "'"'(FORMATTED)'"'" ]' '" LPAREN FORMATTED RPAREN ";
	replace //STRING_LITERAL[text() = "'"'(UNFORMATTED)'"'" ]' '" LPAREN UNFORMATTED RPAREN ";
	replace //STRING_LITERAL[text() = "'"'O'"'" ]' '" O ";
	replace //STRING_LITERAL[text() = "'"'Z'"'" ]' '" Z ";
	replace //STRING_LITERAL[text() = "'"'.NIL.'"'" ]' '" NIL ";
	
' | \
	trsponge -c

echo "Fix mutual left-recursion in various rules."
trparse -t ANTLRv4 FortranParser.g4 | \
	trquery '
	replace //parserRuleSpec[RULE_REF/text() = "level_5_expr"] "
level_5_expr: equiv_operand (equiv_op equiv_operand)* ;
";
	replace //parserRuleSpec[RULE_REF/text() = "level_3_expr"] "
level_3_expr: level_2_expr (concat_op level_2_expr)* ;
";
	replace //parserRuleSpec[RULE_REF/text() = "level_2_expr"] "
level_2_expr: (add_operand | add_op add_operand) (add_op add_operand)* ;
";
	replace //parserRuleSpec[RULE_REF/text() = "add_operand"] "
add_operand: mult_operand (add_operand mult_operand)* ;
";
	replace //parserRuleSpec[RULE_REF/text() = "or_operand"] "
or_operand: and_operand (and_op and_operand)* ;
";
	replace //parserRuleSpec[RULE_REF/text() = "equiv_operand"] "
equiv_operand: or_operand (or_op or_operand)* ;
";
	replace //parserRuleSpec[RULE_REF/text() = "expr"] "
expr: level_5_expr (defined_binary_op level_5_expr)* ;
";

' | \
	trsponge -c
