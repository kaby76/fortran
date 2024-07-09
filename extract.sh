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
	tritext.exe --filter 'l1<50;l2>780;l2<60' --markup $@ 2> /dev/null > $to
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

echo "Change lines with brackets in order to make grammar and parse work."
sed -i "s%R779 <i>lbracket</i> <b>is</b> \[%R779 <i>lbracket</i> <b>is</b> '['%" ebnf.ebnf
sed -i "s%R780 <i>rbracket</i> <b>is</b> \]%R780 <i>rbracket</i> <b>is</b> ']'%" ebnf.ebnf
dos2unix ebnf.ebnf

echo "Adding rules that spec does not define."
trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery 'insert //EOF "

<i>scalar-int-constant-expr</i> <b>is</b> <i>int-constant-expr</i>

<i>function-name</i> <b>is</b> <i>name</i>

<i>select-construct-name</i> <b>is</b> <i>name</i>

<i>scalar-int-expr</i> <b>is</b> <i>int-expr</i>

<i>scalar-expr</i> <b>is</b> <i>expr</i>

<i>section-subscript-list</i> <b>is</b> <i>section-subscript</i> [ , <i>section-subscript</i> ] ...

<i>dummy-arg-list</i> <b>is</b> <i>dummy-arg</i> [ , <i>dummy-arg</i> ] ...

<i>dummy-arg-name-list</i> <b>is</b> <i>dummy-arg-name</i> [ , <i>dummy-arg-name</i> ] ...

<i>result-name</i> <b>is</b> <i>name</i>

<i>entry-name</i> <b>is</b> <i>name</i>

<i>procedure-name</i> <b>is</b> <i>name</i>

<i>subroutine-name</i> <b>is</b> <i>name</i>

<i>scalar-logical-expr</i> <b>is</b> <i>logical-expr</i>

<i>binding-name</i> <b>is</b> <i>name</i>

<i>actual-arg-spec-list</i> <b>is</b> <i>actual-arg-spec</i> [ , <i>actual-arg-spec</i> ] ...

<i>intrinsic-procedure-name</i> <b>is</b> <i>name</i>

<i>intrinsic-procedure-name-list</i> <b>is</b> <i>intrinsic-procedure-name</i> [ , <i>intrinsic-procedure-name</i> ] ...

<i>proc-decl-list</i> <b>is</b> <i>proc-decl</i> [ , <i>proc-decl</i> ] ...

<i>external-name-list</i> <b>is</b> <i>external-name</i> [ , <i>external-name</i> ] ...

<i>specific-procedure-list</i> <b>is</b> <i>specific-procedure</i> [ , <i>specific-procedure</i> ] ...

<i>block-data-name</i> <b>is</b> <i>name</i>

<i>submodule-name</i> <b>is</b> <i>name</i>

<i>output-item-list</i> <b>is</b> <i>output-item</i> [ , <i>output-item</i> ] ...

<i>input-item-list</i> <b>is</b> <i>input-item</i> [ , <i>input-item</i> ] ...

<i>module-name</i> <b>is</b> <i>name</i>

<i>rename-list</i> <b>is</b> <i>rename</i> [ , <i>rename</i> ] ...

<i>local-name</i> <b>is</b> <i>name</i>

<i>use-name</i> <b>is</b> <i>name</i>

<i>ancestor-module-name</i> <b>is</b> <i>name</i>

<i>parent-submodule-name</i> <b>is</b> <i>name</i>

<i>generic-name</i> <b>is</b> <i>name</i>

<i>procedure-entity-name</i> <b>is</b> <i>name</i>

<i>external-name</i> <b>is</b> <i>name</i>

<i>only-list</i> <b>is</b> <i>only</i> [ , <i>only</i> ] ...

<i>v-list</i> <b>is</b> <i>v</i> [ , <i>v</i> ] ...

<i>program-name</i> <b>is</b> <i>name</i>

<i>scalar-default-char-variable</i> <b>is</b> <i>default-char-variable</i>

<i>scalar-int-variable</i> <b>is</b> <i>int-variable</i>

<i>scalar-logical-variable</i> <b>is</b> <i>logical-variable</i>

<i>inquire-spec-list</i> <b>is</b> <i>inquire-spec</i> [ , <i>inquire-spec</i> ] ...

<i>flush-spec-list</i> <b>is</b> <i>flush-spec</i> [ , <i>flush-spec</i> ] ...

<i>position-spec-list</i> <b>is</b> <i>position-spec</i> [ , <i>position-spec</i> ] ...

<i>wait-spec-list</i> <b>is</b> <i>wait-spec</i> [ , <i>wait-spec</i> ] ...

<i>io-implied-do-object-list</i> <b>is</b> <i>io-implied-do-object</i> [ , <i>io-implied-do</i> ] ...

<i>scalar-default-char-expr</i> <b>is</b> <i>default-char-expr</i>

<i>scalar-default-char-constant-expr</i> <b>is</b> <i>default-char-constant-expr</i>

<i>namelist-group-name</i> <b>is</b> <i>name</i>

<i>io-control-spec-list</i> <b>is</b> <i>io-control-spec</i> [ , <i>io-control-spec</i> ] ...

<i>close-spec-list</i> <b>is</b> <i>close-spec</i> [ , <i>close-spec</i> ] ...

<i>connect-spec-list</i> <b>is</b> <i>connect-spec</i> [ , <i>connect-spec</i> ] ...

<i>scalar-variable</i> <b>is</b> <i>variable</i>

<i>sync-stat-list</i> <b>is</b> <i>sync-stat</i> [ , <i>sync-stat</i> ] ...

<i>lock-stat-list</i> <b>is</b> <i>lock-stat</i> [ , <i>lock-stat</i> ] ...

<i>event-wait-spec-list</i> <b>is</b> <i>event-wait-spec</i> [ , <i>event-wait-spec</i> ] ...

<i>label-list</i> <b>is</b> <i>label</i> [ , <i>label</i> ] ...

<i>form-team-spec-list</i> <b>is</b> <i>form-team-spec</i> [ , <i>form-team-spec</i> ] ...

<i>construct-name</i> <b>is</b> <i>name</i>

<i>scalar-int-constant-expr</i> <b>is</b> <i>int-constant-expr</i>

<i>type-attr-spec-list</i> <b>is</b> <i>type-param-attr-spec</i> [ , <i>type-param-attr-spec</i> ] ...

<i>type-param-name-list</i> <b>is</b> <i>type-param-name</i> [ , <i>type-param-name</i> ] ...

<i>type-name</i> <b>is</b> <i>name</i>

<i>type-param-decl-list</i> <b>is</b> <i>type-param-decl</i> [ , <i>type-param-decl</i> ] ...

<i>type-param-name</i> <b>is</b> <i>name</i>

<i>component-attr-spec-list</i> <b>is</b> <i>component-attr-spec</i> [ , <i>component-attr-spec</i> ] ...

<i>component-name</i> <b>is</b> <i>name</i>

<i>deferred-shape-spec-list</i> <b>is</b> <i>deferred-shape-spec</i> [ , <i>deferred-shape-spec</i> ] ...

<i>explicit-shape-spec-list</i> <b>is</b> <i>explicit-shape-spec</i> [ , <i>explicit-shape-spec</i> ] ...

<i>proc-component-attr-spec-list</i> <b>is</b> <i>proc-component-attr-spec</i> [ , <i>proc-component-attr-spec</i> ] ...

<i>arg-name</i> <b>is</b> <i>name</i>

<i>component-decl-list</i> <b>is</b> <i>component-decl</i> [ , <i>component-decl</i> ] ...

<i>scalar-int-constant-name</i> <b>is</b> <i>int-constant-name</i>

<i>int-constant-name</i> <b>is</b> <i>name</i>

<i>associate-construct-name</i> <b>is</b> <i>name</i>

<i>associate-name</i> <b>is</b> <i>name</i>

<i>case-value-range-list</i> <b>is</b> <i>case-value-range</i> [ , <i>case-value-range</i> ] ...

<i>case-construct-name</i> <b>is</b> <i>name</i>

<i>scalar-constant-expr</i> <b>is</b> <i>constant-expr</i>

<i>binding-attr-list</i> <b>is</b> <i>binding-attr</i> [ , <i>binding-attr</i> ] ...

";' | trtext > ebnf1.ebnf
mv ebnf1.ebnf ebnf.ebnf
dos2unix ebnf.ebnf

echo "From here on, we use trparse to parse the EBNF and convert it to Antlr format."
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

echo "Deleting several rules involving 'xyz', which should not be in the EBNF."
echo "Delete rule numbers from rules, and add semi-colon rule terminators."
echo "Add function-name, which is missing from Spec EBNF."
echo "Change syntax around for ASSIGN, ALT, zero_or_one, zero_or_more, and any."
trparse -p ebnf/Generated-CSharp ebnf.ebnf | \
	trquery '
		delete //rule_[lhs/id_/ID/text() = "<i>xyz</i>"];
		delete //rule_[lhs/id_/ID/text() = "<i>xyz-list</i>"];
		delete //rule_[lhs/id_/ID/text() = "<i>xyz-name</i>"];
		delete //rule_[lhs/id_/ID/text() = "<i>scalar-xyz</i>"];
		delete //RULE_NUMBER;
		replace //ASSIGN ":";
		replace //ALT "|";
		replace //zero_or_one/LEFT_SQUARE_BRACKET "(";
		replace //zero_or_one/RIGHT_SQUARE_BRACKET ")?";
		delete //zero_or_more/DOT_DOT_DOT;
		replace //zero_or_more/LEFT_SQUARE_BRACKET "(";
		replace //zero_or_more/RIGHT_SQUARE_BRACKET ")*";
		move //any/@WS[1] ..;
		insert before //any "'\''";
		insert after //any "'\''";
		insert after //rule_ " ;";' | \
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
