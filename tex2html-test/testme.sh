#!/bin/bash

silent=
if [[ $1 == silent ]]; then
    silent="&>/dev/null"
fi

if [ ! -f tex2html ]; then
    echo "Uf-puf-puf"
    exit 1
fi

wa=0
for a in test.*.in
do
    cat "$a" | ./tex2html > output
    if ! diff -Nua "${a%.*}.out" output $silent ; then
        echo "Wrong answer, test \`$a'" >&2
        wa=1
    fi
done

rm -f output

testfiles=(testme.sh test.*.in test.*.out)
testfiles=${#testfiles[@]}
allfiles=(*)
allfiles=${#allfiles[@]}

(( allfiles == (testfiles + 2) )) && echo "Don't drop your garbage here!" && exit 1

[[ $wa == 0 ]] && echo "Accepted"
[[ $wa == 1 ]] && echo "Rejected"
[[ $wa == 2 ]] && echo "Partially accepted"

exit $wa
