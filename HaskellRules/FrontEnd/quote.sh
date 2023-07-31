pn=$1

echo ''
echo $pn :: '(String, String)'
echo $pn = '("'$pn'", "\'
sed -e 's/^/\\/' -e 's/$/\\n\\/' $pn.verse
echo '\\")'
