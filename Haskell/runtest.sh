verse=$1
dump=$2
test=$3
tmp=tmp
tests=tests
set -x
$verse --$dump $tests/$test.verse > $tmp/$test.$dump && diff $tests/$test.$dump $tmp/$test.$dump
