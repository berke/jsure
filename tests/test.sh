#!/usr/bin/zsh

JSURE=(../jsure.native -quiet)

jsure() {
  $JSURE >/dev/null 2>&1 $1
}

fail=0
tests=0

for x in *.js ; do
  tests=$((tests + 1))
    ( jsure $x ) || ( fail=$((fail + 1)) ; echo "FAIL POSITIVE $x" )
done

for x in fail/*.js ; do
  tests=$((tests + 1))
    ( jsure $x ) && ( fail=$((fail + 1)) ; echo "FAIL NEGATIVE $x" )
done

echo "TESTS : " $tests
echo "PASS  : " $((tests-fail))
echo "FAIL  : " $fail

if [ $fail -lt 0 ]; then
  exit 1
else
  exit 0
fi
