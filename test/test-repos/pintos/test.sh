pintos -v -k -T 60 --qemu  -- -q  run alarm-single < /dev/null 2> tests/devices/alarm-single.errors > tests/devices/alarm-single.output
perl -I../.. ../../tests/devices/alarm-single.ck tests/devices/alarm-single tests/devices/alarm-single.result
pintos -v -k -T 60 --qemu  -- -q  run alarm-multiple < /dev/null 2> tests/devices/alarm-multiple.errors > tests/devices/alarm-multiple.output
perl -I../.. ../../tests/devices/alarm-multiple.ck tests/devices/alarm-multiple tests/devices/alarm-multiple.result
pintos -v -k -T 60 --qemu  -- -q  run alarm-simultaneous < /dev/null 2> tests/devices/alarm-simultaneous.errors > tests/devices/alarm-simultaneous.output
perl -I../.. ../../tests/devices/alarm-simultaneous.ck tests/devices/alarm-simultaneous tests/devices/alarm-simultaneous.result
pintos -v -k -T 60 --qemu  -- -q  run alarm-no-busy-wait < /dev/null 2> tests/devices/alarm-no-busy-wait.errors > tests/devices/alarm-no-busy-wait.output
perl -I../.. ../../tests/devices/alarm-no-busy-wait.ck tests/devices/alarm-no-busy-wait tests/devices/alarm-no-busy-wait.result
pintos -v -k -T 60 --qemu  -- -q  run alarm-one < /dev/null 2> tests/devices/alarm-one.errors > tests/devices/alarm-one.output
perl -I../.. ../../tests/devices/alarm-one.ck tests/devices/alarm-one tests/devices/alarm-one.result
pintos -v -k -T 60 --qemu  -- -q  run alarm-zero < /dev/null 2> tests/devices/alarm-zero.errors > tests/devices/alarm-zero.output
perl -I../.. ../../tests/devices/alarm-zero.ck tests/devices/alarm-zero tests/devices/alarm-zero.result
pintos -v -k -T 60 --qemu  -- -q  run alarm-negative < /dev/null 2> tests/devices/alarm-negative.errors > tests/devices/alarm-negative.output
perl -I../.. ../../tests/devices/alarm-negative.ck tests/devices/alarm-negative tests/devices/alarm-negative.result
pintos -v -k -T 60 --qemu  -- -q  run test_buggy < /dev/null 2> tests/devices/test_buggy.errors > tests/devices/test_buggy.output
perl -I../.. ../../tests/devices/test_buggy.ck tests/devices/test_buggy tests/devices/test_buggy.result
for d in tests/devices/alarm-single tests/devices/alarm-multiple tests/devices/alarm-simultaneous tests/devices/alarm-no-busy-wait tests/devices/alarm-one tests/devices/alarm-zero tests/devices/alarm-negative tests/devices/test_buggy ; do        \
	if echo PASS | cmp -s $d.result -; then  \
		echo "pass $d";                      \
	else                                      \
		echo "FAIL $d";                      \
	fi;                                       \
done > results
cat results
COUNT="`egrep '^(pass|FAIL) ' results | wc -l | sed 's/[ 	]//g;'`"; \
FAILURES="`egrep '^FAIL ' results | wc -l | sed 's/[ 	]//g;'`";     \
if [ $FAILURES = 0 ]; then                                       \
	echo "All $COUNT tests passed!";                             \
else                                                              \
	echo "Warning: $FAILURES of $COUNT tests failed!";          \
  exit 1;
fi
