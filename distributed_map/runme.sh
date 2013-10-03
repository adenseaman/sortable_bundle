#!/bin/bash

NODELIST="node1@localhost"

echo "Starting Erlang worker nodes"
for i in $(seq 2 5);do
  erl -pa ebin -noinput -setcookie oatmealraisin -sname node${i}@localhost &
  NODELIST="${NODELIST} node${i}@localhost"
done

echo "Starting Erlang main console"
echo ""
echo "==== IMPORTANT ====="
echo "type one of the following to begin the demo:"
echo "  \"distmap:slow_example()\" for a throttled test on a small amount of data"
echo "  \"distmap:fast_example\" for an unthrottled test on a large amount of data"
echo "===================="
erl -pa ebin -setcookie oatmealraisin -sname node1@localhost -run distmap setupcluster ${NODELIST}

echo "Killing Erlang worker nodes"
for PID in $(ps axu | grep "\-noinput \-setcookie oatmealraisin" | grep -v "grep" | sed "s/^[^\ ]*\ *\([0-9]*\)\ *.*/\1/g"); do
  kill -HUP ${PID}
done
