#!/bin/bash

NODELIST="node1@localhost"

echo "Starting Erlang worker nodes"
for i in $(seq 2 5);do
  erl -pa ebin -pa ../distributed_map/ebin -noinput -setcookie oatmealraisin -sname node${i}@localhost &
  NODELIST="${NODELIST} node${i}@localhost"
done

echo "Starting Erlang main console"
echo ""
echo "==== IMPORTANT ====="
echo "type the following to begin the demo:"
echo "  \"challenge:distributed_map()\" for a parallel distributed solution to the Sortable Challenge using the Distributed Map application"
echo "===================="
erl -pa ebin -pa ../distributed_map/ebin -setcookie oatmealraisin -sname node1@localhost -run distmap setupcluster ${NODELIST}

echo "Killing Erlang worker nodes"
for PID in $(ps axu | grep "\-noinput \-setcookie oatmealraisin" | grep -v "grep" | sed "s/^[^\ ]*\ *\([0-9]*\)\ *.*/\1/g"); do
  kill -HUP ${PID}
done
