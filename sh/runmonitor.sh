#!/bin/sh

PID=0
prun() {
    f=$1;
    eval "$*" ; exitcode=$?
    echo "'$f' returned '$exitcode'"
    exit $exitcode
}

runtest() {
    local timeout=$1; shift
    
    prun $* &
    PID="$!"
    
    status="timeout"
    for i in `seq $timeout`; do
	if kill -0 $PID 2>/dev/null; then
	    true;
	elif wait "$PID"; then
	    status="success $?"; break
	else
	    status="failure $?"; break
	fi
	sleep 1
    done
    case "$status" in
	success*) ;;
	failure*)
	    ;;
	timeout*)
	    echo "timeout for $* (${PID})"
	    kill -9 ${PID}
	    ;;
    esac
}

CR=$(printf "\n")

runtest 20 bash runmonitortest.sh | (
    IFS="${CR}"
    while read line; do
	case ${line} in
	    p:5*)
		echo "Found ${line}: pid ${PID}";
		kill -9 ${PID}
		;;
	    *)
		echo $line;
	    ;;
	esac
    done
)

