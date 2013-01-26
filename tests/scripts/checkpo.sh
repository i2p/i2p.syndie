#!/bin/sh
# Run 'msgfmt -c' on all .po files
# Returns nonzero on failure
#
# zzz 2011-02
# public domain
#

[ $(which msgfmt) ] || (echo "ERROR: gettext not found!" >&2 && exit 1)

cd `dirname $0`/../..

DIRS="\
    locale"

for i in `find $DIRS -maxdepth 1 -type f -name '*.po'`
do
    echo "Checking $i ..."
    msgfmt -c $i -o /dev/null
    if [ $? -ne 0 ]; then
        echo "********* FAILED CHECK FOR $i *************"
        FAIL=1
    fi
done

if [ "$FAIL" != "" ]; then
    echo "******** At least one file failed check *********"
else
    echo "All files passed"
fi
exit $FAIL
