#!/bin/sh
# Validate XML files using xmllint and HTML files using tidy
# Returns nonzero on failure
#
# zzz 2011-03
# public domain
#

cd `dirname $0`/../..

XMLFILES="\
./swt/build.xml \
./resources/Win_shortcutSpec.xml \
./resources/Unix_shortcutSpec.xml \
./build.xml \
./resources/izpack/patches/bin/langpacks/installer/ukr.xml"

HTMLFILES="\
./data/archive/index.html \
./doc/web/archive.html \
./doc/web/roadmap.html \
./doc/web/help/panel_message.html \
./doc/web/help/panel_threadtree.html \
./doc/web/help/common.html \
./doc/web/help/panel_forumselection.html \
./doc/web/help/index.html \
./doc/web/about.html \
./doc/web/spec.html \
./doc/web/db.html \
./doc/web/tour/index.html \
./doc/web/faq.html \
./doc/web/download.html \
./doc/web/donate.html \
./doc/web/desktop_design.html \
./doc/web/related.html \
./doc/web/manual.html \
./doc/web/monotone_howto.html \
./doc/web/usecases.html \
./doc/web/features.html \
./doc/web/dev.html \
./doc/web/index.html"

if $(which xmllint >/dev/null 2>&1); then
    CHECKXML=1
    echo 'Checking XML files....................'
    for i in $XMLFILES
    do
        echo "Checking $i ..."
        xmllint --noout $i
        if [ $? -ne 0 ]; then
            echo "********* FAILED CHECK FOR $i *************" >&2
            FAIL=1
        fi
    done
else
    CHECKXML=0
fi

if $(which tidy > /dev/null 2>&1); then
    CHECKHTML=1
    echo 'Checking HTML files....................'
    for i in $HTMLFILES
    do
        echo "Checking $i ..."
        tidy -e -q $i
        if [ $? -ne 0 ]; then
            echo "********* FAILED CHECK FOR $i *************" >&2
            FAIL=1
        fi
    done
else
    CHECKHTML=0
fi

if [ "$FAIL" != "" ]; then
    echo "******** At least one file failed check *********" >&2
else
    echo "All checked files passed."
fi

if [ $CHECKXML -ne 1 ]; then
    echo "**** WARNING: XML files could not be checked: xmllint not found.****" >&2
    FAIL=3
fi

if [ $CHECKHTML -ne 1 ];then
    echo "**** WARNING: HTML files could not be checked: tidy not found.****" >&2
    FAIL=3
fi
exit $FAIL
