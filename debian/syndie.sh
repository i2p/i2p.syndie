#!/bin/sh
SHARE="/usr/share"
JAVASHARE="${SHARE}/java"

java -cp ${JAVASHARE}/hsqldb.jar:${JAVASHARE}/swt.jar:${SHARE}/syndie/syndie.jar:${SHARE}/i2p/lib/i2p.jar:${SHARE}/syndie/servlet.jar \
	-Djava.library.path=/usr/lib/jni syndie.gui.SWTUI "$@"
