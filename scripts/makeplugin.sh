#!/bin/sh
#
#  basic packaging up of a plugin
#
#  usage: makeplugin.sh plugindir
#
#  zzz 2010-02
#
PUBKEYDIR=$HOME/.i2p-plugin-keys
PUBKEYFILE=$PUBKEYDIR/plugin-public-signing.key
PRIVKEYFILE=$PUBKEYDIR/plugin-private-signing.key
B64KEYFILE=$PUBKEYDIR/plugin-public-signing.txt
export I2P=../i2p/pkg-temp
 
# put your files in here
PLUGINDIR=${1:-plugin}

PC=plugin.config
PCT=${PC}.tmp

if [ ! -f $PRIVKEYFILE ]
then
	mkdir -p $PUBKEYDIR
	java -cp $I2P/lib/i2p.jar net.i2p.crypto.TrustedUpdate keygen $PUBKEYFILE $PRIVKEYFILE || exit 1
	java -cp $I2P/lib/i2p.jar net.i2p.data.Base64 encode $PUBKEYFILE $B64KEYFILE || exit 1
	rm -rf logs/
	chmod 444 $PUBKEYFILE $B64KEYFILE
	chmod 400 $PRIVKEYFILE
	echo "Created new keys: $PUBKEYFILE $PRIVKEYFILE"
fi

rm -f plugin.zip
if [ ! -d $PLUGINDIR ]
then
	echo "You must have a $PLUGINDIR directory"
	exit 1
fi

OPWD=$PWD
cd $PLUGINDIR

if [ ! -f $PC ]
then
	echo "You must have a $PC file"
	exit 1
fi

grep -q '^signer=' $PC
if [ "$?" -ne "0" ]
then
	echo "You must have a signer in $PC"
        echo 'For example signer=joe@mail.i2p'
	exit 1
fi

grep -q '^name=' $PC
if [ "$?" -ne "0" ]
then
	echo "You must have a plugin name in $PC"
        echo 'For example name=foo'
	exit 1
fi

grep -q '^version=' $PC
if [ "$?" -ne "0" ]
then
	echo "You must have a version in $PC"
        echo 'For example version=0.1.2'
	exit 1
fi

# update the date
grep -v '^date=' $PC > $PCT
DATE=`date '+%s000'`
echo "date=$DATE" >> $PCT
mv $PCT $PC

# add our Base64 key
grep -v '^key=' $PC > $PCT
B64KEY=`cat $B64KEYFILE`
echo "key=$B64KEY" >> $PCT || exit 1
mv $PCT $PC

# zip it
zip -r $OPWD/plugin.zip * || exit 1

# get the version and use it for the sud header
VERSION=`grep '^version=' $PC | cut -f 2 -d '='`
# get the name and use it for the file name
NAME=`grep '^name=' $PC | cut -f 2 -d '='`
XPI2P=${NAME}.xpi2p
cd $OPWD

# sign it
java -cp $I2P/lib/i2p.jar net.i2p.crypto.TrustedUpdate sign plugin.zip $XPI2P $PRIVKEYFILE $VERSION || exit 1
rm -f plugin.zip

# verify
echo 'Verifying. ...'
java -cp $I2P/lib/i2p.jar net.i2p.crypto.TrustedUpdate showversion $XPI2P || exit 1
java -cp $I2P/lib/i2p.jar -Drouter.trustedUpdateKeys=$B64KEY net.i2p.crypto.TrustedUpdate verifysig $XPI2P || exit 1
rm -rf logs/

echo -n 'Plugin created: '
wc -c $XPI2P
