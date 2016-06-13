#!/bin/sh
#
#  basic packaging up of a plugin
#
#  usage: makeplugin.sh plugindir
#
#  zzz 2010-02
#  zzz 2014-08 added support for su3 files
#
PUBKEYDIR=$HOME/.i2p-plugin-keys
PUBKEYFILE=$PUBKEYDIR/plugin-public-signing.key
PRIVKEYFILE=$PUBKEYDIR/plugin-private-signing.key
B64KEYFILE=$PUBKEYDIR/plugin-public-signing.txt
PUBKEYSTORE=$PUBKEYDIR/plugin-su3-public-signing.crt
PRIVKEYSTORE=$PUBKEYDIR/plugin-su3-keystore.ks
KEYTYPE=RSA_SHA512_4096

# put your files in here
PLUGINDIR=${1:-plugin}

PC=plugin.config
PCT=${PC}.tmp

if [ ! -d $PLUGINDIR ]
then
	echo "You must have a $PLUGINDIR directory"
	exit 1
fi

if [ ! -f $PLUGINDIR/$PC ]
then
	echo "You must have a $PLUGINDIR/$PC file"
	exit 1
fi

SIGNER=`grep '^signer=' $PLUGINDIR/$PC`
if [ "$?" -ne "0" ]
then
	echo "You must have a plugin name in $PC"
	echo 'For example name=foo'
	exit 1
fi
SIGNER=`echo $SIGNER | cut -f 2 -d '='`

if [ ! -f $PRIVKEYFILE ]
then
	echo "Creating new XPI2P DSA keys"
	mkdir -p $PUBKEYDIR || exit 1
	java -cp $I2P/lib/i2p.jar net.i2p.crypto.TrustedUpdate keygen $PUBKEYFILE $PRIVKEYFILE || exit 1
	java -cp $I2P/lib/i2p.jar net.i2p.data.Base64 encode $PUBKEYFILE $B64KEYFILE || exit 1
	rm -rf logs/
	chmod 444 $PUBKEYFILE $B64KEYFILE
	chmod 400 $PRIVKEYFILE
	echo "Created new XPI2P keys: $PUBKEYFILE $PRIVKEYFILE"
fi

if [ ! -f $PRIVKEYSTORE ]
then
	echo "Creating new SU3 $KEYTYPE keys for $SIGNER"
	java -cp $I2P/lib/i2p.jar net.i2p.crypto.SU3File keygen -t $KEYTYPE $PUBKEYSTORE $PRIVKEYSTORE $SIGNER || exit 1
	echo '*** Save your password in a safe place!!! ***'
	rm -rf logs/
	# copy to the router dir so verify will work
        CDIR=$I2P/certificates/plugin
	mkdir -p $CDIR || exit 1
	CFILE=$CDIR/`echo $SIGNER | sed s/@/_at_/`.crt
	cp $PUBKEYSTORE $CFILE
	chmod 444 $PUBKEYSTORE
	chmod 400 $PRIVKEYSTORE
	chmod 644 $CFILE
	echo "Created new SU3 keys: $PUBKEYSTORE $PRIVKEYSTORE"
	echo "Copied public key to $CFILE for testing"
fi

rm -f plugin.zip

OPWD=$PWD
cd $PLUGINDIR

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
echo "date=$DATE" >> $PCT || exit 1
mv $PCT $PC || exit 1

# add our Base64 key
grep -v '^key=' $PC > $PCT
B64KEY=`cat $B64KEYFILE`
echo "key=$B64KEY" >> $PCT || exit 1
mv $PCT $PC || exit 1

# zip it
zip -r $OPWD/plugin.zip * || exit 1

# get the version and use it for the sud header
VERSION=`grep '^version=' $PC | cut -f 2 -d '='`
# get the name and use it for the file name
NAME=`grep '^name=' $PC | cut -f 2 -d '='`
XPI2P=${NAME}.xpi2p
SU3=${NAME}.su3
cd $OPWD

# sign it
echo 'Signing. ...'
java -cp $I2P/lib/i2p.jar net.i2p.crypto.TrustedUpdate sign plugin.zip $XPI2P $PRIVKEYFILE $VERSION || exit 1
java -cp $I2P/lib/i2p.jar net.i2p.crypto.SU3File sign -c PLUGIN -t $KEYTYPE plugin.zip $SU3 $PRIVKEYSTORE $VERSION $SIGNER || exit 1
rm -f plugin.zip

# verify
echo 'Verifying. ...'
java -cp $I2P/lib/i2p.jar net.i2p.crypto.TrustedUpdate showversion $XPI2P || exit 1
java -cp $I2P/lib/i2p.jar -Drouter.trustedUpdateKeys=$B64KEY net.i2p.crypto.TrustedUpdate verifysig $XPI2P || exit 1
java -cp $I2P/lib/i2p.jar net.i2p.crypto.SU3File showversion $SU3 || exit 1
java -cp $I2P/lib/i2p.jar net.i2p.crypto.SU3File verifysig -k $PUBKEYSTORE $SU3 || exit 1
rm -rf logs/

echo 'Plugin files created: '
wc -c $XPI2P
wc -c $SU3

exit 0
