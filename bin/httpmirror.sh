#!bash
# trivial script to recursively get a website, grabbing its pages
# and images, and shove it into a syndie post.
# the channel written to is the nym's first channel (created on db
# initialization), but can be overridden by explicitly setting POST_CHANNEL

URL=$*
TEMPDIR=./.httpMirrorTemp
HTMLTOTXT="links -dump "
# the -E turns all of the fetched pages (*.html, *.pl, etc) into *.html
FETCH="wget -q -nd -nH -P $TEMPDIR -E --convert-links -A htm,html,php,pl,cgi,jsp,asp --relative --no-parent -r "
POST_CHANNEL="0"
# change to none if no expiration wanted
EXPIRATION=`date --date='+1 week' +%Y/%m/%d`
TAGS="httpfetch,$URL"
BINDIR=`dirname $0`
SYNDIE="$BINDIR/syndie"

if [[ "X$URL" == "X" ]]; then
	echo "Usage: $0 http://some/url/to/fetch";
	exit 1;
fi

rm -rf $TEMPDIR
echo "Fetching $URL"
mkdir $TEMPDIR
$FETCH $URL
find $TEMPDIR -name \*.html -exec echo $HTMLTOTXT {} \> {}.txt \; | sh
echo login >script
echo menu post >> script
echo channels >> script
echo create --channel $POST_CHANNEL >> script
echo set --subject \"fetch of $URL\" >> script
echo addref --name \"original site\" --uri "$URL" >> script
echo set --expiration $EXPIRATION >> script
echo set --publicTags $TAGS >> script
find $TEMPDIR -name \*.txt -exec echo addpage --in {} \; >> script
find $TEMPDIR -name \*.png -exec echo addattachment --in {} --type image/png \; >> script
find $TEMPDIR -name \*.gif -exec echo addattachment --in {} --type image/gif \; >> script
find $TEMPDIR -name \*.jpg -exec echo addattachment --in {} --type image/jpg \; >> script
find $TEMPDIR -name \*.jpeg -exec echo addattachment --in {} --type image/jpg \; >> script
echo execute >> script
# perhaps this should push the new post with
# "menu syndicate ; getindex ; schedule --put outbound ; post" ?
echo exit >> script

# script built, now fire away
. $SYNDIE @script
rm script
rm -rf $TEMPDIR
