#!/usr/bin/perl -w

use strict;
use CGI qw(-debug);

$| = 1; # flush, goddamn you
$CGI::POST_MAX = 4*1024*1024; # 4MB max across all uploaded messages

my($requiredPassphrase) = undef; # set to a string to require authentication to import
my($uploadDir) = '/tmp/cgiImport';
if (! -e $uploadDir) {
  mkdir $uploadDir;
}
#$ENV{TMPDIR} = $uploadDir; # CGI.pm stores temporary files here

my($query);
$query = new CGI; # actually parses & caches the data here

if (defined $requiredPassphrase) {
  if ($requiredPassphrase eq $query->param('pass')) {
    # authenticated
  } else {
    print $query->header('text/plain','403 Not authorized');
    exit;
  }
}

print $query->header('text/plain');

my($num) = 0;
# pull in the meta
my($metaIndex) = 0;
my($moreMeta) = 1;
while ($moreMeta) {
 my($metaFile) = $query->upload("meta$metaIndex");
 if (!$metaFile) {
   $moreMeta = 0;
 } else {
   $num = 0;
   my($metaFilename) = "";
   while ($num >= 0) {
     $metaFilename = $uploadDir;
     $metaFilename .= '/meta';
     $metaFilename .= '_';
     $metaFilename .= $num;
     $metaFilename .= '_';
     $metaFilename .= $metaIndex;
     $metaFilename .= '.syndie';
     if (-e $metaFilename) { 
       $num = $num + 1;
     } else {
       $num = -1; # aka break
     }
   }
   open META, ">$metaFilename";
   binmode META;
   while (<$metaFile>) { print META; }
   close META;
   chmod 666, $metaFilename; # so it can be deleted by whomever
#   print STDOUT "Uploaded to $metaFilename\n";
   $metaIndex = $metaIndex + 1;
 }
}

# now pull in the posts
my($postIndex) = 0;
my($morePost) = 1;
while ($morePost) {
 my($postFile) = $query->upload("post$postIndex");
 if (!$postFile) {
   $morePost = 0;
 } else {
   $num = 0;
   my($postFilename) = "";
   while ($num >= 0) {
     #$postFilename = $uploadDir . '/post_' . $num . '_' . $postIndex . '.syndie';
     $postFilename = $uploadDir;
     $postFilename .= '/post';
     $postFilename .= '_';
     $postFilename .= $num;
     $postFilename .= '_';
     $postFilename .= $postIndex;
     $postFilename .= '.syndie';
     if (-e $postFilename) { 
       $num = $num + 1;
     } else {
       $num = -1; # aka break
     }
   }
   open POST, ">$postFilename";
   binmode POST;
   while (<$postFile>) { print POST; }
   close POST;
   chmod 666, $postFilename; # so it can be deleted by whomever
#   print STDOUT "Uploaded to $metaFilename\n";
#   print STDOUT "Uploaded to $postFilename\n";
   $postIndex = $postIndex + 1;
 }
}

print STDOUT 'Uploaded ' . $postIndex . ' posts and ' . $metaIndex . " metadata\n";
if ($postIndex == 0 && $metaIndex == 0) {
  print STDOUT "No files updated.  query: \n";
  print STDOUT $query->Vars;
}
1;
