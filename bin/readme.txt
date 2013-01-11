You should launch Syndie with syndie.jar.  However, you can launch
Syndie as a standard java application:
 - specify the classpath to include:
  - lib/syndie.jar
  - lib/hsqldb.jar
  - swt.jar (likely in lib/, but you may use another one)
 - specify the main class:
  - syndie.gui.SWT
 - specify any command line parameters:
  - use "--cli" if you want to run the text interface
  - an additional parameter defines an alternate Syndie data directory

OSX users will also need to specify -XstartOnFirstThread as a command
line parameter

Additionally, you can specify a dictionary to use on the command
line with -Dsyndie.dict=filename, where filename is the path to a file
containing a newline delimited list of words.  On windows and OSX, an
English dictionary is bundled with Syndie, and on linux, Syndie uses
the contents of /usr/share/dict/words by default.

So, on Windows:
   javaw -cp lib\syndie.jar;lib\hsqldb.jar;lib\swt.jar syndie.gui.SWT
on Linux:
   java -cp lib/syndie.jar:lib/hsqldb.jar:${SWTDIR}/swt.jar \
       syndie.gui.SWT /tmp/syndieroot
or on OSX:
    java -d32 -XstartOnFirstThread -cp lib/syndie.jar:lib/hsqldb.jar:lib/swt.jar \
       syndie.gui.SWT
