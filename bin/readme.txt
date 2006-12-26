You should launch Syndie with syndie.exe.  However, you can launch
Syndie as a standard java application:
 - specify the classpath to include:
  - lib/syndie.jar
  - lib/hsqldb.jar
  - swt.jar (likely in lib/, but you may use another one)
 - specify the main class:
  - syndie.gui.SWTUI
 - specify any command line parameters:
  - use "--cli" if you want to run the text interface
  - an additional parameter defines an alternate Syndie data directory

If using an swt.jar prior to SWT-3.3M4, you will also need to add the
flag -Djava.library.path=/some/directory/containing/native/swt/libs
On linux machines, that may be /usr/lib, or for those with eclipse
installed, it may be some convoluted directory under the eclipse dir.
OSX users will also need to specify -XstartOnFirstThread as a command
line parameter

Additionally, you can specify a dictionary to use on the command
line with -Dsyndie.dict=filename, where filename is the path to a file
containing a newline delimited list of words.  On windows and OSX, an
English dictionary is bundled with Syndie, and on linux, Syndie uses
the contents of /usr/share/dict/words by default.

So, as a windows example:
 javaw -cp lib\syndie.jar;lib\hsqldb.jar;lib\swt.jar syndie.gui.SWTUI
or as a linux example:
 SWTDIR=../swt-I20060922-0010-gtk-linux-x86 \
      java -cp lib/syndie.jar:lib/hsqldb.jar:${SWTDIR}/swt.jar \
      -Djava.library.path=${SWTDIR} syndie.gui.SWTUI /tmp/syndieroot
or on OSX:
 java -XstartOnFirstThread -cp lib/syndie.jar:lib/hsqldb.jar:lib/swt.jar \
      Syndie.gui.SWTUI
