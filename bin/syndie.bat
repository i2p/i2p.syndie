@ECHO OFF
SET JAVA=java
REM SET JAVA=/opt/kaffe/bin/kaffe
REM SET JAVA=/usr/local/kaffe117/bin/kaffe
REM SET JAVA=/u1/gcc-4.2-20060520-x86_64/bin/gij
%JAVA% -cp lib\syndie.jar;lib\hsqldb.jar syndie.db.TextUI %*
