package org.hsqldb.persist;

public class GCJKludge {
    public static final Class _kludge[] = {
//        org.hsqldb.persist.NIOScaledRAFile.class
//        , 
        //org.hsqldb.persist.NIOLockFile.class
        java.nio.MappedByteBuffer.class
    };
}
