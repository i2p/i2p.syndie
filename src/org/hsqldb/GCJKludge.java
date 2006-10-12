package org.hsqldb;

public class GCJKludge {
    public static final Class _kludge[] = {
        org.hsqldb.DatabaseInformationFull.class
        , org.hsqldb.DatabaseInformationMain.class
        //, org.hsqldb.HsqlSocketFactorySecure.class // removed for gcj 3.4 support
        , org.hsqldb.Library.class
    };
}
