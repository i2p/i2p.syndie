package syndie.db;

/**
 *  Result of Importer.processMessage()
 *  @since 1.103-x
 */
public class ImportResult {

    public interface Result {
            public boolean ok();
            public String msg();
        }

    public static enum Detail implements Result {
        // good ones
        IMPORT_OK {
            public boolean ok() { return true; }
            public String msg() { return _x("Import successful"); }
        },
        IMPORT_ALREADY {
            public boolean ok() { return true; }
            public String msg() { return _x("Already imported"); }
        },
        IMPORT_DELETED {
            public boolean ok() { return true; }
            public String msg() { return _x("Already deleted"); }
        },
        IMPORT_OK_FORUM {
            public boolean ok() { return true; }
            public String msg() { return _x("Import forum info successful"); }
        },
        IMPORT_OK_POST {
            public boolean ok() { return true; }
            public String msg() { return _x("Import post successful"); }
        },
        IMPORT_OK_KEYS {
            public boolean ok() { return true; }
            public String msg() { return _x("Import keys successful"); }
        },

        // bad ones
        IMPORT_NO_ENC_TYPE {
            public boolean ok() { return false; }
            public String msg() { return _x("No enclosure type"); }
        },
        IMPORT_BAD_ENC_FMT {
            public boolean ok() { return false; }
            public String msg() { return _x("Bad enclosure format"); }
        },
        IMPORT_BAD_ENC_TYPE {
            public boolean ok() { return false; }
            public String msg() { return _x("Bad enclosure type"); }
        },
        IMPORT_BAD_MSG_TYPE {
            public boolean ok() { return false; }
            public String msg() { return _x("Bad message format"); }
        },
        IMPORT_NO_URI {
            public boolean ok() { return false; }
            public String msg() { return _x("No URI"); }
        },
        IMPORT_NO_CHAN {
            public boolean ok() { return false; }
            public String msg() { return _x("No channel"); }
        },
        IMPORT_BAN_CHAN {
            public boolean ok() { return false; }
            public String msg() { return _x("Banned channel"); }
        },
        IMPORT_BAN_AUTH {
            public boolean ok() { return false; }
            public String msg() { return _x("Banned author"); }
        },
        IMPORT_UNK_CHAN {
            public boolean ok() { return false; }
            public String msg() { return _x("Unknown channel"); }
        },
        IMPORT_UNK_AUTH {
            public boolean ok() { return false; }
            public String msg() { return _x("Unknown author"); }
        },
        IMPORT_NO_READ_KEY {
            public boolean ok() { return false; }
            public String msg() { return _x("No read key"); }
        },
        IMPORT_NO_REPLY_KEY {
            public boolean ok() { return false; }
            public String msg() { return _x("No reply key"); }
        },
        IMPORT_NO_PASS {
            public boolean ok() { return false; }
            public String msg() { return _x("Passphrase required"); }
        },
        IMPORT_NO_AUTH {
            public boolean ok() { return false; }
            public String msg() { return _x("Not authorized"); }
        },
        IMPORT_BAD_META_VERIFY {
            public boolean ok() { return false; }
            public String msg() { return _x("Meta does not verify"); }
        },

        // generic fails
        IMPORT_SQLE {
            public boolean ok() { return false; }
            public String msg() { return _x("Database error"); }
        },
        IMPORT_IOE {
            public boolean ok() { return false; }
            public String msg() { return _x("I/O error"); }
        },
        IMPORT_CORRUPT {
            public boolean ok() { return false; }
            public String msg() { return _x("Corrupt"); }
        },
        IMPORT_DECRYPT {
            public boolean ok() { return false; }
            public String msg() { return _x("Unable to decrypt"); }
        },
        IMPORT_BAD_HEADER {
            public boolean ok() { return false; }
            public String msg() { return _x("Bad header"); }
        },
        IMPORT_ERROR {
            public boolean ok() { return false; }
            public String msg() { return _x("Internal error"); }
        },
        IMPORT_UNREADABLE {
            public boolean ok() { return false; }
            public String msg() { return _x("Unreadable"); }
        },
    }

    /** tag for translation */
    private static String _x(String s) {
        return s;
    }
}
