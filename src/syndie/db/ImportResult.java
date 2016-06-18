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
        /** Message had cancel requests only, processed and deleted */
        IMPORT_CANCEL_STUB {
            public boolean ok() { return true; }
            public String msg() { return _x("Cancel requests processed"); }
        },
        /** generic read or reply, or no password */
        IMPORT_UNREADABLE {
            public boolean ok() { return true; }
            public String msg() { return _x("Unreadable"); }
        },
        IMPORT_NO_READ_KEY {
            public boolean ok() { return true; }
            public String msg() { return _x("Read key unknown"); }
        },
        IMPORT_NO_REPLY_KEY {
            public boolean ok() { return true; }
            public String msg() { return _x("Reply key unknown"); }
        },
        IMPORT_PASS_REQD {
            public boolean ok() { return true; }
            public String msg() { return _x("Passphrase required"); }
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
        IMPORT_NO_AUTH {
            public boolean ok() { return false; }
            public String msg() { return _x("Not authorized"); }
        },
        IMPORT_BAD_META_VERIFY {
            public boolean ok() { return false; }
            public String msg() { return _x("Meta does not verify"); }
        },
        IMPORT_FETCH_FAIL {
            public boolean ok() { return false; }
            public String msg() { return _x("Unable to fetch"); }
        },
        IMPORT_BAD_FREENET_URL {
            public boolean ok() { return false; }
            public String msg() { return _x("Invalid Freenet archive URL"); }
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
        /** @since 1.106b-2 */
        IMPORT_INTERRUPTED {
            public boolean ok() { return false; }
            public String msg() { return _x("Interrupted"); }
        },
    }

    /** tag for translation */
    private static String _x(String s) {
        return s;
    }
}
