package syndie.db;

import java.io.*;
import net.i2p.I2PAppContext;
import syndie.data.EnclosureBody;

/**
 *
 */
class UnreadableEnclosureBody extends EnclosureBody {
    public UnreadableEnclosureBody(I2PAppContext ctx) { super(ctx); }
    public String toString() { return "Unreadable enclosureBody"; }
}
