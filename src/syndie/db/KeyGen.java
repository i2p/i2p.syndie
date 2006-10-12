package syndie.db;

import java.util.*;
import net.i2p.I2PAppContext;
import net.i2p.data.*;
import syndie.Constants;

/**
 *CLI keygen
 * --type (signing|encryption|post)
 * [--scope $base64(channelHash)]
 * (--pubOut $pubKeyFile --privOut $privKeyFile | --sessionOut $sessionKeyFile)
 */
public class KeyGen extends CommandImpl {
    KeyGen() {}
    public DBClient runCommand(Opts args, UI ui, DBClient client) {
        List missing = args.requireOpts(new String[] { "type" });
        if (missing.size() > 0) {
            ui.errorMessage("Invalid options, missing " + missing);
            ui.commandComplete(-1, null);
            return client;
        }
        
        String type = args.getOptValue("type");
        String scopeStr = args.getOptValue("scope");
        String pubOut = args.getOptValue("pubOut");
        String privOut = args.getOptValue("privOut");
        String sessOut = args.getOptValue("sessionOut");
        
        Hash scope = null;
        if (scopeStr != null) {
            byte b[] = Base64.decode(scopeStr);
            if ( (b != null) && (b.length == Hash.HASH_LENGTH) )
                scope = new Hash(b);
        }
        
        if (Constants.KEY_FUNCTION_MANAGE.equals(type) || // DSA
            Constants.KEY_FUNCTION_POST.equals(type) ||   // DSA
            Constants.KEY_FUNCTION_REPLY.equals(type)) {  // ElGamal
            if ( (privOut == null) || (pubOut == null) ||
                 (privOut.length() <= 0) || (pubOut.length() <= 0) ) {
                ui.errorMessage("pubOut and privOut are required for asymmetric key types");
                ui.commandComplete(-1, null);
                return client;
            } else {
                if (Constants.KEY_FUNCTION_REPLY.equals(type)) { // ElGamal
                    Object keys[] = I2PAppContext.getGlobalContext().keyGenerator().generatePKIKeypair();
                    PublicKey pub = (PublicKey)keys[0];
                    PrivateKey priv = (PrivateKey)keys[1];
                    writeKey(ui, privOut, type, scope, priv.toBase64());
                    writeKey(ui, pubOut, type + "-pub", scope, pub.toBase64());
                } else { // DSA
                    Object keys[] = I2PAppContext.getGlobalContext().keyGenerator().generateSigningKeypair();
                    SigningPublicKey pub = (SigningPublicKey)keys[0];
                    SigningPrivateKey priv = (SigningPrivateKey)keys[1];
                    writeKey(ui, privOut, type, scope, priv.toBase64());
                    writeKey(ui, pubOut, type + "-pub", scope, pub.toBase64());
                }
            }
        } else if (Constants.KEY_FUNCTION_READ.equals(type)) { // AES
            if ( (sessOut == null) || (sessOut.length() <= 0) ) {
                ui.errorMessage("sessionOut is required for symetric key types");
                ui.commandComplete(-1, null);
                return client;
            } else {
                SessionKey key = I2PAppContext.getGlobalContext().keyGenerator().generateSessionKey();
                writeKey(ui, sessOut, type, scope, key.toBase64());
            }
        } else {
            ui.errorMessage("key type not known");
            ui.commandComplete(-1, null);
            return client;
        }
        ui.commandComplete(0, null);
        return client;
    }
}
