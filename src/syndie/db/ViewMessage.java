package syndie.db;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.sql.SQLException;
import java.util.*;

import net.i2p.I2PAppContext;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.util.SecureFile;
import net.i2p.util.SecureFileOutputStream;

import syndie.data.ChannelInfo;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;

/**
 *CLI viewmessage
 * --db $url
 * --login $login
 * --pass $pass
 * --internalid $internalMessageId
 * --out $outputDirectory
 */
public class ViewMessage extends CommandImpl {
    ViewMessage() {}
    public DBClient runCommand(Opts args, UI ui, DBClient client) {
        if ( (client == null) || (!client.isLoggedIn()) ) {
            List missing = args.requireOpts(new String[] { "db", "login", "pass", "internalid", "out" });
            if (missing.size() > 0) {
                ui.errorMessage("Invalid options, missing " + missing);
                ui.commandComplete(-1, null);
                return client;
            }
        } else {
            List missing = args.requireOpts(new String[] { "internalid", "out" });
            if (missing.size() > 0) {
                ui.errorMessage("Invalid options, missing " + missing);
                ui.commandComplete(-1, null);
                return client;
            }
        }
        
        try {
            long nymId = -1;
            if (args.dbOptsSpecified()) {
                if (client == null)
                    client = new DBClient(I2PAppContext.getGlobalContext(), new File(TextEngine.getRootPath()));
                else
                    client.close();
                nymId = client.connect(args.getOptValue("db"), args.getOptValue("login"), args.getOptValue("pass"));
                if (nymId < 0) {
                    ui.errorMessage("Login incorrect");
                    ui.commandComplete(-1, null);
                    return client;
                }
            } else {
                nymId = client.getLoggedInNymId();
                if (nymId < 0) {
                    ui.errorMessage("Not logged in");
                    ui.commandComplete(-1, null);
                    return client;
                }
            }
            long id = args.getOptLong("internalid", -1);
            if (id < 0) {
                ui.errorMessage("Message ID is invalid");
                ui.commandComplete(-1, null);
            } else {
                MessageInfo info = client.getMessage(id);
                if (info == null) {
                    ui.errorMessage("Message ID is not known");
                    ui.commandComplete(-1, null);
                } else {
                    extractMessage(client, ui, info, args.getOptValue("out"));
                }
            }
        } catch (SQLException se) {
            ui.errorMessage("Invalid database URL", se);
            ui.commandComplete(-1, null);
        //} finally {
        //    if (client != null) client.close();
        }
        return client;
    }
    
    private void extractMessage(DBClient client, UI ui, MessageInfo info, String outDir) {
        try {
            File dir = new SecureFile(outDir);
            if (dir.exists()) {
                ui.errorMessage("Output directory already exists.  Aborting");
                ui.commandComplete(-1, null);
                return;
            }
            dir.mkdirs();
            
            File statusFile = new File(outDir, "status.txt");
            FileOutputStream fos = null;
            try {
                fos = new SecureFileOutputStream(statusFile);
                fos.write(DataHelper.getUTF8(info.toString()));
                fos.close();
                fos = null;
            } finally {
                if (fos != null) try { fos.close(); } catch (IOException ioe) {}
            }
            
            // now extract the pages and attachments
            for (int i = 0; i < info.getPageCount(); i++) {
                String data = client.getMessagePageData(info.getInternalId(), i);
                if (data != null) {
                    try {
                        fos = new SecureFileOutputStream(new File(dir, "page" + i + ".dat"));
                        fos.write(DataHelper.getUTF8(data));
                        fos.close();
                        fos = null;
                    } finally {
                        if (fos != null) try { fos.close(); } catch (IOException ioe) {}
                    }
                }
                
                String cfg = client.getMessagePageConfig(info.getInternalId(), i);
                if (cfg != null) {
                    try {
                        fos = new SecureFileOutputStream(new File(dir, "page" + i + ".cfg"));
                        fos.write(DataHelper.getUTF8(cfg));
                        fos.close();
                        fos = null;
                    } finally {
                        if (fos != null) try { fos.close(); } catch (IOException ioe) {}
                    }
                }
            }
            for (int i = 0; i < info.getAttachmentCount(); i++) {
                byte data[] = client.getMessageAttachmentData(info.getInternalId(), i);
                if (data != null) {
                    try {
                        fos = new SecureFileOutputStream(new File(dir, "attachment" + i + ".dat"));
                        fos.write(data);
                        fos.close();
                        fos = null;
                    } finally {
                        if (fos != null) try { fos.close(); } catch (IOException ioe) {}
                    }
                }
                
                String cfg = client.getMessageAttachmentConfigRaw(info.getInternalId(), i);
                if (cfg != null) {
                    try {
                        fos = new SecureFileOutputStream(new File(dir, "attachment" + i + ".cfg"));
                        fos.write(DataHelper.getUTF8(cfg));
                        fos.close();
                        fos = null;
                    } finally {
                        if (fos != null) try { fos.close(); } catch (IOException ioe) {}
                    }
                }
            }
            
            List refs = info.getReferences();
            if (refs.size() > 0) {
                String refStr = ReferenceNode.walk(refs);
                try {
                    fos = new SecureFileOutputStream(new File(dir, "references.cfg"));
                    fos.write(DataHelper.getUTF8(refStr));
                    fos.close();
                    fos = null;
                } finally {
                    if (fos != null) try { fos.close(); } catch (IOException ioe) {}
                }
            }
            
            ui.statusMessage("Message extracted to " + dir.getAbsolutePath());
            ui.commandComplete(0, null);
        } catch (IOException ioe) {
            ui.errorMessage("Error viewing", ioe);
            ui.commandComplete(-1, null);
        }
    }
    
    public static void main(String args[]) {
        try {
        CLI.main(new String[] { "viewmessage", 
                                "--db", "jdbc:hsqldb:file:/tmp/cli",
                                "--login", "j",
                                "--pass", "j",
                                "--internalid", "0",
                                "--out", "/tmp/msgOut" });
        } catch (Exception e) { e.printStackTrace(); }
    }
}
