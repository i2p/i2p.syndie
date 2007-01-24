package syndie.gui;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.SessionKey;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import syndie.Constants;
import syndie.data.NymKey;
import syndie.data.SyndieURI;
import syndie.db.CommandImpl;
import syndie.db.Importer;
import syndie.db.KeyImport;

/**
 *
 */
public class BackupSecrets implements Themeable, Translatable {
    private Composite _parent;
    private BrowserControl _browser;
    private SyndieURI _uri;
    private Composite _root;
    private Tree _tree;
    private Map _itemToNymKey;
    private Button _includeMeta;
    private Button _includeExpiredKeys;
    private Button _passphraseRequired;
    private Text _passphrase;
    private Button _ok;
    private Button _cancel;
    
    public BackupSecrets(BrowserControl browser, Composite parent, SyndieURI uri) {
        _parent = parent;
        _browser = browser;
        _uri = uri;
        initComponents();
    }
    
    private void initComponents() {
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new GridLayout(2, true));
        
        _tree = new Tree(_root, SWT.MULTI | SWT.BORDER | SWT.CHECK | SWT.V_SCROLL);
        _tree.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));
        _tree.setLinesVisible(true);
        _itemToNymKey = new HashMap();
        SyndieTreeListener lsnr = new SyndieTreeListener(_tree) {
            public void doubleclick() { toggle(); }
            public void returnHit() { toggle(); }
            private void toggle() {
                TreeItem sel[] = _tree.getSelection();
                if (sel != null) {
                    for (int i = 0; i < sel.length; i++) {
                        boolean checked = !sel[i].getChecked();
                        sel[i].setChecked(checked);
                        if (sel[i].getItemCount() > 0) {
                            TreeItem children[] = sel[i].getItems();
                            for (int j = 0; j < children.length; j++)
                                children[j].setChecked(checked);
                        }
                    }
                }
            }
            public boolean collapseOnReturn() { return false; }
        };
        _tree.addKeyListener(lsnr);
        _tree.addMouseListener(lsnr);
        _tree.addSelectionListener(lsnr);
        _tree.addTraverseListener(lsnr);
        
        _includeMeta = new Button(_root, SWT.CHECK);
        _includeMeta.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _includeExpiredKeys = new Button(_root, SWT.CHECK);
        _includeExpiredKeys.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 2, 1));
        
        _passphraseRequired = new Button(_root, SWT.CHECK);
        _passphraseRequired.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _passphrase = new Text(_root, SWT.BORDER | SWT.SINGLE);
        _passphrase.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _ok = new Button(_root, SWT.PUSH);
        _ok.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        _ok.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { backupSelection(); }
            public void widgetSelected(SelectionEvent selectionEvent) { backupSelection(); }
        });
        _cancel = new Button(_root, SWT.PUSH);
        _cancel.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
        _cancel.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { _browser.unview(_uri); }
            public void widgetSelected(SelectionEvent selectionEvent) { _browser.unview(_uri); }
        });
        
        populateFields();
        
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    private void backupSelection() {
        String pass = _passphrase.getText().trim();
        if (_passphraseRequired.getSelection()) {
            if (pass.length() <= 0) {
                MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
                box.setMessage(_browser.getTranslationRegistry().getText(T_PASS_REQUIRED_MSG, "A blank passphrase is not allowed - if you don't want to protect your secret keys, please uncheck the passphrase checkbox"));
                box.setText(_browser.getTranslationRegistry().getText(T_PASS_REQUIRED_TITLE, "Passphrase required"));
                box.open();
                return;
            }
        } else {
            pass = null;
        }
        
        boolean includeExpired = _includeExpiredKeys.getSelection();
        boolean includeMeta = _includeMeta.getSelection();
        
        ArrayList nymKeys = new ArrayList();
        ArrayList chanMeta = new ArrayList();
        TreeItem chans[] = _tree.getItems();
        for (int i = 0; i < chans.length; i++) {
            boolean keysIncluded = false;
            Hash chan = null;
            TreeItem keyItems[] = chans[i].getItems();
            for (int j = 0; j < keyItems.length; j++) {
                if (keyItems[j].getChecked()) {
                    NymKey key = (NymKey)_itemToNymKey.get(keyItems[j]);
                    if (includeExpired || !key.getIsExpired()) {
                        nymKeys.add(key);
                        chan = key.getChannel();
                    }
                }
            }
            if (includeMeta && (chan != null))
                chanMeta.add(chan);
        }
        
        if (nymKeys.size() > 0) {
            FileDialog dialog = new FileDialog(_root.getShell(), SWT.SAVE | SWT.SINGLE);
            dialog.setFileName("nymkeys.dat");
            dialog.setText(_browser.getTranslationRegistry().getText(T_LOCATION, "File to write the backup to"));
            String filename = dialog.open();
            if (filename != null) {
                String err = backup(nymKeys, chanMeta, pass, filename);
                if (err != null) {
                    MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_ERROR | SWT.OK);
                    box.setMessage(_browser.getTranslationRegistry().getText(T_ERROR_MSG, "There was an error backing up the keys: ") + err);
                    box.setText(_browser.getTranslationRegistry().getText(T_ERROR_TITLE, "Error"));
                    box.open();
                } else {
                    _browser.unview(_uri);
                    MessageBox box = new MessageBox(_root.getShell(), SWT.ICON_INFORMATION | SWT.OK);
                    box.setMessage(_browser.getTranslationRegistry().getText(T_OK_MSG, "The keys were backed up to: ") + filename);
                    box.setText(_browser.getTranslationRegistry().getText(T_OK_TITLE, "Backup successful"));
                    box.open();
                }
            }
        }
    }
    private static final String T_PASS_REQUIRED_MSG = "syndie.gui.backupsecrets.passrequired.msg";
    private static final String T_PASS_REQUIRED_TITLE = "syndie.gui.backupsecrets.passrequired.title";
    private static final String T_LOCATION = "syndie.gui.backupsecrets.location";
    private static final String T_ERROR_MSG= "syndie.gui.backupsecrets.error.msg";
    private static final String T_ERROR_TITLE = "syndie.gui.backupsecrets.error.title";
    private static final String T_OK_MSG = "syndie.gui.backupsecrets.ok.msg";
    private static final String T_OK_TITLE = "syndie.gui.backupsecrets.ok.title";
    
    /**
     * zip containing
     *  nymkey$n.syndie
     *  meta$n.syndie
     * if there is a passphrase, that zip is AES-256 encrypted with a passphrase 
     * derived session key and an explicit 16 byte IV prepended to it
     */
    private String backup(List nymKeys, List chanHashes, String pass, String filename) {
        OutputStream out = null;
        File target = new File(filename);
        target.getParentFile().mkdirs();
        try {
            if (pass == null) {
                out = new FileOutputStream(target);
            } else {
                out = new ByteArrayOutputStream(16*1024);
            }
            
            backup(nymKeys, chanHashes, out);
            
            if (pass != null) {
                byte data[] = ((ByteArrayOutputStream)out).toByteArray();
                byte salt[] = new byte[16];
                byte encrypted[] = _browser.getClient().pbeEncrypt(data, pass, salt);
                FileOutputStream fos = new FileOutputStream(target);
                fos.write(salt);
                fos.write(encrypted);
                fos.close();
            } else {
                out.close();
            }
            out = null;
            
            return null;
        } catch (IOException ioe) {
            target.delete();
            return ioe.getMessage();
        } finally {
            if (out != null) try { out.close(); } catch (IOException ioe) {}
        }
    }
    
    private void backup(List nymKeys, List chanHashes, OutputStream out) throws IOException {
        ZipOutputStream zos = new ZipOutputStream(out);
        for (int i = 0; i < chanHashes.size(); i++) {
            Hash chan = (Hash)chanHashes.get(i);
            File src = new File(new File(_browser.getClient().getArchiveDir(), chan.toBase64()), "meta" + Constants.FILENAME_SUFFIX);
            if (!src.exists())
                continue;
            ZipEntry entry = new ZipEntry("meta" + i + ".syndie");
            entry.setTime(0);
            entry.setSize((int)src.length());
            zos.putNextEntry(entry);
            byte buf[] = new byte[4096];
            int read = -1;
            FileInputStream fin = null;
            try {
                fin = new FileInputStream(src);
                while ( (read = fin.read(buf)) != -1)
                    zos.write(buf, 0, read);
                fin.close();
                fin = null;
            } finally {
                if (fin != null) fin.close();
            }
            zos.closeEntry();
        }
        for (int i = 0; i < nymKeys.size(); i++) {
            ZipEntry entry = new ZipEntry("nymkey" + i + ".syndie");
            entry.setTime(0);
            zos.putNextEntry(entry);
            NymKey key = (NymKey)nymKeys.get(i);
            CommandImpl.writeKey(zos, key.getFunction(), key.getChannel(), Base64.encode(key.getData()));
            zos.closeEntry();
        }
        zos.finish();
    }
    
    private static final byte ZIP_HEADER[] = new byte[] { 'P', 'K', 0x03, 0x04 };
    static void restore(final BrowserControl browser, final Shell parent, final File src) {
        final byte origData[] = new byte[(int)src.length()];
        FileInputStream fin = null;
        try {
            fin = new FileInputStream(src);
            int read = DataHelper.read(fin, origData);
            if (read != origData.length)
                throw new EOFException();
        } catch (IOException ioe) {
            browser.getUI().errorMessage("Error importing " + src.getPath(), ioe);
            fail(browser, parent, ioe.getMessage());
        }
        if (origData.length <= ZIP_HEADER.length) {
            MessageBox box = new MessageBox(parent, SWT.ICON_ERROR | SWT.OK);
            box.setMessage(browser.getTranslationRegistry().getText(T_READ_SHORT, "The secrets file was too short"));
            box.setText(browser.getTranslationRegistry().getText(T_READ_SHORT_TITLE, "Error reading"));
            box.open();
            return;
        }
        
        restore(browser, parent, src, origData, origData);
    }
    private static void restore(final BrowserControl browser, final Shell parent, final File str, final byte origData[], final byte decrypted[]) {
        if (!DataHelper.eq(ZIP_HEADER, 0, decrypted, 0, ZIP_HEADER.length)) {
            PassphrasePrompt prompt = new PassphrasePrompt(browser, parent, false);
            prompt.setPassphraseListener(new PassphrasePrompt.PassphraseListener() {
                public void promptComplete(String passphraseEntered, String promptEntered) {
                    if (passphraseEntered.length() > 0) {
                        byte decr[] = browser.getClient().pbeDecrypt(origData, 16, origData, 0, passphraseEntered, origData.length-16);
                        restore(browser, parent, str, origData, decr);
                    } else {
                        restore(browser, parent, str, origData, origData);
                    }
                }
                public void promptAborted() {}
            });
            prompt.open();
        } else {
            browser.getUI().debugMessage("decryption ok");
            // decryption ok
            ZipInputStream zin = null;    
            try {
                zin = new ZipInputStream(new ByteArrayInputStream(decrypted));
                ZipEntry entry = null;
                byte buf[] = new byte[4096];
                int keysRead = 0;
                int metaRead = 0;
                int failedMeta = 0;
                while ( (entry = zin.getNextEntry()) != null) {
                    String name = entry.getName();
                    if (name.startsWith("nymkey")) {
                        browser.getUI().debugMessage("importing key "+ name);
                        KeyImport imp = new KeyImport();
                        imp.importKey(browser.getUI(), browser.getClient(), zin, true, false);
                        keysRead++;
                    } else if (name.startsWith("meta")) {
                        browser.getUI().debugMessage("importing meta "+ name);
                        Importer imp = new Importer(browser.getClient());
                        boolean ok = imp.processMessage(browser.getUI(), zin, browser.getClient().getLoggedInNymId(), null, null, false);
                        browser.getUI().debugMessage("import meta result: " + ok + " missingKey? " + imp.wasMissingKey() + " pbe? " + imp.wasPBE());
                        if (ok && !imp.wasMissingKey() && !imp.wasPBE())
                            metaRead++;
                        else
                            failedMeta++;
                    }
                }
                zin.close();
                zin = null;
                
                MessageBox box = new MessageBox(parent, SWT.ICON_INFORMATION | SWT.OK);
                box.setMessage(browser.getTranslationRegistry().getText(T_RESTORED, "Restored keys/meta/corrupt meta: ") + keysRead + "/" + metaRead + "/" + failedMeta);
                box.setText(browser.getTranslationRegistry().getText(T_RESTORED_TITLE, "Restored"));
                box.open();
            } catch (IOException ioe) {
                fail(browser, parent, ioe.getMessage());
                return;
            } finally {
                if (zin != null) try { zin.close(); } catch (IOException ioe) {}
            }
        }
    }
    private static final String T_READ_SHORT = "syndie.gui.backupsecrets.readshort";
    private static final String T_READ_SHORT_TITLE = "syndie.gui.backupsecrets.readshort.title";
    private static final String T_RESTORED = "syndie.gui.backupsecrets.restored";
    private static final String T_RESTORED_TITLE = "syndie.gui.backupsecrets.restored.title";
    
    private static void fail(BrowserControl browser, Shell parent, String err) {
        MessageBox box = new MessageBox(parent, SWT.ICON_ERROR | SWT.OK);
        box.setMessage(browser.getTranslationRegistry().getText(T_READ_ERR, "The secrets file was corrupt: ") + err);
        box.setText(browser.getTranslationRegistry().getText(T_READ_ERR_TITLE, "Error reading"));
        box.open();
    }
    private static final String T_READ_ERR = "syndie.gui.backupsecrets.readerr";
    private static final String T_READ_ERR_TITLE = "syndie.gui.backupsecrets.readerr.title";
    
    private void populateFields() {
        _includeExpiredKeys.setSelection(true);
        _includeMeta.setSelection(true);
        _passphraseRequired.setSelection(true);
        
        Map chanToKeys = new HashMap();
        java.util.List keys = _browser.getClient().getNymKeys(null, null);
        for (int i = 0; i < keys.size(); i++) {
            NymKey key = (NymKey)keys.get(i);
            Hash chan = key.getChannel();
            Set chanKeys = (Set)chanToKeys.get(chan);
            if (chanKeys == null) {
                chanKeys = new HashSet();
                chanToKeys.put(chan, chanKeys);
            }
            chanKeys.add(key);
        }
        java.util.List channelKeys = _browser.getClient().getPrivateChannelReadKeys();
        for (int i = 0; i < channelKeys.size(); i++) {
            NymKey key = (NymKey)channelKeys.get(i);
            Hash chan = key.getChannel();
            Set chanKeys = (Set)chanToKeys.get(chan);
            if (chanKeys == null) {
                chanKeys = new HashSet();
                chanToKeys.put(chan, chanKeys);
            }
            chanKeys.add(key);
        }
        
        for (Iterator iter = chanToKeys.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            Hash chan = (Hash)entry.getKey();
            // do all manage keys first, then all reply, then all post, then all read keys
            Set chanKeys = new TreeSet(KEY_COMPARATOR);
            chanKeys.addAll((Set)entry.getValue());
            
            String chanName = _browser.getClient().getChannelName(chan);
            if (chanName == null)
                chanName = "";
            
            TreeItem chanItem = new TreeItem(_tree, SWT.NONE);
            chanItem.setChecked(true);
            chanItem.setText(chan.toBase64().substring(0,6) + ": " + chanName + " [" + chanKeys.size() + "]");
            
            for (Iterator kiter = chanKeys.iterator(); kiter.hasNext(); ) {
                NymKey key = (NymKey)kiter.next();
                TreeItem item = new TreeItem(chanItem, SWT.NONE);
                item.setChecked(true);
                String str = null;
                if (Constants.KEY_FUNCTION_MANAGE.equals(key.getFunction()))
                    str = _browser.getTranslationRegistry().getText(T_TYPE_MANAGE, "Forum management key");
                else if (Constants.KEY_FUNCTION_REPLY.equals(key.getFunction()))
                    str = _browser.getTranslationRegistry().getText(T_TYPE_REPLY, "Forum reply key");
                else if (Constants.KEY_FUNCTION_POST.equals(key.getFunction()))
                    str = _browser.getTranslationRegistry().getText(T_TYPE_POST, "Forum post key");
                else if (Constants.KEY_FUNCTION_READ.equals(key.getFunction()))
                    str = _browser.getTranslationRegistry().getText(T_TYPE_READ, "Forum read key");
                
                if (key.getIsExpired())
                    str = str + " [" + _browser.getTranslationRegistry().getText(T_KEY_EXPIRED, "expired") + "]";
                
                str = str + " (" + _browser.getClient().sha256(key.getData()).toBase64().substring(0,12) + ")";
                item.setText(str);
                
                _itemToNymKey.put(item, key);
            }
            
        }
    }

    private static final String T_KEY_EXPIRED = "syndie.gui.backupsecrets.key.expired";
    private static final String T_TYPE_MANAGE = "syndie.gui.backupsecrets.type.manage";
    private static final String T_TYPE_REPLY = "syndie.gui.backupsecrets.type.reply";
    private static final String T_TYPE_POST = "syndie.gui.backupsecrets.type.post";
    private static final String T_TYPE_READ = "syndie.gui.backupsecrets.type.read";
    
    // do all manage keys first, then all reply, then all post, then all read keys
    private static final Comparator KEY_COMPARATOR = new Comparator() {
        public int compare(Object o1, Object o2) {
            NymKey lhs = (NymKey)o1;
            NymKey rhs = (NymKey)o2;
            
            int lhsType = getType(lhs.getFunction());
            int rhsType = getType(rhs.getFunction());
            if (lhsType < rhsType) return -1;
            else if (lhsType == rhsType) return DataHelper.compareTo(lhs.getData(), rhs.getData());
            else return 1;
        }
        private final int getType(String function) {
            if (Constants.KEY_FUNCTION_MANAGE.equals(function))
                return 0;
            else if (Constants.KEY_FUNCTION_REPLY.equals(function))
                return 1;
            else if (Constants.KEY_FUNCTION_POST.equals(function))
                return 2;
            else if (Constants.KEY_FUNCTION_READ.equals(function))
                return 3;
            else
                return 4;
        }
    };
    
    public void dispose() {
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
    }
    
    public void applyTheme(Theme theme) {
        _tree.setFont(theme.TREE_FONT);
        _includeMeta.setFont(theme.DEFAULT_FONT);
        _includeExpiredKeys.setFont(theme.DEFAULT_FONT);
        _passphraseRequired.setFont(theme.DEFAULT_FONT);
        _passphrase.setFont(theme.DEFAULT_FONT);
        _ok.setFont(theme.BUTTON_FONT);
        _cancel.setFont(theme.BUTTON_FONT);
    }
    
    private static final String T_META = "syndie.gui.backupsecrets.meta";
    private static final String T_EXPIRED = "syndie.gui.backupsecrets.expired";
    private static final String T_PASSPHRASE = "syndie.gui.backupsecrets.passphrase";
    private static final String T_OK = "syndie.gui.backupsecrets.ok";
    private static final String T_CANCEL = "syndie.gui.backupsecrets.cancel";
    
    public void translate(TranslationRegistry registry) {
        _includeMeta.setText(registry.getText(T_META, "Backup related forum profile?"));
        _includeExpiredKeys.setText(registry.getText(T_EXPIRED, "Backup expired keys?"));
        _passphraseRequired.setText(registry.getText(T_PASSPHRASE, "Passphrase required to restore:"));
        _ok.setText(registry.getText(T_OK, "Ok"));
        _cancel.setText(registry.getText(T_CANCEL, "Cancel"));
    }
}
