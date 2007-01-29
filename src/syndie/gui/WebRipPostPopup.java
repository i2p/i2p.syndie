package syndie.gui;

import java.util.Collections;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import syndie.Constants;
import syndie.data.SyndieURI;
import syndie.data.WebRipRunner;
import syndie.db.DBClient;
import syndie.db.JobRunner;
import syndie.db.UI;

/**
 *
 */
public class WebRipPostPopup implements Themeable, Translatable, WebRipPageControl.RipControlListener {
    private BrowserControl _browser;
    private Shell _parent;
    private Shell _shell;
    private WebRipPostControl _rip;
    
    public WebRipPostPopup(BrowserControl browser, Shell parent) {
        _browser = browser;
        _parent = parent;
        initComponents();
    }
    
    public void open() {
        _shell.pack();
        _shell.open();
    }
    
    private void initComponents() {
        _shell = new Shell(_parent, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL);
        _shell.setLayout(new FillLayout());
        _rip = new WebRipPostControl(_browser, _shell);
        _rip.setExistingAttachments(0);
        _rip.setListener(this);
        
        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) { evt.doit = false; cancelRip(); }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });
        _browser.getTranslationRegistry().register(this);
        _browser.getThemeRegistry().register(this);
    }
    
    private void cancelRip() {
        dispose();
    }
    
    public void dispose() {
        _rip.dispose();
        _shell.dispose();
        _browser.getTranslationRegistry().unregister(this);
        _browser.getThemeRegistry().unregister(this);
    }
    
    public void applyTheme(Theme theme) {
        _shell.setFont(theme.SHELL_FONT);
    }
    
    private static final String T_SHELL = "syndie.gui.webrippostpopup.shell";
    public void translate(TranslationRegistry registry) {
        _shell.setText(registry.getText(T_SHELL, "New web rip"));
    }

    public void ripComplete(boolean successful, WebRipRunner runner) {
        if (successful) {
            postRip(runner);
        } else {
            if (runner != null) {
                List msgs = runner.getErrorMessages();
                final StringBuffer err = new StringBuffer();
                for (int i = 0; i < msgs.size(); i++)
                    err.append((String)msgs.get(i)).append("\n");

                _shell.getDisplay().asyncExec(new Runnable() {
                    public void run() {
                        MessageBox box = new MessageBox(_parent, SWT.ICON_ERROR | SWT.OK);
                        box.setMessage(_browser.getTranslationRegistry().getText(T_ERROR_MSG, "Web rip could not be created: " + err.toString()));
                        box.setText(_browser.getTranslationRegistry().getText(T_ERROR_TITLE, "Web rip failed"));
                        box.open();
                    }
                });
            } else {
                // aborted
            }
        }
        dispose();
    }
    
    private void postRip(WebRipRunner runner) {
        // read the attachments/rewritten html from the runner
        // pull the post options off the _rip
        // post accordingly
        final String html = runner.getRewrittenHTML();
        final List attachmentNames = runner.getAttachmentNames();
        final List attachmentTypes = runner.getAttachmentTypes();
        final List attachmentData = runner.getAttachmentData();
        if (attachmentData == null) {
            _browser.getUI().errorMessage("Internal error reading web rip attachments");
            return;
        }
        final Hash author = _rip.getAuthor();
        final Hash target = _rip.getTarget();
        final String tags = _rip.getTags();
        final int privacy = _rip.getPrivacy();
        final String passphrase;
        final String passphrasePrompt;
        if (privacy == WebRipPostControl.PRIV_PBE) {
            passphrase = _rip.getPassphrase();
            passphrasePrompt = _rip.getPassphrasePrompt();
        } else {
            passphrase = null;
            passphrasePrompt = null;
        }
        _browser.getUI().debugMessage("Post html from " + author + " to " + target + " (tags: " + tags + "), priv=" + privacy + " (pass: " + passphrase + "/" + passphrasePrompt + ")");
        JobRunner.instance().enqueue(new Runnable() {
            public void run() {
                post(html, attachmentNames, attachmentTypes, attachmentData, author, target, tags, privacy, passphrase, passphrasePrompt);
            }
        });
    }
    
    private static final String T_CREATE_MSG = "syndie.gui.webrippostpopup.create.msg";
    private static final String T_CREATE_TITLE = "syndie.gui.webrippostpopup.create.title";
    
    private static final String T_FAIL_MSG = "syndie.gui.webrippostpopup.fail.msg";
    private static final String T_FAIL_TITLE = "syndie.gui.webrippostpopup.fail.title";
    private static final String T_ERROR_MSG = "syndie.gui.webrippostpopup.err.msg";
    private static final String T_ERROR_TITLE = "syndie.gui.webrippostpopup.err.title";
    
    private void post(final String html, final List attachmentNames, final List attachmentTypes, final List attachmentData, final Hash author, final Hash target, final String tags, final int privacy, final String passphrase, final String passphrasePrompt) {
        MessageCreator creator = new MessageCreator(new MessageCreator.MessageCreatorSource() {
            public BrowserControl getBrowser() { return _browser; }
            public DBClient getClient() { return _browser.getClient(); }
            public UI getUI() { return _browser.getUI(); }
            public Hash getAuthor() { return author; }
            public Hash getTarget() { return target; }
            public Hash getSignAs() { return null; }
            public boolean getAuthorHidden() { return false; }
            public int getPageCount() { return 1; }
            public String getPageContent(int page) { return html; }
            public String getPageType(int page) { return "text/html"; }
            public List getAttachmentNames() { return attachmentNames; }
            public List getAttachmentTypes() { return attachmentTypes; }
            public byte[] getAttachmentData(int attachmentIndex) { return (byte[])attachmentData.get(attachmentIndex-1); }
            public String getSubject() { return "web rip"; } // not translated for anon reasons
            public boolean getPrivacyPBE() { return privacy == WebRipPostControl.PRIV_PBE; }
            public String getPassphrase() { return passphrase; }
            public String getPassphrasePrompt() { return passphrasePrompt; }
            public boolean getPrivacyPublic() { return privacy == WebRipPostControl.PRIV_PUBLIC; }
            public String getAvatarUnmodifiedFilename() { return null; }
            public byte[] getAvatarModifiedData() { return null; }
            public boolean getPrivacyReply() { return false; }
            public String[] getPublicTags() { return new String[0]; }
            public String[] getPrivateTags() { return Constants.split(" ,\t\n\r", tags, false); }
            public List getReferenceNodes() { return Collections.EMPTY_LIST; }
            public int getParentCount() { return 0; }
            public SyndieURI getParent(int depth) { return null; }
            public String getExpiration() { return null; }
            public boolean getForceNewThread() { return false; }
            public boolean getRefuseReplies() { return false; }
        });
        boolean ok = creator.execute();
        if (ok) {
            final SyndieURI uri = creator.getCreatedURI();
            _parent.getDisplay().asyncExec(new Runnable() { 
                public void run() {
                    MessageBox box = new MessageBox(_parent, SWT.ICON_INFORMATION | SWT.OK);
                    box.setMessage(_browser.getTranslationRegistry().getText(T_CREATE_MSG, "Web rip created - please syndicate it to others if you want to share"));
                    box.setText(_browser.getTranslationRegistry().getText(T_CREATE_TITLE, "Web rip created"));
                    box.open();
                    _browser.view(uri);
                }
            });
        } else {
            final String errors = creator.getErrors();
            _parent.getDisplay().asyncExec(new Runnable() { 
                public void run() {
                    MessageBox box = new MessageBox(_parent, SWT.ICON_ERROR | SWT.OK);
                    box.setMessage(_browser.getTranslationRegistry().getText(T_CREATE_MSG, "Web rip failed: ") + errors);
                    box.setText(_browser.getTranslationRegistry().getText(T_CREATE_TITLE, "Web rip failed"));
                    box.open();
                }
            });
        }
    }
}
