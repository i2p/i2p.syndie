package syndie.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.i2p.data.Hash;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;

/**
 * collection of icons relevent for a particular message
 */
public class MessageFlagBar implements Translatable {
    private BrowserControl _browser;
    private Composite _parent;
    private Composite _root;
    private MessageInfo _msg;
    private boolean _includeTooltips;
    private List _images;
    private boolean _realized;
    
    public MessageFlagBar(BrowserControl browser, Composite parent, boolean includeTooltips) {
        _browser = browser;
        _parent = parent;
        _includeTooltips = includeTooltips;
        _images = new ArrayList();
        _realized = false;
        initComponents();
    }
    
    public Control getControl() {
        return _root;
    }
    public Image[] getFlags() {
        return (Image[])_images.toArray(new Image[0]);
        /*
        Control ctl[] = _root.getChildren();
        Image rv[] = new Image[ctl.length];
        for (int i = 0; i < ctl.length; i++)
            rv[i] = ((Label)ctl[i]).getImage();
        return rv;
         */
    }
    public String getTooltip() {
        if (!_realized) realizeComponents();
        Control ctl[] = _root.getChildren();
        StringBuffer rv = new StringBuffer();
        for (int i = 0; i < ctl.length; i++) {
            String tt = ((Label)ctl[i]).getToolTipText();
            rv.append(tt);
            if (i + 1 < ctl.length)
                rv.append(" ");
        }
        return rv.toString();
    }
    
    public void setMessage(MessageInfo msg) { 
        _msg = msg;
        rebuildFlags();
    }
    
    private void rebuildFlags() {
        if (_realized) {
            _browser.getUI().debugMessage("rebuilding flags for " + (_msg != null ? _msg.getURI().toString() : "no message"));
            _root.setRedraw(false);
            disposeImages();
            Control ctl[] = _root.getChildren();
            for (int i = 0; i < ctl.length; i++) {
                if (!ctl[i].isDisposed())
                    ctl[i].dispose();
            }
            buildImages();
            for (int i = 0; i < _images.size(); i++) {
                Label l = new Label(_root, SWT.NONE);
                l.setImage((Image)_images.get(i));
            }
            translate(_browser.getTranslationRegistry());
            _root.layout(true, true);
            _root.setRedraw(true);
        } else {
            _browser.getUI().debugMessage("rebuilding flags for " + (_msg != null ? _msg.getURI().toString() : "no message"));
            disposeImages();
            buildImages();
        }
    }
    private void disposeImages() {
        for (int i = 0; i < _images.size(); i++) {
            Image img = (Image)_images.get(i);
            ImageUtil.dispose(img);
        }
    }
    private void buildImages() {
        if (_msg == null) return;
        
        boolean unreadable = _msg.getPassphrasePrompt() != null || _msg.getReadKeyUnknown() || _msg.getReplyKeyUnknown();
        boolean isPrivate = _msg.getWasPrivate();
        
        _images.clear();
        
        if (unreadable) {
            buildUnreadableImages(isPrivate);
        } else {
            boolean wasPBE = _msg.getWasPassphraseProtected();
            boolean wasPublic = !_msg.getWasEncrypted();
            boolean authenticated = _msg.getWasAuthenticated();
            boolean authorized = _msg.getWasAuthorized();

            Hash author = null;
            Hash forum = _msg.getTargetChannel();
            if (_msg.getTargetChannelId() == _msg.getAuthorChannelId()) {
                author = forum;
            } else if (_msg.getScopeChannelId() == _msg.getAuthorChannelId()) {
                author = _msg.getScopeChannel();
            } else if (authenticated) {
                author = _browser.getClient().getChannelHash(_msg.getAuthorChannelId());
            }

            List bannedChannels = _browser.getClient().getBannedChannels();
            boolean banned = bannedChannels.contains(author) || bannedChannels.contains(forum);

            boolean bookmarked = _browser.isBookmarked(SyndieURI.createScope(forum));
            boolean scheduledForExpire = _msg.getExpiration() > 0;

            List refs = _msg.getReferences();
            if (refs == null)
                refs = Collections.EMPTY_LIST;
            
            boolean hasKeys = false;
            boolean hasArchives = false;
            boolean hasRefs = refs.size() > 0;
            boolean hasAttachments = _msg.getAttachmentCount() > 0;
            boolean isNew = _browser.getClient().getMessageStatus(_msg.getInternalId(), _msg.getTargetChannelId()) == DBClient.MSG_STATUS_NEW_UNREAD;
            
            for (int i = 0; i < refs.size(); i++) {
                if (hasArchives((ReferenceNode)refs.get(i))) {
                    hasArchives = true;
                    break;
                }
            }
            
            for (int i = 0; i < refs.size(); i++) {
                if (hasKeys((ReferenceNode)refs.get(i))) {
                    hasKeys = true;
                    break;
                }
            }
            
            if (wasPBE) _images.add(ImageUtil.ICON_MSG_FLAG_PBE);
            if (isPrivate) _images.add(ImageUtil.ICON_MSG_TYPE_PRIVATE);
            if (wasPublic) _images.add(ImageUtil.ICON_MSG_FLAG_PUBLIC);
            if (authenticated) _images.add(ImageUtil.ICON_MSG_FLAG_AUTHENTICATED);
            if (authorized) _images.add(ImageUtil.ICON_MSG_FLAG_AUTHORIZED);
            if (banned) _images.add(ImageUtil.ICON_MSG_FLAG_BANNED);
            if (bookmarked) _images.add(ImageUtil.ICON_MSG_FLAG_BOOKMARKED);
            if (scheduledForExpire) _images.add(ImageUtil.ICON_MSG_FLAG_SCHEDULEDFOREXPIRE);
            if (hasKeys) _images.add(ImageUtil.ICON_MSG_FLAG_HASKEYS);
            if (hasArchives) _images.add(ImageUtil.ICON_MSG_FLAG_HASARCHIVES);
            if (hasRefs) _images.add(ImageUtil.ICON_MSG_FLAG_HASREFS);
            if (hasAttachments) _images.add(ImageUtil.ICON_MSG_FLAG_HASATTACHMENTS);
            if (isNew) _images.add(ImageUtil.ICON_MSG_FLAG_ISNEW);
        }
        
        // reapply translation to pull the per-image tooltips
        translate(_browser.getTranslationRegistry());
    }
    
    private void realizeComponents() {
        if (_realized) return;
        _root = new Composite(_parent, SWT.NONE);
        _root.setLayout(new FillLayout(SWT.HORIZONTAL));
        _browser.getTranslationRegistry().register(this);
        _realized = true;
    }
    
    private boolean hasArchives(ReferenceNode node) {
        if (node == null) return false;
        SyndieURI uri = node.getURI();
        if (uri != null) {
            if (uri.isArchive())
                return true;
        }
        for (int i = 0; i < node.getChildCount(); i++) {
            if (hasArchives(node.getChild(i)))
                return true;
        }
        return false;
    }
    
    private boolean hasKeys(ReferenceNode node) {
        if (node == null) return false;
        SyndieURI uri = node.getURI();
        if (uri != null) {
            if ( (uri.getArchiveKey() != null) || (uri.getManageKey() != null) ||
                 (uri.getPostKey() != null) || (uri.getReadKey() != null) || (uri.getReplyKey() != null) )
                return true;
        }
        for (int i = 0; i < node.getChildCount(); i++) {
            if (hasKeys(node.getChild(i)))
                return true;
        }
        return false;
    }
    
    private void buildUnreadableImages(boolean isPrivate) {
        _images.add(ImageUtil.ICON_MSG_FLAG_UNREADABLE);
        if (isPrivate) _images.add(ImageUtil.ICON_MSG_TYPE_PRIVATE);
        if (_msg.getPassphrasePrompt() != null) _images.add(ImageUtil.ICON_MSG_FLAG_PBE);
        if (_msg.getReadKeyUnknown()) _images.add(ImageUtil.ICON_MSG_FLAG_READKEYUNKNOWN);
        if (_msg.getReplyKeyUnknown()) _images.add(ImageUtil.ICON_MSG_FLAG_REPLYKEYUNKNOWN);
    }
    
    private void initComponents() {
        if (_includeTooltips) 
            realizeComponents(); // if we want tooltips, we want a real gui display
    }
    
    public void dispose() {
        disposeImages();
        if (_realized)
            _browser.getTranslationRegistry().unregister(this);
    }
    
    private static final String T_PBE = "syndie.gui.messageflagbar.pbe";
    private static final String T_READKEYUNKNOWN = "syndie.gui.messageflagbar.readkeyunknown";
    private static final String T_REPLYKEYUNKNOWN = "syndie.gui.messageflagbar.replykeyunknown";
    private static final String T_UNREADABLE = "syndie.gui.messageflagbar.unreadable";
    private static final String T_PUBLIC = "syndie.gui.messageflagbar.pbe";
    private static final String T_AUTHENTICATED = "syndie.gui.messageflagbar.authenticated";
    private static final String T_AUTHORIZED = "syndie.gui.messageflagbar.authorized";
    private static final String T_BANNED = "syndie.gui.messageflagbar.banned";
    private static final String T_BOOKMARKED = "syndie.gui.messageflagbar.bookmarked";
    private static final String T_SCHEDULED = "syndie.gui.messageflagbar.scheduled";
    private static final String T_HASKEYS = "syndie.gui.messageflagbar.haskeys";
    private static final String T_HASARCHIVES = "syndie.gui.messageflagbar.hasarchives";
    private static final String T_HASREFS = "syndie.gui.messageflagbar.hasrefs";
    private static final String T_HASATTACHMENTS = "syndie.gui.messageflagbar.hasattachments";
    private static final String T_ISNEW = "syndie.gui.messageflagbar.isnew";
    private static final String T_ISPRIVATE = "syndie.gui.messageflagbar.isprivate";
    
    public void translate(TranslationRegistry registry) {
        if (!_includeTooltips) return;
        if (!_realized) return;
        Control ctl[] = _root.getChildren();
        for (int i = 0; i < ctl.length; i++) {
            Label l = (Label)ctl[i];
            if (!l.isDisposed()) {
                _browser.getUI().debugMessage("translating icon " + i);
                Image img = l.getImage();
                if (img == ImageUtil.ICON_MSG_FLAG_PBE)
                    l.setToolTipText(registry.getText(T_PBE, "Post is passphrase protected"));
                else if (img == ImageUtil.ICON_MSG_FLAG_READKEYUNKNOWN)
                    l.setToolTipText(registry.getText(T_READKEYUNKNOWN, "Read key unknown"));
                else if (img == ImageUtil.ICON_MSG_FLAG_REPLYKEYUNKNOWN)
                    l.setToolTipText(registry.getText(T_REPLYKEYUNKNOWN, "Private key unknown"));
                else if (img == ImageUtil.ICON_MSG_FLAG_UNREADABLE)
                    l.setToolTipText(registry.getText(T_UNREADABLE, "Message is not readable"));
                else if (img == ImageUtil.ICON_MSG_FLAG_PUBLIC)
                    l.setToolTipText(registry.getText(T_PUBLIC, "Message was publically readable"));
                else if (img == ImageUtil.ICON_MSG_FLAG_AUTHENTICATED)
                    l.setToolTipText(registry.getText(T_AUTHENTICATED, "Author is authentic"));
                else if (img == ImageUtil.ICON_MSG_FLAG_AUTHORIZED)
                    l.setToolTipText(registry.getText(T_AUTHORIZED, "Author is allowed to post in the forum"));
                else if (img == ImageUtil.ICON_MSG_FLAG_BANNED)
                    l.setToolTipText(registry.getText(T_BANNED, "Post is banned"));
                else if (img == ImageUtil.ICON_MSG_FLAG_BOOKMARKED)
                    l.setToolTipText(registry.getText(T_BOOKMARKED, "Author is bookmarked"));
                else if (img == ImageUtil.ICON_MSG_FLAG_SCHEDULEDFOREXPIRE)
                    l.setToolTipText(registry.getText(T_SCHEDULED, "Message is scheduled to expire"));
                else if (img == ImageUtil.ICON_MSG_FLAG_HASKEYS)
                    l.setToolTipText(registry.getText(T_HASKEYS, "Message includes keys you can import"));
                else if (img == ImageUtil.ICON_MSG_FLAG_HASARCHIVES)
                    l.setToolTipText(registry.getText(T_HASARCHIVES, "Message refers to archives"));
                else if (img == ImageUtil.ICON_MSG_FLAG_HASREFS)
                    l.setToolTipText(registry.getText(T_HASREFS, "Message includes references"));
                else if (img == ImageUtil.ICON_MSG_FLAG_HASATTACHMENTS)
                    l.setToolTipText(registry.getText(T_HASATTACHMENTS, "Message includes attachments"));
                else if (img == ImageUtil.ICON_MSG_FLAG_ISNEW)
                    l.setToolTipText(registry.getText(T_ISNEW, "Message is unread"));
                else if (img == ImageUtil.ICON_MSG_TYPE_PRIVATE)
                    l.setToolTipText(registry.getText(T_ISPRIVATE, "Message was privately encrypted"));
                else {
                    _browser.getUI().debugMessage("translating icon " + i + ": UNKNOWN icon");
                }
            }
        }
    }
}
