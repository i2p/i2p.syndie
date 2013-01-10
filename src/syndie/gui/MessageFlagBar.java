package syndie.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.i2p.data.Hash;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import syndie.data.MessageInfo;
import syndie.data.ReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

/**
 * collection of icons relevent for a particular message
 */
public class MessageFlagBar extends BaseComponent implements Translatable {
    private final BookmarkControl _bookmarkControl;
    private final Composite _parent;
    private Composite _root;
    private MessageInfo _msg;
    private final boolean _includeTooltips;
    private final List<Image> _images;
    private boolean _realized;
    private Color _bg;
    
    public MessageFlagBar(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, BookmarkControl bookmarkControl, Composite parent, boolean includeTooltips) {
        super(client, ui, themes, trans);
        _bookmarkControl = bookmarkControl;
        _parent = parent;
        _includeTooltips = includeTooltips;
        _images = new ArrayList();
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
        StringBuilder rv = new StringBuilder();
        for (int i = 0; i < ctl.length; i++) {
            String tt = ((Label)ctl[i]).getToolTipText();
            rv.append(tt);
            if (i + 1 < ctl.length)
                rv.append(" ");
        }
        return rv.toString();
    }
    
    public void setMessage(MessageInfo msg) { setMessage(msg, true); }
    public void setMessage(MessageInfo msg, boolean relayout) { 
        _msg = msg;
        rebuildFlags(relayout);
    }
    
    public void setBackground(Color bg) {
        if (bg == null) return;
        _bg = bg;
        if (_realized) {
            _root.setBackground(bg);
            Control ctl[] = _root.getChildren();
            for (int i = 0; i < ctl.length; i++) {
                if (!ctl[i].isDisposed())
                    ctl[i].setBackground(bg);
            }
        }
    }
    
    private void rebuildFlags(boolean relayout) {
        if (_realized) {
            //_browser.getUI().debugMessage("rebuilding flags for " + (_msg != null ? _msg.getURI().toString() : "no message"));
            //_root.setRedraw(false);
            ////_root.setVisible(false);
            clearFlags();
            buildImages();
            createLabels();
            translate(_translationRegistry);
            if (relayout)
                finishRebuild();
            ////_root.setVisible(true);
        } else {
            //_browser.getUI().debugMessage("rebuilding flags for " + (_msg != null ? _msg.getURI().toString() : "no message"));
            disposeImages();
            buildImages();
        }
    }
    private void clearFlags() {
        disposeImages();
        Control ctl[] = _root.getChildren();
        for (int i = 0; i < ctl.length; i++) {
            if (!ctl[i].isDisposed())
                ctl[i].dispose();
        }
    }
    private void createLabels() {
        for (int i = 0; i < _images.size(); i++) {
            ImageCanvas c = new ImageCanvas(_root, false, false, false);
            c.forceSize(16, 16);
            //Label l = new Label(_root, SWT.NONE);
            //l.setImage((Image)_images.get(i));
            c.setImage((Image)_images.get(i));
            if (_bg != null)
               c.setBackground(_bg);
        }
    }
    private void finishRebuild() {
        _root.layout(true, true);
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
                author = _client.getChannelHash(_msg.getAuthorChannelId());
            }

            List bannedChannels = _client.getBannedChannels();
            boolean banned = bannedChannels.contains(author) || bannedChannels.contains(forum);

            boolean authorBookmarked = author != null ? _bookmarkControl.isBookmarked(SyndieURI.createScope(author)) : false;
            boolean forumBookmarked = forum != null ? _bookmarkControl.isBookmarked(SyndieURI.createScope(forum)) : false;
            boolean scheduledForExpire = _msg.getExpiration() > 0;

            List refs = _msg.getReferences();
            if (refs == null)
                refs = Collections.EMPTY_LIST;
            
            boolean hasKeys = false;
            boolean hasArchives = false;
            boolean hasRefs = refs.size() > 0;
            boolean hasAttachments = _msg.getAttachmentCount() > 0;
            boolean isNew = _client.getMessageStatus(_msg.getInternalId(), _msg.getTargetChannelId()) == DBClient.MSG_STATUS_UNREAD;
            
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
            if (authorBookmarked) _images.add(ImageUtil.ICON_MSG_FLAG_BOOKMARKED_AUTHOR);
            if (forumBookmarked) _images.add(ImageUtil.ICON_MSG_FLAG_BOOKMARKED_FORUM);
            if (scheduledForExpire) _images.add(ImageUtil.ICON_MSG_FLAG_SCHEDULEDFOREXPIRE);
            if (hasKeys) _images.add(ImageUtil.ICON_MSG_FLAG_HASKEYS);
            if (hasArchives) _images.add(ImageUtil.ICON_MSG_FLAG_HASARCHIVES);
            if (hasRefs) _images.add(ImageUtil.ICON_MSG_FLAG_HASREFS);
            if (hasAttachments) _images.add(ImageUtil.ICON_MSG_FLAG_HASATTACHMENTS);
            if (isNew) _images.add(ImageUtil.ICON_MSG_FLAG_ISNEW);
        }
        
        // reapply translation to pull the per-image tooltips
        translate(_translationRegistry);
    }
    
    private void realizeComponents() {
        if (_realized) return;
        _root = new Composite(_parent, SWT.NONE);
        //_root.setBackgroundMode(SWT.INHERIT_FORCE);
        FillLayout layout = new FillLayout(SWT.HORIZONTAL);
        layout.spacing = 20;
        layout.marginHeight= 8;
        layout.marginWidth= 5;
        _root.setLayout(layout);
        _translationRegistry.register(this);
        _realized = true;
        setBackground(_bg);
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
            _translationRegistry.unregister(this);
    }
    
    
    public void translate(TranslationRegistry registry) {
        if (!_includeTooltips) return;
        if (!_realized) return;
        Control ctl[] = _root.getChildren();
        for (int i = 0; i < ctl.length; i++) {
            if (!ctl[i].isDisposed()) {
                _ui.debugMessage("translating icon " + i);
                Image img = (Image)_images.get(i); //l.getImage();
                if (img == ImageUtil.ICON_MSG_FLAG_PBE)
                    ctl[i].setToolTipText(registry.getText("Post is passphrase protected"));
                else if (img == ImageUtil.ICON_MSG_FLAG_READKEYUNKNOWN)
                    ctl[i].setToolTipText(registry.getText("Read key unknown"));
                else if (img == ImageUtil.ICON_MSG_FLAG_REPLYKEYUNKNOWN)
                    ctl[i].setToolTipText(registry.getText("Private key unknown"));
                else if (img == ImageUtil.ICON_MSG_FLAG_UNREADABLE)
                    ctl[i].setToolTipText(registry.getText("Message is not readable"));
                else if (img == ImageUtil.ICON_MSG_FLAG_PUBLIC)
                    ctl[i].setToolTipText(registry.getText("Message was publically readable"));
                else if (img == ImageUtil.ICON_MSG_FLAG_AUTHENTICATED)
                    ctl[i].setToolTipText(registry.getText("Author is authentic"));
                else if (img == ImageUtil.ICON_MSG_FLAG_AUTHORIZED)
                    ctl[i].setToolTipText(registry.getText("Author is allowed to post in the forum"));
                else if (img == ImageUtil.ICON_MSG_FLAG_BANNED)
                    ctl[i].setToolTipText(registry.getText("Post is banned"));
                else if (img == ImageUtil.ICON_MSG_FLAG_BOOKMARKED_AUTHOR)
                    ctl[i].setToolTipText(registry.getText("Author is bookmarked"));
                else if (img == ImageUtil.ICON_MSG_FLAG_BOOKMARKED_FORUM)
                    ctl[i].setToolTipText(registry.getText("Forum is bookmarked"));
                else if (img == ImageUtil.ICON_MSG_FLAG_SCHEDULEDFOREXPIRE)
                    ctl[i].setToolTipText(registry.getText("Message is scheduled to expire"));
                else if (img == ImageUtil.ICON_MSG_FLAG_HASKEYS)
                    ctl[i].setToolTipText(registry.getText("Message includes keys you can import"));
                else if (img == ImageUtil.ICON_MSG_FLAG_HASARCHIVES)
                    ctl[i].setToolTipText(registry.getText("Message refers to archives"));
                else if (img == ImageUtil.ICON_MSG_FLAG_HASREFS)
                    ctl[i].setToolTipText(registry.getText("Message includes references"));
                else if (img == ImageUtil.ICON_MSG_FLAG_HASATTACHMENTS)
                    ctl[i].setToolTipText(registry.getText("Message includes attachments"));
                else if (img == ImageUtil.ICON_MSG_FLAG_ISNEW)
                    ctl[i].setToolTipText(registry.getText("Message is unread"));
                else if (img == ImageUtil.ICON_MSG_TYPE_PRIVATE)
                    ctl[i].setToolTipText(registry.getText("Message was privately encrypted"));
                else {
                    _ui.debugMessage("translating icon " + i + ": UNKNOWN icon");
                }
            }
        }
    }
}
