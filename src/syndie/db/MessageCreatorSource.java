package syndie.db;

import java.util.List;
import net.i2p.data.Hash;
import syndie.data.SyndieURI;

public interface MessageCreatorSource {
    //public DataCallback getDataCallback();
    public MessageCreator.ExecutionListener getListener();
    public DBClient getClient();
    public UI getUI();
    public Hash getAuthor();
    public Hash getTarget();
    /** if not null, contains the hash of the public key that should be used to sign the post */
    public Hash getSignAs();
    /** 
     * if true, the author should be hidden within the encrypted block (only makes sense if
     * getSignAs() is set and not equal to getAuthor())
     */
    public boolean getAuthorHidden();
    public int getPageCount();
    public String getPageContent(int page);
    public String getPageType(int page);
    public String getPageTitle(int page);
    public List getAttachmentNames();
    public List getAttachmentTypes();
    /** @param attachmentIndex starts at 1 */
    public byte[] getAttachmentData(int attachmentIndex);
    public String getSubject();
    public boolean getPrivacyPBE();
    public String getPassphrase();
    public String getPassphrasePrompt();
    public boolean getPrivacyPublic();
    public String getAvatarUnmodifiedFilename();
    public byte[] getAvatarModifiedData();
    public boolean getPrivacyReply();
    public String[] getPublicTags();
    public String[] getPrivateTags();
    public List getReferenceNodes();
    /** list of SyndieURIs for messages that should be cancelled by this post */
    public List getCancelURIs();
    public int getParentCount();
    /**
     * ordered list of earlier messages (SyndieURI) this follows in the thread 
     * of (most recent parent first)
     */
    public SyndieURI getParent(int depth);
    public String getExpiration();
    public boolean getForceNewThread();
    public boolean getRefuseReplies();
}   
