package syndie.gui;

import java.io.File;
import java.util.List;
import net.i2p.data.Hash;
import syndie.data.SyndieURI;

public interface URIControl {
    
    public SyndieURI createPostURI(Hash forum, SyndieURI parent);
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply);
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, boolean asPrivateReply, List references, File attachments[]);
    public SyndieURI createPostURI(Hash forum, SyndieURI parent, String pbePass, String pbePrompt, List references, File attachments[]);
    public SyndieURI createPostURI(long postponeId, int postponeVersion);
    public SyndieURI createTextUIURI();
    public SyndieURI createLogsURI();
    public SyndieURI createSQLURI();
    /** manage the forum */
    public SyndieURI createManageURI(Hash forum);
    /** view the details of the forum, not the messages */
    public SyndieURI createMetaURI(Hash forum);
    /** view the details, but zoom in on the published references */
    public SyndieURI createMetaRefsURI(Hash forum);
    /** view the details, but zoom in on the published archives */
    public SyndieURI createMetaArchivesURI(Hash forum);
    /** view the details, but zoom in on the published authorized posters */
    public SyndieURI createMetaPostersURI(Hash forum);
    /** view the details, but zoom in on the published authorized managers */
    public SyndieURI createMetaManagersURI(Hash forum);
    
    public SyndieURI createSyndicationArchiveURI();
    public SyndieURI createSyndicationConfigURI();
    public SyndieURI createSyndicationDiffURI();
    public SyndieURI createSyndicationStatusURI();
    public SyndieURI createBugReportURI();
    public SyndieURI createHighlightWatchedURI(boolean threaded, boolean unreadOnly, boolean useImportDate);
}
