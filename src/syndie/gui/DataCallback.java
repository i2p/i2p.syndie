package syndie.gui;

import syndie.data.SyndieURI;

public interface DataCallback {  
    /** a new message was imported */
    public void messageImported();
    /** a new or updated metadata was imported */
    public void metaImported();
    /** the read status of a message was updated */
    public void readStatusUpdated();
    /** a new forum was created */
    public void forumCreated();
}
