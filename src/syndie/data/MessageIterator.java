package syndie.data;

import syndie.db.ThreadReferenceNode;

/**
 *
 */
public interface MessageIterator {
    /** adjust the iterator's operations to start at the given uri */
    public void recenter(SyndieURI uri);

    /** get the URI to the next unread message in the message tree, or null if there are no more  */
    public SyndieURI getNextNew();
    /** get the URI to the previous unread message in the message tree, or null if there are no more  */
    public SyndieURI getPreviousNew();
    /** get the URI to the next message in the current thread, or null if there are no more */
    public SyndieURI getNextInThread();
    /** get the URI to the previous message in the current thread, or null if there are no more */
    public SyndieURI getPreviousInThread();
    /** get the URI to the root of the next thread, or null if there are no more */
    public SyndieURI getNextThread();
    /** get the URI to the root of the previous thread, or null if there are no more */
    public SyndieURI getPreviousThread();
    /** get the URI to the message tree itself */
    public SyndieURI getMessageTreeURI();
    
    public ThreadReferenceNode getThreadRoot();
}
