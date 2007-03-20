package syndie.gui;

import java.net.URISyntaxException;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Control;
import syndie.data.NymReferenceNode;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

public class BookmarkDnDHelper {
    public static interface BookmarkDnDTarget {
        public void dropped(SyndieURI uri);
    }
    public static class WatchTarget implements BookmarkDnDTarget {
        private DBClient _client;
        public WatchTarget(DBClient client) { _client = client; }
        public void dropped(SyndieURI uri) {
            if ( (uri != null) && (uri.getScope() != null) )
                _client.watchChannel(uri.getScope(), true, false, false, false, false);
        }
    }
    public static DropTarget initWatchTarget(final DBClient client, final UI ui, final Control c) {
        return initBookmarkDnDTarget(client, ui, c, new WatchTarget(client));
    }
    public static DropTarget initBookmarkDnDTarget(final DBClient client, final UI ui, final Control c, final BookmarkDnDTarget onDrop) {
        ui.debugMessage("init watch target begin");
        int ops = DND.DROP_COPY | DND.DROP_LINK;
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        ui.debugMessage("init watch target: transfer created");
        DropTarget target = new DropTarget(c, ops);
        ui.debugMessage("init watch target: target created");
        target.setTransfer(transfer);
        ui.debugMessage("init watch target: transfer set");
        target.addDropListener(new DropTargetListener() {
            public void dragEnter(DropTargetEvent evt) {
                ui.debugMessage("dnd watch target enter");
                // we can take the element
                evt.detail = evt.operations | DND.DROP_COPY;
            }
            public void dragLeave(DropTargetEvent evt) {}
            public void dragOperationChanged(DropTargetEvent evt) {}
            public void dragOver(DropTargetEvent evt) {
                evt.feedback = DND.FEEDBACK_SELECT;
            }
            public void drop(DropTargetEvent evt) {
                ui.debugMessage("dnd watch target drop");
                if (evt.data == null) {
                    evt.detail = DND.DROP_NONE;
                    return;
                } else {
                    BookmarkDnD bookmark = new BookmarkDnD();
                    bookmark.fromString(evt.data.toString());
                    if (bookmark.uri != null) { // parsed fine
                        ui.debugMessage("dnd watch target drop: ok!");
                        onDrop.dropped(bookmark.uri);
                    } else { // wasn't in bookmark syntax, try as a uri
                        String str = evt.data.toString();
                        try {
                            SyndieURI uri = new SyndieURI(str);
                            ui.debugMessage("dnd watch target drop: ok!");
                            onDrop.dropped(bookmark.uri);
                        } catch (URISyntaxException use) {
                            ui.debugMessage("invalid uri: " + str, use);
                        }
                    }
                }
            }
            public void dropAccept(DropTargetEvent evt) {}
        });
        ui.debugMessage("init watch target: listener added");
        return target;
    }
    
    public static DragSource initSource(final Control c, final SyndieURI uri) { return initSource(c, uri, null); }
    public static DragSource initSource(final Control c, final SyndieURI uri, final String name) {
        Transfer transfer[] = new Transfer[] { TextTransfer.getInstance() };
        int ops = DND.DROP_COPY;
        DragSource source = new DragSource(c, ops);
        source.setTransfer(transfer);
        source.addDragListener(new DragSourceListener() {
            public void dragFinished(DragSourceEvent evt) {}
            public void dragSetData(DragSourceEvent evt) {
                BookmarkDnD bookmark = new BookmarkDnD();
                bookmark.desc = "";
                bookmark.name = (name != null ? name : "");
                bookmark.uri = uri;
                evt.data = bookmark.toString();
            }
            public void dragStart(DragSourceEvent evt) {}
        });
        return source;
    }
}
