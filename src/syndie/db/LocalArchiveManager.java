package syndie.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import net.i2p.data.Hash;
import net.i2p.data.SessionKey;
import net.i2p.util.EepGet;
import syndie.Constants;
import syndie.data.NymKey;
import syndie.data.SyndieURI;

/**
 *
 */
public class LocalArchiveManager {
    public static SharedArchive.About getLocalAbout(DBClient client, SharedArchiveEngine.PullStrategy pullStrategy) {
        SharedArchive.About about = new SharedArchive.About();
        about.setAdminChannel(SharedArchive.ABOUT_NO_ADMIN_CHANNEL);
        
        Properties prefs = client.getNymPrefs();
        int maxSize = getInt(prefs, "archive.maxMsgSizeKB", -1);
        if (maxSize < 0)
            maxSize = pullStrategy.maxKBPerMessage;
        
        int archiveCount = 0;
        while (prefs.containsKey("archive.altURI" + archiveCount))
            archiveCount++;
        
        SyndieURI archives[] = new SyndieURI[archiveCount];
        for (int i = 0; i < archiveCount; i++) {
            String val = prefs.getProperty("archive.altURI" + i);
            try {
                archives[i] = new SyndieURI(val);
            } catch (URISyntaxException use) {}
        }
        
        int republishFrequencyHours = getInt(prefs, "archive.republishFrequencyHours", 1);
    
        about.setPublishRebuildFrequencyHours(republishFrequencyHours);
        about.setAlternativeArchives(archives);
        about.setMaxMessageSize(maxSize);
        about.setPostingRequiresPassphrase(false);
        about.setWantKnownChannelsOnly(pullStrategy.knownChannelsOnly);
        about.setWantPBE(pullStrategy.includePBEMessages);
        about.setWantPrivate(pullStrategy.includePrivateMessages);
        about.setWantRecentOnly(pullStrategy.includeRecentMessagesOnly);
        return about;
    }
    
    public static void setLocalAbout(DBClient client, UI ui, SharedArchive.About about) {
        Properties prefs = client.getNymPrefs();
        if (about.maxMessageSize() > 0)
            prefs.setProperty("archive.maxMsgSizeKB", about.maxMessageSize()+"");
        SyndieURI uris[] = about.getAlternateArchives();
        
        int old = 0;
        while (prefs.remove("archive.altURI" + old) != null)
            old++;
        
        if (uris != null)
            for (int i = 0; i < uris.length; i++)
                prefs.setProperty("archive.altURI" + i, uris[i].toString());

        prefs.setProperty("archive.republishFrequencyHours", about.getPublishRebuildFrequencyHours()+"");

        client.setNymPrefs(prefs);
    }
    
    private static int getInt(Properties prefs, String key, int def) {
        String val = prefs.getProperty(key);
        if (val == null) return def;
        try {
            return Integer.parseInt(val);
        } catch (NumberFormatException nfe) {
            return def;
        }
    }
    
    private static boolean getBoolean(Properties prefs, String key, boolean def) {
        String val = prefs.getProperty(key);
        if (val == null) return def;
        return Boolean.valueOf(val).booleanValue();
    }
    
    public static final String SHARED_INDEX_FILE = "shared-index.dat";
    
    public static void buildIndex(DBClient client, UI ui, SharedArchiveEngine.PullStrategy pullStrategy) {
        if (!client.isLoggedIn()) return;
        SharedArchiveBuilder builder = new SharedArchiveBuilder(client, ui, getLocalAbout(client, pullStrategy));
        SharedArchive archive = builder.buildSharedArchive();
        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(new File(client.getArchiveDir(), SHARED_INDEX_FILE));
            archive.write(fos);
            fos.close();
            fos = null;
        } catch (IOException ioe) {
            ui.errorMessage("Error writing the shared index", ioe);
        } finally {
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
        return;
    }
    
    public static int getLocalRebuildDelayHours(DBClient client) { return 1; }
}
