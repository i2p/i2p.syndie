package syndie.db;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.util.Properties;

import net.i2p.data.Base64;
import net.i2p.util.SecureFileOutputStream;

import syndie.Constants;
import syndie.data.SyndieURI;

/**
 *  Configuration for the local published index,
 *  and method for rebuilding it.
 */
public class LocalArchiveManager {

    private static final int DEFAULT_REBUILD_DELAY_HOURS = 1;

    public static SharedArchive.About getLocalAbout(DBClient client, PullStrategy pullStrategy) {
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
        
        int republishFrequencyHours = getInt(prefs, "archive.republishFrequencyHours", DEFAULT_REBUILD_DELAY_HOURS);
    
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
    
    /**
     *  @return success
     */
    public static boolean buildIndex(DBClient client, UI ui, PullStrategy pullStrategy) {
        return buildIndex(client, ui, pullStrategy, new File(client.getWebDir(), SHARED_INDEX_FILE));
    }

    /**
     *  TODO force-rebuild parameter
     *  @return success
     */
    public static boolean buildIndex(DBClient client, UI ui, PullStrategy pullStrategy, File targetFile) {
        if (!client.isLoggedIn()) return false;
        SharedArchiveBuilder builder = new SharedArchiveBuilder(client, ui, getLocalAbout(client, pullStrategy));
        SharedArchive archive = builder.buildSharedArchive();
        FileOutputStream fos = null;
        try {
            fos = new SecureFileOutputStream(targetFile);
            archive.write(fos);
            fos.close();
            fos = null;
        } catch (IOException ioe) {
            ui.errorMessage("Error writing the shared index", ioe);
            return false;
        } finally {
            if (fos != null) try { fos.close(); } catch (IOException ioe) {}
        }
        return true;
    }
    
    public static int getLocalRebuildDelayHours(DBClient client) {
        Properties prefs = client.getNymPrefs();
        int republishFrequencyHours = getInt(prefs, "archive.republishFrequencyHours", DEFAULT_REBUILD_DELAY_HOURS);
        return republishFrequencyHours;
    }
}
