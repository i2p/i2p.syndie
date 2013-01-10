package syndie.gui.desktop;

import java.io.StringWriter;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Time;
import java.sql.Timestamp;
import java.sql.Types;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import net.i2p.data.Base64;
import net.i2p.data.DataHelper;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import syndie.db.DBClient;
import syndie.db.NestedUI;
import syndie.db.Opts;
import syndie.db.TextEngine;
import syndie.db.UI;
import syndie.data.SyndieURI;
import syndie.gui.ColorUtil;
import syndie.gui.ImageUtil;
import syndie.gui.Theme;
import syndie.gui.ThemeRegistry;
import syndie.gui.Themeable;
import syndie.gui.Translatable;
import syndie.gui.TranslationRegistry;

/**
 *
 */
class SQLPanel extends DesktopPanel implements Translatable, Themeable {
    private Combo _in;
    private Text _msgs;
    private Table _results;
    private String _name;
    private String _description;
    private Color _nullBG;
    
    private static final int HISTORY = 20;
    
    public SQLPanel(Desktop desktop, DBClient client, ThemeRegistry themes, TranslationRegistry trans, Composite parent, UI ui, SyndieURI uri) {
        super(desktop, client, themes, trans, parent, ui, uri);
        initComponents();
    }
    
    protected void initComponents() {
        getRoot().setLayout(new GridLayout(1, true));
        
        _nullBG = ColorUtil.getColor("darkgray", null);
        
        _results = new Table(getRoot(), SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE);
        _results.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true));
        _results.setHeaderVisible(true);
        _results.setLinesVisible(true);
        
        _msgs = new Text(getRoot(), SWT.BORDER | SWT.MULTI | SWT.READ_ONLY);
        _msgs.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        
        _in = new Combo(getRoot(), SWT.BORDER | SWT.DROP_DOWN);
        _in.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));
        _in.addTraverseListener(new TraverseListener() {
            public void keyTraversed(TraverseEvent evt) {
                if (evt.detail == SWT.TRAVERSE_RETURN)
                    runCommand();
            }
        });
        
        _translationRegistry.register(this);
        _themeRegistry.register(this);
    }

    public void shown(Desktop desktop, SyndieURI uri, String name, String desc) {
        super.shown(desktop, uri, name, desc);
        forceFocus();
    }
    public void forceFocus() { _in.forceFocus(); }
    
    private static final SimpleDateFormat _dateFmt = new SimpleDateFormat("yyyy/MM/dd");
    private static final SimpleDateFormat _timeFmt = new SimpleDateFormat("HH:mm:ss.SSS");
    private static final SimpleDateFormat _tsFmt = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss.SSS");
    private static final String getDate(Date date) {
        if (date == null) return "[null]";
        synchronized (_dateFmt) { return _dateFmt.format(date); }
    }
    private static final String getTime(Time date) {
        if (date == null) return "[null]";
        synchronized (_timeFmt) { return _timeFmt.format(date); }
    }
    private static final String getTimestamp(Timestamp date) {
        if (date == null) return "[null]";
        synchronized (_tsFmt) { return _tsFmt.format(date); }
    }
    private static final String toString(byte val[]) {
        if (val == null)
            return "[null]";
        int len = val.length;
        if (len > 32)
            len = 32;
        return Base64.encode(val, 0, len);
    }
    
    private void runCommand() {
        _results.setRedraw(false);
        while (_results.getItemCount() > 0)
            _results.getItem(0).dispose();
        while (_results.getColumnCount() > 0)
            _results.getColumn(0).dispose();
        
        String sql = _in.getText().trim();
        _in.setText("");
        if (sql.length() > 0) {
            _in.add(sql, 0);
            while (_in.getItemCount() > HISTORY)
                _in.remove(HISTORY);
            Connection con = _client.con();
            Statement stmt = null;
            ResultSet rs = null;
            try {
                stmt = con.createStatement();
                boolean hasRS = stmt.execute(sql);
                if (hasRS) {
                    rs = stmt.getResultSet();
                    ResultSetMetaData md = rs.getMetaData();
                    int columns = md.getColumnCount();
                    int types[] = new int[columns];
                    for (int i = 1; i <= columns; i++) {
                        String name = md.getColumnLabel(i);
                        types[i-1] = md.getColumnType(i);
                        TableColumn col = new TableColumn(_results, SWT.LEFT);
                        col.setText(name);
                    }
                    int rows = 0;
                    while (rs.next()) {
                        rows++;
                        TableItem row = new TableItem(_results, SWT.NONE);
                        for (int i = 1; i <= columns; i++) {
                            switch (types[i-1]) {
                                case Types.TINYINT:
                                case Types.SMALLINT:
                                case Types.BIGINT:
                                case Types.INTEGER: {
                                    long val = rs.getLong(i);
                                    if (rs.wasNull())
                                        row.setBackground(i-1, _nullBG);
                                    else
                                        row.setText(i-1, Long.toString(val));
                                    break;
                                }
                                case Types.DECIMAL:
                                case Types.DOUBLE:
                                case Types.FLOAT:
                                case Types.NUMERIC: {
                                    double val = rs.getDouble(i);
                                    if (rs.wasNull())
                                        row.setBackground(i-1, _nullBG);
                                    else
                                        row.setText(i-1, Double.toString(val));
                                    break;
                                }
                                case Types.BIT:
                                case Types.BOOLEAN: {
                                    boolean val = rs.getBoolean(i);
                                    if (rs.wasNull())
                                        row.setBackground(i-1, _nullBG);
                                    else
                                        row.setText(i-1, val ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
                                    break;
                                }
                                case Types.CHAR:
                                case Types.LONGVARCHAR:
                                case Types.VARCHAR: {
                                    String val = rs.getString(i);
                                    if (rs.wasNull())
                                        row.setBackground(i-1, _nullBG);
                                    else
                                        row.setText(i-1, val); // perhaps we should trim this?
                                    break;
                                }
                                case Types.DATE: {
                                    Date val = rs.getDate(i);
                                    if (val == null)
                                        row.setBackground(i-1, _nullBG);
                                    else
                                        row.setText(i-1, getDate(val));
                                    break;
                                }
                                case Types.TIME: {
                                    Time val = rs.getTime(i);
                                    if (val == null)
                                        row.setBackground(i-1, _nullBG);
                                    else
                                        row.setText(i-1, getTime(val));
                                    break;
                                }
                                case Types.TIMESTAMP: {
                                    Timestamp val = rs.getTimestamp(i);
                                    if (val == null)
                                        row.setBackground(i-1, _nullBG);
                                    else
                                        row.setText(i-1, getTimestamp(val));
                                    break;
                                }
                                case Types.BINARY:
                                case Types.BLOB:
                                case Types.CLOB:
                                case Types.LONGVARBINARY:
                                case Types.VARBINARY: {
                                    byte val[] = rs.getBytes(i);
                                    if (val != null)
                                        row.setText(i-1, toString(val));
                                    else
                                        row.setBackground(i-1, _nullBG);
                                    break;
                                }
                                case Types.NULL:
                                case Types.OTHER:
                                case Types.REAL:
                                case Types.REF:
                                case Types.STRUCT:
                                default:
                                    break;
                            }
                        } // end looping over columns
                    } // end looping over the rs
                    _msgs.setText("rows: " + rows);
                    for (int col = 0; col < _results.getColumnCount(); col++)
                        _results.getColumn(col).pack();
                    _results.setTopIndex(0);
                } else {
                    // does not have a result set
                    int rows = stmt.getUpdateCount();
                    _msgs.setText("rows: " + rows);
                }
            } catch (SQLException se) {
                _ui.debugMessage("Error executing [" + sql + "]", se);
                _msgs.setText(se.getMessage());
            } finally {
                if (rs != null) try { rs.close(); } catch (SQLException se) {}
                if (stmt != null) try { stmt.close(); } catch (SQLException se) {}
            }
        }
        _results.setRedraw(true);
    }

    public String getPanelName() { return _name; }
    public String getPanelDescription() { return _description; }
    
    public void dispose() { 
        _translationRegistry.unregister(this);
        _themeRegistry.unregister(this);
        super.dispose();
    }

    
    public void translate(TranslationRegistry registry) {
        _name = registry.getText("SQL");
        _description = registry.getText("Advanced SQL interface");
    }
    
    public void applyTheme(Theme theme) {
        _results.setFont(theme.TABLE_FONT);
        _in.setFont(theme.DEFAULT_FONT);
        _msgs.setFont(theme.DEFAULT_FONT);
    }
}
