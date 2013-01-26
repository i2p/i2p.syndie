package syndie.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import syndie.data.SyndieURI;
import syndie.db.DBClient;
import syndie.db.UI;

class MaxView extends BaseComponent {
    private Shell _parent;
    private Shell _shell;
    private PageRenderer _maxRenderer;
    private MaxListener _listener;

    public MaxView(DBClient client, UI ui, ThemeRegistry themes, TranslationRegistry trans, Shell parent, SyndieURI pageURI, MaxListener lsnr) {
        super(client, ui, themes, trans);
        _parent = parent;
        _shell = new Shell(_parent, SWT.NO_TRIM | SWT.PRIMARY_MODAL);
        _shell.setLayout(new GridLayout(1, true));
        _listener = lsnr;
        Button unmax = new Button(_shell, SWT.PUSH);
        unmax.setText(trans.getText("Restore normal size"));
        unmax.setFont(themes.getTheme().BUTTON_FONT);
        unmax.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));

        _maxRenderer = ComponentBuilder.instance().createPageRenderer(_shell, true, true);
        _maxRenderer.getComposite().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, true, 2, 1));

        _maxRenderer.renderPage(new PageRendererSource(_client, _themeRegistry), pageURI);

        unmax.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { unmax(); }
            public void widgetSelected(SelectionEvent selectionEvent) { unmax(); }
            private void fire() { unmax(); }
        });

        Monitor mon[] = _parent.getDisplay().getMonitors();
        Rectangle rect = null;
        if ( (mon != null) && (mon.length > 1) )
            rect = mon[0].getClientArea();
        else
            rect = _parent.getDisplay().getClientArea();
        _shell.setSize(rect.width, rect.height);
        _shell.setMaximized(true);

        _shell.addShellListener(new ShellListener() {
            public void shellActivated(ShellEvent shellEvent) {}
            public void shellClosed(ShellEvent evt) {
                evt.doit = false;
                unmax();
            }
            public void shellDeactivated(ShellEvent shellEvent) {}
            public void shellDeiconified(ShellEvent shellEvent) {}
            public void shellIconified(ShellEvent shellEvent) {}
        });

        _shell.open();
        _maxRenderer.forceFocus();
    }

    public void dispose() { 
        _shell.dispose();
        _maxRenderer.dispose();
    }
    private void unmax() { _listener.unmax(this); }
    
    public interface MaxListener { public void unmax(MaxView view); }

}
