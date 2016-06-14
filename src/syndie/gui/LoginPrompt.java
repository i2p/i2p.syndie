package syndie.gui;

import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Button;

import syndie.db.DBClient;
import syndie.util.Timer;

/**
 *
 */
public final class LoginPrompt {
    private DBClient _client;
    private Browser _browser;
    private Shell _shell;
    
    public LoginPrompt(DBClient client, Browser browser) {
        _client = client;
        _browser = browser;
        initComponents();
        Thread t = new Thread(new Runnable() {
            public void run() {
                while (!_client.isLoggedIn())  
                    try {
                        Thread.sleep(50);
                    } catch (InterruptedException ex) {}
                loginComplete();
            }
        });
        t.start();
    }
    
    public void login() { _shell.open(); }
    
    private void loginComplete() {
        _browser.debugMessage("loginComplete");
        synchronized (this) {
            Display.getDefault().asyncExec(new Runnable() { 
                public void run() {
                    Shell s = _shell;
                    if ( (s != null) && (!s.isDisposed()) )
                        s.dispose();
                    _browser.startup(new Timer("LoginComplete", _browser.getUI()));
                }
            });
        }
    }
    
    private void initComponents() {
        _shell = new Shell(Display.getDefault(), SWT.TITLE);
        _shell.setText("Syndie login");
        _shell.setLayout(new FillLayout());
        Button b = new Button(_shell, SWT.PUSH);
        b.setText("login");
        b.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent selectionEvent) { loginComplete(); }
            public void widgetSelected(SelectionEvent selectionEvent) { loginComplete(); }
        });
        _shell.pack();
    }
    
}
