package syndie.gui;

/**
 * generalized reference picker.  in practice its either the old tree based one or
 * the newer icon based one
 */
interface ReferenceChooserPopup {
    public void setListener(ReferenceChooserTree.AcceptanceListener lsnr);
    public void show();
    public void hide();
    public void dispose();
}
