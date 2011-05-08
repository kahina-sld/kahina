package org.kahina.core.gui;

import javax.swing.JPanel;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaView;

public class KahinaDefaultWindow extends KahinaWindow
{
    KahinaView v;
    
    public KahinaDefaultWindow(KahinaView v, KahinaWindowManager wm)
    {
    	super(wm);
        setContent(v, wm.control);
        setTitle(v.getTitle());
    }
    
    public KahinaDefaultWindow(KahinaView v, KahinaWindowManager wm, int winID)
    {
    	super(wm, winID);
        setContent(v, wm.control);
        setTitle(v.getTitle());
    }
    
    public void setContent(KahinaView v, KahinaController control)
    {
        this.v = v;
        mainPanel.removeAll();
        mainPanel.add(v.wrapInPanel(control));
    }
    
    public KahinaView getContent()
    {
        return v;
    }
    
    public boolean isContentWindow()
    {
    	return true;
    }
    
    public KahinaWindow createDynamicClone()
    {
    	//TODO: we need a .getCopyWithSameModel() that KEEPS SYNCHRONIZED!!!
    	//much of this can be done with the event system, but what if e.g. a new model is displayed?
    	KahinaView vCopy = v; 
    	vCopy.display(v.getModel());
    	KahinaDefaultWindow cloneWindow = new KahinaDefaultWindow(vCopy,wm);
    	cloneWindow.cloned = true;
    	cloneWindow.setTitle(cloneWindow.getTitle() + " (clone)");
    	cloneWindow.setSize(this.getSize());
    	cloneWindow.setLocation(this.getX() + 100, this.getY() + 100);
    	return cloneWindow;
    }
    
    public KahinaWindow createSnapshotClone()
    {
    	//TODO: we need to get a way to generate another view with exactly the same properties
    	//this is likely to work best with a forced implementation of another getCopyWithoutModel() method
    	//default behavior of this method could be a deep clone, with all the risks for storage and retrieval
    	//listeners should not be copied over, severing a snapshot clone from the event system
    	KahinaView vCopy = v; 
    	//TODO: for every type of content we need a cloning method for all entities
    	//or perhaps be more flexible: copying the entire tree can perhaps be avoided
    	//	if snapshots at different stages of construction are desired
    	vCopy.display(v.getModel());
    	KahinaDefaultWindow cloneWindow = new KahinaDefaultWindow(vCopy,wm);
    	cloneWindow.cloned = true;
    	cloneWindow.setTitle(cloneWindow.getTitle() + " (at step " + wm.gui.kahina.getState().nextStepID() + ")");
    	cloneWindow.setSize(this.getSize());
    	cloneWindow.setLocation(this.getX() + 100, this.getY() + 100);
    	return cloneWindow;
    }
}
