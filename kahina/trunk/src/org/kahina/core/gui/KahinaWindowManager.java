package org.kahina.core.gui;

import java.awt.Toolkit;
import java.io.File;
import java.util.HashMap;

import javax.swing.JFrame;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaPerspectiveEvent;
import org.kahina.core.io.util.XMLUtilities;
import org.kahina.core.visual.KahinaView;
import org.w3c.dom.Node;

public class KahinaWindowManager implements KahinaListener
{
    public KahinaMainWindow mainWindow;
    
    KahinaPerspective currentPerspective;
    
    HashMap<KahinaView<?>, KahinaWindow> contentWindows;
    HashMap<KahinaView<?>, KahinaWindow> topLevelWindows;
    
    KahinaGUI gui;
    
    public KahinaWindowManager(KahinaGUI gui, KahinaController control)
    {
        this.gui = gui;  
        
        this.currentPerspective = new KahinaPerspective(gui.views);
        
        this.contentWindows = new HashMap<KahinaView<?>, KahinaWindow>();
        this.topLevelWindows = new HashMap<KahinaView<?>, KahinaWindow>();
        
        mainWindow = createMainWindow(this, control, gui.kahina);
        
        //create windows for all the other registered views
        KahinaArrangement arr = currentPerspective.getArrangement();
        for (KahinaView<?> view : gui.views)
        {
        	String viewID = view.getTitle();
        	//for now, generate the perspective from the predefined configurations
        	currentPerspective.setConfiguration(view.getTitle(), view.getConfig());
        	
            KahinaWindow viewWindow = integrateInDefaultWindow(view, control);
            viewWindow.setSize(arr.getWidth(viewID), arr.getHeight(viewID));
            viewWindow.setLocation(arr.getXPos(viewID), arr.getYPos(viewID));
        }
        
		control.registerListener(KahinaEventTypes.PERSPECTIVE, this);
    }
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, KahinaController control, KahinaInstance<?, ?, ?> kahina)
	{
		return new KahinaMainWindow(this, control, gui.kahina);
	}

	public void disposeAllWindows()
    {
        for (JFrame viewWindow : topLevelWindows.values())
        {
            viewWindow.dispose();
        }
        mainWindow.dispose();
    }
    
    public KahinaWindow integrateInDefaultWindow(KahinaView<?> view, KahinaController control)
    {
        KahinaWindow viewWindow = new KahinaDefaultWindow(view, control);
        contentWindows.put(view,viewWindow);
        topLevelWindows.put(view,viewWindow);
        return viewWindow;
    }
    
    public void integrateInVerticallySplitWindow(KahinaView<?> v1, KahinaView<?> v2, String newTitle, KahinaController control)
    {
        KahinaWindow wrapperWindow1 = topLevelWindows.get(v1);
        if (wrapperWindow1 == null)
        {
            wrapperWindow1 = integrateInDefaultWindow(v1, control);
        }
        KahinaWindow wrapperWindow2 = topLevelWindows.get(v2);
        if (wrapperWindow2 == null)
        {
            wrapperWindow2 = integrateInDefaultWindow(v2, control);
        }
        KahinaVerticallySplitWindow splitWindow = new KahinaVerticallySplitWindow();
        splitWindow.setTitle(newTitle);
        splitWindow.setUpperWindow(wrapperWindow1);
        splitWindow.setLowerWindow(wrapperWindow2);
        topLevelWindows.put(v1,splitWindow);
        topLevelWindows.put(v2,splitWindow);
    }
    
    public void integrateInHorizontallySplitWindow(KahinaView<?> v1, KahinaView<?> v2, String newTitle, KahinaController control)
    {
        KahinaWindow wrapperWindow1 = topLevelWindows.get(v1);
        if (wrapperWindow1 == null)
        {
            wrapperWindow1 = integrateInDefaultWindow(v1, control);
        }
        KahinaWindow wrapperWindow2 = topLevelWindows.get(v2);
        if (wrapperWindow2 == null)
        {
            wrapperWindow2 = integrateInDefaultWindow(v2, control);
        }
        KahinaHorizontallySplitWindow splitWindow = new KahinaHorizontallySplitWindow();
        splitWindow.setTitle(newTitle);
        splitWindow.setLeftWindow(wrapperWindow1);
        splitWindow.setRightWindow(wrapperWindow2);
        topLevelWindows.put(v1,splitWindow);
        topLevelWindows.put(v2,splitWindow);
    }
    
    public void displayWindows()
    {
    	mainWindow.setVisible(true);
        for (KahinaWindow window : topLevelWindows.values())
        {
            //window.computeGoodSize();
            window.setVisible(true);
        }
    }
    
	@Override
	public void processEvent(KahinaEvent e)
	{
		if (e instanceof KahinaPerspectiveEvent)
		{
			processPerspectiveEvent((KahinaPerspectiveEvent) e);
		}
	}
    
	private void processPerspectiveEvent(KahinaPerspectiveEvent e)
	{
		int type = e.getPerspectiveEventType();
		if (type == KahinaPerspectiveEvent.SAVE_PERSPECTIVE)
		{
			savePerspectiveAs(e.getFile());
		} 
		else if (type == KahinaPerspectiveEvent.LOAD_PERSPECTIVE)
		{
			loadPerspective(e.getFile());
		}
	}
	
	private void loadPerspective(File file)
	{
		//TODO: load XML file and apply new perspective
	}
	
	private void savePerspectiveAs(File file)
	{
		Node node = currentPerspective.exportXML(XMLUtilities.newEmptyDocument());
		XMLUtilities.writeXML(node,file.getAbsolutePath());
	}
}
