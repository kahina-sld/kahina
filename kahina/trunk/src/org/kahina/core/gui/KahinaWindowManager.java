package org.kahina.core.gui;

import java.io.File;
import java.util.HashMap;

import javax.swing.JFrame;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaPerspectiveEvent;
import org.kahina.core.event.KahinaWindowEvent;
import org.kahina.core.event.KahinaWindowEventType;
import org.kahina.core.io.util.XMLUtilities;
import org.kahina.core.visual.KahinaEmptyView;
import org.kahina.core.visual.KahinaView;
import org.w3c.dom.Node;

public class KahinaWindowManager implements KahinaListener
{
    public KahinaMainWindow mainWindow;
    
    KahinaPerspective currentPerspective;
    
    HashMap<KahinaView<?>, KahinaWindow> contentWindows;
    HashMap<KahinaView<?>, KahinaWindow> topLevelWindows;
    
    KahinaGUI gui;
    
    KahinaController control;
    
    public KahinaWindowManager(KahinaGUI gui, KahinaController control)
    {
        this.gui = gui;  
        this.control = control;
        
        this.currentPerspective = new KahinaPerspective("default", "Default", gui.views);
        
        this.contentWindows = new HashMap<KahinaView<?>, KahinaWindow>();
        this.topLevelWindows = new HashMap<KahinaView<?>, KahinaWindow>();
        
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
        
        mainWindow = createMainWindow(this, control, gui.kahina);
        
		control.registerListener(KahinaEventTypes.PERSPECTIVE, this);
		control.registerListener(KahinaEventTypes.WINDOW, this);
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
		else if (e instanceof KahinaWindowEvent)
		{
			processWindowEvent((KahinaWindowEvent) e);
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
	
	private void processWindowEvent(KahinaWindowEvent e)
	{
		int type = e.getWindowEventType();
		if (type == KahinaWindowEventType.NEW_DEFAULT)
		{
			KahinaView view = new KahinaEmptyView(control);
	        KahinaWindow viewWindow = new KahinaDefaultWindow(view, control);
	        contentWindows.put(view,viewWindow);
	        topLevelWindows.put(view,viewWindow);
            viewWindow.setSize(300,100);
            viewWindow.setLocation(200,200);
	        viewWindow.setVisible(true);
		} 
		else if (type == KahinaWindowEventType.NEW_HORI_SPLIT)
		{
			KahinaView lView = new KahinaEmptyView(control);
			KahinaView rView = new KahinaEmptyView(control);
			KahinaWindow lViewWindow = new KahinaDefaultWindow(lView, control);
	        KahinaWindow rViewWindow = new KahinaDefaultWindow(rView, control);
	        KahinaHorizontallySplitWindow splitWindow = new KahinaHorizontallySplitWindow();
	        splitWindow.setTitle("New Horizontal Split");
	        splitWindow.setLeftWindow(lViewWindow);
	        splitWindow.setRightWindow(rViewWindow);
	        topLevelWindows.put(lView,splitWindow);
	        topLevelWindows.put(rView,splitWindow);
            splitWindow.setSize(600,150);
            splitWindow.setLocation(200,200);
	        splitWindow.setVisible(true);
		} 
		else if (type == KahinaWindowEventType.NEW_VERT_SPLIT)
		{
			KahinaView tView = new KahinaEmptyView(control);
			KahinaView bView = new KahinaEmptyView(control);
			KahinaWindow tViewWindow = new KahinaDefaultWindow(tView, control);
	        KahinaWindow bViewWindow = new KahinaDefaultWindow(bView, control);
	        KahinaVerticallySplitWindow splitWindow = new KahinaVerticallySplitWindow();
	        splitWindow.setTitle("New Vertical Split");
	        splitWindow.setUpperWindow(tViewWindow);
	        splitWindow.setLowerWindow(bViewWindow);
	        topLevelWindows.put(tView,splitWindow);
	        topLevelWindows.put(bView,splitWindow);
            splitWindow.setSize(300,250);
            splitWindow.setLocation(200,200);
	        splitWindow.setVisible(true);
		} 
		else if (type == KahinaWindowEventType.NEW_TABBED)
		{
			KahinaView view = new KahinaEmptyView(control);
			KahinaWindow viewWindow = new KahinaDefaultWindow(view, control);
	        KahinaTabbedWindow tabbedWindow = new KahinaTabbedWindow();
	        tabbedWindow.setTitle("New Tabbed Window");
	        tabbedWindow.addWindow(viewWindow);
            tabbedWindow.setSize(300,250);
            tabbedWindow.setLocation(200,200);
	        tabbedWindow.setVisible(true);
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
