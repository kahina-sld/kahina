package org.kahina.core.gui;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;

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
    
    //main registry for windows: access windows by their title = windowID
    private HashMap<String,KahinaWindow> windowByID;
    HashSet<String> topLevelWindows;
    
    HashMap<KahinaView<?>, KahinaWindow> contentWindows;
    //HashMap<KahinaView<?>, KahinaWindow> topLevelWindows;
    
    KahinaGUI gui;
    
    KahinaController control;
    
    public KahinaWindowManager(KahinaGUI gui, KahinaController control)
    {
        this.gui = gui;  
        this.control = control;
		control.registerListener(KahinaEventTypes.PERSPECTIVE, this);
		control.registerListener(KahinaEventTypes.WINDOW, this);
        
        this.currentPerspective = new KahinaPerspective("default", "Default", gui.views);
        
        this.contentWindows = new HashMap<KahinaView<?>, KahinaWindow>();
        
        this.windowByID = new HashMap<String,KahinaWindow>();
        this.topLevelWindows = new HashSet<String>();
        
        //create windows for all the other registered views
        KahinaArrangement arr = currentPerspective.getArrangement();
        for (KahinaView<?> view : gui.views)
        {
        	String viewID = view.getTitle();
        	System.err.println("Generating view: " + viewID);
        	//for now, generate the perspective from the predefined configurations
        	currentPerspective.setConfiguration(view.getTitle(), view.getConfig());
        	
            KahinaWindow viewWindow = integrateInDefaultWindow(view);
            viewWindow.setSize(arr.getWidth(viewID), arr.getHeight(viewID));
            viewWindow.setLocation(arr.getXPos(viewID), arr.getYPos(viewID));
        }
        
        mainWindow = createMainWindow(this, control, gui.kahina);
        windowByID.put("main", mainWindow);
        //TODO: think about special treatment for main window
        //topLevelWindowByID.add("main");
    }
    
    public KahinaWindow getWindowByID(String windowID)
    {
    	return windowByID.get(windowID);
    }
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, KahinaController control, KahinaInstance<?, ?, ?> kahina)
	{
		return new KahinaMainWindow(this, control, gui.kahina);
	}

	public void disposeAllWindows()
    {
        for (String windowID : topLevelWindows)
        {
            getWindowByID(windowID).dispose();
        }
        mainWindow.dispose();
    }
	
	public boolean isTopLevelWindow(KahinaWindow w)
	{
		return topLevelWindows.contains(w.getTitle());
	}
    
    public KahinaWindow integrateInDefaultWindow(KahinaView<?> view)
    {
        KahinaWindow viewWindow = new KahinaDefaultWindow(view, this);
        viewWindow.setTitle(view.getTitle());
        topLevelWindows.add(view.getTitle());
        windowByID.put(view.getTitle(), viewWindow);
        contentWindows.put(view,viewWindow);
        return viewWindow;
    }
    
    public void integrateInVerticallySplitWindow(String window1ID, String window2ID, String newTitle, KahinaController control)
    {
        KahinaWindow wrapperWindow1 = windowByID.get(window1ID);
        if (wrapperWindow1 == null)
        {
            wrapperWindow1 =  new KahinaDefaultWindow(new KahinaEmptyView(control), this);
            System.err.println("WARNING: split window could not access window \"" + window1ID + "\"");
        }
        KahinaWindow wrapperWindow2 = windowByID.get(window2ID);
        if (wrapperWindow2 == null)
        {
        	wrapperWindow2 =  new KahinaDefaultWindow(new KahinaEmptyView(control), this);
            System.err.println("WARNING: split window could not access window \"" + window1ID + "\"");
        }
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_VERT_SPLIT, newTitle));
        KahinaVerticallySplitWindow splitWindow = (KahinaVerticallySplitWindow) windowByID.get(newTitle);
        splitWindow.setUpperWindow(wrapperWindow1);
        splitWindow.setLowerWindow(wrapperWindow2);
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, window1ID));
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, window2ID));
    }
    
    public void integrateInHorizontallySplitWindow(String window1ID, String window2ID, String newTitle, KahinaController control)
    {
        KahinaWindow wrapperWindow1 = windowByID.get(window1ID);
        if (wrapperWindow1 == null)
        {
            wrapperWindow1 =  new KahinaDefaultWindow(new KahinaEmptyView(control), this);
            System.err.println("WARNING: split window could not access window \"" + window1ID + "\"");
        }
        KahinaWindow wrapperWindow2 = windowByID.get(window2ID);
        if (wrapperWindow2 == null)
        {
        	wrapperWindow2 =  new KahinaDefaultWindow(new KahinaEmptyView(control), this);
            System.err.println("WARNING: split window could not access window \"" + window2ID + "\"");
        }
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_HORI_SPLIT, newTitle));
        KahinaHorizontallySplitWindow splitWindow = (KahinaHorizontallySplitWindow) windowByID.get(newTitle);
        splitWindow.setLeftWindow(wrapperWindow1);
        splitWindow.setRightWindow(wrapperWindow2);
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, window1ID));
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, window2ID));
    }
    
    public void displayWindows()
    {
    	mainWindow.setVisible(true);
        for (String winID : topLevelWindows)
        {
            getWindowByID(winID).setVisible(currentPerspective.isVisible(winID));
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
	        KahinaWindow viewWindow = new KahinaDefaultWindow(view, this);
	        topLevelWindows.add(e.getWindowID());
	        windowByID.put(e.getWindowID(), viewWindow);
	        contentWindows.put(view,viewWindow);
	        viewWindow.setTitle(e.getWindowID());
            viewWindow.setSize(300,100);
            viewWindow.setLocation(200,200);
	        viewWindow.setVisible(true);
		} 
		else if (type == KahinaWindowEventType.NEW_HORI_SPLIT)
		{
			KahinaView lView = new KahinaEmptyView(control);
			KahinaView rView = new KahinaEmptyView(control);
			KahinaWindow lViewWindow = new KahinaDefaultWindow(lView, this);
	        KahinaWindow rViewWindow = new KahinaDefaultWindow(rView, this);
	        KahinaHorizontallySplitWindow splitWindow = new KahinaHorizontallySplitWindow(this);
	        topLevelWindows.add(e.getWindowID());
	        windowByID.put(e.getWindowID(), splitWindow);
	        splitWindow.setTitle(e.getWindowID());
	        splitWindow.setLeftWindow(lViewWindow);
	        splitWindow.setRightWindow(rViewWindow);
            splitWindow.setSize(600,150);
            splitWindow.setLocation(200,200);
	        splitWindow.setVisible(true);
		} 
		else if (type == KahinaWindowEventType.NEW_VERT_SPLIT)
		{
			KahinaView tView = new KahinaEmptyView(control);
			KahinaView bView = new KahinaEmptyView(control);
			KahinaWindow tViewWindow = new KahinaDefaultWindow(tView, this);
	        KahinaWindow bViewWindow = new KahinaDefaultWindow(bView, this);
	        KahinaVerticallySplitWindow splitWindow = new KahinaVerticallySplitWindow(this);
	        topLevelWindows.add(e.getWindowID());
	        windowByID.put(e.getWindowID(), splitWindow);
	        splitWindow.setTitle(e.getWindowID());
	        splitWindow.setUpperWindow(tViewWindow);
	        splitWindow.setLowerWindow(bViewWindow);
            splitWindow.setSize(300,250);
            splitWindow.setLocation(200,200);
	        splitWindow.setVisible(true);
		} 
		else if (type == KahinaWindowEventType.NEW_TABBED)
		{
			KahinaView view = new KahinaEmptyView(control);
			KahinaWindow viewWindow = new KahinaDefaultWindow(view, this);
	        KahinaTabbedWindow tabbedWindow = new KahinaTabbedWindow(this);
	        topLevelWindows.add(e.getWindowID());
	        windowByID.put(e.getWindowID(), tabbedWindow);
	        tabbedWindow.setTitle(e.getWindowID());
	        tabbedWindow.addWindow(viewWindow);
            tabbedWindow.setSize(300,250);
            tabbedWindow.setLocation(200,200);
	        tabbedWindow.setVisible(true);
		} 
		else if (type == KahinaWindowEventType.TOGGLE_VISIBLE)
		{
			if (topLevelWindows.contains(e.getWindowID()))
			{
				currentPerspective.toggleVisibility(e.getWindowID());
				KahinaWindow window = windowByID.get(e.getWindowID());
				if (window == null)
				{
					System.err.println("WARNING: could not find window \"" + e.getWindowID() + "\"");
				}
				else
				{
					window.setVisible(currentPerspective.isVisible(e.getWindowID()));
				}
			}
			else
			{
				System.err.println("WARNING: cannot hide/show non-top-level window \"" + e.getWindowID() + "\"");
			}
		} 
		else if (type == KahinaWindowEventType.REMOVE)
		{
			if (topLevelWindows.contains(e.getWindowID()))
			{
				currentPerspective.setVisibility(e.getWindowID(), false);
				KahinaWindow window = windowByID.remove(e.getWindowID());
				topLevelWindows.remove(e.getWindowID());
				if (window == null)
				{
					System.err.println("WARNING: could not find window \"" + e.getWindowID() + "\"");
				}
				else
				{
					window.dispose();
				}
			}
			else
			{
				//TODO: implement removal of non-top-level windows
				System.err.println("WARNING: removal of non-top-level windows not yet implemented!");
			}
		} 
		else if (type == KahinaWindowEventType.RENAME)
		{
			String[] winIDs = e.getWindowID().split("#");
			KahinaWindow window = windowByID.remove(winIDs[0]);
			if (window == null)
			{
				System.err.println("WARNING: could not find window \"" + winIDs[0] + "\"");
			}
			else
			{
				window.setTitle(winIDs[1]);
				windowByID.put(winIDs[1], window);
			}
			if (topLevelWindows.contains(winIDs[0]))
			{
				topLevelWindows.remove(winIDs[0]);
				topLevelWindows.add(winIDs[1]);
			}
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
