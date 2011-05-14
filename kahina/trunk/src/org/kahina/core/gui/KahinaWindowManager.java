package org.kahina.core.gui;

import java.awt.Toolkit;
import java.io.File;
import java.io.FileFilter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

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
    
    KahinaPerspective psp;
    KahinaArrangement arr;
    
    //store recent perspectives and cache default perspectives
    List<KahinaPerspective> recentPerspectives;
    List<KahinaPerspective> defaultPerspectives;
    
    //main registry for windows: access windows by their windowID
    private HashMap<Integer,KahinaWindow> windowByID;
    
    KahinaGUI gui;
    
    KahinaController control;
    
    public KahinaWindowManager(KahinaGUI gui, KahinaController control)
    {
        this.gui = gui;  
        this.control = control;
		control.registerListener(KahinaEventTypes.PERSPECTIVE, this);
		control.registerListener(KahinaEventTypes.WINDOW, this);
		
        this.windowByID = new HashMap<Integer,KahinaWindow>();
        
        recentPerspectives = new LinkedList<KahinaPerspective>();
        //load the default perspectives in the bin folder of the respective KahinaGUI instance
        defaultPerspectives = new LinkedList<KahinaPerspective>();
        // This filter only returns XML files
        FileFilter fileFilter = new FileFilter() 
        {
            public boolean accept(File file) 
            {
            	//System.err.println("Filtering file " + file.getName() + ": " + file.getName().endsWith("xml"));
                return file.getName().endsWith("xml");
            }
        };
        File[] files = new File(gui.getClass().getResource(".").getFile()).listFiles(fileFilter);
        for (File f : files)
        {
        	System.err.println("Loading default perspective: " + f.getAbsolutePath());
        	defaultPerspectives.add(loadPerspective(f));
        }
    }
    
    /**
     * Builds the windows according to some perspective. Must be called before first display.
     */
    public void createWindows(KahinaPerspective psp)
    {	
    	registerRecentPerspective(psp);
        this.psp = psp;     
        this.arr = psp.getArrangement();
        
		//first create a window stub for all the windows mentioned in the arrangement... 
        for (int winID : arr.getAllWindows())
        {
        	String binding = arr.getBindingForWinID(winID);
        	//if it has a binding, it is primary or a dynamic clone, so we construct a default window for it
        	if (binding != null)
        	{
        		if (!binding.equals("main"))
        		{
        			KahinaView<?> view = gui.varNameToView.get(binding);
        			System.err.println("Generating view " + winID + " for binding " + binding + " (primary window: " + arr.getPrimaryWinIDForName(binding) + ")");
        			KahinaWindow viewWindow = new KahinaDefaultWindow(view, this, winID);
        			viewWindow.setTitle(arr.getTitle(winID));
        		}
        	}
            //otherwise build stubs according to the type of embedding window
        	else
        	{
        		switch (arr.getWindowType(winID))
        		{
        			case KahinaWindowType.HORI_SPLIT_WINDOW:
        			{
                		KahinaWindow viewWindow = new KahinaHorizontallySplitWindow(this, winID);
                		viewWindow.setTitle(arr.getTitle(winID));
                		break;
        			}
        			case KahinaWindowType.VERT_SPLIT_WINDOW:
        			{
                		KahinaWindow viewWindow = new KahinaVerticallySplitWindow(this, winID);
                		viewWindow.setTitle(arr.getTitle(winID));
                		break;
        			}
        			case KahinaWindowType.TABBED_WINDOW:
        			{
                		KahinaWindow viewWindow = new KahinaTabbedWindow(this, winID);
                		viewWindow.setTitle(arr.getTitle(winID));
                		break;
        			}
        			default:
        			{
        				System.err.println("WARNING: Could not load default window without binding!");
        				System.err.println("         The perspective might contain descriptions of snapshot clones.");
        			}
        		}
        	}
        }
        
        //... then process the embedding structure ...
        for (int winID : arr.getAllWindows())
        {
        	Integer embeddingID = arr.getEmbeddingWindowID(winID);
        	System.err.println("Embedding window " + winID + " into window " + embeddingID);
        	if (embeddingID != null && embeddingID != -1)
        	{
            	System.err.println("Embedding window " + winID + " into window " + embeddingID);
        		boolean success = windowByID.get(embeddingID).addSubwindow(windowByID.get(winID));
        		if (!success)
        		{
        			System.err.println("ERROR: ill-defined window arrangement directly under window " + embeddingID );
        		}
        	}
        }
        
        //... then adapt the coordinates ...     
        for (int winID : arr.getTopLevelWindowsWithoutMainWindow())
        {
        	System.err.println("Setting coordinates of top level window " + winID);
        	
        	KahinaWindow w = getWindowByID(winID);
            w.setSize(arr.getWidth(w.getID()), arr.getHeight(w.getID()));
            w.setLocation(arr.getXPos(w.getID()), arr.getYPos(w.getID()));
        }
        
        //... fill the default windows with the content specified by the bindings ... 
        for (int winID : arr.getDefaultWindows())
        {    
        	//apply configuration as defined by the perspective to the view
            //TODO: also define the main window as a "view" for a more unified treatment
            String binding = arr.getBindingForWinID(winID);
            if (!binding.equals("main"))
            {
            	//TODO: this calls the generic setConfig()-method, instead of the specific overloaded versions
            	//the more specific config eclipses the one we set; we seem to need reflection here as well
            	//! better: overload and check for correct types in each implementation
            	//TODO: allow different configurations (and therefore different views) for clones
            	gui.varNameToView.get(binding).setConfig(psp.getConfiguration(winID));
            }
        }
        
        //... and finally create and register the main window (must be last because the view menu needs to be filled)
        mainWindow = createMainWindow(this, control, gui.kahina, arr.getPrimaryWinIDForName("main"));
        arr.setPrimaryWindow("main", mainWindow.getID());
        mainWindow.setSize(arr.getWidth(mainWindow.getID()), arr.getHeight(mainWindow.getID()));
        mainWindow.setLocation(arr.getXPos(mainWindow.getID()), arr.getYPos(mainWindow.getID()));
    }
    
    /**
     * Discards the current perspective and rebuilds the GUI according to a newly provided one.
     * @param psp the perspective to be applied
     */
    public void setAndApplyPerspective(KahinaPerspective psp)
    {  
    	disposeAllWindows();
        windowByID.clear();
        createWindows(psp);
        displayWindows();
    }
    
    public void registerWindow(KahinaWindow window)
    {
    	 windowByID.put(window.getID(),window);
    	 arr.setWindowType(window.getID(), window.getWindowType());
    }
    
    public KahinaWindow getWindowByID(int winID)
    {
    	return windowByID.get(winID);
    }
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, KahinaController control, KahinaInstance<?, ?, ?> kahina)
	{
		return new KahinaMainWindow(this, control, gui.kahina);
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, KahinaController control, KahinaInstance<?, ?, ?> kahina, int winID)
	{
		return new KahinaMainWindow(this, control, gui.kahina, winID);
	}

	public void disposeAllWindows()
    {
		//TODO: this should be done a little more carefully
        for (int windowID : arr.getTopLevelWindows())
        {
            getWindowByID(windowID).dispose();
        }
        mainWindow.dispose();
    }
	
	public boolean isTopLevelWindow(KahinaWindow w)
	{
		return (arr.getEmbeddingWindowID(w.getID()) == -1);
	}
    
    public KahinaWindow integrateInDefaultWindow(KahinaView<?> view)
    {
        KahinaWindow viewWindow = new KahinaDefaultWindow(view, this);
        viewWindow.setTitle(view.getTitle());
        psp.arr.setEmbeddingWindowID(viewWindow.getID(),-1);
        return viewWindow;
    }
    
    public void integrateInVerticallySplitWindow(int window1ID, int window2ID, String newTitle, KahinaController control)
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
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_VERT_SPLIT, -1, newTitle));
        KahinaVerticallySplitWindow splitWindow = (KahinaVerticallySplitWindow) windowByID.get(newTitle);
        splitWindow.setUpperWindow(wrapperWindow1);
        splitWindow.setLowerWindow(wrapperWindow2);
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, window1ID));
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, window2ID));
    }
    
    public void integrateInHorizontallySplitWindow(int window1ID, int window2ID, String newTitle, KahinaController control)
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
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_HORI_SPLIT, -1, newTitle));
        KahinaHorizontallySplitWindow splitWindow = (KahinaHorizontallySplitWindow) windowByID.get(newTitle);
        splitWindow.setLeftWindow(wrapperWindow1);
        splitWindow.setRightWindow(wrapperWindow2);
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, window1ID));
        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, window2ID));
    }
    
    public void displayWindows()
    {
    	if (psp == null)
    	{
    		System.err.println("No perspective defined, createWindows() was probably not called.");
    		System.err.println("A default perspective can be generated via KahinaPerspective.generateDefaultPerspective()");
    		System.err.println("Unable to display windows, quitting.");
    		System.exit(1);
    	}
    	else
    	{
    		mainWindow.setVisible(true);
    		for (int winID : arr.getTopLevelWindows())
    		{
    			getWindowByID(winID).setVisible(psp.isVisible(winID));
    		}
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
			setAndApplyPerspective(loadPerspective(e.getFile()));
		}
		else if (type == KahinaPerspectiveEvent.LOAD_RECENT_PERSPECTIVE)
		{
			setAndApplyPerspective(recentPerspectives.get(e.getID()));
		}
		else if (type == KahinaPerspectiveEvent.LOAD_DEFAULT_PERSPECTIVE)
		{
			setAndApplyPerspective(defaultPerspectives.get(e.getID()));
		}
	}
	
	private void processWindowEvent(KahinaWindowEvent e)
	{
		int type = e.getWindowEventType();
		if (type == KahinaWindowEventType.NEW_DEFAULT)
		{
	        KahinaWindow viewWindow = new KahinaDummyWindow(this);
	        viewWindow.setTitle(e.getStringContent());
            viewWindow.setSize(300,100);
            viewWindow.setLocation(200,200);
	        viewWindow.setVisible(true);
	        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.ADD_VIEW_MENU_ENTRY, viewWindow.getID()));
		} 
		else if (type == KahinaWindowEventType.NEW_HORI_SPLIT)
		{
	        KahinaHorizontallySplitWindow splitWindow = new KahinaHorizontallySplitWindow(this);
	        splitWindow.setTitle(e.getStringContent());
	        splitWindow.setLeftWindow(new KahinaDummyWindow(this));
	        splitWindow.setRightWindow(new KahinaDummyWindow(this));
            splitWindow.setSize(600,150);
            splitWindow.setLocation(200,200);
	        splitWindow.setVisible(true);
	        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.ADD_VIEW_MENU_ENTRY, splitWindow.getID()));
		} 
		else if (type == KahinaWindowEventType.NEW_VERT_SPLIT)
		{
	        KahinaVerticallySplitWindow splitWindow = new KahinaVerticallySplitWindow(this);
	        splitWindow.setTitle(e.getStringContent());
	        splitWindow.setUpperWindow(new KahinaDummyWindow(this));
	        splitWindow.setLowerWindow(new KahinaDummyWindow(this));
            splitWindow.setSize(300,250);
            splitWindow.setLocation(200,200);
	        splitWindow.setVisible(true);
	        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.ADD_VIEW_MENU_ENTRY, splitWindow.getID()));
		} 
		else if (type == KahinaWindowEventType.NEW_TABBED)
		{
	        KahinaTabbedWindow tabbedWindow = new KahinaTabbedWindow(this);
	        tabbedWindow.setTitle(e.getStringContent());
	        tabbedWindow.addSubwindow(new KahinaDummyWindow(this));
            tabbedWindow.setSize(300,250);
            tabbedWindow.setLocation(200,200);
	        tabbedWindow.setVisible(true);
	        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.ADD_VIEW_MENU_ENTRY, tabbedWindow.getID()));
		} 
		else if (type == KahinaWindowEventType.TOGGLE_VISIBLE)
		{
			if (arr.getEmbeddingWindowID(e.getWindowID()) == -1)
			{
				psp.toggleVisibility(e.getWindowID());
				KahinaWindow window = windowByID.get(e.getWindowID());
				if (window == null)
				{
					System.err.println("WARNING: could not find window \"" + e.getWindowID() + "\"");
				}
				else
				{
					window.setVisible(psp.isVisible(e.getWindowID()));
					if (!window.isVisible()) window.dispose();
				}
			}
			else
			{
				System.err.println("WARNING: cannot hide/show non-top-level window \"" + e.getWindowID() + "\"");
			}
		} 
		else if (type == KahinaWindowEventType.REMOVE)
		{
			psp.setVisibility(e.getWindowID(), false);
			KahinaWindow window = windowByID.get(e.getWindowID());
			window.dispose();
		} 
		else if (type == KahinaWindowEventType.DISPOSE)
		{
			control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.UNDOCK, e.getWindowID()));
			control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, e.getWindowID()));
			windowByID.remove(e.getWindowID());
		} 
		else if (type == KahinaWindowEventType.RENAME)
		{
			KahinaWindow window = windowByID.get(e.getWindowID());
			if (window == null)
			{
				System.err.println("WARNING: Could not find window \"" + e.getWindowID() + "\".");
			}
			else
			{
				//TODO: switch titles of clones as well; let clones always have identical title + " (clone)"
				window.setTitle(e.getStringContent());
				window.mainPanel.repaint();
			}
		} 
		else if (type == KahinaWindowEventType.FLIP)
		{
			KahinaWindow window = windowByID.get(e.getWindowID());
			if (window == null)
			{
				System.err.println("WARNING: Could not find window \"" + e.getWindowID() + "\".");
			}
			else
			{
				if (window.isFlippableWindow())
				{
					window.flipSubwindows();
					window.mainPanel.repaint();
				}
				else
				{
					System.err.println("WARNING: Window \"" + e.getWindowID() + "\" is not flippable. Ignored.");
				}
			}
		} 
		else if (type == KahinaWindowEventType.DYNAMIC_CLONE)
		{
			KahinaWindow window = windowByID.get(e.getWindowID());
			if (window == null)
			{
				System.err.println("WARNING: Could not find window \"" + e.getWindowID() + "\".");
			}
			else
			{
				KahinaWindow cloneWindow = window.createDynamicClone();
		        cloneWindow.setVisible(true);
		        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.ADD_VIEW_MENU_ENTRY, cloneWindow.getID()));
			}
		} 
		else if (type == KahinaWindowEventType.SNAPSHOT_CLONE)
		{
			KahinaWindow window = windowByID.get(e.getWindowID());
			if (window == null)
			{
				System.err.println("WARNING: Could not find window \"" + e.getWindowID() + "\".");
			}
			else
			{
				KahinaWindow cloneWindow = window.createSnapshotClone();
		        cloneWindow.setVisible(true);
		        control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.ADD_VIEW_MENU_ENTRY, cloneWindow.getID()));
			}
		} 
		else if (type == KahinaWindowEventType.UNDOCK)
		{
			KahinaWindow window = windowByID.get(e.getWindowID());
			if (window == null)
			{
				System.err.println("WARNING: Could not find window \"" + e.getWindowID() + "\".");
			}
			else
			{
				KahinaWindow embeddingWindow = window.getEmbeddingWindow();
				if (embeddingWindow == null)
				{
					//warning deactivated because undocking is used generically for drag & drop functionality
					//System.err.println("WARNING: Window \"" + e.getWindowID() + "\" cannot be undocked, as is not embedded.");
				}
				//now comes the interesting case
				else
				{
					//let the embeddingWindow release the window and provide an appropriate replacement
					KahinaWindow replacementWindow = embeddingWindow.getReplacementAfterRelease(window);
					//simpler case: embeddingWindow was embedded
					if (!embeddingWindow.isTopLevelWindow())
					{
						KahinaWindow embEmbeddingWindow = embeddingWindow.getEmbeddingWindow();
						embEmbeddingWindow.replaceSubwindow(embeddingWindow,replacementWindow);
						embEmbeddingWindow.validate();
						embEmbeddingWindow.repaint();
					}
					//complicated case: embeddingWindow was top level window
					else
					{
						control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, embeddingWindow.getID()));
						replacementWindow.setVisible(true);
						control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.ADD_VIEW_MENU_ENTRY, replacementWindow.getID()));
					}
					windowByID.remove(embeddingWindow.getID());
					//register and display the undocked window
					psp.setVisibility(e.getWindowID(), true);
					window.setVisible(true);
				}
			}
		} 
		else if (type == KahinaWindowEventType.VERT_SPLIT)
		{
			System.err.println("Window operation: vertical split of window " + e.getWindowID());
			KahinaWindow window = windowByID.get(e.getWindowID());
			if (window == null)
			{
				System.err.println("WARNING: Could not find window \"" + e.getWindowID() + "\".");
			}
			else
			{
				KahinaWindow oldEmbeddingWindow = window.getEmbeddingWindow();
				KahinaVerticallySplitWindow splitWindow = new KahinaVerticallySplitWindow(this);
		        splitWindow.setTitle(e.getStringContent());
		        splitWindow.setUpperWindow(window);
		        splitWindow.setLowerWindow(new KahinaDummyWindow(this));
	            splitWindow.setSize(window.getWidth(),window.getHeight());
	            splitWindow.setLocation(window.getLocation());
	            window.setSize(window.getWidth(),window.getHeight() / 2);
				if (oldEmbeddingWindow != null)
				{
					oldEmbeddingWindow.replaceSubwindow(window,splitWindow);
					arr.setEmbeddingWindowID(window.getID(),splitWindow.getID());
					oldEmbeddingWindow.validate();
					oldEmbeddingWindow.repaint();
				}
				else
				{
					control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, e.getWindowID()));
					splitWindow.setVisible(true);
					control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.ADD_VIEW_MENU_ENTRY, splitWindow.getID()));
				}
			}
		} 
		else if (type == KahinaWindowEventType.HORI_SPLIT)
		{
			KahinaWindow window = windowByID.get(e.getWindowID());
			if (window == null)
			{
				System.err.println("WARNING: Could not find window \"" + e.getWindowID() + "\".");
			}
			else
			{
				KahinaWindow oldEmbeddingWindow = window.getEmbeddingWindow();
				KahinaHorizontallySplitWindow splitWindow = new KahinaHorizontallySplitWindow(this);
		        splitWindow.setTitle(e.getStringContent());
		        splitWindow.setLeftWindow(window);
		        splitWindow.setRightWindow(new KahinaDummyWindow(this));
	            splitWindow.setSize(window.getWidth(),window.getHeight());
	            splitWindow.setLocation(window.getLocation());
	            window.setSize(window.getWidth(),window.getHeight() / 2);
				if (oldEmbeddingWindow != null)
				{
					oldEmbeddingWindow.replaceSubwindow(window,splitWindow);
					arr.setEmbeddingWindowID(window.getID(),splitWindow.getID());
					oldEmbeddingWindow.validate();
					oldEmbeddingWindow.repaint();
				}
				else
				{
					control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.REMOVE, e.getWindowID()));
					splitWindow.setVisible(true);
					control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.ADD_VIEW_MENU_ENTRY, splitWindow.getID()));
				}
			}
		}
	}
	
	private KahinaPerspective loadPerspective(File file)
	{
		return KahinaPerspective.importXML(XMLUtilities.parseXMLFile(file, false).getDocumentElement());
	}
	
	//by default, 5 recent perspectives are kept in memory
	private void registerRecentPerspective(KahinaPerspective psp)
	{
		recentPerspectives.add(0,psp);
		if (recentPerspectives.size() > 5)
		{
			recentPerspectives.remove(5);
		}
	}
	
	private void savePerspectiveAs(File file)
	{
		Node node = psp.exportXML(XMLUtilities.newEmptyDocument());
		XMLUtilities.writeXML(node,file.getAbsolutePath());
	}
}
