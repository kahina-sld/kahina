package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.HashMap;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaPerspectiveEvent;
import org.kahina.core.event.KahinaWindowEvent;
import org.kahina.core.event.KahinaWindowEventType;
import org.kahina.core.visual.KahinaEmptyView;
import org.kahina.core.visual.KahinaView;

public class KahinaViewMenu  extends JMenu implements ActionListener, KahinaListener
{
	private static final long serialVersionUID = -8816851369583949953L;
	
    HashMap<String,JCheckBoxMenuItem> windowEntries;
    KahinaWindowManager manager;

	public KahinaViewMenu(KahinaWindowManager manager)
    {
        super("View"); 
        
        windowEntries = new HashMap<String,JCheckBoxMenuItem>();
        this.manager = manager;
        
        manager.control.registerListener(KahinaEventTypes.WINDOW, this);
        
        for (String winID : manager.topLevelWindows)
        {
        	KahinaWindow window = manager.getWindowByID(winID);
            JCheckBoxMenuItem windowCheckBoxItem = new JCheckBoxMenuItem(window.getTitle());
            windowCheckBoxItem.setActionCommand("toggleVisibility:" + window.getTitle());
            windowCheckBoxItem.addActionListener(this);
            windowCheckBoxItem.setSelected(manager.currentPerspective.isVisible(window.getTitle()));
            windowEntries.put(window.getTitle(), windowCheckBoxItem);
            this.add(windowCheckBoxItem);
        }
        
        this.addSeparator();
        
        JMenuItem newDefaultWindowItem = new JMenuItem("New Default Window");
        newDefaultWindowItem.setActionCommand("newDefaultWindow");
        newDefaultWindowItem.addActionListener(this);
        this.add(newDefaultWindowItem);
          
        JMenuItem newVertSplitWindowItem = new JMenuItem("New Vertically Split Window");
        newVertSplitWindowItem.setActionCommand("newVertSplitWindow");
        newVertSplitWindowItem.addActionListener(this);
        this.add(newVertSplitWindowItem);
        
        JMenuItem newHoriSplitWindowItem = new JMenuItem("New Horizontally Split Window");
        newHoriSplitWindowItem.setActionCommand("newHoriSplitWindow");
        newHoriSplitWindowItem.addActionListener(this);
        this.add(newHoriSplitWindowItem);
        
        JMenuItem newTabbedWindowItem = new JMenuItem("New Tabbed Window");
        newTabbedWindowItem.setActionCommand("newTabbedWindow");
        newTabbedWindowItem.addActionListener(this);
        this.add(newTabbedWindowItem);

        this.addSeparator();
        
        JMenuItem loadPerspectiveItem = new JMenuItem("Load Perspective ...");
        loadPerspectiveItem.setActionCommand("loadPerspective");
        loadPerspectiveItem.addActionListener(this);
        this.add(loadPerspectiveItem);
            
        JMenuItem savePerspectiveItem = new JMenuItem("Save Perspective As...");
        savePerspectiveItem.setActionCommand("savePerspectiveAs");
        savePerspectiveItem.addActionListener(this);
        this.add(savePerspectiveItem);
        
        //TODO: add "Change Perspective" menu with all the perspectives defined for the application
    }
	
	@Override
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("newDefaultWindow"))
        {	
        	String title = getNewUniqueTitle();
            KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_DEFAULT, title));
        }
        else if (s.equals("newVertSplitWindow"))
        {	
        	String title = getNewUniqueTitle();
            KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_VERT_SPLIT, title));
        }
        else if (s.equals("newHoriSplitWindow"))
        {	
        	String title = getNewUniqueTitle();
            KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_HORI_SPLIT, title));
        }
        else if (s.equals("newTabbedWindow"))
        {	
        	String title = getNewUniqueTitle();
            KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_TABBED, title));
        }
        else if (s.startsWith("toggleVisibility"))
        {
            String windowID = s.substring(s.indexOf(':') + 1);
            KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.TOGGLE_VISIBLE, windowID));
        }
        else if (s.equals("loadPerspective"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Load Perspective");
            chooser.showOpenDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null) KahinaRunner.processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.LOAD_PERSPECTIVE, dataFile));
        }
        else if (s.equals("savePerspectiveAs"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Save Perspective As");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null)  KahinaRunner.processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.SAVE_PERSPECTIVE, dataFile));
        }
    }
	
	private String getNewUniqueTitle()
	{
		String title;
    	while (true)
    	{
    		title = (String) JOptionPane.showInputDialog(this,
                "Enter a unique title for the new window.",
                "New Default Window",
                JOptionPane.PLAIN_MESSAGE);
    		if (manager.getWindowByID(title) != null)
    		{
    			JOptionPane.showMessageDialog(this,
    				    "A window with that name already exists.", 
    				    "Uniqueness Enforcement",JOptionPane.WARNING_MESSAGE);
    		}
    		else
    		{
    			break;
    		}
    	}
    	return title;
	}
	
	public void processEvent(KahinaEvent e)
	{
		if (e instanceof KahinaWindowEvent)
		{
			processWindowEvent((KahinaWindowEvent) e);
		}
	}
	
	private void processWindowEvent(KahinaWindowEvent e)
	{
		int type = e.getWindowEventType();
		//handle generation of different types of new windows
		if (type >= 0 && type <= 3)
		{
			JCheckBoxMenuItem windowCheckBoxItem = new JCheckBoxMenuItem(e.getWindowID());
            windowCheckBoxItem.setActionCommand("toggleVisibility:" + e.getWindowID());
            windowCheckBoxItem.addActionListener(this);
            windowCheckBoxItem.setSelected(manager.currentPerspective.isVisible(e.getWindowID()));
            windowEntries.put(e.getWindowID(), windowCheckBoxItem);
            this.add(windowCheckBoxItem,0);
		} 
		else if (type == KahinaWindowEventType.TOGGLE_VISIBLE)
		{
			//react to a window that is being closed
			boolean newVisibility = manager.currentPerspective.isVisible(e.getWindowID());
			JCheckBoxMenuItem windowCheckBoxItem = windowEntries.get(e.getWindowID());
			if (windowCheckBoxItem == null)
			{
				System.err.println("WARNING: no check box item for window \"" + e.getWindowID() + "\"");
			}
			else
			{
				windowCheckBoxItem.setSelected(newVisibility);
			}
		}
		else if (type == KahinaWindowEventType.REMOVE)
		{
			JCheckBoxMenuItem windowCheckBoxItem = windowEntries.get(e.getWindowID());
			if (windowCheckBoxItem == null)
			{
				System.err.println("WARNING: no check box item for window \"" + e.getWindowID() + "\"");
			}
			else
			{
				this.remove(windowCheckBoxItem);
			}
		} 
		else if (type == KahinaWindowEventType.RENAME)
		{
			String[] winIDs = e.getWindowID().split("#");
			JCheckBoxMenuItem windowCheckBoxItem = windowEntries.remove(winIDs[0]);
			if (windowCheckBoxItem != null)
			{
				windowCheckBoxItem.setText(winIDs[1]);
				windowCheckBoxItem.setActionCommand("toggleVisibility:" + winIDs[1]);
			}
			windowEntries.put(winIDs[1],windowCheckBoxItem);
		} 
		else if (type == KahinaWindowEventType.UNDOCK)
		{
			JCheckBoxMenuItem windowCheckBoxItem = new JCheckBoxMenuItem(e.getWindowID());
            windowCheckBoxItem.setActionCommand("toggleVisibility:" + e.getWindowID());
            windowCheckBoxItem.addActionListener(this);
            windowCheckBoxItem.setSelected(true);
            windowEntries.put(e.getWindowID(), windowCheckBoxItem);
            this.add(windowCheckBoxItem,0);
		} 
		else if (type == KahinaWindowEventType.ADD_VIEW_MENU_ENTRY)
		{
			JCheckBoxMenuItem windowCheckBoxItem = new JCheckBoxMenuItem(e.getWindowID());
            windowCheckBoxItem.setActionCommand("toggleVisibility:" + e.getWindowID());
            windowCheckBoxItem.addActionListener(this);
            windowCheckBoxItem.setSelected(true);
            windowEntries.put(e.getWindowID(), windowCheckBoxItem);
            this.add(windowCheckBoxItem,0);
		} 
	}
}
