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
	
    private HashMap<Integer,JCheckBoxMenuItem> windowEntries;
    private KahinaWindowManager manager;

	public KahinaViewMenu(KahinaWindowManager manager)
    {
        super("View"); 
        
        windowEntries = new HashMap<Integer,JCheckBoxMenuItem>();
        this.manager = manager;
        
        manager.control.registerListener(KahinaEventTypes.WINDOW, this);
        
        for (int winID : manager.arr.getTopLevelWindowsWithoutMainWindow())
        {
        	KahinaWindow window = manager.getWindowByID(winID);
            JCheckBoxMenuItem windowCheckBoxItem = new JCheckBoxMenuItem(window.getTitle());
            windowCheckBoxItem.setActionCommand("toggleVisibility:" + window.getID());
            windowCheckBoxItem.addActionListener(this);
            windowCheckBoxItem.setSelected(manager.psp.isVisible(window.getID()));
            windowEntries.put(window.getID(), windowCheckBoxItem);
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
        
        JMenu changePerspectiveMenu = new JMenu("Change Perspective");
        
        JMenu recentPerspectivesMenu = new JMenu("Recent Perspectives");
        int count = 0;
        for (KahinaPerspective psp : manager.recentPerspectives)
        {
        	JMenuItem recentPerspectiveItem = new JMenuItem(psp.getName());
        	recentPerspectiveItem.setActionCommand("loadRecentPerspective:" + count++);
        	recentPerspectiveItem.addActionListener(this);
            recentPerspectivesMenu.add(recentPerspectiveItem);
        }
        changePerspectiveMenu.add(recentPerspectivesMenu);
        
        JMenu predefinedPerspectivesMenu = new JMenu("Predefined Perspectives");
        count = 0;
        for (KahinaPerspective psp : manager.defaultPerspectives)
        {
        	JMenuItem predefinedPerspectiveItem = new JMenuItem(psp.getName());
        	predefinedPerspectiveItem.setActionCommand("loadDefaultPerspective:" + count++);
        	predefinedPerspectiveItem.addActionListener(this);
        	predefinedPerspectivesMenu.add(predefinedPerspectiveItem);
        }
        changePerspectiveMenu.add(predefinedPerspectivesMenu);
        
        JMenuItem loadPerspectiveItem = new JMenuItem("Load Perspective ...");
        loadPerspectiveItem.setActionCommand("loadPerspective");
        loadPerspectiveItem.addActionListener(this);
        changePerspectiveMenu.add(loadPerspectiveItem);
        
        this.add(changePerspectiveMenu);
        
        JMenuItem savePerspectiveItem = new JMenuItem("Save Perspective");
        savePerspectiveItem.setActionCommand("savePerspective");
        savePerspectiveItem.addActionListener(this);
        this.add(savePerspectiveItem);
            
        JMenuItem savePerspectiveAsItem = new JMenuItem("Save Perspective As...");
        savePerspectiveAsItem.setActionCommand("savePerspectiveAs");
        savePerspectiveAsItem.addActionListener(this);
        this.add(savePerspectiveAsItem);
    }
	
	@Override
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("newDefaultWindow"))
        {	
        	String title = getNewUniqueTitle("Default Window");
            KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_DEFAULT, -1, title));
        }
        else if (s.equals("newVertSplitWindow"))
        {	
        	String title = getNewUniqueTitle("Vertically Split Window");
            KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_VERT_SPLIT, -1, title));
        }
        else if (s.equals("newHoriSplitWindow"))
        {	
        	String title = getNewUniqueTitle("Horizontally Split Window");
            KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_HORI_SPLIT, -1, title));
        }
        else if (s.equals("newTabbedWindow"))
        {	
        	String title = getNewUniqueTitle("Tabbed Window");
            KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_TABBED, -1, title));
        }
        else if (s.startsWith("toggleVisibility"))
        {
            int windowID = Integer.parseInt(s.substring(s.indexOf(':') + 1));
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
        else if (s.startsWith("loadRecentPerspective:"))
        {
        	int counter = Integer.parseInt(s.substring(22));
        	KahinaRunner.processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.LOAD_RECENT_PERSPECTIVE, counter));
        }
        else if (s.startsWith("loadDefaultPerspective:"))
        {
        	int counter = Integer.parseInt(s.substring(23));
        	KahinaRunner.processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.LOAD_DEFAULT_PERSPECTIVE, counter));
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
	
	private String getNewUniqueTitle(String windowType)
	{
		String title;
		//uniqueness check not needed any more, code and function name remain for legacy reasons
    	//while (true)
    	{
    		title = (String) JOptionPane.showInputDialog(this,
                "Enter a unique title for the new window.",
                "New " + windowType,
                JOptionPane.PLAIN_MESSAGE);
    		/*if (manager.getWindowByID(title) != null)
    		{
    			JOptionPane.showMessageDialog(this,
    				    "A window with that name already exists.", 
    				    "Uniqueness Enforcement",JOptionPane.WARNING_MESSAGE);
    		}
    		else
    		{
    			break;
    		}*/
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
		if (type == KahinaWindowEventType.TOGGLE_VISIBLE)
		{
			//react to a window that is being closed
			boolean newVisibility = manager.psp.isVisible(e.getWindowID());
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
			System.err.println("Received order to remove check box item for window " + e.getWindowID());
			JCheckBoxMenuItem windowCheckBoxItem = windowEntries.remove(e.getWindowID());
			if (windowCheckBoxItem == null)
			{
				System.err.println("WARNING: no check box item for window \"" + e.getWindowID() + "\"");
			}
			else
			{
				System.err.println("Removing check box item for window " + e.getWindowID());
				this.remove(windowCheckBoxItem);
			}
		} 
		else if (type == KahinaWindowEventType.RENAME)
		{
			JCheckBoxMenuItem windowCheckBoxItem = windowEntries.get(e.getWindowID());
			if (windowCheckBoxItem != null)
			{
				windowCheckBoxItem.setText(manager.getWindowByID(e.getWindowID()).getTitle());
			}
		} 
		else if (type == KahinaWindowEventType.UNDOCK)
		{
			JCheckBoxMenuItem windowCheckBoxItem = new JCheckBoxMenuItem(manager.getWindowByID(e.getWindowID()).getTitle());
            windowCheckBoxItem.setActionCommand("toggleVisibility:" + e.getWindowID());
            windowCheckBoxItem.addActionListener(this);
            windowCheckBoxItem.setSelected(true);
            windowEntries.put(e.getWindowID(), windowCheckBoxItem);
            this.add(windowCheckBoxItem,0);
		} 
		else if (type == KahinaWindowEventType.ADD_VIEW_MENU_ENTRY)
		{
			JCheckBoxMenuItem windowCheckBoxItem = new JCheckBoxMenuItem(manager.getWindowByID(e.getWindowID()).getTitle());
            windowCheckBoxItem.setActionCommand("toggleVisibility:" + e.getWindowID());
            windowCheckBoxItem.addActionListener(this);
            windowCheckBoxItem.setSelected(true);
            windowEntries.put(e.getWindowID(), windowCheckBoxItem);
            this.add(windowCheckBoxItem,0);
		} 
	}
}
