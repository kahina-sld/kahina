package org.kahina.core.gui.menus;

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
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.event.KahinaPerspectiveEvent;
import org.kahina.core.gui.event.KahinaWindowEvent;
import org.kahina.core.gui.event.KahinaWindowEventType;
import org.kahina.core.visual.KahinaEmptyView;
import org.kahina.core.visual.KahinaView;

public class KahinaViewMenu  extends JMenu implements ActionListener, KahinaListener
{
	private static final long serialVersionUID = -8816851369583949953L;
	
    private KahinaWindowManager manager;
    
    private File lastPerspectiveFile;

	public KahinaViewMenu(KahinaWindowManager manager)
    {
        super("View"); 
        
        this.manager = manager;      
        manager.getGuiControl().registerListener(KahinaEventTypes.WINDOW, this);
      
        rebuild();
    }
	
	public void rebuild()
	{
		this.removeAll();
		
        for (int winID : manager.getArrangement().getTopLevelWindowsWithoutMainWindow())
        {
            JCheckBoxMenuItem windowCheckBoxItem = new JCheckBoxMenuItem(manager.getArrangement().getTitle(winID));
            windowCheckBoxItem.setActionCommand("toggleVisibility:" + winID);
            windowCheckBoxItem.addActionListener(this);
            windowCheckBoxItem.setSelected(manager.getPerspective().isVisible(winID));
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
        
        JMenuItem newListWindowItem = new JMenuItem("New List Window");
        newListWindowItem.setActionCommand("newListWindow");
        newListWindowItem.addActionListener(this);
        this.add(newListWindowItem);
        
        this.addSeparator();
        
        JMenu restoreFrameMenu = new JMenu("Restore Frame");
        
        for (int winID : manager.getArrangement().getAllWindows())
        {
        	if (!manager.getArrangement().hasBorder(winID))
        	{
        		JMenuItem restoreFrameItem = new JMenuItem(manager.getArrangement().getTitle(winID));
        		restoreFrameItem.setActionCommand("restoreFrame:" + winID);
        		restoreFrameItem.addActionListener(this);
        		restoreFrameMenu.add(restoreFrameItem);
        	}
        }
        
        this.add(restoreFrameMenu);

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
        	manager.getGuiControl().processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_DEFAULT, -1, "Empty view"));
        }
        else if (s.equals("newVertSplitWindow"))
        {	
        	String title = getNewTitle("Vertically Split Window");
        	manager.getGuiControl().processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_VERT_SPLIT, -1, title));
        }
        else if (s.equals("newHoriSplitWindow"))
        {	
        	String title = getNewTitle("Horizontally Split Window");
        	manager.getGuiControl().processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_HORI_SPLIT, -1, title));
        }
        else if (s.equals("newTabbedWindow"))
        {	
        	String title = getNewTitle("Tabbed Window");
        	manager.getGuiControl().processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_TABBED, -1, title));
        }
        else if (s.equals("newListWindow"))
        {	
        	String title = getNewTitle("List Window");
        	manager.getGuiControl().processEvent(new KahinaWindowEvent(KahinaWindowEventType.NEW_LIST, -1, title));
        }
        else if (s.startsWith("toggleVisibility"))
        {
            int windowID = Integer.parseInt(s.substring(s.indexOf(':') + 1));
            manager.getGuiControl().processEvent(new KahinaWindowEvent(KahinaWindowEventType.TOGGLE_VISIBLE, windowID));
        }
        else if (s.startsWith("restoreFrame:"))
        {
        	int windowID = Integer.parseInt(s.substring(s.indexOf(':') + 1));
        	manager.getGuiControl().processEvent(new KahinaWindowEvent(KahinaWindowEventType.RESTORE_FRAME, windowID));
        }
        else if (s.equals("loadPerspective"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Load Perspective");
            chooser.showOpenDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null) manager.getGuiControl().processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.LOAD_PERSPECTIVE, dataFile));
        }
        else if (s.startsWith("loadRecentPerspective:"))
        {
        	int counter = Integer.parseInt(s.substring(22));
        	manager.getGuiControl().processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.LOAD_RECENT_PERSPECTIVE, counter));
        }
        else if (s.startsWith("loadDefaultPerspective:"))
        {
        	int counter = Integer.parseInt(s.substring(23));
        	manager.getGuiControl().processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.LOAD_DEFAULT_PERSPECTIVE, counter));
        }
        else if (s.equals("savePerspective"))
        {
        	if (lastPerspectiveFile != null)
        	{
        		manager.getGuiControl().processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.SAVE_PERSPECTIVE, lastPerspectiveFile));
        	}
        	else
        	{
        		this.actionPerformed(new ActionEvent(this,0,"savePerspectiveAs"));
        	}
        }
        else if (s.equals("savePerspectiveAs"))
        {
        	String title = (String) JOptionPane.showInputDialog(this,
                    "Enter a name for the perspective.",
                    "Save Perspective",
                    JOptionPane.PLAIN_MESSAGE);
        	if (title.length() > 0)
        	{
        		manager.getPerspective().setName(title);
        	}
        	else
        	{
        		JOptionPane.showMessageDialog(this, "Empty string is not a valid name!");
        	}
        	
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Save Perspective As");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null)  manager.getGuiControl().processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.SAVE_PERSPECTIVE, dataFile));
            lastPerspectiveFile = dataFile;
        }
    }
	
	private String getNewTitle(String windowType)
	{
		return (String) JOptionPane.showInputDialog(this,
                "Enter a title for the new window.",
                "New " + windowType,
                JOptionPane.PLAIN_MESSAGE);
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
		if (type == KahinaWindowEventType.UPDATE_VIEW_MENU)
		{
			rebuild();
		}
	}
}
