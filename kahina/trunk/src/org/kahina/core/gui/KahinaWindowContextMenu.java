package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaWindowEvent;
import org.kahina.core.event.KahinaWindowEventType;


public class KahinaWindowContextMenu  extends JPopupMenu implements ActionListener
{
	KahinaWindow w;
	
	public KahinaWindowContextMenu(KahinaWindow w)
	{
		this.w = w;
		
		if (!w.isDummyWindow())
		{
			JMenuItem fuseItem = new JCheckBoxMenuItem("Fuse / Remove Frame");
			fuseItem.setActionCommand("Fuse");
			fuseItem.addActionListener(this);
			this.add(fuseItem);
			
			this.addSeparator();
			
			JMenuItem renameItem = new JMenuItem("Rename");
			renameItem.addActionListener(this);
			this.add(renameItem);
			
			if (w.isFlippableWindow())
			{
				JMenuItem undockItem = new JMenuItem("Flip");
				undockItem.addActionListener(this);
				this.add(undockItem);
			}
			
			this.addSeparator();
			
			if (!w.isTopLevelWindow())
			{
				JMenuItem undockItem = new JMenuItem("Undock");
				undockItem.addActionListener(this);
				this.add(undockItem);
			}
			JMenuItem dynCloneItem = new JMenuItem("Dynamic Clone");
			dynCloneItem.addActionListener(this);
			this.add(dynCloneItem);
			JMenuItem snapCloneItem = new JMenuItem("Snapshot Clone");
			snapCloneItem.addActionListener(this);
			this.add(snapCloneItem);
			
			this.addSeparator();
			
			JMenuItem vertSplitItem = new JMenuItem("Vertical Split");
			vertSplitItem.addActionListener(this);
			this.add(vertSplitItem);
			JMenuItem horiSplitItem = new JMenuItem("Horizontal Split");
			horiSplitItem.addActionListener(this);
			this.add(horiSplitItem);
		
			this.addSeparator();
		
			if (w.isContentWindow() && w.isClone())
			{
				JMenuItem disposeItem = new JMenuItem("Dispose");
				disposeItem.addActionListener(this);
				this.add(disposeItem);
			}
		}
		
		JMenuItem closeItem = new JMenuItem("Close");
		closeItem.addActionListener(this);
		this.add(closeItem);
	}
	
	@Override
	public void actionPerformed(ActionEvent e) 
	{
		String s = e.getActionCommand();
		if (s.equals("Undock"))
		{
			KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.UNDOCK, w.getID()));
		}
		else if (s.equals("Rename"))
		{
        	String title = getNewUniqueTitle("Enter a new and unique title for the window.", "Rename window");
        	if (title != null)
        	{
        		KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.RENAME, w.getID(), title));
        	}
		}
		else if (s.equals("Flip"))
		{
			KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.FLIP, w.getID()));
		}
		else if (s.equals("Dynamic Clone"))
		{
			KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.DYNAMIC_CLONE, w.getID()));
		}
		else if (s.equals("Snapshot Clone"))
		{
			KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.SNAPSHOT_CLONE, w.getID()));
		}
		else if (s.equals("Vertical Split"))
		{
        	String title = getNewUniqueTitle("Enter a new and unique title for the split window.", "Split window");
        	if (title != null)
        	{
        		KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.VERT_SPLIT, w.getID(), title));
        	}
		}
		else if (s.equals("Horizontal Split"))
		{
        	String title = getNewUniqueTitle("Enter a new and unique title for the split window.", "Split window");
        	if (title != null)
        	{
        		KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.HORI_SPLIT, w.getID(), title));
        	}
		}
		else if (s.equals("Show Decorations"))
		{
			//TODO: this will not work, as Swing refuses to remove window decorations!
        	w.setVisible(false);
        	w.setUndecorated(!w.isUndecorated());
        	w.setVisible(true);
		}
		else if (s.equals("Close"))
		{
			if (!w.isTopLevelWindow())
			{
				KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.UNDOCK, w.getID()));
			}
			KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.TOGGLE_VISIBLE, w.getID()));
		}
		else if (s.equals("Dispose"))
		{
			KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.DISPOSE, w.getID()));
		}
		else if (s.equals("Fuse"))
		{
			KahinaRunner.processEvent(new KahinaWindowEvent(KahinaWindowEventType.FUSE, w.getID()));
		}
	}
	
	private String getNewUniqueTitle(String description, String dialogTitle)
	{
		String title;
    	//while (true)
    	{
    		title = (String) JOptionPane.showInputDialog(this,
                description,
                dialogTitle,
                JOptionPane.PLAIN_MESSAGE);
    		/*if (w.wm.getWindowByID(title) != null)
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
	
    public static JPopupMenu getMenu(KahinaWindow w)
    {
        return new KahinaWindowContextMenu(w);
    }


}
