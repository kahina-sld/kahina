package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;


public class KahinaWindowContextMenu  extends JPopupMenu implements ActionListener
{
	KahinaWindow w;
	
	public KahinaWindowContextMenu(KahinaWindow w)
	{
		this.w = w;
		
		if (w.isTopLevelWindow())
		{
			JCheckBoxMenuItem decorationsItem = new JCheckBoxMenuItem("Show Decorations");
			decorationsItem.addActionListener(this);
			this.add(decorationsItem);
			
			this.addSeparator();
		}
		
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
		
		if (w.isContentWindow())
		{
			JMenuItem disposeItem = new JMenuItem("Dispose");
			disposeItem.addActionListener(this);
			this.add(disposeItem);
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
			System.err.println("Received order to undock window " + w.getTitle());
		}
		else if (s.equals("Clone"))
		{
			System.err.println("Received order to clone window " + w.getTitle());
		}
	}
	
    public static JPopupMenu getMenu(KahinaWindow w)
    {
        return new KahinaWindowContextMenu(w);
    }


}
