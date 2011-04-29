package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;


public class KahinaWindowContextMenu  extends JPopupMenu implements ActionListener
{
	KahinaWindowManager wm;
	KahinaWindow w;
	
	public KahinaWindowContextMenu(KahinaWindowManager wm, KahinaWindow w)
	{
		this.wm = wm;
		this.w = w;
		if (!wm.isTopLevelWindow(w))
		{
			JMenuItem undockItem = new JMenuItem("Undock");
			undockItem.addActionListener(this);
			this.add(undockItem);
		}
		JMenuItem undockItem = new JMenuItem("Clone");
		undockItem.addActionListener(this);
		this.add(undockItem);
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
	
    public static JPopupMenu getMenu(KahinaWindowManager wm, KahinaWindow w)
    {
        return new KahinaWindowContextMenu(wm, w);
    }


}
