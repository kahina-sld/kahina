package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;


public class KahinaWindowContextMenu  extends JPopupMenu implements ActionListener
{
	KahinaWindow w;
	
	public KahinaWindowContextMenu(KahinaWindow w)
	{
		this.w = w;
		if (!w.isTopLevelWindow())
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
	
    public static JPopupMenu getMenu(KahinaWindow w)
    {
        return new KahinaWindowContextMenu(w);
    }


}
