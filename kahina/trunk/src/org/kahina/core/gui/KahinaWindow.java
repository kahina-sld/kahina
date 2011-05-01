package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaWindowEvent;
import org.kahina.core.event.KahinaWindowEventType;

public class KahinaWindow extends JFrame implements WindowListener
{
	private static final long serialVersionUID = 6613805267152521669L;
    
    private static final boolean verbose = false;
    
    KahinaWindowManager wm;
    JPanel mainPanel;
    
    public KahinaWindow(KahinaWindowManager wm)
    {        
    	this.wm = wm;
        setLayout(new BorderLayout());
        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.setBorder(BorderFactory.createTitledBorder(this.getTitle()));
        mainPanel.addMouseListener(new KahinaWindowListener(this));
        this.add(mainPanel);
        //TODO: find a way to make windows more compact and to avoid having the title twice
        //this.setUndecorated(true);
        this.addWindowListener(this);
    }
    
    public void setTitle(String title)
    {
    	super.setTitle(title);
    	((TitledBorder) mainPanel.getBorder()).setTitle(title);
    }
    
    public boolean isTopLevelWindow()
    {
    	return wm.isTopLevelWindow(this);
    }
    
    //per default, a window is not considered a content window
    public boolean isContentWindow()
    {
    	return false;
    }
    
    //per default, a window is not flippable
    public boolean isFlippableWindow()
    {
    	return false;
    }
    
    public void flipSubwindows()
    {
    	//do nothing per default, implemented by KahinaHorizontallySplitWindow and KahinaVerticallySplitWindow
    }

	@Override
	public void windowActivated(WindowEvent e) 
	{	
	}

	@Override
	public void windowClosed(WindowEvent e) 
	{	
	}

	@Override
	public void windowClosing(WindowEvent e) 
	{
		wm.control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.TOGGLE_VISIBLE,this.getTitle()));
	}

	@Override
	public void windowDeactivated(WindowEvent e) 
	{	
	}

	@Override
	public void windowDeiconified(WindowEvent e) 
	{	
	}

	@Override
	public void windowIconified(WindowEvent e) 
	{		
	}

	@Override
	public void windowOpened(WindowEvent e) 
	{		
	}   
}
