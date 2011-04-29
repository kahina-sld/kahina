package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JFrame;

import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaWindowEvent;
import org.kahina.core.event.KahinaWindowEventType;

public class KahinaWindow extends JFrame implements WindowListener
{
	private static final long serialVersionUID = 6613805267152521669L;
    
    private static final boolean verbose = false;
    
    KahinaWindowManager wm;
    
    public KahinaWindow(KahinaWindowManager wm)
    {        
    	this.wm = wm;
        setLayout(new BorderLayout());
        this.addMouseListener(new KahinaWindowListener(this));
        this.addWindowListener(this);
    }
    
    public boolean isTopLevelWindow()
    {
    	return wm.isTopLevelWindow(this);
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
