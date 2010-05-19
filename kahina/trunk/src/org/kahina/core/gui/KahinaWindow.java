package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JMenuBar;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaAbortEvent;
import org.kahina.core.visual.KahinaView;

public class KahinaWindow extends JFrame
{
	private static final long serialVersionUID = 6613805267152521669L;
    
    private static final boolean verbose = false;

	KahinaGUI gui;
    
    HashMap<KahinaView<?>, JFrame> viewWindows;
    
    public KahinaWindow(KahinaGUI gui)
    {
        this.gui = gui;
        
        this.viewWindows = new HashMap<KahinaView<?>, JFrame>();
        
        this.setTitle("Kahina Debugging Environment");
        this.setLayout(new BorderLayout());
        // this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        // Uncomment this in order to be able to profile using JRat.
        
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(new KahinaStateMenu());
        menuBar.add(new KahinaParseMenu());
        menuBar.add(new KahinaBreakpointMenu());
        menuBar.add(new KahinaHelpMenu());
        this.setJMenuBar(menuBar);
        
        gui.getControlPanel().build();
        this.add(gui.getControlPanel(), BorderLayout.PAGE_START);
        
        int width = gui.getControlPanel().controlButtons.size() * 75;
        int height = 100;
        this.setSize(width, height);
        
        setVisible(true);
        
        this.validate();
        
        int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
        int xPos = 0;
        int yPos = 0;
        int maxY = height;
        
        final List<JFrame> viewWindows = new ArrayList<JFrame>(gui.views.size());
        
        //create windows for all the other registered views
        for (KahinaView<?> view : gui.views)
        {
            JFrame viewWindow = new JFrame();
            viewWindow.setLayout(new BorderLayout());
            viewWindow.add(view.wrapInPanel());
            viewWindow.setTitle(view.getTitle());
            xPos += width + 20;
            width = view.getTitle().length() * 12 + 50;
            if (xPos + width > screenWidth)
            {
                xPos = 0;
                yPos = maxY + 20;
                maxY = 0;
            }
            height = view.getTitle().length() * 6;       
            if (height > maxY)
            {
                maxY = height;
            }
            viewWindow.setSize(width, height);
            viewWindow.setLocation(xPos, yPos);
            viewWindow.setVisible(true);
            viewWindows.add(viewWindow);
        }
        
        this.addWindowListener(new WindowAdapter()
        {
        	
        	@Override
        	public void windowClosing(WindowEvent e)
        	{
        		if (verbose)
        		{
        			System.err.println("Main windows closed.");
        		}
        		KahinaRunner.processEvent(new KahinaAbortEvent());
        		for (JFrame viewWindow : viewWindows)
        		{
        			viewWindow.dispose();
        		}
        		e.getWindow().dispose();
        	}
        	
        });
    }
}
