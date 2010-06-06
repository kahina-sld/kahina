package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JMenuBar;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaAbortEvent;

public class KahinaMainWindow extends KahinaWindow
{
    public static boolean verbose = false;
    
    public KahinaWindowManager windowManager;
    
    public KahinaMainWindow(KahinaWindowManager windowManager)
    {
        this.windowManager = windowManager;
        
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
        
        windowManager.gui.getControlPanel().build();
        this.add(windowManager.gui.getControlPanel(), BorderLayout.PAGE_START);
        
        //TODO: adapt this to the size of the control panel
        int width = 625;
        int height = 120;
        this.setSize(width, height);
        
        setVisible(true);
        
        this.validate();
        
        this.addWindowListener(new WindowAdapter()
        {         
            @Override
            public void windowClosing(WindowEvent e)
            {
                if (verbose)
                {
                    System.err.println("Main windows closed.");
                }
                disposeAllWindows();
                KahinaRunner.processEvent(new KahinaAbortEvent());
            }
            
        });
    }
    
    private void disposeAllWindows()
    {
        windowManager.disposeAllWindows();
    }
}
