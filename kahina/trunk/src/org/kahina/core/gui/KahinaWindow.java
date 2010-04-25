package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.Toolkit;
import java.util.HashMap;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaWindow extends JFrame
{
    /**
     * just to keep the compiler happy
     */
    private static final long serialVersionUID = 1L;
    
    KahinaGUI gui;
    KahinaController control;
    
    HashMap<KahinaView, JFrame> viewWindows;
    
    public KahinaWindow(KahinaGUI gui, KahinaController control)
    {
        this.gui = gui;
        this.control = control;
        
        this.viewWindows = new HashMap<KahinaView, JFrame>();
        
        this.setTitle("Kahina Debugging Environment");
        this.setLayout(new BorderLayout());
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(new KahinaStateMenu(control));
        menuBar.add(new KahinaParseMenu(control));
        menuBar.add(new KahinaHelpMenu(control));
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
        
        //create windows for all the other registered views
        for (KahinaView view : gui.views)
        {
            JFrame viewWindow = new JFrame();
            viewWindow.setLayout(new BorderLayout());
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
            KahinaViewPanel panel = view.wrapInPanel();
            control.registerListener("redraw", panel);
            viewWindow.add(new JScrollPane(panel));
            viewWindow.setTitle(view.getTitle());
            viewWindow.setSize(width, height);
            viewWindow.setLocation(xPos, yPos);
            viewWindow.setVisible(true);
        }
    }
}
