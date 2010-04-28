package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.Toolkit;
import java.util.HashMap;

import javax.swing.JFrame;
import javax.swing.JMenuBar;

import org.kahina.core.visual.KahinaView;

public class KahinaWindow extends JFrame
{
	private static final long serialVersionUID = 6613805267152521669L;

	KahinaGUI gui;
    
    HashMap<KahinaView<?>, JFrame> viewWindows;
    
    public KahinaWindow(KahinaGUI gui)
    {
        this.gui = gui;
        
        this.viewWindows = new HashMap<KahinaView<?>, JFrame>();
        
        this.setTitle("Kahina Debugging Environment");
        this.setLayout(new BorderLayout());
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(new KahinaStateMenu());
        menuBar.add(new KahinaParseMenu());
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
        
        //create windows for all the other registered views
        for (KahinaView<?> view : gui.views)
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
            viewWindow.add(view.wrapInPanel());
            viewWindow.setTitle(view.getTitle());
            viewWindow.setSize(width, height);
            viewWindow.setLocation(xPos, yPos);
            viewWindow.setVisible(true);
        }
    }
}
