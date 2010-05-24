package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.Toolkit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.swing.JFrame;

import org.kahina.core.visual.KahinaView;

public class KahinaWindowManager
{
    KahinaMainWindow mainWindow;
    
    HashMap<KahinaView<?>, JFrame> contentWindows;
    
    KahinaGUI gui;
    
    public KahinaWindowManager(KahinaGUI gui)
    {
        this.gui = gui;     
        this.contentWindows = new HashMap<KahinaView<?>, JFrame>();
        
        mainWindow = new KahinaMainWindow(this);
        
        int width = gui.getControlPanel().controlButtons.size() * 75;
        int height = 100;
        
        int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
        int xPos = 0;
        int yPos = 0;
        int maxY = height;
        
        contentWindows = new HashMap<KahinaView<?>, JFrame>();
        
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
            contentWindows.put(view,viewWindow);
        }
    }
    
    public void disposeAllWindows()
    {
        for (JFrame viewWindow : contentWindows.values())
        {
            viewWindow.dispose();
        }
        mainWindow.dispose();
    }
}
