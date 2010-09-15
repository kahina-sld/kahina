package org.kahina.core.gui;

import java.awt.Toolkit;
import java.util.HashMap;

import javax.swing.JFrame;

import org.kahina.core.visual.KahinaView;

public class KahinaWindowManager
{
    KahinaMainWindow mainWindow;
    
    HashMap<KahinaView<?>, KahinaWindow> contentWindows;
    HashMap<KahinaView<?>, KahinaWindow> topLevelWindows;
    
    KahinaGUI gui;
    
    public KahinaWindowManager(KahinaGUI gui)
    {
        this.gui = gui;     
        this.contentWindows = new HashMap<KahinaView<?>, KahinaWindow>();
        this.topLevelWindows = new HashMap<KahinaView<?>, KahinaWindow>();
        
        mainWindow = new KahinaMainWindow(this);
        
        int width = gui.getControlPanel().getWidth();
        int height = 100;
        
        int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
        int xPos = 0;
        int yPos = 0;
        int maxY = height;
        
        //create windows for all the other registered views
        for (KahinaView<?> view : gui.views)
        {
            KahinaWindow viewWindow = integrateInDefaultWindow(view);
            xPos += width + 20;
            width = view.getTitle().length() * 12 + 50;
            if (xPos + width > screenWidth)
            {
                xPos = 0;
                yPos = maxY + 20;
                maxY = 0;
            }
            height = view.getTitle().length() * 12;       
            if (height > maxY)
            {
                maxY = height;
            }
            viewWindow.setSize(width, height);
            viewWindow.setLocation(xPos, yPos);
        }
    }
    
    public void disposeAllWindows()
    {
        for (JFrame viewWindow : topLevelWindows.values())
        {
            viewWindow.dispose();
        }
        mainWindow.dispose();
    }
    
    public KahinaWindow integrateInDefaultWindow(KahinaView<?> view)
    {
        KahinaWindow viewWindow = new KahinaDefaultWindow(view);
        contentWindows.put(view,viewWindow);
        topLevelWindows.put(view,viewWindow);
        return viewWindow;
    }
    
    public void integrateInVerticallySplitWindow(KahinaView<?> v1, KahinaView<?> v2, String newTitle)
    {
        KahinaWindow wrapperWindow1 = topLevelWindows.get(v1);
        if (wrapperWindow1 == null)
        {
            wrapperWindow1 = integrateInDefaultWindow(v1);
        }
        KahinaWindow wrapperWindow2 = topLevelWindows.get(v2);
        if (wrapperWindow2 == null)
        {
            wrapperWindow2 = integrateInDefaultWindow(v2);
        }
        KahinaDiagonallySplitWindow splitWindow = new KahinaDiagonallySplitWindow();
        splitWindow.setTitle(newTitle);
        splitWindow.setUpperWindow(wrapperWindow1);
        splitWindow.setLowerWindow(wrapperWindow2);
        topLevelWindows.put(v1,splitWindow);
        topLevelWindows.put(v2,splitWindow);
    }
    
    public void integrateInHorizontallySplitWindow(KahinaView<?> v1, KahinaView<?> v2, String newTitle)
    {
        KahinaWindow wrapperWindow1 = topLevelWindows.get(v1);
        if (wrapperWindow1 == null)
        {
            wrapperWindow1 = integrateInDefaultWindow(v1);
        }
        KahinaWindow wrapperWindow2 = topLevelWindows.get(v2);
        if (wrapperWindow2 == null)
        {
            wrapperWindow2 = integrateInDefaultWindow(v2);
        }
        KahinaHorizontallySplitWindow splitWindow = new KahinaHorizontallySplitWindow();
        splitWindow.setTitle(newTitle);
        splitWindow.setLeftWindow(wrapperWindow1);
        splitWindow.setRightWindow(wrapperWindow2);
        topLevelWindows.put(v1,splitWindow);
        topLevelWindows.put(v2,splitWindow);
    }
    
    public void displayWindows()
    {
        for (KahinaWindow window : topLevelWindows.values())
        {
            //window.computeGoodSize();
            window.setVisible(true);
        }
    }
}
