package org.kahina.core.gui;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.TitledBorder;

import org.kahina.core.control.KahinaController;

public class KahinaHorizontallySplitWindow extends KahinaWindow
{
	private static final long serialVersionUID = 7735328776548607273L;
	
	KahinaWindow leftWindow;
    KahinaWindow rightWindow;
    
    JPanel leftPanel;
    JPanel rightPanel;
    
    public KahinaHorizontallySplitWindow(KahinaWindowManager wm)
    {
    	super(wm);
        leftPanel = new JPanel();
        leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
        rightPanel = new JPanel();
        rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel);
        splitPane.setResizeWeight(.5);
        mainPanel.add(splitPane);
    }
    
    public void setLeftWindow(KahinaWindow w)
    {
    	leftWindow = w;
        leftPanel.removeAll();
        leftPanel.add(w.getContentPane());
    }
    
    public void setRightWindow(KahinaWindow w)
    {
    	rightWindow = w;
        rightPanel.removeAll();
        rightPanel.add(w.getContentPane());
    }
    
    public boolean isFlippableWindow()
    {
    	return true;
    }
    
    public void flipSubwindows()
    {
    	KahinaWindow tempWindow = leftWindow;
    	setLeftWindow(rightWindow);
    	setRightWindow(tempWindow);
    }
}
