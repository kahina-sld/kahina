package org.kahina.core.gui;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.TitledBorder;

import org.kahina.core.control.KahinaController;

public class KahinaVerticallySplitWindow extends KahinaWindow
{
	private static final long serialVersionUID = -4836711113431179305L;
	
	KahinaWindow upperWindow;
    KahinaWindow lowerWindow;
    
    JPanel topPanel;
    JPanel bottomPanel;
    
    public KahinaVerticallySplitWindow(KahinaWindowManager wm)
    {
    	super(wm);
        topPanel = new JPanel();
        topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.Y_AXIS));
        bottomPanel = new JPanel();
        bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.Y_AXIS));
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPanel, bottomPanel);
        splitPane.setResizeWeight(.5);
        mainPanel.add(splitPane);
    }
    
    public void setUpperWindow(KahinaWindow w)
    {
    	upperWindow = w;
        topPanel.removeAll();
        topPanel.add(w.getContentPane());
    }
    
    public void setLowerWindow(KahinaWindow w)
    {
    	lowerWindow = w;
        bottomPanel.removeAll();
        bottomPanel.add(w.getContentPane());
    }
    
    public boolean isFlippableWindow()
    {
    	return true;
    }
    
    public void flipSubwindows()
    {
    	KahinaWindow tempWindow = upperWindow;
    	setUpperWindow(lowerWindow);
    	setLowerWindow(tempWindow);
    }
}
