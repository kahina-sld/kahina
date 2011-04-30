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
    
    KahinaWindowListener leftPanelListener;
    KahinaWindowListener rightPanelListener;
    
    public KahinaHorizontallySplitWindow(KahinaWindowManager wm)
    {
    	super(wm);
        leftPanel = new JPanel();
        leftPanel.setBorder(BorderFactory.createTitledBorder("Drag window 1 here!"));
        leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
        leftPanelListener = new KahinaWindowListener(this);
        leftPanel.addMouseListener(leftPanelListener);
        rightPanel = new JPanel();
        rightPanel.setBorder(BorderFactory.createTitledBorder("Drag window 2 here!"));
        rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
        rightPanelListener = new KahinaWindowListener(this);
        rightPanel.addMouseListener(rightPanelListener);
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel);
        splitPane.setResizeWeight(.5);
        getContentPane().add(splitPane);
    }
    
    public void setLeftWindow(KahinaWindow w)
    {
    	leftWindow = w;
    	leftPanelListener.setWindow(w);
        ((TitledBorder) leftPanel.getBorder()).setTitle(w.getTitle());
        leftPanel.removeAll();
        leftPanel.add(w.getContentPane());
    }
    
    public void setRightWindow(KahinaWindow w)
    {
    	rightWindow = w;
    	rightPanelListener.setWindow(w);
        ((TitledBorder) rightPanel.getBorder()).setTitle(w.getTitle());
        rightPanel.removeAll();
        rightPanel.add(w.getContentPane());
    }
    
    public boolean isFlippableWindow()
    {
    	return true;
    }
}
