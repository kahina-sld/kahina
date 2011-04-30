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
    
    KahinaWindowListener topPanelListener;
    KahinaWindowListener bottomPanelListener;
    
    public KahinaVerticallySplitWindow(KahinaWindowManager wm)
    {
    	super(wm);
        topPanel = new JPanel();
        topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.Y_AXIS));
        topPanel.setBorder(BorderFactory.createTitledBorder("Drag window 1 here!"));
        topPanelListener = new KahinaWindowListener(this);
        topPanel.addMouseListener(topPanelListener);
        bottomPanel = new JPanel();
        bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.Y_AXIS));
        bottomPanel.setBorder(BorderFactory.createTitledBorder("Drag window 2 here!"));
        bottomPanelListener = new KahinaWindowListener(this);
        bottomPanel.addMouseListener(bottomPanelListener);
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPanel, bottomPanel);
        splitPane.setResizeWeight(.5);
        getContentPane().add(splitPane);
    }
    
    public void setUpperWindow(KahinaWindow w)
    {
    	upperWindow = w;
    	topPanelListener.setWindow(w);
        ((TitledBorder) topPanel.getBorder()).setTitle(w.getTitle());
        topPanel.removeAll();
        topPanel.add(w.getContentPane());
    }
    
    public void setLowerWindow(KahinaWindow w)
    {
    	lowerWindow = w;
    	bottomPanelListener.setWindow(w);
        ((TitledBorder) bottomPanel.getBorder()).setTitle(w.getTitle());
        bottomPanel.removeAll();
        bottomPanel.add(w.getContentPane());
    }
    
    public boolean isFlippableWindow()
    {
    	return true;
    }
}
