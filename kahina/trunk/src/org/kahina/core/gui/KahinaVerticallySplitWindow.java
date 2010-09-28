package org.kahina.core.gui;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.TitledBorder;

public class KahinaVerticallySplitWindow extends KahinaWindow
{

	private static final long serialVersionUID = -4836711113431179305L;
	
	KahinaWindow upperWindow;
    KahinaWindow lowerWindow;
    
    JPanel topPanel;
    JPanel bottomPanel;
    
    public KahinaVerticallySplitWindow()
    {
        topPanel = new JPanel();
        topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.Y_AXIS));
        topPanel.setBorder(BorderFactory.createTitledBorder("Drag window 1 here!"));
        bottomPanel = new JPanel();
        bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.Y_AXIS));
        bottomPanel.setBorder(BorderFactory.createTitledBorder("Drag window 2 here!"));
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPanel, bottomPanel);
        splitPane.setResizeWeight(.5);
        getContentPane().add(splitPane);
    }
    
    public void setUpperWindow(KahinaWindow w)
    {
        ((TitledBorder) topPanel.getBorder()).setTitle(w.getTitle());
        topPanel.add(w.getContentPane());
    }
    
    public void setLowerWindow(KahinaWindow w)
    {
        ((TitledBorder) bottomPanel.getBorder()).setTitle(w.getTitle());
        bottomPanel.add(w.getContentPane());
    }
    
    /**
     * Just for testing.
     * @param args
     */
    public static void main(String[] args)
    {
    	KahinaWindow window = new KahinaVerticallySplitWindow();
    	window.setSize(640, 480);
    	window.setDefaultCloseOperation(EXIT_ON_CLOSE);
    	window.setVisible(true);
    }
}
