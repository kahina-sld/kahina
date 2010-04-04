package org.kahina.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.HashMap;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;

import org.kahina.control.KahinaController;
import org.kahina.visual.KahinaView;

public class KahinaWindow extends JFrame
{
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
        //this.setSize(Toolkit.getDefaultToolkit().getScreenSize());
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(new KahinaStateMenu(control));
        menuBar.add(new KahinaParseMenu(control));
        menuBar.add(new KahinaHelpMenu(control));
        this.setJMenuBar(menuBar);
        
        gui.getControlPanel().build();
        this.add(gui.getControlPanel(), BorderLayout.PAGE_START);
        
        setVisible(true);
        
        //TODO: this does not work; perhaps because the components are not displayed yet?
        Dimension dim = new Dimension();
        dim.height = menuBar.getHeight() + gui.getControlPanel().getHeight();
        dim.width = gui.getControlPanel().getWidth();
        System.err.println(dim);
        this.setSize(dim);
        
        this.validate();
        
        //create windows for all the other other registered views
        for (KahinaView view : gui.views)
        {
            JFrame viewWindow = new JFrame();
            viewWindow.add(new JScrollPane(view.wrapInPanel()));
            viewWindow.setTitle(view.getModel().getClass().getName());
            viewWindow.setVisible(true);
        }
    }
}
