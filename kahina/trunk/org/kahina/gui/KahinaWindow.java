package org.kahina.gui;

import java.awt.Toolkit;

import javax.swing.JFrame;
import javax.swing.JMenuBar;

import org.kahina.control.KahinaController;

public class KahinaWindow extends JFrame
{
    KahinaGUI gui;
    KahinaController control;
    
    public KahinaWindow(KahinaGUI gui, KahinaController control)
    {
        this.gui = gui;
        this.control = control;
        
        this.setTitle("Kahina Debugging Environment");
        this.setSize(Toolkit.getDefaultToolkit().getScreenSize());
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(new KahinaStateMenu(control));
        menuBar.add(new KahinaParseMenu(control));
        menuBar.add(new KahinaHelpMenu(control));
        this.setJMenuBar(menuBar);
    }
}
