package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaDialogEvent;
import org.kahina.core.event.KahinaSystemEvent;

public class KahinaParseMenu extends JMenu implements ActionListener
{
    KahinaController control;
    
    public KahinaParseMenu(KahinaController control)
    {
        super("Parse");
        this.control = control;
            
        JMenuItem restartItem = new JMenuItem("Restart");
        restartItem.setActionCommand("restartParse");
        restartItem.addActionListener(this);
        this.add(restartItem);
        
        this.addSeparator();
        
        JMenuItem breakpointsItem = new JMenuItem("Breakpoints...");
        breakpointsItem.setActionCommand("editBreakpoints");
        breakpointsItem.addActionListener(this);
        this.add(breakpointsItem);
             
        JMenuItem parseOptionsItem = new JMenuItem("Options...");
        parseOptionsItem.setActionCommand("parseOptions");
        parseOptionsItem.addActionListener(this);
        this.add(parseOptionsItem);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("restartParse"))
        {
            control.processEvent(new KahinaSystemEvent(KahinaSystemEvent.RESTART));
        }
        else if (s.equals("editBreakpoints"))
        {
            control.processEvent(new KahinaDialogEvent(KahinaDialogEvent.BREAKPOINTS));
        }
        else if (s.equals("parseOptions"))
        {
            control.processEvent(new KahinaDialogEvent(KahinaDialogEvent.PARSE_OPTIONS));
        }
    }
}
