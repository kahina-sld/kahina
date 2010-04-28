package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaDialogEvent;
import org.kahina.core.event.KahinaSystemEvent;

public class KahinaParseMenu extends JMenu implements ActionListener
{
	private static final long serialVersionUID = -1290849167486564257L;

	public KahinaParseMenu()
    {
        super("Parse");
            
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
            KahinaRunner.processEvent(new KahinaSystemEvent(KahinaSystemEvent.RESTART));
        }
        else if (s.equals("editBreakpoints"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.BREAKPOINTS));
        }
        else if (s.equals("parseOptions"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.PARSE_OPTIONS));
        }
    }
}
