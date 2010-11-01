package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaDialogEvent;
import org.kahina.tralesld.TraleSLDInstance;

public class KahinaParseMenu extends JMenu implements ActionListener
{
	private static final long serialVersionUID = -1290849167486564257L;

	public KahinaParseMenu()
    {
        super("Parse");  
        
        // FIXME http://kahina.org/trac/ticket/50
        this.add(new JMenuItem(TraleSLDInstance.COMPILE_ACTION));
        this.add(new JMenuItem(TraleSLDInstance.PARSE_ACTION));
        this.add(new JMenuItem(TraleSLDInstance.RESTART_ACTION));
        
        this.addSeparator();
             
        JMenuItem parseOptionsItem = new JMenuItem("Options...");
        parseOptionsItem.setActionCommand("parseOptions");
        parseOptionsItem.addActionListener(this);
        this.add(parseOptionsItem);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("parseOptions"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.PARSE_OPTIONS));
        }
    }
}
