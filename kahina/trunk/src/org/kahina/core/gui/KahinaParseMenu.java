package org.kahina.core.gui;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;
import org.kahina.tralesld.TraleSLDInstance;

public class KahinaParseMenu extends JMenu
{
	private static final long serialVersionUID = -1290849167486564257L;

	public KahinaParseMenu(KahinaInstance kahina)
    {
        super("Parse");  
        
        // FIXME http://kahina.org/trac/ticket/50
        if (kahina instanceof TraleSLDInstance)
        {
        	TraleSLDInstance instance = (TraleSLDInstance) kahina;
            add(new JMenuItem(instance.COMPILE_ACTION));
            add(new JMenuItem(instance.PARSE_ACTION));        
            add(new JMenuItem(instance.RESTART_ACTION));
        }
    }
}
