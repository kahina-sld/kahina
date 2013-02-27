package org.kahina.tralesld.gui;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.qtype.gui.QTypeParseExampleMenu;
import org.kahina.tralesld.TraleSLDInstance;

public class TraleSLDParseMenu extends JMenu
{
	private static final long serialVersionUID = -1290849167486564257L;
	
	private JMenuItem compileItem;
	private JMenuItem parseItem;
	private JMenu exampleMenu;
	private JMenuItem restartItem;

	public TraleSLDParseMenu(KahinaInstance<?, ?, ?, ?> kahina)
	{
		super("Parse");
		TraleSLDInstance instance = (TraleSLDInstance) kahina;
		compileItem = new JMenuItem(instance.COMPILE_ACTION);
		add(compileItem);
		parseItem = new JMenuItem(instance.PARSE_ACTION);
		add(parseItem);
	    exampleMenu = new TraleSLDParseExampleMenu(kahina);
	    add(exampleMenu);
		restartItem = new JMenuItem(instance.RESTART_ACTION);
		add(restartItem);
	}
	
	public void setActivationPattern(KahinaProjectStatus status)
    {
        compileItem.setEnabled(true);
        switch (status)
        {
            case NO_OPEN_PROJECT:
            {
                this.setEnabled(false);
                break;
            }
            case PROGRAM_UNCOMPILED:
            {
                this.setEnabled(true);
                parseItem.setEnabled(false);
                exampleMenu.setEnabled(false);
                restartItem.setEnabled(false);
                break;
            }
            case PROGRAM_COMPILED:
            {
                this.setEnabled(true);
                parseItem.setEnabled(true);
                exampleMenu.setEnabled(true);
                restartItem.setEnabled(false);
                break;
            }
            case DEBUGGING_RUN:
            {
                this.setEnabled(true);
                parseItem.setEnabled(true);
                exampleMenu.setEnabled(true);
                restartItem.setEnabled(true);
                break;
            }
        }
    }
}
