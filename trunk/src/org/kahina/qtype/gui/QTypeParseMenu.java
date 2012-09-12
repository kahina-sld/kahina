package org.kahina.qtype.gui;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.qtype.QTypeCommander;
import org.kahina.qtype.QTypeDebuggerInstance;

public class QTypeParseMenu extends JMenu
{
	private static final long serialVersionUID = -1290849167486564257L;
	
	private JMenuItem compileItem;
	private JMenuItem parseItem;
	private JMenu exampleMenu;
	private JMenuItem restartItem;

	public QTypeParseMenu(KahinaInstance<?, ?, ?, ?> kahina)
	{
		super("Grammar");
		QTypeCommander commander = ((QTypeDebuggerInstance) kahina).getCommander();
		compileItem = new JMenuItem(commander.COMPILE_ACTION);
		add(compileItem);
	    parseItem = new JMenuItem(commander.PARSE_ACTION);
	    add(parseItem);
	    exampleMenu = new QTypeParseExampleMenu(kahina);
		add(exampleMenu);
		restartItem = new JMenuItem(commander.RESTART_ACTION);
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
