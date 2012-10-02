package org.kahina.qtype.gui;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaActivationEvent;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.menus.KahinaProjectMenu;
import org.kahina.lp.gui.LogicProgrammingMainWindow;

public class QTypeMainWindow extends LogicProgrammingMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;
    
    protected QTypeParseMenu grammarMenu;

	public QTypeMainWindow(QTypeWindowManager windowManager, KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(windowManager, kahina);
	}
	
	public QTypeMainWindow(QTypeWindowManager windowManager, KahinaInstance<?, ?, ?, ?> kahina, int winID)
	{
		super(windowManager, kahina, winID);
	}
	
	@Override
	protected void addMenusInFront()
	{
        grammarMenu = new QTypeParseMenu(kahina);
		menuBar.add(grammarMenu);
	}
    
    public void processProjectStatus(KahinaProjectStatus projectStatus)
    {
        grammarMenu.setActivationPattern(projectStatus);
        switch (projectStatus)
        {
            case NO_OPEN_PROJECT:
            {
                projectMenu.saveProjectItem.setEnabled(false);
                break;
            }
            default:
            {
                projectMenu.saveProjectItem.setEnabled(true);
            }
        }
        switch (projectStatus)
        {
            case NO_OPEN_PROJECT:
            {
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",false));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",false));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",false));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",false));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",false));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("stop",false));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",false));
                break;
            }
            default:
            {
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",true));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",true));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",true));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",true));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",true));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("stop",true));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",true));
            }
        }
    }
}
