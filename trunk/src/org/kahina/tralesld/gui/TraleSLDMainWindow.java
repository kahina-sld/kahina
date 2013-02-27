package org.kahina.tralesld.gui;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaActivationEvent;
import org.kahina.core.control.KahinaActivationStatus;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.menus.KahinaProjectMenu;
import org.kahina.lp.gui.LogicProgrammingMainWindow;

public class TraleSLDMainWindow extends LogicProgrammingMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;
	
	TraleSLDParseMenu grammarMenu;

	public TraleSLDMainWindow(TraleSLDWindowManager windowManager, KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(windowManager, kahina);
	}
	
	public TraleSLDMainWindow(TraleSLDWindowManager windowManager, int winID, KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(windowManager, kahina, winID);
	}
	
	protected void addMenusInFront()
	{
	    grammarMenu = new TraleSLDParseMenu(kahina);
		menuBar.add(grammarMenu);
		//menuBar.add(new TraleSLDWorkbenchMenu(((TraleSLDGUI) wm.gui)));
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
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("abort",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("backInHistory",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("forwardInHistory",KahinaActivationStatus.INACTIVE));
                break;
            }
        }
    }
}
