package org.kahina.core.visual.graph;

import org.kahina.core.task.KahinaTask;
import org.kahina.core.task.KahinaTaskManager;

public class KahinaGraphViewTaskManager extends KahinaTaskManager
{
    KahinaGraphViewPanel viewPanel;
    
    public KahinaGraphViewTaskManager(KahinaGraphViewPanel viewPanel)
    {
        this.viewPanel = viewPanel;
    }
    
    public void taskStarted(KahinaTask task)
    {
        super.taskStarted(task);
        viewPanel.showProgressBar();
        viewPanel.repaint();
    }
    
    public void taskFinished(KahinaTask task)
    {
        super.taskFinished(task);
        viewPanel.hideProgressBar();
        viewPanel.repaint();
    }
    
    public void cancelTasks()
    {
        super.cancelTasks();
        viewPanel.hideProgressBar();
        viewPanel.repaint();
    }
}
