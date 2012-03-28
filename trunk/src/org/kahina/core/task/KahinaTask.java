package org.kahina.core.task;

import org.kahina.core.gui.KahinaProgressBar;

public abstract class KahinaTask implements Runnable
{
    KahinaProgressBar progressBar;
    KahinaTaskManager manager;
    
    private boolean cancelled;
    private boolean finished;
    
    public KahinaTask(KahinaProgressBar progressBar, KahinaTaskManager manager)
    {
        this.progressBar = progressBar;
        this.manager = manager;
        this.cancelled = false;
        this.finished = false;
    }
    
    protected void setProgressAndStatus(double progress, String status)
    {
        //System.err.println("setProgressAndStatus(" + progress + "," + status + ")");
        if (progressBar != null)
        {
            progressBar.tellTaskProgress((int) (100 * progress), status);
        }
    }
    
    protected boolean isCanceled()
    {
        if (cancelled) return true;
        if (progressBar == null) return false;
        if (progressBar.cancelButtonClicked())
        {
            //TODO: find a better architecture for managing cancellation of one vs. all tasks
            manager.cancelTasks();
            return true;
        }
        return false;
    }
    
    public boolean isFinished()
    {
        return finished;
    }
    
    public void setCanceled()
    {
        this.cancelled = true;
    }
    
    public void setFinished()
    {
        this.finished = true;
        manager.taskFinished(this);
    }
}
