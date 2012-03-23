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
        if (progressBar != null)
        {
            progressBar.tellTaskProgress((int) (progress + 0.5), status);
        }
    }
    
    protected boolean isCanceled()
    {
        if (cancelled) return true;
        if (progressBar == null) return false;
        return progressBar.cancelButtonClicked();
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
