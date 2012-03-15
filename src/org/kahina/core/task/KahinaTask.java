package org.kahina.core.task;

import org.kahina.core.gui.KahinaProgressBar;

public abstract class KahinaTask implements Runnable
{
    KahinaProgressBar progressBar;
    
    boolean finished;
    
    public KahinaTask(KahinaProgressBar progressBar)
    {
        this.progressBar = progressBar;
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
        if (progressBar == null) return false;
        return progressBar.cancelButtonClicked();
    }
    
    public boolean isFinished()
    {
        return finished;
    }
}
