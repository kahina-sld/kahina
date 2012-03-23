package org.kahina.core.task;

public interface KahinaTaskManager
{
    public abstract void taskCanceled(KahinaTask task);
    public abstract void taskFinished(KahinaTask task);
}
