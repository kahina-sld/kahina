package org.kahina.core.task;

import java.util.LinkedList;
import java.util.List;

public class KahinaTaskManager
{
    List<KahinaTask> taskQueue;
    KahinaTask currentTask;
    
    public KahinaTaskManager()
    {
        taskQueue = new LinkedList<KahinaTask>();
        currentTask = null;
    }
    
    public void addTask(KahinaTask task)
    {
        taskQueue.add(task);
        if (currentTask == null)
        {
            currentTask = task;
            (new Thread(currentTask)).start(); 
            taskStarted(currentTask);
        }
    }
    

    
    /**
     * DANGEROUS: overriding methods MUST call super.taskStarted(task)!
     * @param task
     */
    public void taskStarted(KahinaTask task)
    {
        taskQueue.remove(task);
    }
    
    public void taskFinished(KahinaTask task)
    {
        if (taskQueue.size() == 0)
        {
            currentTask = null;
        }
        else
        {
            currentTask = taskQueue.get(0);
            (new Thread(currentTask)).start(); 
            taskStarted(currentTask);
        }
    }
    
    public void cancelTasks()
    {
        taskQueue.clear();
        if (currentTask != null)
        {
            currentTask.setCanceled();
            currentTask = null;
        }
    }
}
