package org.kahina.core.task;

import java.util.LinkedList;
import java.util.List;

public class KahinaTaskManager
{
    List<KahinaTask> taskQueue;
    KahinaTask currentTask;
    
    public static boolean VERBOSE = false;
    
    public KahinaTaskManager()
    {
        taskQueue = new LinkedList<KahinaTask>();
        currentTask = null;
    }
    
    /**
     * Adds a new task to the task queue, starting it immediately if no other task was queued.
     * @param task the task to be queued and started when all the preceding tasks are processed.
     */
    public void addTask(KahinaTask task)
    {
        if (VERBOSE) System.err.println("KahinaTaskManager.addTask(" + task + ")");
        taskQueue.add(task);
        if (VERBOSE) System.err.println("  taskQueue = " + taskQueue);
        if (VERBOSE) System.err.println("  currentTask = " + currentTask);
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
        if (VERBOSE) System.err.println("KahinaTaskManager.taskStarted(" + task + ")");
        taskQueue.remove(task);
        if (VERBOSE) System.err.println("  taskQueue = " + taskQueue);
    }
    
    public void taskFinished(KahinaTask task)
    {
        if (VERBOSE) System.err.println("KahinaTaskManager.taskFinished(" + task + ")");
        if (VERBOSE) System.err.println("  taskQueue = " + taskQueue);
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
        if (VERBOSE) System.err.println("KahinaTaskManager.cancelTasks()");
        taskQueue.clear();
        if (VERBOSE) System.err.println("  currentTask = " + currentTask);
        if (currentTask != null)
        {
            currentTask.setCanceled();
            currentTask = null;
        }
    }
}
