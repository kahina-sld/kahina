package org.kahina.logic.sat.muc.task;

import java.awt.Color;

import org.kahina.core.data.dag.ColoredPath;
import org.kahina.core.io.color.ColorUtil;
import org.kahina.core.task.KahinaTask;
import org.kahina.core.task.KahinaTaskManager;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCState;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.heuristics.UCReductionHeuristics;
import org.kahina.logic.sat.muc.visual.UCReducerPanel;

public class UCReducer extends KahinaTaskManager
{
    //provides access to the decision DAG that is constructed by multiple UCReducers
    MUCState state;
    
    //remember at which step ID we started
    int startID;
    int startSize;
    int numSATReductions;
    int numUNSATReductions;  
    
    //store the current UC and its ID after each (attempted) reduction step
    MUCStep uc;
    int ucID = -1;
    
    boolean stopped = false;
    
    MiniSATFiles files;
    
    UCReductionHeuristics heuristics;
    private Color signalColor;
    
    private UCReducerPanel panel;
    
    private ColoredPath path;
    
    public UCReducer(MUCState state, int startID, MiniSATFiles files)
    {
        this.state = state;
        System.err.println("Retrieving start MUCStep with ID " + startID);
        this.uc = state.retrieve(MUCStep.class, startID);
        this.ucID = startID;
        this.startID = startID;
        
        path = new ColoredPath(signalColor);
        getPath().append(startID);
        state.getDecisionGraph().addColorPath(getPath());
        
        this.files = files;  
        this.setSignalColor(ColorUtil.randomColor());
        this.setPanel(null);
    }
    
    public void start()
    {
        this.numSATReductions = 0;
        this.numUNSATReductions = 0;
        this.startSize = uc.getUc().size();
        startNextReduction();
    }
    
    public void taskFinished(KahinaTask task)
    {
        super.taskFinished(task);
        if (task instanceof UCReductionTask)
        {
            UCReductionTask ucTask = (UCReductionTask) task;
            MUCStep result = ucTask.getResult();
            //attempt was unsuccessful
            if (uc == result)
            {
                //uc and ucID just stay the same
                state.addAndDistributeUnreducibilityInfo(ucID, ucTask.candidate);
                numSATReductions++;
                //System.err.println(this + ": Reduction #" + ucTask.reductionID + " of clause " + ucTask.candidate + " at step " + ucID + " led to satisfiable instance! No change!");
            }
            //attempt was successful, we might have arrived at a new UC
            else
            {
                numUNSATReductions++;
                System.err.println(this + ": Reduction #" + ucTask.reductionID + " of clause " + ucTask.candidate + " at step " + ucID + " was successful!");
                int stepID = state.registerMUC(result, ucID, ucTask.candidate);
                getPath().append(stepID);
                uc.setRemovalLink(ucTask.candidate, stepID);
                uc = state.retrieve(MUCStep.class, stepID);
                ucID = stepID;
                heuristics.setNewUC(uc);
                if (getPanel() != null) getPanel().requestViewUpdate();
            }
            startNextReduction();
        }
        else
        {
            System.err.println("ERROR: UCReducer has completed a task that is not an UCReductionTask???");
        }
    }
    
    private void startNextReduction()
    {
        if (stopped)
        {
            if (getPanel() != null)
            {
                getPanel().displayStatus1("UC reduction using " + heuristics.getName() + " stopped at size " + uc.getUc().size());
                getPanel().displayStatus2("after " + (numUNSATReductions + numSATReductions) 
                                   + " reductions (" + numSATReductions + " SAT, " 
                                   + numUNSATReductions + " UNSAT)" + " from size " + startSize + ". ");
            }
            return;
        }

        int candidate = heuristics.getNextCandidate();
        if (candidate == -1)
        {
            //we are done reducing, no further reductions are possible; this UCReducer has done its duty
            if (getPanel() != null)
            {
                getPanel().displayStatus1("MUC of size " + uc.getUc().size() + " found using " + heuristics.getName());
                getPanel().displayStatus2("after " + (numUNSATReductions + numSATReductions) 
                                   + " reductions (" + numSATReductions + " SAT, " 
                                   + numUNSATReductions + " UNSAT)" + " from size " + startSize + ". ");
                getPanel().displayCompletedState();
                stopped = true;
                getPanel().repaint();
            }
        }
        else
        {
            //System.err.println(this + ": Reducing " + candidate + " at step " + ucID);
            if (getPanel() != null)
            {
                getPanel().displayStatus1("UC of size " + uc.getUc().size() + ", " + heuristics.getName() + " attempting to reduce clause " + candidate + ".");
                getPanel().displayStatus2((numUNSATReductions + numSATReductions) 
                        + " reductions (" + numUNSATReductions + " UNSAT, " 
                        + numSATReductions + " SAT)" + " so far. ");
                getPanel().repaint();
            }
            Integer removalLink = uc.getRemovalLink(candidate);
            if (removalLink == null)
            {
                this.addTask(new UCReductionTask(null, this, state.getStatistics(), uc, ucID, candidate, files));
            }
            else if (removalLink == -1)
            {
                this.addTask(new UCReductionTask(null, this, state.getStatistics(), uc, ucID, candidate, uc));
            }
            else
            {
                MUCStep newUC = state.retrieve(MUCStep.class, removalLink);
                this.addTask(new UCReductionTask(null, this, state.getStatistics(), uc, ucID, candidate, newUC));
            }
        }    
    }
    
    public MiniSATFiles getFiles()
    {
        return files;
    }
    
    public UCReductionHeuristics getHeuristics()
    {
        return heuristics;
    }
    
    public void setPanel(UCReducerPanel panel)
    {
        if (panel != null)
        {
            if (this.panel != null)
            {
                panel.displayStatus1(this.panel.getStatus1());
                panel.displayStatus2(this.panel.getStatus2());
    
            }
            if (stopped) panel.displayCompletedState();
            this.panel = panel;
            panel.setReducer(this);
        }
        else
        {
            this.panel = null;
        }
    }
    
    public void setHeuristics(UCReductionHeuristics heuristics)
    {
        this.heuristics = heuristics;
        heuristics.setNewUC(uc);
    }

    public void setSignalColor(Color signalColor)
    {
        this.signalColor = signalColor;
        this.getPath().setColor(signalColor);
        if (getPanel() != null)
        {
            getPanel().displayColor(signalColor);
        }
    }

    public Color getSignalColor()
    {
        return signalColor;
    }

    public ColoredPath getPath()
    {
        return path;
    }
    
    public void cancelTasks()
    {
        stopped = true;
    }

    public UCReducerPanel getPanel()
    {
        return panel;
    }
}
