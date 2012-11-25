package org.kahina.logic.sat.muc.visual;

import java.util.HashMap;
import java.util.Map;

import javax.swing.JComponent;

import org.kahina.core.visual.KahinaView;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.data.UCReducerList;
import org.kahina.logic.sat.muc.heuristics.ReductionHeuristics;
import org.kahina.logic.sat.muc.task.ReductionAgent;

public class UCReducerListView extends KahinaView<UCReducerList>
{
    ReductionAgent newReducer;
    
    MUCInstance kahina;
    MiniSATFiles files;
    
    Map<String,Class<? extends ReductionHeuristics>> heuristics;
    
    public UCReducerListView(MUCInstance kahina, MiniSATFiles files)
    {
        super(kahina);
        this.kahina = kahina;
        this.model = new UCReducerList();
        
        newReducer = new ReductionAgent(kahina.getState(),1,files);
        heuristics = new HashMap<String,Class<? extends ReductionHeuristics>>();
    }
    
    public void addHeuristic(Class<? extends ReductionHeuristics> heuristic)
    {
        //TODO: this is really ugly, but seems to be the only feasible way to read out the desired name string
        try
        {
            heuristics.put(heuristic.newInstance().getName(),heuristic);
        }
        catch (IllegalArgumentException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (SecurityException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (IllegalAccessException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (InstantiationException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    @Override
    public JComponent makePanel()
    {
        UCReducerListViewPanel panel = new UCReducerListViewPanel(kahina, heuristics);
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return panel;
    }

}
