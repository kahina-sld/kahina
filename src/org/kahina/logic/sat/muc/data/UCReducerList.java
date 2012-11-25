package org.kahina.logic.sat.muc.data;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kahina.core.data.KahinaObject;
import org.kahina.logic.sat.muc.task.ReductionAgent;

public class UCReducerList extends KahinaObject implements Iterable<ReductionAgent>
{
    List<ReductionAgent> reducers;
    
    public UCReducerList()
    {
        reducers = new ArrayList<ReductionAgent>();
    }

    public int size()
    {
        return reducers.size();
    }
    
    public void add(ReductionAgent reducer)
    {
        reducers.add(0,reducer);
    }

    @Override
    public Iterator<ReductionAgent> iterator()
    {
        return reducers.iterator();
    }

    public boolean remove(ReductionAgent reducer)
    {
        return reducers.remove(reducer);   
    }
}
