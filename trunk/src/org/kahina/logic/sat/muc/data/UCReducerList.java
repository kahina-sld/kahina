package org.kahina.logic.sat.muc.data;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kahina.core.data.KahinaObject;
import org.kahina.logic.sat.muc.task.UCReducer;

public class UCReducerList extends KahinaObject implements Iterable<UCReducer>
{
    List<UCReducer> reducers;
    
    public UCReducerList()
    {
        reducers = new ArrayList<UCReducer>();
    }

    public int size()
    {
        return reducers.size();
    }
    
    public void add(UCReducer reducer)
    {
        reducers.add(0,reducer);
    }

    @Override
    public Iterator<UCReducer> iterator()
    {
        return reducers.iterator();
    }

    public boolean remove(UCReducer reducer)
    {
        return reducers.remove(reducer);   
    }
}
