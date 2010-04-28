package org.kahina.core.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.KahinaObject;

public class KahinaBehavior<T extends KahinaObject>
{
    protected T object;
    protected KahinaInstance kahina;
    
    public KahinaBehavior(T object, KahinaInstance kahina)
    {
        this.object = object;
        this.kahina = kahina;
    }
}
