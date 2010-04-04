package org.kahina.behavior;

import org.kahina.control.KahinaController;
import org.kahina.core.KahinaInstance;
import org.kahina.data.KahinaObject;

public class KahinaBehavior<T extends KahinaObject>
{
    protected T object;
    protected KahinaController control;
    protected KahinaInstance kahina;
    
    public KahinaBehavior(T object, KahinaController control, KahinaInstance kahina)
    {
        this.object = object;
        this.control = control;
        this.kahina = kahina;
    }
}
