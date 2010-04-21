package org.kahina.core;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.MemDataManager;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.io.database.DatabaseHandler;

public class KahinaRunner
{
    static DataManager dm;
    static KahinaController control;
    
    public static void initialize(int dataHandlingType)
    {
        if (dataHandlingType == KahinaDataHandlingMethod.MEMORY)
        {
            setDataManager(new MemDataManager());
        }
        else 
        {
            setDataManager(new DbDataManager(new DatabaseHandler()));
        }
        control = new KahinaController();
    }
    
    public static DataManager getDataManager()
    {
        return dm;
    }
    
    public static void setDataManager(DataManager dm)
    {
        KahinaRunner.dm = dm;
    }
    
    public static DatabaseHandler getDatabaseHandler()
    {
        if (dm instanceof DbDataManager)
        {
            return ((DbDataManager) dm).getDatabaseHandler();
        }
        else
        {
            return null;
        }
    }
    
    public static void processEvent(KahinaEvent e)
    {
        control.processEvent(e);
    }
    
    public static KahinaController getControl()
    {
        return control;
    }
    
    public static void main(String[] args)
    {
        KahinaInstance kahina = new KahinaInstance();
        // TODO database handler should be closed
    }
}
