package org.kahina.core;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.CachingDbDataManager;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.MagazineDataManager;
import org.kahina.core.data.MemDataManager;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.io.database.DatabaseHandler;

public class KahinaRunner
{
    private static DataManager dm;
    private static KahinaController control;
    private static KahinaDataHandlingMethod dataHandlingMethod;
    
    public static void initialize(KahinaDataHandlingMethod dataHandlingType)
    {
    	dataHandlingMethod = dataHandlingType;
    	control = new KahinaController();
        if (dataHandlingType == KahinaDataHandlingMethod.MEMORY)
        {
            setDataManager(new MemDataManager());
        }
        else if (dataHandlingType == KahinaDataHandlingMethod.DATABASE)
        {
            setDataManager(new CachingDbDataManager(new DatabaseHandler(DatabaseHandler.DatabaseType.DERBY)));
        } else
        {
        	setDataManager(new MagazineDataManager());
        }
    }
    
    public static DataManager getDataManager()
    {
        return dm;
    }
    
    private static void setDataManager(DataManager dm)
    {
    	dm.initialize();
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

	public static KahinaDataHandlingMethod getDataHandlingMethod()
	{
		return dataHandlingMethod;
	}
}
