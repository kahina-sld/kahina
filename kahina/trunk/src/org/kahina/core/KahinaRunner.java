package org.kahina.core;

import org.kahina.data.DbDataManager;
import org.kahina.io.database.DatabaseHandler;

public class KahinaRunner
{
    public static void main(String[] args)
    {
        KahinaInstance kahina = new KahinaInstance(new DbDataManager(new DatabaseHandler()));
        System.err.println("Test.");
    }
}
