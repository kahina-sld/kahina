package org.kahina.logic.sat.data;

import java.util.HashMap;

import org.kahina.core.data.KahinaObject;

public class KahinaSatInstance extends KahinaObject
{
    protected HashMap<Integer,String> symbolTable;
    
    public KahinaSatInstance()
    {
        symbolTable = new HashMap<Integer,String>();
    }
    
    public void setSymbolMapping(int id, String symbol)
    {
        symbolTable.put(id, symbol);
    }
    
    public String getSymbolForLiteral(int literal)
    {
        if (Integer.signum(literal) == 1)
        {
            String symbol = symbolTable.get(literal);
            if (symbol == null) return literal + "";
            return symbol;
        }
        else
        {
            String symbol = symbolTable.get(-literal);
            if (symbol == null) return literal + "";
            return "-" + symbol;
        }
    }
}
