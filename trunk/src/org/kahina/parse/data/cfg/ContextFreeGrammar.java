package org.kahina.parse.data.cfg;

import org.kahina.core.data.KahinaObject;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class ContextFreeGrammar extends KahinaObject
{
    Map<String,Set<List<String>>> rules;
    
    public ContextFreeGrammar()
    {
        rules = new TreeMap<String,Set<List<String>>>();
    }
    
    public Map<String,Set<List<String>>> getRules()
    {
        return rules;
    }
    
    public void addRule(String head, List<String> body)
    {
        Set<List<String>> bodiesForHead = rules.get(head);
        if (bodiesForHead == null)
        {
            bodiesForHead = new HashSet<List<String>>();
            rules.put(head, bodiesForHead);
        }
        bodiesForHead.add(body);
    }
    
    public String toString()
    {
        StringBuilder s = new StringBuilder("CFG rules:\n");
        for (String head : rules.keySet())
        {
            s.append(head);
            s.append(" -> ");
            for (List<String> body : rules.get(head))
            {
                for (String symbol : body)
                {
                    s.append(symbol);
                    s.append(" ");
                }
                s.append("| ");
            }
            s.delete(s.length() - 3, s.length());
            s.append("\n");
        }
        return s.toString();
    }
}
