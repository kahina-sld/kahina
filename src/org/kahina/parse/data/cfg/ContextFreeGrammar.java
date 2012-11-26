package org.kahina.parse.data.cfg;

import org.kahina.core.data.KahinaObject;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class ContextFreeGrammar extends KahinaObject
{
    Map<String,List<List<String>>> rules;
    Set<String> symbols;
    Map<String,Set<String>> unaryLinks;
    
    //lexicon: a map from strings to terminals
    Map<String, String> lexicon;
    //inverse lexicon: map from terminals to realizations
    Map<String, List<String>> realizations;
    
    public ContextFreeGrammar()
    {
        rules = new TreeMap<String,List<List<String>>>();
        symbols = new TreeSet<String>();
        unaryLinks = new TreeMap<String,Set<String>>();
        lexicon = new TreeMap<String, String>();
        realizations = new TreeMap<String, List<String>>();
    }
    
    public Set<String> getSymbols()
    {
        return symbols;
    }
    
    public Set<String> getWords()
    {
        return lexicon.keySet();
    }
    
    public Map<String,List<List<String>>> getRules()
    {
        return rules;
    }
    
    public List<String> getRealizations(String terminal)
    {
        return realizations.get(terminal);
    }
    
    public String getCategory(String word)
    {
        return lexicon.get(word);
    }
    
    public Set<String> getTerminals()
    {
        HashSet<String> terminals = new HashSet<String>();
        for (String symbol : symbols)
        {
            if (rules.get(symbol) == null)
            {
                terminals.add(symbol);
            }
        }
        return terminals;
    }
    
    public void addLexEntry(String word, String category)
    {
        lexicon.put(word, category);
        List<String> wordList = realizations.get(category);
        if (wordList == null)
        {
            wordList = new LinkedList<String>();
            realizations.put(category, wordList);
        }
        wordList.add(word);
    }
    
    public void addRule(String head, List<String> body)
    {
        symbols.add(head);
        List<List<String>> bodiesForHead = rules.get(head);
        if (bodiesForHead == null)
        {
            bodiesForHead = new LinkedList<List<String>>();
            rules.put(head, bodiesForHead);
        }
        bodiesForHead.add(body);
        for (String symbol : body)
        {
            symbols.add(symbol);
        }
        //process unary rules (effectively generating equivalence classes)
        if (body.size() == 1)
        {
            processUnaryLink(head,body.get(0));
        }
    }
    
    private void processUnaryLink(String symbol1, String symbol2)
    {
        if (!hasUnaryLink(symbol1, symbol2))
        {
            //a new link between two classes
            Set<String> links1 = unaryLinks.get(symbol1);
            if (links1 == null)
            {
                links1 = new HashSet<String>();
                unaryLinks.put(symbol1, links1);
            }
            links1.add(symbol1);
            Set<String> links2 = unaryLinks.get(symbol2);
            if (links2 == null)
            {
                links2 = new HashSet<String>();
                unaryLinks.put(symbol2, links2);
            }
            links2.add(symbol2);
            Set<String> links1copy = new HashSet<String>();
            links1copy.addAll(links1);
            Set<String> links2copy = new HashSet<String>();
            links2copy.addAll(links2);
            for (String l1 : links1copy)
            {
                for (String l2 : links2copy)
                {
                    //System.err.println("Adding link " + symbol1 + " -> " + symbol2);
                    unaryLinks.get(l1).add(l2);
                    unaryLinks.get(l2).add(l1);
                }
            }
            links1.remove(symbol1);
            links2.remove(symbol2);
        }
    }
    
    public boolean hasUnaryLink(String symbol1, String symbol2)
    {
        Set<String> links = unaryLinks.get(symbol1);
        if (links == null) return false;
        return links.contains(symbol2);
    }
    
    public String toString()
    {
        StringBuilder s = new StringBuilder("Used symbols:\n");
        for (String symbol : symbols)
        {
            s.append(symbol);
            s.append(" ");
        }
        s.append("\n");
        s.append("CFG rules:\n");
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
        s.append("Lexicon :\n");
        for (String terminal : realizations.keySet())
        {
            s.append(terminal);
            s.append(" -> ");
            for (String word : realizations.get(terminal))
            {
                s.append(word + " | ");
            }
            s.delete(s.length() - 3, s.length());
            s.append("\n");
        }
        return s.toString();
    }
}
