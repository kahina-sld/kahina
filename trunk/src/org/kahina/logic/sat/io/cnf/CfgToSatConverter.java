package org.kahina.logic.sat.io.cnf;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.parse.data.cfg.ContextFreeGrammar;
import org.kahina.parse.io.cfg.ContextFreeGrammarParser;

public class CfgToSatConverter
{
    public static CnfSatInstance parsingToSat(ContextFreeGrammar cfg, String[] sentence)
    {
        HashMap<String,Integer> varIndex = new HashMap<String,Integer>();
        //compile the grammar into SAT up to the necessary length
        CnfSatInstance sat = grammarToSat(cfg, varIndex, sentence.length);
        //add constraint that the sentence must be covered
        List<Integer> sentenceConstraint = new LinkedList<Integer>();
        sentenceConstraint.add(varIndex.get("s[" + 0 + "," + sentence.length + "]"));
        sat.addClause(sentenceConstraint);
        //add the units describing the input
        for (int i = 0; i < sentence.length; i++)
        {
            String category = cfg.getCategory(sentence[i]);
            if (category == null)
            {
                System.err.println("WARNING: no lexicon entry found for \"" + sentence[i] + "\", assuming proper name (pn).");
                category = "pn";
            }
            List<Integer> wordConstraint = new LinkedList<Integer>();
            wordConstraint.add(varIndex.get(category + "[" + i + "," + (i+1) + "]"));
            sat.addClause(wordConstraint);
        }
        return sat;
    }
    
    public static CnfSatInstance grammarToSat(ContextFreeGrammar cfg, HashMap<String,Integer> varIndex, int lengthLimit)
    {
        CnfSatInstance sat = new CnfSatInstance();
        int varCounter = 1;
        //generate symbol exclusivity constraints for each interval 
        for (int i = 0; i < lengthLimit; i++)
        {
            for (int j = i+1; j <= lengthLimit; j++)
            {
                for (String symbol : cfg.getSymbols())
                {
                    String symbolWithRange = symbol + "[" + i + "," + j + "]"; 
                    sat.setSymbolMapping(varCounter, symbolWithRange);
                    varIndex.put(symbolWithRange, varCounter++);                
                }
                for (String posSymbol : cfg.getSymbols())
                {
                    int posSymbolWithRange = varIndex.get(posSymbol + "[" + i + "," + j + "]");
                    for (String otherSymbol : cfg.getSymbols())
                    {
                        //symbols linked by unary rules may not exclude each other!
                        if (!cfg.hasUnaryLink(otherSymbol,posSymbol))
                        {
                            if (!otherSymbol.equals(posSymbol))
                            {
                                List<Integer> exclConstraint = new LinkedList<Integer>();
                                exclConstraint.add(- posSymbolWithRange);
                                exclConstraint.add(- varIndex.get(otherSymbol + "[" + i + "," + j + "]"));
                                sat.addClause(exclConstraint);
                            }
                        }
                        else
                        {
                            //System.err.println("Unary link: " + posSymbol + " -> " + otherSymbol);
                        }
                    }
                }
            }
        }
        //generate rule description constraints for each interval
        for (int i = 0; i < lengthLimit; i++)
        {
            for (int j = i+1; j <= lengthLimit; j++)
            {
                for (String head : cfg.getRules().keySet())
                {
                    //the branching clause enforcing presence of an RHS
                    List<Integer> branchingClause = new LinkedList<Integer>();
                    branchingClause.add(- varIndex.get(head + "[" + i + "," + j + "]"));
                    for (List<String> rhs : cfg.getRules().get(head))
                    {
                        StringBuilder rhsSymbol = new StringBuilder(head + "->");
                        for (String symbol : rhs)
                        {
                            rhsSymbol.append(symbol + ",");
                        }
                        rhsSymbol.deleteCharAt(rhsSymbol.length() - 1);
                        rhsSymbol.append("[" + i + "," + j + "]");
                        branchingClause.add(varCounter);
                        sat.setSymbolMapping(varCounter, rhsSymbol.toString());
                        varIndex.put(rhsSymbol.toString(), varCounter);     
                        //TODO: generalize to n-ary rules!
                        //the individual RHS enforcement clause for each choice an partition
                        if (rhs.size() == 1)
                        {
                            List<Integer> rhsEnforcementClause = new LinkedList<Integer>();
                            rhsEnforcementClause.add(- varCounter++);
                            rhsEnforcementClause.add(varIndex.get(rhs.get(0) + "[" + i + "," + j + "]"));
                            sat.addClause(rhsEnforcementClause);
                        }
                        else if (rhs.size() == 2)
                        {
                            //need further distribution symbols here (distinguishing splittings)
                            List<Integer> partitionSplitClause = new LinkedList<Integer>();
                            partitionSplitClause.add(- varCounter++);                    
                            for (int k = i+1 ; k < j; k++)
                            {
                                sat.setSymbolMapping(varCounter, rhsSymbol.toString() + ":" + k);
                                varIndex.put(rhsSymbol.toString() + ":" + k, varCounter); 
                                
                                List<Integer> leftEnforcementClause = new LinkedList<Integer>();
                                leftEnforcementClause.add(- varCounter);
                                leftEnforcementClause.add(varIndex.get(rhs.get(0) + "[" + i + "," + k + "]"));
                                sat.addClause(leftEnforcementClause);
                                List<Integer> rightEnforcementClause = new LinkedList<Integer>();
                                rightEnforcementClause.add(- varCounter);
                                rightEnforcementClause.add(varIndex.get(rhs.get(1) + "[" + k + "," + j + "]"));
                                sat.addClause(rightEnforcementClause);
                                
                                partitionSplitClause.add(varCounter);
                                varCounter++;
                            }
                            sat.addClause(partitionSplitClause);
                        }
                        else
                        {
                            System.err.println("RHS SKIPPED: only unary and binary rules are supported for now!");
                        }
                    }
                    sat.addClause(branchingClause);
                }
            }
        }
        //add clauses forbidding large ranges for terminals
        for (String terminal : cfg.getTerminals())
        {
            for (int i = 0; i < lengthLimit; i++)
            {
                for (int j = i+2; j <= lengthLimit; j++)
                {
                    int posSymbolWithRange = varIndex.get(terminal + "[" + i + "," + j + "]");
                    List<Integer> terminalConstraint = new LinkedList<Integer>();
                    terminalConstraint.add(- posSymbolWithRange);   
                    sat.addClause(terminalConstraint);
                }
            }
        }
        return sat;
    }
    
    public static void main(String[] args)
    {
        if (args.length < 2)
        {
            System.err.println("Usage: java CfgToSatConverter [CFG file] [output file]");
            System.exit(0);
        }
        ContextFreeGrammar cfg = ContextFreeGrammarParser.parseCFGFile(args[0]);
        CnfSatInstance instance = CfgToSatConverter.grammarToSat(cfg, new HashMap<String,Integer>(), 5);
        DimacsCnfOutput.writeDimacsCnfFile(args[1], instance);
    }
}
