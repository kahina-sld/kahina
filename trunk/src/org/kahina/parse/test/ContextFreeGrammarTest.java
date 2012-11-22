package org.kahina.parse.test;

import org.kahina.parse.data.cfg.ContextFreeGrammar;
import org.kahina.parse.io.cfg.ContextFreeGrammarParser;

public class ContextFreeGrammarTest
{
    public static void main(String[] args)
    {
        ContextFreeGrammar cfg = ContextFreeGrammarParser.parseCFGFile("src/org/kahina/parse/data/test/test.cfg");
        System.err.print(cfg.toString());
    }
}
