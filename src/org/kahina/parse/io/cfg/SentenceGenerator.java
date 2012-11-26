package org.kahina.parse.io.cfg;

import java.util.LinkedList;
import java.util.List;

import org.kahina.parse.data.cfg.ContextFreeGrammar;

public class SentenceGenerator
{
    ContextFreeGrammar cfg;
    String[] words;
    
    public SentenceGenerator(ContextFreeGrammar cfg)
    {
        this.cfg = cfg;
        this.words = cfg.getWords().toArray(new String[0]);
    }

    //generates the i-th sentence in an order over all grammatical sentences
    //i will be used as a seed for making expansion decisions
    //i must be handed over as an array of size 1 to pass by reference
    //duplicates will occur for sentences with multiple parses
    public List<String> generateGrammaticalSentence(String startCat, int[] i)
    {
        System.err.print("(" + startCat);
        List<String> sentence = new LinkedList<String>();
        List<List<String>> bodyList = cfg.getRules().get(startCat);
        if (bodyList != null)
        {
            int idx = 0;

            //energy used up, search for the first non-recursive rule
            if (i[0] == 0)
            {
                for (int j = 0; j < bodyList.size(); j++)
                {
                    if (!bodyList.get(j).contains(startCat))
                    {
                        idx = j;
                        break;
                    }
                }
                System.err.print("%" + bodyList.size() + "=" + i[0] % bodyList.size() + "R" + idx + "->" + i[0]);
            }
            else
            {
                idx = i[0] % bodyList.size();
                i[0] -= idx;
                i[0] /= bodyList.size();
                System.err.print("%" + bodyList.size() + "=" + idx + "->" + i[0]);
            }
            for (String symbol : bodyList.get(idx))
            {
                sentence.addAll(generateGrammaticalSentence(symbol, i));
            }
        }
        else
        {
            List<String> realizations = cfg.getRealizations(startCat);
            int idx = i[0] % realizations.size();
            i[0] -= idx;
            i[0] /= realizations.size();
            sentence.add(realizations.get(idx));
            System.err.print("%" + realizations.size() + "=" + idx + ":" + realizations.get(idx) + "->" + i[0]);
        }
        System.err.print(")");
        return sentence;
    }
    
    //simply uses the |words|-ary encoding of i
    public List<String> generateWordSequence(int i)
    {
        List<String> sequence = new LinkedList<String>();
        int idx = i % words.length;
        while (i > 0)
        {
            sequence.add(words[i]);
            i -= idx;
            i /= words.length;
            idx = i % words.length;
        }
        return sequence;
    }
}
