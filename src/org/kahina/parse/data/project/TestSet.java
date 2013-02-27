package org.kahina.parse.data.project;

import java.util.LinkedList;
import java.util.List;

import org.kahina.core.data.KahinaObject;

public class TestSet extends KahinaObject
{
    List<String> sentences;
    
    public TestSet()
    {
        sentences = new LinkedList<String>();
    }
    
    public TestSet copy()
    {
        TestSet copy = new TestSet();
        for (String sentence : sentences)
        {
            copy.addSentence(new String(sentence));
        }
        return copy;
    }
    
    public List<String> getSentences()
    {
        return sentences;
    }
    
    public void addSentence(String sentence)
    {
        sentences.add(sentence);
    }
}
