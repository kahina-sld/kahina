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
    
    public List<String> getSentences()
    {
        return sentences;
    }
    
    public void addSentence(String sentence)
    {
        sentences.add(sentence);
    }
}
