package org.kahina.core.data.text;

import org.kahina.core.data.KahinaObject;

public class KahinaLineReference extends KahinaObject
{
    /**
	 * 
	 */
	private static final long serialVersionUID = -7344672902839311198L;
	public KahinaTextModel text;
    public int line;
    
    public int step;
    
    public KahinaLineReference()
    {
        this.text = null;
        this.line = -1;
        this.step = -1;
    }
    
    public KahinaLineReference(int line, int step)
    {
        this.text = null;
        this.line = line;
        this.step = step;
    }
    
    public KahinaLineReference(KahinaTextModel text, int line, int step)
    {
        this.text = text;
        this.line = line;
        this.step = step;
    }
    
    @Override
	public String toString()
    {
        return text.text.getLine(line);
    }
    
    public int getStepID()
    {
        return step;
    }
    
    public KahinaTextModel getText()
    {
        return text;
    }
    
    public int getLine()
    {
        return line;
    }
}
