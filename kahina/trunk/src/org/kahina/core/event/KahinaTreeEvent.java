package org.kahina.core.event;

public class KahinaTreeEvent extends KahinaEvent
{
    //TODO: include info on which tree the event concerns
    
    private int treeEventType;
    private int firstID;
    private int secondID;
    
    public KahinaTreeEvent(int treeEventType, int firstID, int secondID)
    {
        super("tree");
        this.treeEventType = treeEventType;
        this.firstID = firstID;
        this.secondID = secondID;
    }
    
    public int getTreeEventType()
    {
        return treeEventType;
    }
    
    public int getFirstID()
    {
        return firstID;
    }
    
    public int getSecondID()
    {
        return secondID;
    }
}
