package org.kahina.core.visual.dag;

public class NodeAvPosPair implements Comparable<NodeAvPosPair>
{
	int nodeID;
	double avPos;
	
	public NodeAvPosPair(int nodeID, double avPos)
	{
		this.nodeID = nodeID;
		this.avPos = avPos;
	}
	
	public int compareTo(NodeAvPosPair otherPair)
	{
		if (avPos == otherPair.avPos)
		{
			//sort nodes with odd inDegree further to the left?
			/*if (currentDAG.getIncomingEdges(otherPair.nodeID).size() % 2 == 1)
			{
				return -1;
			}
			else*/
			{
				return 0;
			}
		}
		else
		{
			if (avPos < otherPair.avPos)
			{
				return -1;
			}
			else
			{
				return 1;
			}
		}
	}
}
