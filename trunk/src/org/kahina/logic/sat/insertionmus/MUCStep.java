package org.kahina.logic.sat.insertionmus;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.ConcurrentSkipListSet;

import org.kahina.core.KahinaStep;
import org.kahina.logic.sat.insertionmus.algorithms.AbstractAlgorithm;
import org.kahina.logic.sat.insertionmus.algorithms.AlgorithmData;
import org.kahina.logic.sat.insertionmus.algorithms.BasicAlgorithm;
import org.kahina.logic.sat.insertionmus.algorithms.Heuristics.ISortingHeuristic;

public class MUCStep extends KahinaStep
{
    //IC = interesting constraint
    
    //state data (should be kept as a sorted structure to allow quick identity check!)
//    private final List<Integer> uc; //current unsatisfiable core (not necessarily minimal)
    
    //for each IC, store the state that we arrive in after removing it
    //null = unchecked (the links has not yet been established
    //-1 = we already know that removing the IC leads to a satisfiable clause set
    //otherwise: result is the ID of the step we end up in after removing the IC
//    private Map<Integer,Integer> reductionTable; 
    
    //makes it possible to annotate UCs with satisfiability information
    boolean satisfiable;

	private AlgorithmData data;
	private AlgorithmData oldData;

	static public AbstractAlgorithm alg = new BasicAlgorithm();;

	private int ID;
    
    
//    public MUCStep(AlgorithmData data)
//    {
//    	this.data = data;
//    	
//
//        uc =  new ArrayList<Integer>();
////        reductionTable = new ConcurrentSkipListMap<Integer,Integer>();
//        satisfiable = false;
//    }

    public MUCStep(AlgorithmData data, AbstractAlgorithm alg) {
    	this.oldData = data.clone();
    	this.data = data;
    	this.alg = alg;
//        uc =  new ArrayList<Integer>();
//      reductionTable = new ConcurrentSkipListMap<Integer,Integer>();
      satisfiable = false;
	}

	public int getSize()
    {
        return data.M.size();
    }
    
    public int getStepType()
    {
//        int numberRed = 0;
//        boolean hasLightGreen = false;
//        for (int clauseID : uc)
//        {
//            Integer status = reductionTable.get(clauseID);
////            if (status == null) return MUCStepType.UNKNOWN;
//            if (status == -1) numberRed++;
//            if (status == -2) hasLightGreen = true;
//        }
//        if (numberRed == uc.size())
//        {
//            //the status for a MUC
//            return 2;
//        }
//        if (hasLightGreen)
//        {
//            return 3;
//        }
    	if (data.isMUS()){
    		return 2;
    	}
        return 1;
    }
//
//    public Integer getIcStatus(int index)
//    {
//        Integer status = reductionTable.get(index);
//        if (status == null) return 0;
//        else if (status == -1) return 2;
//        else if (status == -2) return 3;
//        return 1;
//    }
    
    //returns null if we have no information yet!
//    public Integer getRemovalLink(int index)
//    {
//        return this.reductionTable.get(index);
//    }
//    
//    public void setRemovalLink(int index, int link)
//    {
//        this.reductionTable.put(index,link);
//    }
    
    //bit of a suboptimal hash function, but very easy to "compute"
    @Override
    public int hashCode()
    {
    	if (data.M.size() > 0){
    		return this.data.M.size()*this.data.instanceIDs.size()*this.data.M.first();
    	}else{
    		return this.data.instanceIDs.size();
    	}
    }
//    
    @Override
    public boolean equals(Object o)
    {
        if (o instanceof MUCStep)
        {
//            List<Integer> otherUC = ((MUCStep) o).getUc();
//            if (otherUC.size() == uc.size())
//            {
//                for (int ic : otherUC)
//                {
//                    if (!uc.contains(ic)) return false;
//                }
//                return true;
//            }
        	MUCStep step = (MUCStep)o;
        	return this.data.equals(step.data);
        }
        return false;
    }
//    
//    public int numUnknownClauses()
//    {
//        return uc.size() - reductionTable.size();
//    }
//    
//    public boolean isSatisfiable()
//    {
//        return satisfiable;
//    }

	public Integer[] getUc() {
		return data.instanceIDs.toArray(new Integer[data.instanceIDs.size()]);
	}

	public AbstractAlgorithm getAlgorithm() {
		return this.alg;
	}

	public void setID(int id) {
		this.ID = id;
	}

	public int getID() {
		return ID;
	}

	public AlgorithmData getData() {
		return this.data;
	}

	public void reset() {
		this.data = oldData.clone();
//		this.alg.setData(data);
	}
    
//    public void setSatisfiable(boolean satisfiable)
//    {
//        this.satisfiable = satisfiable;
//    }
//    
    /**
     * Computes the relation of a given block of meta variables to the current UC.
     * @param block a list of meta variables (typically negative literals)
     * @return 1 if block is guaranteed to be in MUC (all red -> red, selectable), 
     *         2 if block was successfully removed from UC (all green -> green, selectable), 
     *         3 if no element of the block was contained in UC (-> grey), 
     *         4 if a part of the block is guaranteed to be in MUC (some red -> light red, selectable),
     *         5 if a part of the block is guaranteed fall away (some green -> light green, selectable),
     *         0 otherwise (-> block, selectable)
     */
//    public int relationToBlock(TreeSet<Integer> block)
//    {
//        int numRed = 0;
//        int numGreen = 0;
//        int numOut = 0;
//        for (int lit : block)
//        {
//            if (!uc.contains(lit))
//            {
//                numOut++;
//            }
//            else
//            {
//                int icStatus = getIcStatus(lit);
//                if (icStatus == 2)
//                {
//                    numRed++;
//                }
//                else if (icStatus == 1)
//                {
//                    numGreen++;
//                }
//            }
//        }
//        if (numGreen == block.size())
//        {
//            return 2;
//        }
//        if (numOut == block.size())
//        {
//            return 3;
//        }
//        if (numRed == block.size())
//        {
//            return 1;
//        }
//        if (numRed > 0)
//        {
//            return 4;
//        }
//        if (numGreen > 0)
//        {
//            return 5;
//        }
//        return 0;
//    }
	
	public void changeHeuristic(ISortingHeuristic heuristic){
		this.data.setHeuristic(heuristic);
	}

	public void setAlgorithm(AbstractAlgorithm alg2) {
		alg = alg2;
	}
}
