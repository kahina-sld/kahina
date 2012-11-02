package org.kahina.logic.sat.data.model;

import java.util.Collection;
import java.util.TreeMap;

public class PartialAssignment
{
   private TreeMap<Integer,Boolean> assignments;
   
   public PartialAssignment()
   {
       assignments = new TreeMap<Integer, Boolean>();
   }
   
   public Boolean getValue(int var)
   {
       return assignments.get(var);
   }
   
   public void assign(int var, boolean value)
   {
       assignments.put(var, value);
   }
   
   public Collection<Integer> assignedVars()
   {
       return assignments.keySet();
   }
   
   public int size()
   {
       return assignments.size();
   }
}
