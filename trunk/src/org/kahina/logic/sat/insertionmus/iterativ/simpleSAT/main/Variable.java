package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.IActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause.ClauseState;


/**
 * @author Paul Seitz
 *
 */

public class Variable {

	//aktivitaet der Variable
	public double activity = 0;

	public int level = -1;

	public Clause reason = null;


	//auf welchen wert die Variable gesetzt werden soll
	//ist davon abhängig wie häufig die variable positiv, bzw negativ 
	//in 
	protected int setHeuristic = 0;
	public State setNext = State.OPEN;

	// Belegung der Variable oder OPEN, falls unbelegt
	public enum State {
		TRUE, FALSE, OPEN
	};

	// Momentane Belegung der Variable
	public State state;

	// Nummer dieser Variable (von 1..n)
	public final int num;


	// Klauseln, in denen die Variable beobachtet wird
//	public final ArrayList<Clause> watched;
	public final List<Clause> watched;

	//  protected final ArrayList<Clause> positiv = new ArrayList<Clause>(); 
	//  protected final ArrayList<Clause> negativ = new ArrayList<Clause>(); ;


	// Der Konstruktor fuer eine Variable
	public Variable(final int num) {
		this.num = num;
		state = State.OPEN;
		watched = new ArrayList<Clause>();
//		watched = new LinkedList<Clause>();
	}

	public Variable(int num, double activity) {
		state = State.OPEN;
		watched = null;
		this.num = num;
		this.activity = activity;
	}

	public boolean getBoolHeur(){
		if (this.setNext != State.OPEN){
			this.setNext = State.OPEN;
			return setNext == State.TRUE?true:false;
		}
		return this.setHeuristic > 0;
	}

	//  public boolean delet(boolean value, Variable[] vars, ArrayList<Clause> units){
	//	  if (value){
	//		  for (int i = this.positiv.size()-1; i > 0; i--){
	////		  for (Clause c: this.positiv){
	//			  final Clause c = this.positiv.get(i);
	//			  c.delet(vars);
	//		  }
	//		  for (Clause c: this.negativ){
	//			  final ClauseState state = c.removeLiteral(-this.num, vars);
	//			  if (state == ClauseState.UNIT){
	//				  units.add(c);
	//			  }else if (state == ClauseState.EMPTY){
	//				  return false;
	//			  }
	//		  }
	//	  }else{
	//		  for (int i = this.negativ.size()-1; i > 0; i--){
	////		  for (Clause c: this.negativ){
	//			  final Clause c = this.negativ.get(i);
	//			  c.delet(vars);
	//		  }
	//		  for (Clause c: this.positiv){
	//			  final ClauseState state = c.removeLiteral(this.num, vars);
	//			  if (state == ClauseState.UNIT){
	//				  units.add(c);
	//			  }else if (state == ClauseState.EMPTY){
	//				  return false;
	//			  }
	//		  }
	//	  }
	//	  this.watched.clear();
	////	  this.positiv.clear();
	////	  this.negativ.clear();
	//	  return true;
	//  }
	/**
	 * Belegt eine Variable
	 * 
	 * @param val - den Wert auf den belegt werden soll
	 * @param variables - der aktuelle Variablenvektor
	 * @param units - Liste, in der die Unit Klauseln gespeichert werden
	 * @param lvl 
	 * @param stack 
	 * @return eine leere Klausel, falls vorhanden, ansonsten null
	 *            
	 */
	public Clause assign(final boolean val,final Variable[] variables, final ArrayList<Clause> units, final Stack<Variable> stack, final int lvl) {
		//	  System.err.println("BEFORE:");
		//	  for (Clause c: this.watched){
		//		  c.testWatchedLiterals(variables);
		//	  }
		//	  System.err.println();
		//neuer status
		this.state = val ? State.TRUE:State.FALSE;
		//level auf dem belegt wurde
		level = lvl;
		stack.push(this);

		Clause ret = null;
		
		/////////////////////////////////////////////////////////////////
		final ArrayList<Clause> removeLater = new ArrayList<Clause>();
		final int till = this.watched.size();
		for (int i = 0; i<till; i++){
			final Clause c = this.watched.get(i);

			//	  for (final Clause c : this.watched){		  

			final ClauseState s = c.reWatch(variables, removeLater);
			if (s == ClauseState.UNIT){
				units.add(c);
			}else if (s == ClauseState.EMPTY){
				ret = c;
				break;
			}
		}
		this.watched.removeAll(removeLater);
///////////////////////////////////////////////////////////////////////		
		

//		Iterator<Clause> it = this.watched.iterator();
//		while (it.hasNext()){
//			final Clause c = it.next();
//			final ClauseState s = c.reWatch(variables, it);
//			if (s == ClauseState.UNIT){
//				units.add(c);
//			}else if (s == ClauseState.EMPTY){
//				ret = c;
//				break;
//			}
//		}
		
		
		//	  System.err.println("AFTER:");
		//	  for (Clause c: removeLater){
		//		  c.testWatchedLiterals(variables);
		//	  }
		//	  for (Clause c: this.watched){
		//		  c.testWatchedLiterals(variables);
		//	  }
		//	  System.err.println();
		return ret;
	}

	@Override
	public String toString() {
		String res = this.num +  ": [" + state + " ";
		res += "\n\tAdjacence List: ";
		for (Clause clause : watched) {
			res += clause + " ";
		}
		return "activity: " + this.activity + " | " + res + "reason: " + this.reason + " Level: "+ this.level +"]\n";
	}
	/**
	 * Nach dem aufruf ist diese Variable unbelegt.
	 * @param activity2 
	 */
	public void unassign(IActivity activity2) {
		this.state = State.OPEN;
		this.level = -1;
		activity2.unasignedVariable(this);
	}

	/**
	 * vB: literal entspricht der Variablen
	 * @param literal
	 * @return ob das literal zur zeit erfüllt ist.
	 */
	public boolean litIsSat(int literal) {
		if (this.state == State.OPEN) return false;
		
		return (this.state==State.FALSE)?(literal < 0):(literal > 0);
	}
}
