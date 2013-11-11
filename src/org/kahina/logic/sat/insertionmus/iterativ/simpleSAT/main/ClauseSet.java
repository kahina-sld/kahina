package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main;

import java.io.IOException;
import java.util.ArrayList;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.IActivity;

/**
 * @author Paul Seitz
 *
 */


public class ClauseSet {

	// Anzahl an Variablen
	private int varNum;

	// Die Klauseln dieser Klauselmenge
	//  protected final ArrayList<Clause> clauses;

	// Die Liste aller Variablen
	public Variable[] variables;
	
	public double initVarAddition = 30.0f;



	// Initiale Unit Klauseln
	public ArrayList<Clause> initUnits = new ArrayList<Clause>();

	// Konstruktor
	public ClauseSet(){
	}
	
	public ClauseSet(int numVariables) {
		this.variables = new Variable[numVariables];
		for (int i = 0; i < numVariables; i++){
			variables[i] = new Variable(i);
		}
	}

	public void pars(final String filename) throws NumberFormatException, IOException {
		//mach ich in der readDimacs() methode. Dort kenne ich die groesse des Vectors
				//bereits und kann daher den Vector mit der korrekten groesse initialisieren
				//    clauses = new Vector<Clause>(); 
				//    variables = new Vector<Variable>();
				//    readDIMACS(filename);
				Parser parser = new Parser(filename);

				//	  this.clauses = new ArrayList<Clause>(parser.anzClauses);
				this.variables = new Variable[parser.anzVariables+1];
				this.varNum = parser.anzVariables;

				for (int i = 0; i <= varNum; i++){
					this.variables[i] = new Variable(i);
				}

				for (int i = 0; i < parser.anzClauses ; i++){
					final Clause c = new Clause(parser.next(), variables);
					if (!c.isTautology()){
						//			  this.clauses.add(c);
						switch (c.initWatch(variables)){
						case UNIT: 
							this.initUnits.add(c);
							break;
						case EMPTY:
							//poesser hack damit in einem solchen fall die unit propagation sofort false ergibt
							this.initUnits = new ArrayList<Clause>();
							ArrayList<Integer> v1 = new ArrayList<Integer>(1);
							v1.add(1);
							Clause c1 = new Clause(v1, variables);
							this.initUnits.add(c1);
							c1.initWatch(variables);
							//				  this.clauses.add(c1);
							ArrayList<Integer> v2 = new ArrayList<Integer>(1);
							v2.add(-1);
							Clause c2 = new Clause(v2, variables);
							this.initUnits.add(c2);
							c2.initWatch(variables);
							//				  this.clauses.add(c2);
							return;
						}
					}
					for (final int l: c.literals){
						final Variable v = this.variables[Math.abs(l)];
						v.activity += initVarAddition;
					}
				}
	}
	
	public String printVars() {
		String res = "";
		for (int i = 1; i <= varNum; i++) {
			res += "Variable " + i + ": " + variables[i] + "\n\n";
		}
		return res;
	}
}
