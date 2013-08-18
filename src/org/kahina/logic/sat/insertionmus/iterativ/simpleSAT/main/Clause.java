package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main;
import java.util.ArrayList;
import java.util.Iterator;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Clause.ClauseState;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Variable.State;

/**
 * @LICENSE
 * 			FREE BEER LICENSE VERSION 1.02

The free beer license is a license to give free software to you and free
beer (in)to the author(s).

Your rights are :

0. You can use this piece of software in anyway you like.

1. You can redistribute this piece of software in source form or in 
   compiled form. 

2. You can alter the source to your needs and redistribute the altered 
   source in source form or in compiled form.

However :

0. This program is provided without warranty of any kind. So, if it 
   breaks anything, for example itself, it is up to you. 

1. If you redistribute this piece of software, you are not allowed to 
   charge money for this piece of software itself.

2. If you redistribute this pieces of software in binary form, you must 
   supply the source code as well.

3. If you redistribute this software, modified or not, you must 
   redistribute it under this license and you must include the name of 
   the original author(s) and you must point out where the original 
   source can be obtained.

4. If you use this piece of software frequently, and you think it is 
   worth a couple of euros, you are not allowed to send the author 
   anything else than beer or means that provide facilities to get beer 
   into the author(s) (i.e. openers, glasses).

 * @author Paul Seitz
 *
 */
public class Clause {

	public enum ClauseState {
		SAT, EMPTY, UNIT, SUCCESS
	};

	// Die Literale, die in der Klausel vorkommen
	public final ArrayList<Integer> literals;

	// Der Konstruktor fuer eine Klausel beim einlesen
	public Clause(final ArrayList<Integer> literals,final Variable[] variables) {

		this.literals = literals;
		for (final int lit: literals){
			final Variable v = variables[Math.abs(lit)];
			v.activity++;
			if (lit > 0){
				v.setHeuristic++;
				//    		v.positiv.add(this);
			}else{
				v.setHeuristic--;
				//    		v.negativ.add(this);
			}
		}
	}

	//Der Konstruktor fuer gelernte Klauseln
	public Clause (final ArrayList<Integer> literals){
		this.literals = literals;	  
	}
	public void setBoolHeur(final Variable[] variables){
		for (final int lit: literals){
			final Variable v = variables[Math.abs(lit)];
			if (lit > 0){

				v.setHeuristic++;
				//	    		v.positiv.add(this);
			}else{
				v.setHeuristic--;
				//	    		v.negativ.add(this);
			}
		}
	}
	public void delBoolHeur(final Variable[] variables){
		for (final int lit: literals){
			final Variable v = variables[Math.abs(lit)];
			if (lit > 0){
				v.setHeuristic--;
			}else{
				v.setHeuristic++;
			}
		}
	}


	// Gibt ein unbelegtes Literal dieser Klausel zurück
	public int getUnassigned(final Variable[] variables) {
		final int from = literals.size() -1;
		for (int i  = from; i >= 0; i--) {
			final int l = literals.get(i);
			if (variables[Math.abs(l)].state == State.OPEN){
				if (i > 0){//TODO
					final int temp = literals.get(from);
					i--;
					literals.set(from, literals.get(i));
					literals.set(i, temp);
				}

				return l;	    	
			}
		}
		return 0;
	}

	// Gibt den index eines unbelegten Literals dieser Klausel zurück
	public int getUnassignedIndex(final Variable[] variables) {
		final int from = literals.size() -1;
		for (int i  = from; i >= 0; i--) {
			final int l = literals.get(i);
			if (variables[Math.abs(l)].state == State.OPEN){
				return i;	    	
			}
		}
		return -1;
	}
	@Override
	public String toString() {
		String res = "{ ";
		for (Integer i : literals) {
			res += i + " ";
		}
		return res + "}";
	}



	/**
	 * Initialisiert die watched literals
	 * 
	 * @param variables - der aktuelle Variablenvektor
	 * @return Den Status der Initialisierung (EMPTY, UNIT oder SUCCESS)
	 *            
	 */  
	public ClauseState initWatch(final Variable[] variables) {
		switch (this.literals.size()){
		case 0:
			return ClauseState.EMPTY;
		case 1:
			variables[Math.abs(this.literals.get(0))].watched.add(this);
			return ClauseState.UNIT;
		}

		variables[Math.abs(this.literals.get(0))].watched.add(this);
		variables[Math.abs(this.literals.get(1))].watched.add(this);
		return ClauseState.SUCCESS;
	}

	/**
	 * Sucht ein neues watched literal fuer diese Klausel
	 * 
	 * @param variables - der aktuelle Variablenvektor
	 * @param lit - das Literal, das belegt wurde
	 * @param removeLater 
	 * @return den Status der Suche (SUCCESS, EMPTY, SAT oder UNIT)
	 * 
	 * during this process the clause could have 3 watched literals, they should be removed by another function.
	 *            
	 */
	public ClauseState reWatch(final Variable[] variables, final ArrayList<Clause> removeLater) {
//TODO nach rewatch 3 watched literals!!
		boolean secoundOpenVar = false;

		boolean watchedSet = false;

		final int size = this.literals.size() -1;
		for (int i = size; i >= 0; i--){//pruefe fuer jede variable ob sie Frei oder Erfuellt ist

			final int l = literals.get(i);
			final Variable v = variables[Math.abs(l)];
			if (v.state == State.OPEN){
				//Die zweite Unbelegte Variable wurde gefunden
				if (watchedSet) {
					//setze offenes literal an das ende der liste. Dadurch wird es beim naechsten rewatch oder 
					//getUnassigned frueher gefunden.
					literals.set(i, literals.get(size));
					literals.set(size, l);
					return ClauseState.SUCCESS;
				}

				//Offene variable gefunden. Diese wird jedoch schon beobachtet.
				if (v.watched.contains(this)){
					secoundOpenVar = true;
				}else{				
					//Offene und nicht beobachtete Variable gefunden
					v.watched.add(this);
					removeLater.add(this);
					//es wurde schon die zweite offene und beobachtete variable gefunden
					if (secoundOpenVar) return ClauseState.SUCCESS;
					else watchedSet = true;
				}
				//Variable ist erfuellt
			}else if (v.state == State.FALSE && l < 0 || v.state == State.TRUE && l > 0){
				return ClauseState.SAT;
			} 
		}


		if (secoundOpenVar || watchedSet) return ClauseState.UNIT;	  

		return ClauseState.EMPTY;
	}

	/**
	 * Testet ob diese Klausel eine Tautologie ist
	 */
	public boolean isTautology() {
		final int literalsSize = literals.size();
		for (int i = 0; i < literalsSize; i++){
			final int iLit = literals.get(i);
			for (int j = i+1; j < literalsSize; j++){
				if (iLit == -this.literals.get(j))
					return true;
			}
		}
		return false;
	}

	/**
	 * Setzt die watched literals auf eine unbelegte variable und die variable mit dem hoechsten lvl
	 * vb.: Es ist genau eine Variable unbelegt
	 */
	public void setWatched(final Variable[] variables) {
//		for (int l :this.literals){
//			if (variables[Math.abs(l)].watched.contains(this)){
//				System.err.println(l);
//				System.err.println(this);
//				System.err.println("WHY???");
//			}
//		}

		//waehle als erste freie variable eine unbelegte variable aus.
		final Integer index = this.getUnassigned(variables);
		//		final int lIndex = getUnassignedIndex(variables);
		//		final int index = this.literals.get(lIndex);
		variables[Math.abs(index)].watched.add(this);


		final int literalsSize = this.literals.size();
		if (literalsSize > 1){
			
			chooseHighestAsLastWatched(variables, index);
		}
		//		literals.set(lIndex, this.literals.get(literalsSize -1));
		//		literals.set(literalsSize-1, index);		
	}

	/*
	 * vb.: Es wurde bereits ein watched literal gesetzt.
	 */
	private void chooseHighestAsLastWatched(final Variable[] variables,
			final Integer index) {
		final int literalsSize = this.literals.size();
		if (literalsSize == 1) return;
		
		//waehle als zweite variable jene mit dem hoechsten level aus
		int i = 0;
		int varIndex2 = Math.abs(this.literals.get(i));
		if (varIndex2 == index){
			i++;
			varIndex2 = Math.abs(this.literals.get(i));
		}
		Variable max = variables[varIndex2];
		
		for (i++; i < literalsSize; i++){
			final int varIndex = Math.abs(this.literals.get(i));
			final Variable var = variables[varIndex];
			if (max.level < var.level && varIndex != index){
				max = var;
			}
		}
		max.watched.add(this);
	}
	/**
	 * Setzt die watched literals auf zwei unbelegte variablen
	 * vb.: es sind 2 unbelegte variablen vorhanden
	 */
	public void setWatchedToUnsigned(final Variable[] variables) {
		//waehle zwei unbelegte variablen aus
		final int size = this.literals.size()-1;
		int i = size;
		Variable v1;
		Variable v2;
		int index;
		int literal;
		do {
			if (i < 0){
				//TODO SAT or UNSAT? (keine unbelegte variable)
				for (int j = 0; j <= size; j++){
					final int l = this.literals.get(j);
					v1 = variables[Math.abs(j)];
					if (v1.state == ((l > 0)?State.TRUE:State.FALSE)){
						v1.watched.add(this);
						chooseHighestAsLastWatched(variables, Math.abs(j));
					}else{
						System.out.println("Clause is UNSAT");
						//TODO backjump
					}
				}
				return;
			}
			literal = this.literals.get(i);
			index = Math.abs(literal);
			v1 = variables[index];
			i--;
		}while (v1.state != State.OPEN || v1.litIsSat(literal));
		

		do {
			if (i < 0){
				v1.watched.add(this);
				chooseHighestAsLastWatched(variables, index);
				return;
			}
			v2 = variables[Math.abs(this.literals.get(i))];
			i--;
		}while (v2.state != State.OPEN);
		
		v1.watched.add(this);
		v2.watched.add(this);
		//		if (i < -1)System.err.println(this);
	}




	//////////////// Mathoden die ich nur zum debugen genutzt habe ///////////////////////

	public void testWatchedLiterals(Variable[] variables){
		int count = 0;
		int countwrong = 0;
		for (Variable v: variables){
			if (v.watched.contains(this)) {
				count++;
				if (v.state == State.FALSE && this.literals.contains(v.num) || v.state == State.TRUE && this.literals.contains(-1*v.num)){
					countwrong++;
				}
			}
		}
		if (count != 2) {
			if (this.literals.size() == 1 && count == 1){

			}else{
				System.err.println(this + "has " + count + " watched literals");
			}
		}
	}

	public boolean testSat(Variable[] variables) {
		for (Integer l: this.literals){
			if (variables[Math.abs(l)].state == State.FALSE && l < 0 || variables[Math.abs(l)].state == State.TRUE && l > 0){
				//				System.out.println(this + " true because of " + l);
				return true;
			}
		}
		return false;
	}
	//	public boolean testUnSat(Vector<Variable> variables) {
	//		for (Integer l: this.literals){
	//			if (variables.get(Math.abs(l)).state == State.FALSE && l < 0 || variables.get(Math.abs(l)).state == State.TRUE && l > 0 || variables.get(Math.abs(l)).state == State.OPEN){
	//				return false;
	//			}
	//		}
	//		return true;
	//	}

	public ClauseState reWatch(Variable[] variables, Iterator<Clause> it) {

		//TODO nach rewatch 3 watched literals!!
				boolean secoundOpenVar = false;

				boolean watchedSet = false;

				final int size = this.literals.size() -1;
				for (int i = size; i >= 0; i--){//pruefe fuer jede variable ob sie Frei oder Erfuellt ist

					final int l = literals.get(i);
					final Variable v = variables[Math.abs(l)];
					if (v.state == State.OPEN){
						//Die zweite Unbelegte Variable wurde gefunden
						if (watchedSet) {
							//setze offenes literal an das ende der liste. Dadurch wird es beim naechsten rewatch oder 
							//getUnassigned frueher gefunden.
							literals.set(i, literals.get(size));
							literals.set(size, l);
							return ClauseState.SUCCESS;
						}

						//Offene variable gefunden. Diese wird jedoch schon beobachtet.
						if (v.watched.contains(this)){
							secoundOpenVar = true;
						}else{				
							//Offene und nicht beobachtete Variable gefunden
							v.watched.add(this);
							it.remove();
							//es wurde schon die zweite offene und beobachtete variable gefunden
							if (secoundOpenVar) return ClauseState.SUCCESS;
							else watchedSet = true;
						}
						//Variable ist erfuellt
					}else if (v.state == State.FALSE && l < 0 || v.state == State.TRUE && l > 0){
						return ClauseState.SAT;
					} 
				}


				if (secoundOpenVar || watchedSet) return ClauseState.UNIT;	  

				return ClauseState.EMPTY;
	}

//	public void delet(Variable[] vars) {
//		for (final int l : this.literals){
//			final Variable v = vars[Math.abs(l)];
//			//			if (l > 0){
//			//				v.positiv.remove(this);
//			//			}else{
//			//				v.negativ.remove(this);
//			//			}
//			v.watched.remove(this);
//		}
//		this.testWatchedLiterals(vars);//TODO remove this line
//	}

//	public ClauseState removeLiteral(int l, Variable[] vars) {
//		int till = this.literals.size();
//		for (int i = 0; i < till; i++){
//			if (this.literals.get(i) == l){
//				this.literals.remove(i);
//				if (vars[Math.abs(l)].watched.remove(this)){
//					return this.reWatch(vars, new ArrayList<Clause>());
//				}
//				return ClauseState.SUCCESS;
//			}
//		}
//		System.err.println("This code should not be reached");
//		return ClauseState.SUCCESS;
//	}

}
