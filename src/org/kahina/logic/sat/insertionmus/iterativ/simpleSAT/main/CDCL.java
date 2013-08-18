package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.Stack;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.AbstractActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.IActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.NonRandomHeapActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.RandomHeapActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.DefaultAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.IAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.LearnMoreAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation.IUnitPropagation;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation.RekursivUnitPropagation;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation.UnitPropagation;


/**
 * @LICENSE FREE BEER LICENSE VERSION 1.02
 * 
 *          The free beer license is a license to give free software to you and
 *          free beer (in)to the author(s).
 * 
 *          Your rights are :
 * 
 *          0. You can use this piece of software in anyway you like.
 * 
 *          1. You can redistribute this piece of software in source form or in
 *          compiled form.
 * 
 *          2. You can alter the source to your needs and redistribute the
 *          altered source in source form or in compiled form.
 * 
 *          However :
 * 
 *          0. This program is provided without warranty of any kind. So, if it
 *          breaks anything, for example itself, it is up to you.
 * 
 *          1. If you redistribute this piece of software, you are not allowed
 *          to charge money for this piece of software itself.
 * 
 *          2. If you redistribute this pieces of software in binary form, you
 *          must supply the source code as well.
 * 
 *          3. If you redistribute this software, modified or not, you must
 *          redistribute it under this license and you must include the name of
 *          the original author(s) and you must point out where the original
 *          source can be obtained.
 * 
 *          4. If you use this piece of software frequently, and you think it is
 *          worth a couple of euros, you are not allowed to send the author
 *          anything else than beer or means that provide facilities to get beer
 *          into the author(s) (i.e. openers, glasses).
 * 
 * @author Paul Seitz
 * 
 */
public class CDCL {



	// die zu bearbeitende instanz
	public ClauseSet instance;
	// der Stack der belegten variablen
	public Stack<Variable> stack;

	public int lvl = 0;


	// Gelernte Klausel, wird nach dem backtrack hinzugefuegt
	public ArrayList<Clause> addLater;
	public ArrayList<Clause> addUnit;
	protected Clause addLaterUnitClause;

	public final AbstractActivity activity;
	protected final IUnitPropagation propagation;
	public final IAnalyseConflict analyseConflict;

	protected boolean set = false;
	ArrayList<Integer> restards;
	int restardIT = 0;
	int restardCount = 0;
	double restardMultiplikator = 2;
	
	public CDCL(final ClauseSet set) {
		restards = new ArrayList<Integer>();
		restards.add(20);
		restards.add(40);

		this.instance = set;
		activity = new NonRandomHeapActivity();
		propagation = new UnitPropagation(this.instance);
//		propagation = new RekursivUnitPropagation(this.instance);
		analyseConflict = new DefaultAnalyseConflict();
		analyseConflict.init(this);
//		analyseConflict = new LearnMoreAnalyseConflict(this);
	}

	public CDCL(final ClauseSet set, final AbstractActivity activity, final IUnitPropagation unitPropagation,
			final IAnalyseConflict analyseConflict){
		restards = new ArrayList<Integer>();
		restards.add(20);
		restards.add(40);

		this.instance = set;
		
		this.activity = activity;
		this.propagation = unitPropagation;
		this.analyseConflict = analyseConflict;
		this.analyseConflict.init(this);
	}
	
	/**
	 * Resolviert zwei Klauseln
	 * 
	 * @param c1
	 * @param c2
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public Clause resolve(final Clause c1, final Clause c2) {

		// Alle literale der entstehenden Klausel
		final ArrayList<Integer> literals = new ArrayList<Integer>(
				c1.literals.size() + c2.literals.size() - 2);
		// Kopie aller literale aus c2
		final ArrayList<Integer> literalsC2 = (ArrayList<Integer>) c2.literals
				.clone();

		final int c1Sice = c1.literals.size();
		for (int i = 0; i < c1Sice; i++) {
			final int c2Sice = literalsC2.size();
			final int litc1 = c1.literals.get(i);

			boolean add = true; // litc1 ist nicht das wegfallende literal

			for (int j = 0; j < c2Sice; j++) {
				final int litc2 = literalsC2.get(j);
				if (litc1 == litc2) {// litc1 kommt in c1 und c2 vor
					literalsC2.remove(j);
					break;
				} else if (litc1 == -1 * litc2) {// litc1 kommt negiert in c2
													// vor
					add = false;
					literalsC2.remove(j);
					break;
				}
			}
			if (add)
				literals.add(litc1);
		}

		// test for subsumtions
		if (literalsC2.size() == 0) {
			// res subsumiert c1
			final Clause c = new Clause(literals);
			this.addLater.add(c);
			this.removeClause(c1);
			if (c2.literals.size() - 1 == literals.size()) {
				// res subsumiert c2
				this.removeClause(c2);
			}
			return c;
		}
		literals.addAll(literalsC2);
		final Clause c = new Clause(literals);
		if (c2.literals.size() - 1 == literals.size()) {
			// res subsumiert c2
			this.addLater.add(c);
			this.removeClause(c2);
			return c;
		}
		return c;
	}

	public void removeClause(final Clause c) {
		for (int l : c.literals) {
			final Variable v = this.instance.variables[Math.abs(l)];
			v.watched.remove(c);
			if (l > 0) {
				v.setHeuristic--;
//				v.positiv.remove(c);
			} else {
				v.setHeuristic++;
//				v.negativ.remove(c);
			}
		}
	}

	/**
	 * Resolviert zwei Klauseln wobei c1.literals[0] negiert in c2.literals
	 * vorkommen muss.
	 * 
	 * @param c1
	 * @param c2
	 * @return
	 */
	public Clause resolveC1Begin(final Clause c1, final Clause c2) {

		// Alle literale der entstehenden Klausel
		ArrayList<Integer> lit = new ArrayList<Integer>(c1.literals.size()
				+ c2.literals.size() - 2);

		// alle Literale die auser dem ersten kommen in der entstehenden Klausel
		// vor
		int i = 1;
		final int till = c1.literals.size();
		for (; i < till; i++) {
			lit.add(c1.literals.get(i));
		}

		final int res = -1 * c1.literals.get(0); // kommt nicht in der
													// entstehenden Klausel vor
		// alle Literale auser res die noch nicht in lit vorhanden sind kommen
		// in der entstehenden Klausel vor
		for (i = 0; i < c2.literals.size(); i++) {
			final int l = c2.literals.get(i);
			if (l != res && !c1.literals.contains(l)) {
				lit.add(l);
			}
		}

		final Clause c = new Clause(lit);
		// test for subsumtions
		final int litsize = lit.size() + 1;
		if (c2.literals.size() == litsize) {
			// c2 wird subsumiert
			this.addLater.add(c);
			this.removeClause(c2);

			if (c1.literals.size() == litsize) {
				this.addLater.remove(c1);
			}
			return c;
		}
		if (c1.literals.size() == litsize && this.addLater.contains(c1)) {
			// c1 wird subsumiert
			// c ist aber nicht element von C, jedoch subsumiert c eine klausel
			// aus einem vorherigen
			// schritt die wiederum eine klausel aus C subsumiert
			this.addLater.remove(c1);

			this.addLater.add(c);

			// return c;
		}

		return c;
	}




	/**
	 * Fuer jedes literal in der neuen Clausel wird untersucht ob eine
	 * Resolution mit dem grund fuer die Variable die Clausel verkleinern
	 * wuerde.
	 * 
	 * @param newClause
	 */
	public void resolveIfShortsClause(final Clause newClause) {

		int size = newClause.literals.size();
		for (int i = 0; i < size; i++) {
			final int litNewClause = newClause.literals.get(i);
			final Clause reason = this.instance.variables[Math
					.abs(litNewClause)].reason;

			final int reasonSize;
			if (reason != null && (reasonSize = reason.literals.size()) <= size) {
				boolean delete = true;
				boolean compl = false;
				for (final int l : reason.literals) {
					if (!newClause.literals.contains(l)) {
						if (l != -litNewClause) {
							delete = false;
							break;
						} else {
							if (!compl) {
								compl = true;
							} else {
								delete = false;
								break;
							}
						}
					}
				}

				if (delete) {

					newClause.literals.remove(i);
					// if (newClause.toString().equals("{ -29 95 -48 -18 }")){
					// System.out.println("for the breakpoint");
					// }
					size--;
					// newClause subsumiert reason
					if (size < reasonSize){
						this.removeClause(reason);

						//TODO set min backjump value
					}
				}
			}
		}
	}

	/**
	 * Zweithoechste lvl der variablen in newClause
	 * 
	 * @param newClause
	 * @return
	 */
	public int computeBacktrackLevel(final Clause newClause) {
		int ret = 0;
		for (int lit : newClause.literals) {
			Variable v = this.instance.variables[Math.abs(lit)];

			if (v.level > ret && v.level != this.lvl)
				ret = v.level;
		}
		return ret;
	}

	private void addUnitClause(final Clause newClause) {
	
		
		// if (newClause.toString().equals("{ -29 95 6 }")){
		// System.out.println("blabla");
		// System.out.println(stack);
		// }

		newClause.setWatched(this.instance.variables);
		
		this.instance.initUnits.add(newClause);

		newClause.setBoolHeur(this.instance.variables);

		// Berechne die neuen Aktivitaetswerte der Variablen
		this.activity.NewUnitClause(newClause);
	}



	int delcount = 0;

	public Solution solve(final long timeout) {

		final long timestart = System.currentTimeMillis();
		
		// Initialisierungen
		final int variableSize = this.instance.variables.length - 1;
		this.lvl = 0;
		stack = new Stack<Variable>();
		this.addLater = new ArrayList<Clause>();
		this.addUnit = new ArrayList<Clause>();
		
		this.activity.init(this.instance);

		// loesche alle variablen die eine unit Clause haben
		// if (!deletNoLongerNeededClauses()){
		// return false;
		// }

		while (true) {


			final Clause cl;
			if (null != (cl = this.propagation.unitPropagation(this.stack, this.lvl))) {

				// nach dem conflict wird kommt es zu einem backjump -->
				// bisherige unit klauseln sind keine unit klauseln mehr
				this.instance.initUnits.clear();

				// bestimme naechstes lvl und fuehre alle noetigen resolutionen
				// durch
				this.lvl = this.analyseConflict.analyseConflict(cl);
//				this.lvl = this.analyseConflict(cl);

				// UNSAT
				if (lvl == -1)
					return Solution.UNSAT;

				// final boolean restart = this.testRestart();

				this.backtrackToLvl();

				// fuege gelernte Klausel hinzu
				this.addNewClauses();

				// if (!deletNoLongerNeededClauses()){
				// return false;
				// }
				// if (restart) this.instance.initUnits.clear();

				doRestardIfNeeded();
			} else {
				if (this.stack.size() == variableSize) {
					// alle Variablen sind belegt und es gab keinen konflikt --> SAT
					
//					for (Variable v: this.instance.variables){
//						if (v.state == State.OPEN) System.err.println("OPEN: " + v);
//					}
					return Solution.SAT;
				}
				
				if (System.currentTimeMillis() - timestart > timeout){
					return Solution.TIMEOUT;
				}


				this.lvl++;
				final Variable v = this.activity.getNextVar(); // waehle
																// naechste
																// variable
				v.assign(v.getBoolHeur(), this.instance.variables, // belege
																	// naechste
																	// variable
						this.instance.initUnits, stack, lvl);
			}

			// for (Variable v: this.instance.variables){
			// if (v.num == 13){
			// for (Clause c: v.watched){
			// if (c.toString().equals("{ 13 }")){
			// System.out.println(c);
			// }
			// }
			// }
			// }

			// ///////////// Tests die ich zum debugen benutzt habe /////////
			//
//			for (Variable v : instance.variables) {
//				for (Clause c: v.watched)
//					c.testWatchedLiterals(instance.variables);
//			}
//			for (Variable v : instance.variables) {
//				if (v.state == Variable.State.OPEN && stack.contains(v)) {
//					System.err.println("should not be open: " + v);
//					System.err.println("Variablestate is OPEN but variable isn't");
//				} else if ((v.state == Variable.State.FALSE || v.state ==
//						Variable.State.TRUE)
//						&& !stack.contains(v)) {
//					System.err.println("should be open: " + v);
//				}
//			}
			// ////////////////////// ende ///////////////////////////////
		}
	}

	private void doRestardIfNeeded() {
		this.restardCount++;
		if (this.restardCount > this.restards.get(this.restardIT)) {
//			System.out.println("Restard from lvl: " + this.lvl + " after "
//					+ restardCount);
			this.restardCount = 0;

			this.restardIT++;
			if (restardIT == this.restards.size()) {
				restards.add((int) (this.restardMultiplikator * restards.get(restardIT - 1)));
				restardIT = 0;
			}
			this.instance.initUnits.clear();
			this.lvl = 0;
			this.backtrackToLvl();

//			this.lvl++;
//			final Variable v = this.activity.getNextVar(); // waehle naechste
//															// variable
//			v.assign(v.getBoolHeur(), this.instance.variables, // belege naechste variable
//					this.instance.initUnits, stack, lvl);
		}
	}

//	private boolean deletNoLongerNeededClauses() {
//		if (lvl == 0) {
//			// loesche alle variablen die eine unit Clause haben
//			while (this.instance.initUnits.size() > 0) {
//				final ArrayList<Clause> newUnits = new ArrayList<Clause>();
//				for (final Clause c : this.instance.initUnits) {
//					// if (c.literals.size() != 1){
//					// System.err.println("Clause is not unit: " + c);
//					// }
//					// delcount++;
//					final int l = c.literals.get(0);
//					if (!this.instance.variables[Math.abs(l)].delet(l > 0,
//							this.instance.variables, newUnits)) {
//						return false;
//					}
//				}
//				this.instance.initUnits = newUnits;
//			}
//		}
//		return true;
//	}

	/**
	 * Fuegt alle neu gelernten Klauseln der Klauselmenge hinzu.
	 * v.B.: Die letzte klausel in addLater ist eine UnitKlausel
	 */
	private void addNewClauses() {

//		for(Clause c: this.addLater){
//			for(int literal: c.literals){
//				Variable v = this.instance.variables[Math.abs(literal)];
//				if (v.watched.contains(c)){
//					System.err.println(c + " has a watched lit!");
//				}
//			}
//		}
		
		final int till = this.addLater.size();
		int i = 0;
		for (; i < till; i++) {
			// Alle diese klauseln sind nicht unit --> die watched literals
			// koennen auf unbelegte variablen gelegt werden
			final Clause c = this.addLater.get(i);
			c.setWatchedToUnsigned(this.instance.variables);
			c.setBoolHeur(this.instance.variables);
			
			this.activity.NewUnitClause(c);
		}
		// dies ist die gelernte unit klausel
		final int till2 = this.addUnit.size();
		i = 0;
		for (; i < till2; i++) {
			this.addUnitClause(this.addUnit.get(i));
		}
//		this.addUnitClause(this.addLater.get(till));

		
//		for (Clause c: this.addLater){
//			c.testWatchedLiterals(this.instance.variables);
//		}
		this.addLater.clear();
		this.addUnit.clear();
	}

	/**
	 * Loescht alle belegungen die nach this.lvl entstanden sind
	 */
	private void backtrackToLvl() {
		if (this.lvl == 0) {
			while (!this.stack.empty() && this.stack.peek().level > 0) {
				final Variable v = this.stack.pop();
				v.unassign(this.activity);
			}
		} else {
			while (this.stack.peek().level > this.lvl) {
				final Variable v = this.stack.pop();
				v.unassign(this.activity);
			}
		}
	}

	public Clause resolve2(Clause c1, Clause newClause, int v) {
		ArrayList<Integer> literals = new ArrayList<Integer>();
//		int lit = 0;
		for (final int l: c1.literals){
			if (l != v && l != -v){
				literals.add(l);
			}
//			else{
//				lit = l;
//			}
		}
		for (final int l: newClause.literals){
			if (l != v && l != -v){
				if (!literals.contains(l)){
					literals.add(l);
				}
			}
//			else if(lit == l || lit ==0){
//				System.err.println(v);
//				System.err.println(c1);
//				System.err.println(newClause);
//				System.err.println();
//			}else{
//				System.err.println("hurray");
//			}
		}
		
		return new Clause(literals);
	}
	

	public void setValues(int countTill, int clauseLimit, double actReduction, double actGain,
			double initActGain, double clActGain, int restardValue1, int restardValue2, int restardValue3, double pRestardMultiplikator){
		

		restards = new ArrayList<Integer>();
		restards.add(restardValue1);
		restards.add(restardValue2);
		restards.add(restardValue3);

		restardMultiplikator = pRestardMultiplikator;
		


		this.activity.countTillActivityReduction = countTill;
		this.activity.activityReduction = actReduction;
		this.activity.unitActivityGain = actGain;
		this.activity.clauseActivityGain = clActGain;

		this.instance.initVarAddition = initActGain;
		
		



		if (this.analyseConflict instanceof LearnMoreAnalyseConflict){
			LearnMoreAnalyseConflict lm = (LearnMoreAnalyseConflict) this.analyseConflict;
			lm.LearnClauseLimit = clauseLimit;
		}
	}
}
