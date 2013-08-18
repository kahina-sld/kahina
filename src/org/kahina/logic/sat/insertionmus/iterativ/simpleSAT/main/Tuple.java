package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main;
/**
 * Einfach nur ein Tupel
 * 
 * @author Seitz
 */
public class Tuple<S1, S2> {
	private final S2 s2;
	private final S1 s1;

	public Tuple(final S1 s1, final S2 s2){
		this.s1 = s1;
		this.s2 = s2;
	}
	
	public final S1 getS1(){
		return s1;
	}
	public final S2 getS2(){
		return s2;
	}
}
