package org.kahina.logic.sat.io.minisat;

public class ResultNotRetrievableException extends Exception
{
	String explanation;

	public ResultNotRetrievableException(String explanation) 
	{
		this.explanation = explanation;
	}

	public String toString()
	{
		return "No result from MiniSAT: " + explanation;
	}
}
