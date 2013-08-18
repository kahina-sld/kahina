package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.optimization;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Random;

import org.jgap.Chromosome;
import org.jgap.Configuration;
import org.jgap.FitnessFunction;
import org.jgap.Gene;
import org.jgap.Genotype;
import org.jgap.IChromosome;
import org.jgap.data.DataTreeBuilder;
import org.jgap.data.IDataCreators;
import org.jgap.impl.DefaultConfiguration;
import org.jgap.impl.DoubleGene;
import org.jgap.impl.IntegerGene;
import org.jgap.impl.StockRandomGenerator;
import org.jgap.xml.XMLDocumentBuilder;
import org.jgap.xml.XMLManager;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.DefaultFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.ICDCLFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.LearnMoreFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Solution;
import org.w3c.dom.Document;

public class CDCLFitnessFunction /* extends FitnessFunction */{


//
//	static File[] instances;
//	static final ICDCLFactory factory = new LearnMoreFactory();
//	static long MAX_TIME = 15000;
//
//
//	
//
//	@Override
//	protected double evaluate(IChromosome arg0) {
//
//		IntegerGene countTill = (IntegerGene)arg0.getGene(0);
//		IntegerGene clauseLimit = (IntegerGene)arg0.getGene(1);
//
//		DoubleGene actReduction = (DoubleGene) arg0.getGene(2);
//		DoubleGene actGain = (DoubleGene) arg0.getGene(3);
//		DoubleGene initActGain = (DoubleGene) arg0.getGene(4);
//		DoubleGene clActGain = (DoubleGene) arg0.getGene(5);
//
//
//		System.out.println("countTill: " + countTill.intValue() + " actReduction: " + actReduction.doubleValue() + 
//				" unitActivityGain: " + actGain.doubleValue() + " clActGain: " + clActGain.doubleValue() + 
//				" initActGain: " + initActGain.doubleValue() + " clauseLimit: " + clauseLimit.intValue());
//
//
//		double timeUsed = 0;
//		int solved = 0;
//
//
//		//Starte fuer alle gefundenenn Dateien das Entsprechende Programm
//		for (int j = 0; j < instances.length; j++){
//			//					instance.startInstance.start(fileArray[i], args);
//			ClauseSet set = new ClauseSet( );
//
//
//			CDCL cdcl = factory.getCDCL(set);
//			cdcl.setValues(countTill.intValue(), clauseLimit.intValue(), actReduction.doubleValue(),
//					actGain.doubleValue(), initActGain.doubleValue(), clActGain.doubleValue());
//
//
//
//			try {
//				set.pars(instances[j].getAbsolutePath());
//			} catch (NumberFormatException | IOException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//
//			long start = System.currentTimeMillis();
//
//			if (cdcl.solve(MAX_TIME) != Solution.TIMEOUT){
//				solved++;
//				
//				long stop = System.currentTimeMillis();
//				timeUsed += (stop - start);
//			}
//
//		}
//
//		double ret = MAX_TIME*solved*instances.length - timeUsed;
//		System.out.println("Solved "+ solved + " instances. Fitnessvalue is " + ret);
//		return Math.max(ret, 0.0000000000001d);
//
//	}
//
//	static double test(){
//
//		double timeUsed = 0;
//
//		int solved = 0;
//
//		//Starte fuer alle gefundenenn Dateien das Entsprechende Programm
//		for (int i = 0; i < instances.length; i++){
//			//					instance.startInstance.start(fileArray[i], args);
//			ClauseSet set = new ClauseSet( );
//
//
//			CDCL cdcl = factory.getCDCL(set);
//
//
//			long start = System.currentTimeMillis();
//
//			try {
//				set.pars(instances[i].getAbsolutePath());
//			} catch (NumberFormatException | IOException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//
//
//			if (cdcl.solve(MAX_TIME) != Solution.TIMEOUT){
//				solved++;
//				long stop = System.currentTimeMillis();
//				timeUsed += (stop - start);
//			}
//
//		}
//
//		double ret = MAX_TIME*solved*instances.length - timeUsed;
//		
//		return Math.max(ret, 0.0000000000001d);
//	}



}
