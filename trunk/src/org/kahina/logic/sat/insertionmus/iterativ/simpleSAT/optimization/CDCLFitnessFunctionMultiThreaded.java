package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.optimization;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FilePermission;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

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
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.LearnMoreAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.DefaultFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.ICDCLFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.LearnMoreFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.Solution;
import org.w3c.dom.Document;


public class CDCLFitnessFunctionMultiThreaded extends FitnessFunction {



	static File[] instances;
	static ICDCLFactory factory = new LearnMoreFactory();
	static long MAX_TIME = 50000;


	static final int NUMBER_THREADS = 3;

	double timeUsed;
	int solved;

	@Override
	protected double evaluate(IChromosome arg0) {

		final IntegerGene countTill = (IntegerGene)arg0.getGene(0);
		final IntegerGene clauseLimit = (IntegerGene)arg0.getGene(1);



		final DoubleGene actReduction = (DoubleGene) arg0.getGene(2);
		final DoubleGene actGain = (DoubleGene) arg0.getGene(3);
		final DoubleGene initActGain = (DoubleGene) arg0.getGene(4);
		final DoubleGene clActGain = (DoubleGene) arg0.getGene(5);


		final IntegerGene restardValue1 = (IntegerGene)arg0.getGene(6);
		final IntegerGene restardValue2 = (IntegerGene)arg0.getGene(7);
		final IntegerGene restardValue3 = (IntegerGene)arg0.getGene(8);
		final DoubleGene restardMult = (DoubleGene)arg0.getGene(9);

		final DoubleGene actDuringRes = (DoubleGene)arg0.getGene(10);
		final DoubleGene actConfVar = (DoubleGene)arg0.getGene(11);


		System.out.println("countTill: " + countTill.intValue() + " actReduction: " + actReduction.doubleValue() + 
				" unitActivityGain: " + actGain.doubleValue() + " clActGain: " + clActGain.doubleValue() + 
				" actDuringRes: " + actDuringRes.doubleValue() + " actConfVar: " + actConfVar.doubleValue() +   
				" initActGain: " + initActGain.doubleValue() + " clauseLimit: " + clauseLimit.intValue() +
				" restardValues: "+ restardValue1 + " " + restardValue2 + " " + restardValue3 + " : " + restardMult);


		timeUsed = 0d;
		solved = 0;


		ArrayList<Runnable> jobs = new ArrayList<Runnable>();


		LearnMoreAnalyseConflict.ACTIVITYGAIN = actDuringRes.doubleValue();
		LearnMoreAnalyseConflict.ACTIVITYCONFVAR = actConfVar.doubleValue();

		//Starte fuer alle gefundenenn Dateien das Entsprechende Programm
		for (int i = 0; i < instances.length; i++){
			//					instance.startInstance.start(fileArray[i], args);
			final int j = i;
			final CDCLFitnessFunctionMultiThreaded fit = this;

			Runnable runnable = new Runnable(){

				@Override
				public void run() {

					ClauseSet set = new ClauseSet( );


					CDCL cdcl = factory.getCDCL(set);
					cdcl.setValues(countTill.intValue(), clauseLimit.intValue(), actReduction.doubleValue(),
							actGain.doubleValue(), initActGain.doubleValue(), clActGain.doubleValue()
							, restardValue1.intValue(), restardValue2.intValue(), restardValue3.intValue(), restardMult.doubleValue());




					try {
						set.pars(instances[j].getAbsolutePath());
					} catch (NumberFormatException | IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					long start = System.currentTimeMillis();


					if (cdcl.solve(MAX_TIME) != Solution.TIMEOUT){

						long stop = System.currentTimeMillis();
						synchronized(fit){
							solved++;
							timeUsed += (stop - start);
						}
					}
				}

			};
			jobs.add(runnable);
		}


		ExecutorService executor = Executors.newFixedThreadPool(NUMBER_THREADS);
		for(Runnable r: jobs){
			executor.execute(r);
		}
		executor.shutdown();
		try {
			if (!executor.awaitTermination(MAX_TIME*instances.length, TimeUnit.MILLISECONDS)){
				System.err.println("Thred timed out!");
			}
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		//		double ret = MAX_TIME*solved*instances.length - timeUsed;
		double ret = 10*MAX_TIME*solved - (timeUsed/solved);
		System.out.println("Solved "+ solved + " instances. Fitnessvalue is " + ret);
		return Math.max(ret, 0.0001d);

	}

	static int solved2;
	static int timeUsed2;
	static double test() throws IOException{

		timeUsed2 = 0;

		solved2 = 0;

		//Starte fuer alle gefundenenn Dateien das Entsprechende Programm

		ArrayList<Runnable> jobs = new ArrayList<Runnable>();

		final Object lock = new Object();

		File output = new File("Output");
		if(!output.exists()){
			output.createNewFile();
		}

		FileWriter fw = new FileWriter(output, true);
		final BufferedWriter bw = new BufferedWriter(fw);
		bw.write("new test: \n");

		for (int i = 0; i < instances.length; i++){
			final int j = i;

			Runnable runnable = new Runnable(){

				@Override
				public void run() {
					//					instance.startInstance.start(fileArray[i], args);
					ClauseSet set = new ClauseSet( );


					CDCL cdcl = factory.getCDCL(set);

					try {
						set.pars(instances[j].getAbsolutePath());
					} catch (NumberFormatException | IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					long start = System.currentTimeMillis();


					if (cdcl.solve(MAX_TIME) != Solution.TIMEOUT){
						synchronized(lock){
							solved2++;
							long stop = System.currentTimeMillis();
							timeUsed2 += (stop - start);
							try {
								bw.write(instances[j] + " " +(stop - start) + "\n");
							} catch (IOException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
						}
					}else{
						synchronized(lock){

							try {
								bw.write(instances[j] + " timeout\n");
							} catch (IOException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
						}
					}

				}
			};
			jobs.add(runnable);
		}

		ExecutorService executor = Executors.newFixedThreadPool(NUMBER_THREADS);
		for(Runnable r: jobs){
			executor.execute(r);
		}
		executor.shutdown();
		try {
			if (!executor.awaitTermination(MAX_TIME*instances.length, TimeUnit.MILLISECONDS)){
				System.err.println("Thred timed out!");
			}
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		bw.flush();

		double ret = 10*MAX_TIME*solved2 - (timeUsed2/solved2);

		System.out.println("Solved " + solved2);
		return Math.max(ret, 0.0001d);
	}



}
