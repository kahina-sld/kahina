package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.optimization;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.util.Random;


import org.jgap.Chromosome;
import org.jgap.Configuration;
import org.jgap.Gene;
import org.jgap.Genotype;
import org.jgap.IChromosome;
import org.jgap.NaturalSelector;
import org.jgap.data.DataTreeBuilder;
import org.jgap.data.IDataCreators;
import org.jgap.gp.impl.TournamentSelector;
import org.jgap.impl.DefaultConfiguration;
import org.jgap.impl.DefaultMutationRateCalculator;
import org.jgap.impl.DoubleGene;
import org.jgap.impl.GaussianMutationOperator;
import org.jgap.impl.IntegerGene;
import org.jgap.impl.MutationOperator;
import org.jgap.xml.XMLDocumentBuilder;
import org.jgap.xml.XMLManager;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.activityHeuristics.NonRandomHeapActivity;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.analyseConflict.DisabledLearnMoreAnalyseConflict;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.ICDCLFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.unitPropagation.UnitPropagation;
import org.w3c.dom.Document;



public class CDCLOptimisationMultiThreaded {
//	private static final String SAFE_FILE_NAME = "CDCLmulti.xml";
//	private static final String SAFE_FILE_NAME = "ALL_learnMoreTimeLimit2000.xml";
	private static final String SAFE_FILE_NAME = "ShortTimeDisabled.xml";
	private static final int MAX_ALLOWED_EVOLUTIONS = 50;
	private static final int POPULATION_SICE =  150;
	//	static final ICDCLFactory factory = new DefaultFactory();
	//	private static final String directory = "../cnf/Optimization";
//	private static final String directory = "../cnf/";
	private static final String directory = "../cnf/test";
//	private static final String directory = "../cnf/SWGCP-allSat-scrambled-first500";
	private static IChooseInstance chooseInstance = new ChooseInstance(directory);
//	private static IChooseInstance chooseInstance = new ChooseRandomInstance();

	// In milliseconds
	static int i = 0;
	
//	private static CDCLFitnessFunction fitnesFunction = new CDCLFitnessFunction();

	private static CDCLFitnessFunctionMultiThreaded fitnesFunction = new CDCLFitnessFunctionMultiThreaded();

	public static void main(String[] args) throws Exception {
		
		CDCLFitnessFunctionMultiThreaded.factory =  new ICDCLFactory(){
			@Override
			public CDCL getCDCL(ClauseSet set) {
				return new CDCL(set, new NonRandomHeapActivity(), new UnitPropagation(set), new DisabledLearnMoreAnalyseConflict());
			}
		};
		
		Configuration conf = new DefaultConfiguration();
		conf.setPreservFittestIndividual(true);
		conf.setPopulationSize(POPULATION_SICE);
//		conf.setRandomGenerator(new Ga)
//		conf.setAlwaysCaculateFitness(true);
//		conf.setN
//		conf.removeNaturalSelectors(true);
//		conf.addNaturalSelector(null, true);
//		NaturalSelector ns = new TournamentSelector(2);
//		conf.addNaturalSelector(new TournamentSelector(2), true);
//		conf.setNaturalSelector(new TournamentSelector(2));
//		conf.addNaturalSelector(new TournamentSelector(2), true);

		GaussianMutationOperator gmo = new GaussianMutationOperator(conf,0.001);
		
		conf.addGeneticOperator(gmo);
		
//		MutationOperator mo = new MutationOperator(conf);
//		mo.setMutationRate(6);
//		conf.addGeneticOperator(mo);

		conf.setFitnessFunction(fitnesFunction);


		Gene[] sampleGenes = new Gene[12];
		//	    
		//	    IntegerGene countTill = new IntegerGene(conf, 0, 100000);
		//		IntegerGene clauseLimit = new IntegerGene(conf, 0, 300);
		//
		//		DoubleGene actReduction = new DoubleGene(conf, 0,0.99999999d);
		//		DoubleGene actGain = new DoubleGene(conf, 0,200);
		//		DoubleGene initActGain = new DoubleGene(conf, 0,200);
		//		DoubleGene clActGain = new DoubleGene(conf, 0,200);
		IntegerGene countTill = new IntegerGene(conf, 0, 10000);
		IntegerGene clauseLimit = new IntegerGene(conf, 0, 50);

		DoubleGene actReduction = new DoubleGene(conf, 0.0001d,0.89999999d);
		DoubleGene actGain = new DoubleGene(conf, 0,100);
		DoubleGene initActGain = new DoubleGene(conf, 0,100);
		DoubleGene clActGain = new DoubleGene(conf, 0,100);
		
		

		IntegerGene restard1 = new IntegerGene(conf, 1, 10000);
		IntegerGene restard2 = new IntegerGene(conf, 1, 10000);
		IntegerGene restard3 = new IntegerGene(conf, 1, 10000);
		DoubleGene restardMult = new DoubleGene(conf, 1,100);
		
		DoubleGene actAllDuringRes = new DoubleGene(conf, 1,100);
		DoubleGene actConfVar = new DoubleGene(conf, 1,100);

		sampleGenes[0] = countTill;
		sampleGenes[1] = clauseLimit;
		sampleGenes[2] = actReduction;
		sampleGenes[3] = actGain;
		sampleGenes[4] = initActGain;
		sampleGenes[5] = clActGain;
		sampleGenes[6] = restard1;
		sampleGenes[7] = restard2;
		sampleGenes[8] = restard3;
		sampleGenes[9] = restardMult;
		sampleGenes[10] = actAllDuringRes;
		sampleGenes[11] = actConfVar;

		//		conf.setKeepPopulationSizeConstant(false);
		//		conf.setRandomGenerator(new StockRandomGenerator());



		IChromosome sampleChromosome = new Chromosome(conf, sampleGenes);
		conf.setSampleChromosome(sampleChromosome);


		Genotype population;
		try {
			Document doc = XMLManager.readFile(new File(SAFE_FILE_NAME));
			population = XMLManager.getGenotypeFromDocument(conf, doc);
			System.out.println("found old Population");
		}
		catch (FileNotFoundException fex) {
			System.out.println("creating random generation");
			population = Genotype.randomInitialGenotype(conf);
//			Genotype.
		}
		//	    population = Genotype.randomInitialGenotype(conf);

		for (i = 0; i < MAX_ALLOWED_EVOLUTIONS; i++) {
			System.out.println("Generation " + i);
			chooseInstance.chooseInstances(directory, i);
			for (File f: CDCLFitnessFunctionMultiThreaded.instances){
				System.out.print(f.getName() + " ");
			}
			System.out.println();
			population.evolve();

			DataTreeBuilder builder = DataTreeBuilder.getInstance();
			IDataCreators doc2 = builder.representGenotypeAsDocument(population);
			// create XML document from generated tree
			// ---------------------------------------
			XMLDocumentBuilder docbuilder = new XMLDocumentBuilder();
			Document xmlDoc = (Document) docbuilder.buildDocument(doc2);
			XMLManager.writeFile(xmlDoc, new File(SAFE_FILE_NAME));
			
			IChromosome bestSolutionSoFar = population.getFittestChromosome();
			System.out.println("The best solution has a fitness value of " +
					bestSolutionSoFar.getFitnessValue());
			System.out.println("Max fitnessvalue would have been: " + CDCLFitnessFunctionMultiThreaded.MAX_TIME* 
					CDCLFitnessFunctionMultiThreaded.instances.length* CDCLFitnessFunctionMultiThreaded.instances.length);

			Gene[] genes = bestSolutionSoFar.getGenes();

			System.out.println("countTill: " + genes[0]);
			System.out.println("clauseLimit: " + genes[1]);
			System.out.println("actReduction: " + genes[2]);
			System.out.println("actGain: " + genes[3]);
			System.out.println("initActGain: " + genes[4]);
			System.out.println("clActGain: " + genes[5]);
			System.out.println("Restard1: " + genes[6]);
			System.out.println("Restard2: " + genes[7]);
			System.out.println("Restard3: " + genes[8]);
			System.out.println("RestardMult" + genes[9]);
			System.out.println("actDuringRes: " + genes[10]);
			System.out.println("actConfVar: " + genes[11]);

			System.out.println();
			System.out.println();
		}

		// Display the best solution we found.
		// -----------------------------------
//		IChromosome bestSolutionSoFar = population.getFittestChromosome();
//
//		System.out.println("The best solution has a fitness value of " +
//				bestSolutionSoFar.getFitnessValue());
//
//		Gene[] genes = bestSolutionSoFar.getGenes();
//
//		System.out.println("countTill: " + genes[0]);
//		System.out.println("clauseLimit: " + genes[1]);
//		System.out.println("actReduction: " + genes[2]);
//		System.out.println("actGain: " + genes[3]);
//		System.out.println("initActGain: " + genes[4]);
//		System.out.println("clActGain: " + genes[5]);

		System.out.println();

		System.out.println("Default fittnes: " + CDCLFitnessFunctionMultiThreaded.test());


	}
	



}
