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
import org.jgap.data.DataTreeBuilder;
import org.jgap.data.IDataCreators;
import org.jgap.impl.DefaultConfiguration;
import org.jgap.impl.DoubleGene;
import org.jgap.impl.IntegerGene;
import org.jgap.xml.XMLDocumentBuilder;
import org.jgap.xml.XMLManager;
import org.w3c.dom.Document;


public class CDCLOptimisationSingleThreaded {
//	private static final int MAX_ALLOWED_EVOLUTIONS = 15;
//	private static final int ANZ_INSTANCES = 1;
//	private static final int POPULATION_SICE =  30;
//	private static Random r = new Random();
//	//	static final ICDCLFactory factory = new DefaultFactory();
//	//	private static final String directory = "../cnf/Optimization";
//	//		private static final String directory = "../cnf/";
//	private static final String directory = "../cnf/SWGCP-allSat-scrambled-first1000";
//	private static final int INSTANCES_FACTOR = 1;
//
//
//	// In milliseconds
//	static int i = 0;
//	
//	private static CDCLFitnessFunction fitnesFunction = new CDCLFitnessFunction();
//
//
//	public static void main(String[] args) throws Exception {
//		Configuration conf = new DefaultConfiguration();
//		conf.setPreservFittestIndividual(true);
//		conf.setPopulationSize(POPULATION_SICE);
////		conf.setAlwaysCaculateFitness(true);
//		//		conf.setAlwaysCaculateFitness(true);
//
//
//		conf.setFitnessFunction(fitnesFunction);
//
//
//		Gene[] sampleGenes = new Gene[6];
//		//	    
//		//	    IntegerGene countTill = new IntegerGene(conf, 0, 100000);
//		//		IntegerGene clauseLimit = new IntegerGene(conf, 0, 300);
//		//
//		//		DoubleGene actReduction = new DoubleGene(conf, 0,0.99999999d);
//		//		DoubleGene actGain = new DoubleGene(conf, 0,200);
//		//		DoubleGene initActGain = new DoubleGene(conf, 0,200);
//		//		DoubleGene clActGain = new DoubleGene(conf, 0,200);
//		IntegerGene countTill = new IntegerGene(conf, 0, 10000);
//		IntegerGene clauseLimit = new IntegerGene(conf, 0, 50);
//
//		DoubleGene actReduction = new DoubleGene(conf, 0.3d,0.89999999d);
//		DoubleGene actGain = new DoubleGene(conf, 0,100);
//		DoubleGene initActGain = new DoubleGene(conf, 0,100);
//		DoubleGene clActGain = new DoubleGene(conf, 0,100);
//
//		sampleGenes[0] = countTill;
//		sampleGenes[1] = clauseLimit;
//		sampleGenes[2] = actReduction;
//		sampleGenes[3] = actGain;
//		sampleGenes[4] = initActGain;
//		sampleGenes[5] = clActGain;
//
//		//		conf.setKeepPopulationSizeConstant(false);
//		//		conf.setRandomGenerator(new StockRandomGenerator());
//
//
//
//		IChromosome sampleChromosome = new Chromosome(conf, sampleGenes);
//		conf.setSampleChromosome(sampleChromosome);
//
//
//		Genotype population;
//		try {
//			Document doc = XMLManager.readFile(new File("CDCL.xml"));
//			population = XMLManager.getGenotypeFromDocument(conf, doc);
//			System.out.println("found old Population");
//		}
//		catch (FileNotFoundException fex) {
//			System.out.println("creating random generation");
//			population = Genotype.randomInitialGenotype(conf);
//		}
//		//	    population = Genotype.randomInitialGenotype(conf);
//
//		for (i = 0; i < MAX_ALLOWED_EVOLUTIONS; i++) {
//			System.out.println("Generation " + i);
//			chooseInstances();
//			for (File f: CDCLFitnessFunction.instances){
//				System.out.print(f.getName() + " ");
//			}
//			System.out.println();
//			population.evolve();
//
//			DataTreeBuilder builder = DataTreeBuilder.getInstance();
//			IDataCreators doc2 = builder.representGenotypeAsDocument(population);
//			// create XML document from generated tree
//			// ---------------------------------------
//			XMLDocumentBuilder docbuilder = new XMLDocumentBuilder();
//			Document xmlDoc = (Document) docbuilder.buildDocument(doc2);
//			XMLManager.writeFile(xmlDoc, new File("CDCL.xml"));
//			
//			IChromosome bestSolutionSoFar = population.getFittestChromosome();
//			System.out.println("The best solution has a fitness value of " +
//					bestSolutionSoFar.getFitnessValue());
//			System.out.println("Max fitnessvalue would have been: " + CDCLFitnessFunction.MAX_TIME* 
//					CDCLFitnessFunction.instances.length* CDCLFitnessFunction.instances.length);
//
//			Gene[] genes = bestSolutionSoFar.getGenes();
//
//			System.out.println("countTill: " + genes[0]);
//			System.out.println("clauseLimit: " + genes[1]);
//			System.out.println("actReduction: " + genes[2]);
//			System.out.println("actGain: " + genes[3]);
//			System.out.println("initActGain: " + genes[4]);
//			System.out.println("clActGain: " + genes[5]);
//			System.out.println();
//			System.out.println();
//		}
//
//		// Display the best solution we found.
//		// -----------------------------------
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
//
//		System.out.println();
//
//		System.out.println("Default fittnes: " + CDCLFitnessFunction.test());
//
//
//	}
//	
//
//
//
//	private static void chooseInstances() {
//
//		File dir = new File(directory);
//
//		if (dir.isDirectory()){
//
//			//Finde Alle Dateien im Verzeichnis.
//			File[] fileArray = dir.listFiles(new FileFilter(){
//				@Override
//				public boolean accept(File arg0) {
//					return arg0.isFile() && arg0.getName().toLowerCase().endsWith(".cnf");
//				}});
//			int anz = ANZ_INSTANCES + INSTANCES_FACTOR*i;
//			 CDCLFitnessFunction.instances = new File[anz];
//
//			for (int j = 0; j < anz; j++){
//				 CDCLFitnessFunction.instances[j] = fileArray[r.nextInt(fileArray.length)];
//			}
//
//		}else{
//			System.err.println(dir.getName() + " is not a directory.");
//		}
//	}
}
