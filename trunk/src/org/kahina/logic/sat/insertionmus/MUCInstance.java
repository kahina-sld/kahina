package org.kahina.logic.sat.insertionmus;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedList;
import java.util.concurrent.TimeoutException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFileChooser;

import org.kahina.core.KahinaInstance;
import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaProjectEvent;
import org.kahina.core.control.KahinaProjectEventType;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.io.util.XMLUtil;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.insertionmus.algorithms.AbstractAlgorithm;
import org.kahina.logic.sat.insertionmus.algorithms.AlgorithmData;
import org.kahina.logic.sat.insertionmus.algorithms.BasicAlgorithm;
//import org.kahina.logic.sat.muc.control.MUCControlEventCommands;
//import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.insertionmus.gui.MUCGUI;
import org.kahina.logic.sat.muc.control.MUCControlEventCommands;
//import org.kahina.logic.sat.muc.io.MUCExtension;
import org.kahina.logic.sat.muc.test.CfgDontCareFilter;

import org.w3c.dom.Document;

public class MUCInstance extends KahinaInstance<MUCState, MUCGUI, KahinaBridge, KahinaProject>
{
	public static final boolean CFG_FILTER = false;


//	MUCState state;

	CnfSatInstance satInstance;
	//    MUCStatistics stat;
	MiniSATFiles files;

	private BasicAlgorithm ba;


	public void startNewSessionWithoutBridge()
	{
		try
		{
			super.startNewSessionWithoutBridge();
			registerSessionListener(KahinaEventTypes.CONTROL, this);
			registerSessionListener(KahinaEventTypes.SYSTEM, this);
		}
		catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
	}

	//    public MUCBridge startNewSession()
	//    {
		//        try
		//        {
			//            super.startNewSession();
	//            registerSessionListener(KahinaEventTypes.CONTROL, this);
	//            registerSessionListener(KahinaEventTypes.SYSTEM, this);
	//            return bridge;
	//        }
	//        catch (Exception e)
	//        {
	//            e.printStackTrace();
	//            System.exit(-1);
	//        }
	//        return null;
	//    }

	@Override
	protected MUCState createState()
	{
		System.out.println("create State");
		if (satInstance == null)
		{
			//System.err.println("MUCState.createState(" + this + ",null)");
			return new MUCState(this);
		}
		//System.err.println("MUCState.createState(" + this + "," + satInstance + ")");
		//        return new MUCState(this,satInstance, stat, files);
		return new MUCState(this, this.satInstance);
	}

	@Override
	protected KahinaBridge createBridge()
	{
		System.out.println("create Bridge");
		return new KahinaBridge(this);
	}

	@Override
	protected MUCGUI createGUI()
	{
		System.out.println("create GUI");
		return new MUCGUI(MUCStep.class, this);
	}

	@Override
	public String getApplicationName()
	{
		return "MUStICCa insertion";
	}

	protected void processNewProject()
	{
		System.out.println("new Project");
//		project.register();
		registerRecentProject(project);
		gui.setPerspective(project.getPerspective());
		//gui.displayMainViews();
		setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
		loadSATFile(project.getMainFile());
		
		gui.displayMainViews();
		addFirstStep();
		
		System.out.println("Hello World");
		dispatchInstanceEvent(new KahinaRedrawEvent());
	}

	private void addFirstStep() {
		System.out.println("addFirstStep");
//		AlgorithmData data = new AlgorithmData();
		AbstractAlgorithm alg = new BasicAlgorithm(this.satInstance);
		MUCStep step = new MUCStep(alg);
		this.state.newStep(step, -1);
	}

	public KahinaProject loadProject(InputStream stream)
	{
		System.out.println("load project");
		Document dom;
		KahinaProject project = createNewProject();
		dom = XMLUtil.parseXMLStream(stream, false);
		project = KahinaProject.importXML(dom.getDocumentElement(), project);
		return project;
	}

	@Override
	protected void createTreeBehavior()
	{
		System.out.println("create Tree Behavior");
		// TODO Auto-generated method stub
	}

	public CnfSatInstance getSatInstance()
	{
		return satInstance;
	}

	public void discardCurrentState()
	{
		this.satInstance = null;
		//        this.stat = null;
		this.files = null;
		state = null;
//		state.reset();
	}

	public void loadSATFile(File satFile)
	{
		discardCurrentState();

		satInstance = DimacsCnfParser.parseDimacsCnfFile(satFile.getAbsolutePath());
		System.err.println("Loading SAT instance at " + satFile.getAbsolutePath());
		System.err.println("  Instance Size: (" + satInstance.getSize() + "," + satInstance.getHighestVar() + ")");

		ba = new BasicAlgorithm(satInstance);
		this.state = createState();
		state.setSatInstance(satInstance);
//		this.addFirstStep();

//		if (state == null)

		//TODO: allow the user to configure this filter
		//        if (CFG_FILTER)
		//        {
		//            satInstance.applyDontCareFilter(new CfgDontCareFilter(satInstance));
		//        }

		//        stat = new MUCStatistics();
		//        stat.instanceName = satFile.getAbsolutePath();
		//        
//		files = new MiniSATFiles();
//		files.sourceFile = satFile;
//		files.createTargetFile("test-target");
//		files.createExtendedFile("test-target");
//		files.createTempFiles("test-target-seed");


		//        MUCExtension.extendCNFBySelVars(files.sourceFile, files.tmpFile, stat);

//		state.setSatInstance(satInstance);
		//        state.setStatistics(stat);
//		state.setFiles(files);
	}

	//    public void addFirstUC()
	//    {
	//        List<Integer> muc_cands = new ArrayList<Integer>();
	//        List<Integer> muc = new ArrayList<Integer>();
	//        int numClauses = state.getStatistics().numClausesOrGroups;
	//        for (int i = 1; i <= numClauses; i++)
	//        {
	//            muc_cands.add(i);
	//        }
	//        int ucID = bridge.registerMUC(muc_cands.toArray(new Integer[0]), muc.toArray(new Integer[0]));
	//        MUCStep firstUC = state.retrieve(MUCStep.class, ucID);
	//        
	//        if (state.usesBlocks())
	//        {
	//            //we ensure that the meta instance can compactly represent the first UC
	//            TreeSet<Integer> metaBlock = new TreeSet<Integer>();
	//            for (int i = 1; i <= numClauses; i++)
	//            {
	//                if (firstUC.getUc().contains(i))
	//                {
	//                    metaBlock.add(i);
	//                }
	//            }
	//            state.learnMetaBlock(metaBlock);
	//            state.learnMetaClause(metaBlock);
	//        }
	//        
	//        dispatchInstanceEvent(new KahinaUpdateEvent(1));
	//        dispatchInstanceEvent(new KahinaRedrawEvent());
	//    }



	public void processEvent(KahinaEvent e)
	{
		System.out.println("process Event " + e);
		super.processEvent(e);
		if (e instanceof KahinaControlEvent)
		{
			processControlEvent((KahinaControlEvent) e);
		} 
		else if (e instanceof KahinaSystemEvent)
		{
			processSystemEvent((KahinaSystemEvent) e);
		}
	}

	private void processSystemEvent(KahinaSystemEvent event)
	{
		if (KahinaSystemEvent.QUIT == event.getSystemEventType())
		{

		}
	}

	private void processControlEvent(KahinaControlEvent event)
	{
		String command = event.getCommand();
		if (MUCControlEventCommands.LOAD_FILE.equals(command))
		{
			JFileChooser chooser = new JFileChooser(new File("."));
			chooser.setDialogTitle("Load SAT File");
			chooser.showOpenDialog(gui.getMainWindow());
			File dataFile = chooser.getSelectedFile();

			loadSATFile(dataFile);
			gui.displayMainViews();
			nextStep();
			////            addFirstUC();
			//            //generateFirstUC();
		}
		//        else if (MUCControlEventCommands.LOAD_PATH.equals(command))
		//        {
		//            JOptionPane.showMessageDialog(null, "Path import functionality is not yet implemented.", "Import UC Reduction Path", 1);
		//        }
		//        else if (MUCControlEventCommands.EXPORT_DIMACS.equals(command))
		//        {
		//            MUCStep step = (MUCStep) state.get(state.getSelectedStepID());
		//            CnfSatInstance usInstance = satInstance.selectClauses(step.getUc());
		//            JFileChooser chooser = new JFileChooser(new File("."));
		//            chooser.setDialogTitle("Export US to DIMACS file");
		//            chooser.showSaveDialog(gui.getMainWindow());
		//            File dataFile = chooser.getSelectedFile();
		//            DimacsCnfOutput.writeDimacsCnfFile(dataFile.getAbsolutePath(), usInstance);
		//        }
		//        else if (MUCControlEventCommands.EXPORT_SYMBOLIC_DIMACS.equals(command))
		//        {
		//            MUCStep step = (MUCStep) state.get(state.getSelectedStepID());
		//            CnfSatInstance usInstance = satInstance.selectClauses(step.getUc());
		//            JFileChooser chooser = new JFileChooser(new File("."));
		//            chooser.setDialogTitle("Export US to symbolic DIMACS file");
		//            chooser.showSaveDialog(gui.getMainWindow());
		//            File dataFile = chooser.getSelectedFile();
		//            DimacsCnfOutput.writeSymbolDimacsCnfFile(dataFile.getAbsolutePath(), usInstance);
		//        }
		//        else if (MUCControlEventCommands.EXPORT_VAR_OCCURRENCES.equals(command))
		//        {
		//            MUCStep step = (MUCStep) state.get(state.getSelectedStepID());
		//            CnfSatInstance usInstance = satInstance.selectClauses(step.getUc());
		//            JFileChooser chooser = new JFileChooser(new File("."));
		//            chooser.setDialogTitle("Export variable occurrence statistics");
		//            chooser.showSaveDialog(gui.getMainWindow());
		//            File dataFile = chooser.getSelectedFile();
		//            DimacsCnfOutput.writeVariableOccurrences(dataFile.getAbsolutePath(), usInstance);
		//        }
	}

	private void nextStep() {
		System.out.println("nextStep");
		try {
			ba.selectNext(ba.data.instanceIDs.pollFirst());
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Writing a main method for a Kahina-based debugging environment is simple:
	 * just create an instance of your KahinaInstance subclass and pass its
	 * start method the arguments.
	 * 
	 * @param args
	 */
	public void start(String[] args)
	{
		System.out.println("start");
		startNewSession();
	}

	public static void main(String[] args)
	{
		/*if (args.length > 0)
        {
            if (args[0].equals("nometa"))
            {
                metaLearningMode = MetaLearningMode.NO_META_LEARNING;
            }
            else if  (args[0].equals("nobl"))
            {
                metaLearningMode = MetaLearningMode.NO_BLOCKS;
            }
            else if  (args[0].equals("blprt"))
            {
                metaLearningMode = MetaLearningMode.BLOCK_PARTITION;
            }
            else if  (args[0].equals("blrec"))
            {
                metaLearningMode = MetaLearningMode.RECURSIVE_BLOCKS;
            }
            else
            {
                System.err.println("Mode \"" + args[0] + "\" not recognized, available modes:");
                System.err.println("  nometa - no meta learning");
                System.err.println("  nobl   - meta-learning without blocks");
                System.err.println("  blprt  - meta-learning a partition of blocks");
                System.err.println("  blrec  - meta-learning a tree of blocks (recursive)");
                System.exit(1);
            }
        }
        else
        {
            System.err.println("MUCReducer: No mode specified, using default mode blprt.");
        }*/
		//startup self-test: is minisat on the path?
		if (!MiniSAT.minisatFoundOnPath())
		{
			System.err.println("ERROR: No version of minisat found on path!");
			System.err.println("       Your path must include the minisat-extended directory.");
			System.exit(0);
		}
		MUCInstance kahina = new MUCInstance();
		kahina.start(args);
		if (args.length > 0)
		{
			File dataFile = new File(args[0]);
			kahina.dispatchEvent(new KahinaProjectEvent(KahinaProjectEventType.NEW_PROJECT, dataFile, "default project"));
		}
	}

	@Override
	protected KahinaProject createNewProject()
	{
		return new KahinaProject("default", "none");
	}

	@Override
	protected void prepareProjectLists()
	{
		recentProjects = new LinkedList<KahinaProject>();
		defaultProjects = new LinkedList<KahinaProject>();
		addDefaultProject("data/project/cfg-demo-project.xml");
		addDefaultProject("data/project/asp-demo-project.xml");
		addDefaultProject("data/project/simple-demo-project.xml");
	}

	private void addDefaultProject(String resourcePath)
	{
		//        URL projectLocation = this.getClass().getResource(resourcePath);
		//        try
		//        {
		//            InputStream projectInputStream = projectLocation.openStream();
		//            defaultProjects.add(loadProject(projectInputStream));
		//        }
		//        catch (IOException e)
		//        {
		//            // TODO Auto-generated catch block
		//            e.printStackTrace();
		//        }
	}

	@Override
	protected void preparePerspectiveLists()
	{
		recentPerspectives = new LinkedList<KahinaPerspective>();
		defaultPerspectives = new LinkedList<KahinaPerspective>();       
	}

}
