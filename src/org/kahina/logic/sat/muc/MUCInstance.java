package org.kahina.logic.sat.muc;

import java.awt.event.ActionEvent;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeSet;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kahina.core.KahinaInstance;
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
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.bridge.MUCBridge;
import org.kahina.logic.sat.muc.control.MUCControlEventCommands;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.gui.MUCGUI;
import org.kahina.logic.sat.muc.heuristics.AscendingIndexHeuristic;
import org.kahina.logic.sat.muc.heuristics.AscendingRelevanceHeuristic;
import org.kahina.logic.sat.muc.heuristics.CenteredIndexHeuristic;
import org.kahina.logic.sat.muc.heuristics.CenteredRelevanceHeuristic;
import org.kahina.logic.sat.muc.heuristics.DescendingIndexHeuristic;
import org.kahina.logic.sat.muc.heuristics.DescendingRelevanceHeuristic;
import org.kahina.logic.sat.muc.heuristics.ReductionHeuristic;
import org.kahina.logic.sat.muc.io.MUCExtension;
import org.kahina.logic.sat.muc.test.AspCcgDontCareFilter;
import org.kahina.logic.sat.muc.test.CfgDontCareFilter;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.kahina.qtype.data.project.QTypeProject;
import org.w3c.dom.Document;

public class MUCInstance extends KahinaInstance<MUCState, MUCGUI, MUCBridge, KahinaProject>
{
    public static final boolean CFG_FILTER = false;

    public final Action LOAD_FILE_ACTION = new AbstractAction("Load SAT File")
    {

        private static final long serialVersionUID = -3829326193202814557L;

        @Override
        public void actionPerformed(ActionEvent e)
        {
            dispatchEvent(new KahinaControlEvent(MUCControlEventCommands.LOAD_FILE));
        }

    };
    
    public final Action LOAD_PATH_ACTION = new AbstractAction("Add Path from File")
    {

        private static final long serialVersionUID = -3829326193202814557L;

        @Override
        public void actionPerformed(ActionEvent e)
        {
            dispatchEvent(new KahinaControlEvent(MUCControlEventCommands.LOAD_PATH));
        }

    };
    
    public final Action US_DIMACS_EXPORT_ACTION = new AbstractAction("Export current US to DIMACS file")
    {
        private static final long serialVersionUID = -1461082967969729918L;

        @Override
        public void actionPerformed(ActionEvent e)
        {
            dispatchEvent(new KahinaControlEvent(MUCControlEventCommands.EXPORT_DIMACS));
        }

    };
    
    public final Action US_SYMBOLIC_DIMACS_EXPORT_ACTION = new AbstractAction("Export current US to symbolic DIMACS file")
    {
        private static final long serialVersionUID = 1150689380235228957L;

        @Override
        public void actionPerformed(ActionEvent e)
        {
            dispatchEvent(new KahinaControlEvent(MUCControlEventCommands.EXPORT_SYMBOLIC_DIMACS));
        }

    };
    
    public final Action US_EXPORT_VAR_OCCURRENCES = new AbstractAction("Export var occurrence list for current US ")
    {
        private static final long serialVersionUID = 7555952855709180730L;

        @Override
        public void actionPerformed(ActionEvent e)
        {
            dispatchEvent(new KahinaControlEvent(MUCControlEventCommands.EXPORT_VAR_OCCURRENCES));
        }
    };
    
    public final Action QUIT_ACTION = new AbstractAction("Quit")
    {

        private static final long serialVersionUID = -3829326193202814557L;

        @Override
        public void actionPerformed(ActionEvent e)
        {
            dispatchEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
        }

    };
    
    CnfSatInstance satInstance;
    MUCStatistics stat;
    MiniSATFiles files;
    
    private MUCReductionManager reductionManager;
    
    private MetaLearningMode metaLearningMode;
    
    private List<Class<? extends ReductionHeuristic>> heuristics;
    
    
    public MUCInstance(MetaLearningMode metaLearningMode)
    {
        this.satInstance = null;
        this.stat = null;
        this.files = null;
        this.metaLearningMode = metaLearningMode;
        this.heuristics = new ArrayList<Class<? extends ReductionHeuristic>>();
        logger.disableLogging();
    }
    
    public MUCInstance(MetaLearningMode metaLearningMode, CnfSatInstance satInstance,  MUCStatistics stat, MiniSATFiles files)
    {
        this.satInstance = satInstance;
        this.stat = stat;
        this.files = files;
        this.metaLearningMode = metaLearningMode;
        this.heuristics = new ArrayList<Class<? extends ReductionHeuristic>>();
        state.setSatInstance(satInstance);
        state.setStatistics(stat);
        state.setFiles(files);
        logger.disableLogging();
    }
    
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
    
    public MUCBridge startNewSession()
    {
        try
        {
            super.startNewSession();
            registerSessionListener(KahinaEventTypes.CONTROL, this);
            registerSessionListener(KahinaEventTypes.SYSTEM, this);
            return bridge;
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(-1);
        }
        return null;
    }

    protected MUCState createState()
    {
        if (satInstance == null)
        {
            //System.err.println("MUCState.createState(" + this + ",null)");
            return new MUCState(this);
        }
        //System.err.println("MUCState.createState(" + this + "," + satInstance + ")");
        return new MUCState(this,satInstance, stat, files);
    }

    @Override
    protected MUCBridge createBridge()
    {
        return new MUCBridge(this);
    }

    @Override
    protected MUCGUI createGUI()
    {
        return new MUCGUI(MUCStep.class, this);
    }
    
    @Override
    public String getApplicationName()
    {
        return "MUStICCa";
    }
    
    protected void processNewProject()
    {
        project.register();
        registerRecentProject(project);
        gui.setPerspective(project.getPerspective());
        //gui.displayMainViews();
        setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
        loadSATFile(project.getMainFile());
        gui.displayMainViews();
        addFirstUC();
        //generateFirstUC();
        dispatchInstanceEvent(new KahinaRedrawEvent());
    }
        
    public KahinaProject loadProject(InputStream stream)
    {
        Document dom;
        KahinaProject project = createNewProject();
        dom = XMLUtil.parseXMLStream(stream, false);
        project = KahinaProject.importXML(dom.getDocumentElement(), project);
        return project;
    }

    @Override
    protected void createTreeBehavior()
    {
        // TODO Auto-generated method stub
    }
    
    public CnfSatInstance getSatInstance()
    {
        return satInstance;
    }
    
    public void discardCurrentState()
    {
        this.satInstance = null;
        this.stat = null;
        this.files = null;
        state.reset();
    }
    
    public void loadSATFile(File satFile)
    {
        discardCurrentState();
        
        satInstance = DimacsCnfParser.parseDimacsCnfFile(satFile.getAbsolutePath());
        System.err.println("Loading SAT instance at " + satFile.getAbsolutePath());
        System.err.println("  Instance Size: (" + satInstance.getSize() + "," + satInstance.getHighestVar() + ")");
        
        //TODO: allow the user to configure this filter
        if (CFG_FILTER)
        {
            satInstance.applyDontCareFilter(new CfgDontCareFilter(satInstance));
        }
        
        stat = new MUCStatistics();
        stat.instanceName = satFile.getAbsolutePath();
        
        files = new MiniSATFiles();
        files.sourceFile = satFile;
        files.createTargetFile("test-target");
        files.createExtendedFile("test-target");
        files.createTempFiles("test-target-seed");
        
        this.reductionManager = new MUCReductionManager(this, files);
        
        MUCExtension.extendCNFBySelVars(files.sourceFile, files.tmpFile, stat);
        
        state.setSatInstance(satInstance);
        state.setStatistics(stat);
        state.setFiles(files);
    }
    
    public void addFirstUC()
    {
        List<Integer> muc_cands = new ArrayList<Integer>();
        List<Integer> muc = new ArrayList<Integer>();
        int numClauses = state.getStatistics().numClausesOrGroups;
        for (int i = 1; i <= numClauses; i++)
        {
            muc_cands.add(i);
        }
        int ucID = bridge.registerMUC(muc_cands.toArray(new Integer[0]), muc.toArray(new Integer[0]));
        MUCStep firstUC = state.retrieve(MUCStep.class, ucID);
        
        if (state.usesBlocks())
        {
            //we ensure that the meta instance can compactly represent the first UC
            TreeSet<Integer> metaBlock = new TreeSet<Integer>();
            for (int i = 1; i <= numClauses; i++)
            {
                if (firstUC.getUc().contains(i))
                {
                    metaBlock.add(i);
                }
            }
            state.learnMetaBlock(metaBlock);
            state.learnMetaClause(metaBlock);
        }
        
        dispatchInstanceEvent(new KahinaUpdateEvent(1));
        dispatchInstanceEvent(new KahinaRedrawEvent());
    }
    
    public void generateFirstUC()
    {
        List<Integer> muc_cands = new ArrayList<Integer>();
        List<Integer> muc = new ArrayList<Integer>();
        int[] freezeVariables = new int[stat.numVarsExtended - stat.highestID];
        Arrays.fill(freezeVariables, 1);
        /*for (int i = 0; i < stat.numClausesOrGroups; i++)
        {
            if (satInstance.isDontCareClause(i))
            {
                freezeVariables[i] = -1;
            }
            else
            {
                freezeVariables[i] = 1;
            }
        }*/
        try
        {
            MiniSAT.createFreezeFile(freezeVariables, files.tmpFreezeFile, stat.highestID + 1);
            MiniSAT.solve(files.tmpFile, files.tmpProofFile, files.tmpResultFile, files.tmpFreezeFile);
        }
        catch (Exception e)
        {
            files.deleteTempFiles();
            files.targetFile.delete();
            System.err.println("Timeout: " + files.sourceFile.getAbsolutePath());
            System.exit(0);
        }
        System.out.println("Solver finished");
        // if unsatisfiable
        if (MiniSAT.wasUnsatisfiable())
        {
            List<Integer> relevantAssumptions = MiniSAT.getRelevantAssumptions(freezeVariables, stat.highestID + 1);
            stat.initNumRelAsm = relevantAssumptions.size();
            for (Integer a : relevantAssumptions)
            {
                muc_cands.add(a);
            }
            relevantAssumptions.clear();
            int ucID = bridge.registerMUC(muc_cands.toArray(new Integer[0]), muc.toArray(new Integer[0]));
            MUCStep firstUC = state.retrieve(MUCStep.class, ucID);
            
            if (state.usesBlocks())
            {
                //we ensure that the meta instance can compactly represent the first UC
                TreeSet<Integer> metaBlock = new TreeSet<Integer>();
                int numClauses = state.getStatistics().numClausesOrGroups;
                for (int i = 1; i <= numClauses; i++)
                {
                    if (firstUC.getUc().contains(i))
                    {
                        metaBlock.add(i);
                    }
                }
                state.learnMetaBlock(metaBlock);
                state.learnMetaClause(metaBlock);
            }
        
            /*UCReducer reducer1 = new UCReducer(kahinaInstance.getState(), 1, files);
            reducer1.setHeuristics(new AlwaysFirstHeuristics());
            kahinaInstance.getState().getReducers().add(reducer1);
            reducer1.start();
            
            UCReducer reducer2 = new UCReducer(kahinaInstance.getState(), 1, files);
            reducer2.setHeuristics(new AlwaysLastHeuristics());
            kahinaInstance.getState().getReducers().add(reducer2);
            reducer2.start();
            
            UCReducer reducer3 = new UCReducer(kahinaInstance.getState(), 1, files);
            reducer3.setHeuristics(new CenterHeuristics());
            kahinaInstance.getState().getReducers().add(reducer3);
            reducer3.start();*/
            
            dispatchInstanceEvent(new KahinaUpdateEvent(1));
            dispatchInstanceEvent(new KahinaRedrawEvent());
        }
        else
        {
            System.err.println("ERROR: Original problem is satisfiable!");
        }
    }
    
    public void processEvent(KahinaEvent e)
    {
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
            if (chooser.showOpenDialog(gui.getMainWindow()) == JFileChooser.APPROVE_OPTION)
            {
	            File dataFile = chooser.getSelectedFile();
	            
	            loadSATFile(dataFile);
	            gui.displayMainViews();
	            addFirstUC();
	            //generateFirstUC();
            }
        }
        else if (MUCControlEventCommands.LOAD_PATH.equals(command))
        {
            JOptionPane.showMessageDialog(null, "Path import functionality is not yet implemented.", "Import UC Reduction Path", 1);
        }
        else if (MUCControlEventCommands.EXPORT_DIMACS.equals(command))
        {
            MUCStep step = (MUCStep) state.get(state.getSelectedStepID());
            CnfSatInstance usInstance = satInstance.selectClauses(step.getUc());
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Export US to DIMACS file");
            if (chooser.showSaveDialog(gui.getMainWindow()) == JFileChooser.APPROVE_OPTION)
            {
            	File dataFile = chooser.getSelectedFile();
            	DimacsCnfOutput.writeDimacsCnfFile(dataFile.getAbsolutePath(), usInstance);
            }
            else
            {
            	System.err.println("US export canceled by the user.");
            }
        }
        else if (MUCControlEventCommands.EXPORT_SYMBOLIC_DIMACS.equals(command))
        {
            MUCStep step = (MUCStep) state.get(state.getSelectedStepID());
            CnfSatInstance usInstance = satInstance.selectClauses(step.getUc());
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Export US to symbolic DIMACS file");
            if (chooser.showSaveDialog(gui.getMainWindow()) == JFileChooser.APPROVE_OPTION)
            {
            	File dataFile = chooser.getSelectedFile();
            	DimacsCnfOutput.writeSymbolDimacsCnfFile(dataFile.getAbsolutePath(), usInstance);
            }
            else
            {
            	System.err.println("US export canceled by the user.");
            }
        }
        else if (MUCControlEventCommands.EXPORT_VAR_OCCURRENCES.equals(command))
        {
            MUCStep step = (MUCStep) state.get(state.getSelectedStepID());
            CnfSatInstance usInstance = satInstance.selectClauses(step.getUc());
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Export variable occurrence statistics");
            if (chooser.showSaveDialog(gui.getMainWindow()) == JFileChooser.APPROVE_OPTION)
            {
            	File dataFile = chooser.getSelectedFile();
            	DimacsCnfOutput.writeVariableOccurrences(dataFile.getAbsolutePath(), usInstance);
            }
            else
            {
            	System.err.println("Export of var occurrence statistics canceled by the user.");
            }
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
        startNewSession();
    }

    public static void main(String[] args)
    {
        MetaLearningMode metaLearningMode = MetaLearningMode.RECURSIVE_BLOCKS;
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
    	boolean expectsHeuristicsFile = false;
    	String heuristicsFile = null;
    	File dataFile = null;
    	for (int idx = 0; idx < args.length; idx++)
    	{
    		if (args[idx].startsWith("-"))
            {
    			if (!expectsHeuristicsFile)
    			{
                    if (args[idx].equals("-bp"))
                    {
                        metaLearningMode = MetaLearningMode.BLOCK_PARTITION;
                    }
                    else if (args[idx].equals("-bt"))
                    {
                        metaLearningMode = MetaLearningMode.RECURSIVE_BLOCKS;
                    }
                    else if (args[idx].equals("-nometa"))
                    {
                        metaLearningMode = MetaLearningMode.NO_META_LEARNING;
                    }
                    else if (args[idx].equals("-hf"))
                    {
                        expectsHeuristicsFile = true;
                    }
                    else
                    {
                        System.err.println("ERROR: unknown option \"" + args[idx] + "\"");
                        System.exit(1);
                    }
    			}
    			else
    			{
    				System.err.println("ERROR: -hf option must be accompanied by a filename");
    				System.exit(1);
    			}
            }
    		else
    		{
    			//interpret as name of heuristics file
    			if (expectsHeuristicsFile)
    			{
    				heuristicsFile = args[idx];
    				expectsHeuristicsFile = false;
    			}
    			//interpret as name of file to be opened
    			else
    			{
    				 dataFile = new File(args[idx]);
    			}
    		}
    	}     
        MUCInstance kahina = new MUCInstance(metaLearningMode);
        if (heuristicsFile != null)
        {
        	kahina.loadHeuristicsFile(heuristicsFile);
        }
        else
        {
        	kahina.loadDefaultHeuristics();
        }
        kahina.start(args);
        if (dataFile != null)
        {
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
        URL projectLocation = this.getClass().getResource(resourcePath);
        try
        {
            InputStream projectInputStream = projectLocation.openStream();
            defaultProjects.add(loadProject(projectInputStream));
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    @Override
    protected void preparePerspectiveLists()
    {
        recentPerspectives = new LinkedList<KahinaPerspective>();
        defaultPerspectives = new LinkedList<KahinaPerspective>();       
    }
    
    protected void loadHeuristicsFile(String fileName)
    {
        System.err.println("Loading heuristics specification file " + fileName + "...");
        try
        {
            BufferedReader heuristicsFileInput = new BufferedReader(new FileReader(new File(fileName)));
            String className;
            while ((className = heuristicsFileInput.readLine()) != null)
            {
                try
                {
                    Class<? extends ReductionHeuristic> heuristic = (Class<? extends ReductionHeuristic>) Class.forName(className);
                    heuristics.add(heuristic);
                }
                catch (ClassNotFoundException e)
                {
                    System.err.println("  WARNING: could not find heuristic class " + className + "! Ignoring.");
                }
                catch (ClassCastException e)
                {
                    System.err.println("  WARNING: " + className + " is not a class derived from ReductionHeuristic! Ignoring.");
                }
            }
            heuristicsFileInput.close();
        }
        catch (FileNotFoundException e)
        {
            System.err.println("  ERROR: file " + fileName + " not found! Aborting.");
            System.exit(1);
        }
        catch (IOException e)
        {
            System.err.println("  ERROR: could not read from file " + fileName + "! Aborting.");
            System.exit(1);
        }
    }
    
    protected void loadDefaultHeuristics()
    {
        System.err.println("Loading default reduction heuristics...");
        heuristics.add(AscendingIndexHeuristic.class);
        heuristics.add(CenteredIndexHeuristic.class);
        heuristics.add(DescendingIndexHeuristic.class);
        heuristics.add(AscendingRelevanceHeuristic.class);
        heuristics.add(CenteredRelevanceHeuristic.class);
        heuristics.add(DescendingRelevanceHeuristic.class);
    }
    
    public MetaLearningMode getMetaLearningMode()
    {
        return metaLearningMode;
    }

    public MUCReductionManager getReductionManager()
    {
        return reductionManager;
    }

    public List<Class<? extends ReductionHeuristic>> getReductionHeuristics()
    {
        return heuristics;
    }
}
