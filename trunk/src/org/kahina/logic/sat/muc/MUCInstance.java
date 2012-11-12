package org.kahina.logic.sat.muc;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileInputStream;
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
import org.kahina.logic.sat.muc.io.MUCExtension;
import org.kahina.logic.sat.muc.test.AspCcgDontCareFilter;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.kahina.qtype.data.project.QTypeProject;
import org.w3c.dom.Document;

public class MUCInstance extends KahinaInstance<MUCState, MUCGUI, MUCBridge, KahinaProject>
{
    public static final boolean ASP_CCG_FILTER = false;

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
    
    public MUCInstance(MetaLearningMode metaLearningMode)
    {
        this.satInstance = null;
        this.stat = null;
        this.files = null;
        this.reductionManager = new MUCReductionManager(this);
        this.metaLearningMode = metaLearningMode;
        logger.enableLogging();
    }
    
    public MUCInstance(MetaLearningMode metaLearningMode, CnfSatInstance satInstance,  MUCStatistics stat, MiniSATFiles files)
    {
        this.satInstance = satInstance;
        this.stat = stat;
        this.files = files;
        this.reductionManager = new MUCReductionManager(this);
        this.metaLearningMode = metaLearningMode;
        state.setSatInstance(satInstance);
        state.setStatistics(stat);
        state.setFiles(files);
        logger.enableLogging();
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
            System.err.println("MUCState.createState(" + this + ",null)");
            return new MUCState(this);
        }
        System.err.println("MUCState.createState(" + this + "," + satInstance + ")");
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
        return "Kahina for QType";
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
        generateFirstUC();
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
        System.err.println("  Instance Size: (" + satInstance.getNumClauses() + "," + satInstance.getNumVariables() + ")");
        
        //TODO: allow the user to configure this filter
        if (ASP_CCG_FILTER)
        {
            satInstance.applyDontCareFilter(new AspCcgDontCareFilter(satInstance));
        }
        
        stat = new MUCStatistics();
        stat.instanceName = satFile.getAbsolutePath();
        
        files = new MiniSATFiles();
        files.sourceFile = satFile;
        files.createTargetFile("test-target");
        files.createExtendedFile("test-target");
        files.createTempFiles("test-target-seed");
        
        MUCExtension.extendCNFBySelVars(files.sourceFile, files.tmpFile, stat);
        
        state.setSatInstance(satInstance);
        state.setStatistics(stat);
        state.setFiles(files);
    }
    
    public void generateFirstUC()
    {
        List<Integer> muc_cands = new ArrayList<Integer>();
        List<Integer> muc = new ArrayList<Integer>();
        int[] freezeVariables = new int[stat.numVarsExtended - stat.highestID];
        Arrays.fill(freezeVariables, -1);
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
            
            //we ensure that the meta instance can compactly represent the first UC
            TreeSet<Integer> metaBlock = new TreeSet<Integer>();
            int numClauses = state.getStatistics().numClausesOrGroups;
            for (int i = 1; i <= numClauses; i++)
            {
                if (!firstUC.getUc().contains(i))
                {
                    metaBlock.add(-i);
                }
            }
            state.learnMetaBlock(metaBlock);
        
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
            chooser.showOpenDialog(gui.getMainWindow());
            File dataFile = chooser.getSelectedFile();
            
            loadSATFile(dataFile);
            gui.displayMainViews();
            generateFirstUC();
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
            chooser.showSaveDialog(gui.getMainWindow());
            File dataFile = chooser.getSelectedFile();
            DimacsCnfOutput.writeDimacsCnfFile(dataFile.getAbsolutePath(), usInstance);
        }
        else if (MUCControlEventCommands.EXPORT_SYMBOLIC_DIMACS.equals(command))
        {
            MUCStep step = (MUCStep) state.get(state.getSelectedStepID());
            CnfSatInstance usInstance = satInstance.selectClauses(step.getUc());
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Export US to symbolic DIMACS file");
            chooser.showSaveDialog(gui.getMainWindow());
            File dataFile = chooser.getSelectedFile();
            DimacsCnfOutput.writeSymbolDimacsCnfFile(dataFile.getAbsolutePath(), usInstance);
        }
        else if (MUCControlEventCommands.EXPORT_VAR_OCCURRENCES.equals(command))
        {
            MUCStep step = (MUCStep) state.get(state.getSelectedStepID());
            CnfSatInstance usInstance = satInstance.selectClauses(step.getUc());
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Export variable occurrence statistics");
            chooser.showSaveDialog(gui.getMainWindow());
            File dataFile = chooser.getSelectedFile();
            DimacsCnfOutput.writeVariableOccurrences(dataFile.getAbsolutePath(), usInstance);
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
        MetaLearningMode metaLearningMode = MetaLearningMode.BLOCK_PARTITION;
        if (args.length > 0)
        {
            if (args[0].equals("nometa"))
            {
                metaLearningMode = MetaLearningMode.NO_META_LEARNING;
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
                System.err.println("  blprt  - meta-learning a partition of blocks");
                System.err.println("  blrec  - meta-learning a tree of blocks (recursive)");
                System.exit(1);
            }
        }
        else
        {
            System.err.println("MUCReducer: No mode specified, using default mode blprt.");
        }
        (new MUCInstance(metaLearningMode)).start(args);
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
        addDefaultProject("data/project/cnf-demo-project.xml");
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
    
    public MetaLearningMode getMetaLearningMode()
    {
        return metaLearningMode;
    }

    public MUCReductionManager getReductionManager()
    {
        return reductionManager;
    }
}
