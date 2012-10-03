package org.kahina.logic.sat.muc;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

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
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
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

public class MUCInstance extends KahinaInstance<MUCState, MUCGUI, MUCBridge, KahinaProject>
{

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
    
    public MUCInstance()
    {
        this.satInstance = null;
        this.stat = null;
        this.files = null;
    }
    
    public MUCInstance(CnfSatInstance satInstance,  MUCStatistics stat, MiniSATFiles files)
    {
        this.satInstance = satInstance;
        this.stat = stat;
        this.files = files;
        state.setSatInstance(satInstance);
        state.setStatistics(stat);
        state.setFiles(files);
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
        boolean[] freezeVariables = new boolean[stat.numVarsExtended - stat.highestID];
        Arrays.fill(freezeVariables, Boolean.TRUE);
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
            bridge.registerMUC(muc_cands.toArray(new Integer[0]), muc.toArray(new Integer[0]));
        
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

    public static void main(String[] args)
    {
        (new MUCInstance()).start(args);
    }

    @Override
    protected KahinaProject createNewProject()
    {
        // TODO Auto-generated method stub
        return new KahinaProject("default", "none");
    }

    @Override
    public KahinaProject loadProject(File projectFile)
    {
        // TODO Auto-generated method stub
        return new KahinaProject("default", "none");
    }

    @Override
    protected void prepareProjectLists()
    {
        recentProjects = new LinkedList<KahinaProject>();
        defaultProjects = new LinkedList<KahinaProject>();
    }
}
