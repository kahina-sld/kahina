package org.kahina.logic.sat.freemuc;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
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
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.logic.sat.data.free.BooleanFormula;
import org.kahina.logic.sat.freemuc.control.FreeMUCControlEventCommands;
import org.kahina.logic.sat.freemuc.gui.FreeMUCGUI;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.free.BooleanFormulaParser;
import org.kahina.logic.sat.io.free.TseitinTransformationVisitor;
import org.kahina.logic.sat.io.minisat.MiniSAT;

public class FreeMUCInstance extends KahinaInstance<FreeMUCState, FreeMUCGUI, KahinaBridge, KahinaProject>
{
    public final Action SAT_CHECK_ACTION  = new AbstractAction("Check Satisfiability")
    {

        private static final long serialVersionUID = -3829326193202814557L;

        @Override
        public void actionPerformed(ActionEvent e)
        {
            dispatchEvent(new KahinaControlEvent(FreeMUCControlEventCommands.CHECK_SAT));
        }

    };

    public final Action LOAD_FILE_ACTION = new AbstractAction("Load Formula File")
    {

        private static final long serialVersionUID = -3829326193202814557L;

        @Override
        public void actionPerformed(ActionEvent e)
        {
            dispatchEvent(new KahinaControlEvent(FreeMUCControlEventCommands.LOAD_FILE));
        }

    };
    
    BooleanFormula formula;
    
    public FreeMUCInstance()
    {
        this.formula = null;
    }
    
    public FreeMUCInstance(BooleanFormula formula)
    {
        this.formula = formula;
        state.setFormula(formula);
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
    
    public KahinaBridge startNewSession()
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

    protected FreeMUCState createState()
    {
        if (formula == null)
        {
            return new FreeMUCState(this);
        }
        System.err.println("FreeMUCState.createState(" + this + "," + formula + ")");
        return new FreeMUCState(this, formula);
    }

    @Override
    protected KahinaBridge createBridge()
    {
        return new KahinaBridge(this);
    }

    @Override
    protected FreeMUCGUI createGUI()
    {
        return new FreeMUCGUI(FreeMUCStep.class, this, sessionControl);
    }
    
    public BooleanFormula getFormula()
    {
        return formula;
    }
    
    public void discardCurrentState()
    {
        this.formula = null;
        state.reset();
    }
    
    public void loadFormulaFile(File formulaFile)
    {
        discardCurrentState();      
        try
        {
            formula = BooleanFormulaParser.parseFile(formulaFile.getAbsolutePath());
            System.err.println("Loading boolean formula at " + formulaFile.getAbsolutePath());
            System.err.println("  Formula Size: " + formula.getSize());
            
            state.setFormula(formula);
        }
        catch (FileNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
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
        
        if (FreeMUCControlEventCommands.LOAD_FILE.equals(command))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Load Formula File");
            chooser.showOpenDialog(gui.getMainWindow());
            File dataFile = chooser.getSelectedFile();
            
            loadFormulaFile(dataFile);
            gui.displayMainViews();
            dispatchEvent(new KahinaRedrawEvent());
        }
        else if (FreeMUCControlEventCommands.CHECK_SAT.equals(command))
        {
            if (currentPruningSatisfiable())
            {
                System.err.println("Satisfiable!");
            }
            else
            {
                System.err.println("Unatisfiable!");
            }
        }
    }
    
    public boolean currentPruningSatisfiable()
    {
        TseitinTransformationVisitor visitor = new TseitinTransformationVisitor();
        int topVar = formula.accept(visitor);
        DimacsCnfOutput.writeDimacsCnfFile("tseitin.cnf", visitor.getCNF(topVar));
        try
        {
            File tseitinFile = new File("tseitin.cnf");
            File resultFile = new File("tseitin-result.tmp");
            boolean result = MiniSAT.isSatisfiable(tseitinFile, resultFile);
            tseitinFile.delete();
            resultFile.delete();
            return result;
        }
        catch (TimeoutException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (InterruptedException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        //default assumption: satisfiable!
        return true;
    }
    
    public static void main(String[] args)
    {
        (new FreeMUCInstance()).start(args);
    }

    @Override
    protected void createTreeBehavior()
    {
        // TODO Auto-generated method stub
        
    }

    @Override
    protected KahinaProject createNewProject()
    {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public KahinaProject loadProject(InputStream stream)
    {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    protected void prepareProjectLists()
    {
        recentProjects = new LinkedList<KahinaProject>();
        defaultProjects = new LinkedList<KahinaProject>(); 
    }

    @Override
    protected void preparePerspectiveLists()
    {
        recentPerspectives = new LinkedList<KahinaPerspective>();
        defaultPerspectives = new LinkedList<KahinaPerspective>();      
    }
}
