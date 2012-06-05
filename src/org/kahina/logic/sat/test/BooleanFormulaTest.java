package org.kahina.logic.sat.test;

import java.awt.Color;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;
import org.kahina.logic.sat.data.free.BooleanFormula;
import org.kahina.logic.sat.data.free.RandomFormulaGenerator;
import org.kahina.logic.sat.io.free.BooleanFormulaParser;
import org.kahina.logic.sat.visual.free.FormulaTreeView;

public class BooleanFormulaTest
{

    /**
     * @param args
     */
    public static void main(String[] args)
    {
        try
        {
            final KahinaController control = new KahinaController();
            final KahinaInstance<?, ?, ?> kahina = new KahinaDefaultInstance();
            
            if (false) { BooleanFormula f = BooleanFormulaParser.parseFile("/stud/dellert/timout_01.abc");} ;
            //BooleanFormula f = BooleanFormulaParser.parseFile("/stud/dellert/formula_test.abc");
            BooleanFormula f = RandomFormulaGenerator.randomFormula(10, 10, 2, false);
            final FormulaTreeView view = new FormulaTreeView(kahina);
            view.getConfig().setNodeSize(8);
            view.getConfig().setHorizontalDistance(6);
            view.getConfig().setVerticalDistance(10);
            view.getConfig().setLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
            view.getConfig().setNodePositionPolicy(KahinaTreeViewOptions.CENTERED_NODES);
            view.getConfig().setEdgeTagPolicy(KahinaTreeViewOptions.NO_EDGE_TAGS);
            view.setStatusColorEncoding(0, Color.YELLOW);
            view.setStatusColorEncoding(1, Color.WHITE);
            view.displayFormula(f);  
            
            SwingUtilities.invokeLater(new Runnable() 
            {
                public void run() 
                {
                    JFrame w = new JFrame("FormulaTreeView Demo");
                    w.setSize(1500, 800);
                    w.setLayout(null);
                    w.add(view.makePanel());
                    w.setVisible(true);
                    w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);   
                    kahina.dispatchEvent(new KahinaRedrawEvent());
                }
            });  

            //System.out.println(f.toStringWithMinimumBracing());
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

}
