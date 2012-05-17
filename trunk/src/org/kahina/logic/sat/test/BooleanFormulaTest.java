package org.kahina.logic.sat.test;

import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.logic.sat.data.free.BooleanFormula;
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
            
            BooleanFormula f = BooleanFormulaParser.parseFile("/stud/dellert/formula_test.simplified.abc");
            final FormulaTreeView view = new FormulaTreeView(kahina);
            view.getConfig().setZoomLevel(5);
            view.displayFormula(f);  
            
            SwingUtilities.invokeLater(new Runnable() 
            {
                public void run() 
                {
                    JFrame w = new JFrame("FormulaTreeView Demo");
                    w.setSize(600, 600);
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
