package org.kahina.core.io.tree;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.kahina.core.data.tree.KahinaTree;

public class KahinaTreeOutput
{
    public static void toIndentedText(KahinaTree tree, String fileName)
    {
        try
        {
            FileWriter out = new FileWriter(new File(fileName));
            subTreeToIndentedText(tree, tree.getRootID(), 0, out);
            out.close();
        }
        catch (IOException e)
        {
            System.err.println("ERROR: Could not write to tree output file: " + fileName);
            e.printStackTrace();
        }
    }
    
    private static void subTreeToIndentedText(KahinaTree tree, int nodeID, int depth, FileWriter out) throws IOException
    {
        for (int i = 0; i < depth; i++)
        {
            out.append(' ');
        }
        out.write(tree.getNodeCaption(nodeID) + "\n");
        for (int childID : tree.getChildren(nodeID))
        {
            subTreeToIndentedText(tree, childID, depth + 1, out);
        }
    }
}
