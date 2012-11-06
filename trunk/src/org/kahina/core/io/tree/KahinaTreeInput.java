package org.kahina.core.io.tree;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.data.tree.KahinaUnlayeredMemTree;

public class KahinaTreeInput
{
    public static KahinaTree fromIndentedText(String fileName) throws IOException
    {
        KahinaTree tree = new KahinaUnlayeredMemTree();
        try
        {      
            FileReader in = new FileReader(new File(fileName));
            int currentDepth = 0;
            int currentID = -1;
            int newDepth = 0;
            char next;
            boolean countingSpaces = true;
            StringBuilder labelBuilder = new StringBuilder();
            while ((next = (char) in.read()) != -1)
            {
                switch (next)
                {
                    case '\n':
                    {
                        String label = labelBuilder.toString();
                        if (currentID == -1)
                        {
                            int rootID = tree.addNode(label, "", 0);
                            tree.setRootID(rootID);
                            currentID = rootID;
                            currentDepth = 0;
                        }
                        else
                        {
                            if (newDepth == currentDepth + 1)
                            {
                                int newID = tree.addNode(label, "", 0);
                                tree.addChild(currentID, newID);
                                currentID = newID;
                                currentDepth++;
                            }
                            else
                            {
                                while (currentDepth >= newDepth)
                                {
                                    currentID = tree.getParent(currentID);
                                    currentDepth--;
                                }
                                int newID = tree.addNode(label, "", 0);
                                tree.addChild(currentID, newID);
                                currentID = newID;
                                currentDepth++;
                            }
                        }
                        countingSpaces = true;
                        newDepth = 0;
                        break;
                    }
                    case ' ':
                    {
                        if (countingSpaces)
                        {
                            newDepth++;
                        }
                        else
                        {
                            labelBuilder.append(next);
                        }
                        break;
                    }
                    default:
                    {
                        if (countingSpaces)
                        {
                            labelBuilder = new StringBuilder();
                            countingSpaces = false;
                        }
                        labelBuilder.append(next);
                    }
                }
            }
            in.close();
        }
        catch (IOException e)
        {
            System.err.println("ERROR: Could not read tree file: " + fileName);
            e.printStackTrace();
            throw e;
        }
        return tree;
    }
}
