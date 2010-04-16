package org.kahina.core.data.tree;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;

import org.kahina.core.KahinaException;
import org.kahina.core.KahinaRunner;
import org.kahina.core.data.lightweight.LightweightKahinaObject;
import org.kahina.core.io.database.DatabaseHandler;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class KahinaDbTree extends KahinaTree implements LightweightKahinaObject
{

    private static final String CLIENT_ID = KahinaDbTree.class.getName();

    private static final String TABLE_NAME_PREFIX = KahinaDbTree.class.getSimpleName()
            + "_";

    private static final String NODE_TABLE_NAME = TABLE_NAME_PREFIX + "nodes";

    private DatabaseHandler db;

    private PreparedStatement addNodeStatement;

    private PreparedStatement addEdgeStatement;

    private PreparedStatement addLayerInformationStatement;

    private PreparedStatement getRealParentStatement;

    private PreparedStatement getVirtualParentStatement;

    private PreparedStatement getLayerStatement;

    private PreparedStatement clearStatement;

    private PreparedStatement collapseStatement;

    private PreparedStatement decollapseStatement;

    private PreparedStatement decollapseAllStatement;

    private PreparedStatement getRealChildrenStatement;

    private PreparedStatement getChildrenForLayerStatement;

    private PreparedStatement getEdgeLabelStatement;
    
    private PreparedStatement setEdgeLabelStatement;

    private PreparedStatement getNodeCaptionStatement;
    
    private PreparedStatement setNodeCaptionStatement;

    private PreparedStatement getNodeStatusStatement;
    
    private PreparedStatement setNodeStatusStatement;

    private PreparedStatement getRootStatement;

    private PreparedStatement isCollapsedStatement;

    private PreparedStatement getSizeStatement;

    private PreparedStatement getMaxNodeIDStatement;

    public KahinaDbTree(DatabaseHandler db)
    {
        this(new DefaultLayerDecider(), db);
    }

    public KahinaDbTree(LayerDecider decider, DatabaseHandler db)
    {
        super(decider);
        this.db = db;
        createTablesIfNecessary();
        prepareStatements();
    }

    private void createTablesIfNecessary()
    {
        if (!db.isRegistered(CLIENT_ID))
        {
            db.execute("CREATE TABLE " + NODE_TABLE_NAME + " (id INT, "
                    + "tree INT, " + "nodeCaption LONG VARCHAR, "
                    + "edgeLabel LONG VARCHAR, " + "status INT, "
                    + "collapsed SMALLINT, " + "realParent INT, "
                    + "layer INT, " + "virtualParent INT, "
                    + "PRIMARY KEY (id, tree))");
            db.execute("CREATE INDEX tree ON " + NODE_TABLE_NAME + " (tree)");
            db.execute("CREATE INDEX realParent ON " + NODE_TABLE_NAME
                    + " (realParent)");
            db.execute("CREATE INDEX layer ON " + NODE_TABLE_NAME + " (layer)");
            db.execute("CREATE INDEX virtualParent ON " + NODE_TABLE_NAME
                    + " (virtualParent)");
            db.register(CLIENT_ID);
        }
    }

    private void prepareStatements()
    {
        int treeID = getID();
        addNodeStatement = db.prepareStatement("INSERT INTO " + NODE_TABLE_NAME
                + " (id, tree, nodeCaption, edgeLabel, status) VALUES (?, "
                + treeID + ", ?, ?, ?)");
        addEdgeStatement = db.prepareStatement("UPDATE " + NODE_TABLE_NAME
                + " SET realParent = ? WHERE id = ? AND tree = " + treeID);
        addLayerInformationStatement = db.prepareStatement("UPDATE "
                + NODE_TABLE_NAME
                + " SET layer = ?, virtualParent = ? WHERE id = ? AND tree = "
                + treeID);
        getRealParentStatement = db.prepareStatement("SELECT realParent FROM "
                + NODE_TABLE_NAME + " WHERE id = ? AND tree = " + treeID);
        getVirtualParentStatement = db.prepareStatement("SELECT virtualParent FROM "
                + NODE_TABLE_NAME + " WHERE id = ? AND tree = "
                + treeID);
        getLayerStatement = db.prepareStatement("SELECT layer FROM "
                + NODE_TABLE_NAME + " WHERE id = ? AND tree = " + treeID);
        clearStatement = db.prepareStatement("DELETE FROM " + NODE_TABLE_NAME
                + " WHERE tree = " + treeID);
        collapseStatement = db.prepareStatement("UPDATE " + NODE_TABLE_NAME
                + " SET collapsed = 1 WHERE id = ? AND tree = " + treeID);
        decollapseStatement = db.prepareStatement("UPDATE " + NODE_TABLE_NAME
                + " SET collapsed = 0 WHERE id = ? AND tree = " + treeID);
        decollapseAllStatement = db.prepareStatement("UPDATE " + NODE_TABLE_NAME
                + " SET collapsed = 0 WHERE tree = " + treeID);
        getRealChildrenStatement = db.prepareStatement("SELECT id FROM "
                + NODE_TABLE_NAME + " WHERE realParent = ? AND tree = "
                + treeID);
        getChildrenForLayerStatement = db.prepareStatement("SELECT id FROM "
                + NODE_TABLE_NAME + " WHERE tree = " + treeID
                + " AND (virtualParent = ? OR (realParent = ? AND layer < ?))");
        getEdgeLabelStatement = db.prepareStatement("SELECT edgeLabel FROM "
                + NODE_TABLE_NAME + " WHERE id = ? AND tree = " + treeID);
        setEdgeLabelStatement = db.prepareStatement("UPDATE " + NODE_TABLE_NAME
                + " SET edgeLabel = ? WHERE id = ? AND tree = " + treeID);
        getNodeCaptionStatement = db.prepareStatement("SELECT nodeCaption FROM " + NODE_TABLE_NAME
                + " WHERE id = ? AND tree = " + treeID);
        setNodeCaptionStatement = db.prepareStatement("UPDATE " + NODE_TABLE_NAME
                + " SET nodeCaption = ? WHERE id = ? AND tree = " + treeID);
        getNodeStatusStatement = db.prepareStatement("SELECT status FROM "
                + NODE_TABLE_NAME + " WHERE id = ? AND tree = " + treeID);
        setNodeStatusStatement = db.prepareStatement("UPDATE " + NODE_TABLE_NAME
                + " SET status = ? WHERE id = ? AND tree = " + treeID);
        getRootStatement = db.prepareStatement("SELECT id FROM "
                + NODE_TABLE_NAME + " WHERE realParent IS NULL AND tree = "
                + treeID);
        isCollapsedStatement = db.prepareStatement("SELECT collapsed FROM "
                + NODE_TABLE_NAME + " WHERE id = ? AND tree = " + treeID);
        getSizeStatement = db.prepareStatement("SELECT COUNT(*) FROM "
                + NODE_TABLE_NAME + " WHERE tree = " + treeID);
        getMaxNodeIDStatement = db.prepareStatement("SELECT MAX(id) FROM " + NODE_TABLE_NAME);
    }

    public void addNode(int id, String caption, String label, int nodeStatus)
    {
        try
        {
            addNodeStatement.setInt(1, id);
            addNodeStatement.setString(2, caption);
            addNodeStatement.setString(3, label);
            addNodeStatement.setInt(4, nodeStatus);
            addNodeStatement.execute();
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
    }

    @Override
    public int addNode(String caption, String label, int nodeStatus)
    {
        int id = getMaxNodeID() + 1;
        addNode(id, caption, label, nodeStatus);
        return id;
    }

    @Override
    public void addChild(int parent, int child)
    {
        try
        {
            addEdgeStatement.setInt(1, parent);
            addEdgeStatement.setInt(2, child);
            addEdgeStatement.execute();
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
        computeAndStoreLayerInformation(child);
    }

    private void computeAndStoreLayerInformation(int child)
    {
        int childLayer = decider.decideOnLayer(child, this);
        int virtualParent = getParent(child);
        int parentLayer = getLayer(virtualParent);
        while (parentLayer > childLayer)
        {
            virtualParent = getParent(virtualParent);
            parentLayer = getLayer(virtualParent);
        }
        if (parentLayer < childLayer)
        {
            virtualParent = -1;
        }
        try
        {
            addLayerInformationStatement.setInt(1, childLayer);
            addLayerInformationStatement.setInt(2, virtualParent);
            addLayerInformationStatement.setInt(3, child);
            addLayerInformationStatement.execute();
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
    }

    @Override
    public void setLayerDecider(LayerDecider decider)
    {
        super.setLayerDecider(decider);
        recomputeLayers();
    }

    public void recomputeLayers()
    {
        recomputeLayers(getRootID());
    }

    public void recomputeLayers(int nodeID)
    {
        computeAndStoreLayerInformation(nodeID);
        for (int childID : getChildren(nodeID))
        {
            recomputeLayers(childID);
        }
    }

    @Override
    public int getParent(int nodeID)
    {
        try
        {
            getRealParentStatement.setInt(1, nodeID);
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
        return db.queryInteger(getRealParentStatement, -1);
    }

    private int getVirtualParent(int nodeID)
    {
        try
        {
            getVirtualParentStatement.setInt(1, nodeID);
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
        return db.queryInteger(getVirtualParentStatement, -1);
    }

    private int getLayer(int nodeID)
    {
        try
        {
            getLayerStatement.setInt(1, nodeID);
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
        return db.queryInteger(getLayerStatement, 0);
    }

    @Override
    public void clear()
    {
        db.execute(clearStatement);
    }

    @Override
    public void collapse(int nodeID)
    {
        try
        {
            collapseStatement.setInt(1, nodeID);
            collapseStatement.execute();
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
    }

    @Override
    public void decollapse(int nodeID)
    {
        try
        {
            decollapseStatement.setInt(1, nodeID);
            decollapseStatement.execute();
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
    }

    @Override
    public void decollapseAll()
    {
        db.execute(decollapseAllStatement);
    }

    @Override
    public List<Integer> getChildren(int nodeID, int layer)
    {
        int nodeLayer = getLayer(nodeID);
        if (layer == nodeLayer)
        {
            // the most common case, for which we have precalculated the virtual
            // children
            List<Integer> result = getChildrenForLayer(nodeID, nodeLayer);
            return result;
        }
        if (nodeID == getRootID(layer) || nodeLayer >= layer)
        {
            // usually only the case for the root of a partial tree
            List<Integer> frontLine = getChildren(nodeID);
            for (int i = 0; i < frontLine.size();)
            {
                int child = frontLine.get(i);
                if (getLayer(child) > layer)
                {
                    frontLine.remove(i);
                    frontLine.addAll(i, getChildren(child));
                } else
                {
                    i++;
                }
            }
            return frontLine;
        }
        // When we have reached a "cornerstone", pretend it's a leaf:
        return Collections.emptyList();
    }

    private List<Integer> getChildren(int nodeID)
    {
        try
        {
            getRealChildrenStatement.setInt(1, nodeID);
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
        return db.queryIntList(getRealChildrenStatement);
    }

    private List<Integer> getChildrenForLayer(int nodeID, int layer)
    {
        try
        {
            getChildrenForLayerStatement.setInt(1, nodeID);
            getChildrenForLayerStatement.setInt(2, nodeID);
            getChildrenForLayerStatement.setInt(3, layer);
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
        List<Integer> result = db.queryIntList(getChildrenForLayerStatement);
        return result;
    }

    @Override
    public String getEdgeLabel(int nodeID)
    {
        try
        {
            getEdgeLabelStatement.setInt(1, nodeID);
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
        return db.queryString(getEdgeLabelStatement, "");
    }
    
    @Override
    public void setEdgeLabel(int nodeID, String edgeLabel)
    {
        try
        {
            setEdgeLabelStatement.setInt(1, nodeID);
            setEdgeLabelStatement.setString(2, edgeLabel);
            setEdgeLabelStatement.execute();
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
    }

    @Override
    protected void collectLeaves(int nodeID, List<Integer> leaves)
    {
        List<Integer> children = getChildren(nodeID);
        if (children.isEmpty())
        {
            leaves.add(nodeID);
        } else
        {
            for (int child : children)
            {
                collectLeaves(child, leaves);
            }
        }
    }

    @Override
    public String getNodeCaption(int nodeID)
    {
        try
        {
            getNodeCaptionStatement.setInt(1, nodeID);
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
        return db.queryString(getNodeCaptionStatement, "");
    }
    
    @Override
    public void setNodeCaption(int nodeID, String nodeCaption)
    {
        try
        {
            setNodeCaptionStatement.setString(1, nodeCaption);
            setNodeCaptionStatement.setInt(2, nodeID);
            setNodeCaptionStatement.execute();
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
    }

    @Override
    public int getNodeStatus(int nodeID)
    {
        try
        {
            getNodeStatusStatement.setInt(1, nodeID);
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
        return db.queryInteger(getNodeStatusStatement, 0);
    }
    
    @Override
    public void setNodeStatus(int nodeID, int newStatus)
    {
        try
        {
            setNodeStatusStatement.setInt(1, nodeID);
            setNodeStatusStatement.setInt(2, newStatus);
            setNodeStatusStatement.execute();
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
    }

    @Override
    public int getParent(int nodeID, int layer)
    {
        if (nodeID == getRootID(layer))
        {
            return -1;
        }
        if (getLayer(nodeID) == layer)
        {
            int virtualParent = getVirtualParent(nodeID);
            if (virtualParent != -1)
            {
                return virtualParent;
            }
        }
        int result = getParent(nodeID);
        while (getLayer(result) > layer)
        {
            result = getParent(result);
        }
        return result;
    }

    @Override
    public int getRootID()
    {
        return db.queryInteger(getRootStatement, -1);
    }

    @Override
    public int getRootID(int layer)
    {
        if (layer == 0)
        {
            return getRootID();
        }
        int result = getReferenceNode();
        while (getLayer(result) >= layer)
        {
            result = getParent(result);
        }
        return result;
    }

    @Override
    public boolean isCollapsed(int nodeID)
    {
        try
        {
            isCollapsedStatement.setInt(1, nodeID);
        } catch (SQLException e)
        {
            throw new KahinaException("SQL error.", e);
        }
        int result = db.queryInteger(isCollapsedStatement, 0);
        return result == 1;
    }

    @Override
    public int getSize()
    {
        return db.queryInteger(getSizeStatement, 0);
    }

    private int getMaxNodeID()
    {
        return db.queryInteger(getMaxNodeIDStatement, 0);
    }

    public static KahinaTree importXML(Document dom, LayerDecider decider,
            DatabaseHandler db, KahinaTree primaryModel)
    {
        KahinaDbTree m = new KahinaDbTree(decider, db);
        if (primaryModel != null)
        {
            m.setPrimaryModel(primaryModel);
        }
        Element treeElement = dom.getDocumentElement();
        NodeList childNodes = treeElement.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++)
        {
            Node n = childNodes.item(i);
            if (n.getNodeName().equals("node"))
            {
                importXMLNode(m, (Element) n, -1);
                // TODO: a little risky, root node could be assigned another ID
                m.setRootID(0);
                break;
            }
        }
        return m;
    }

    private static void importXMLNode(KahinaDbTree m, Element node, int parentID)
    {
        String caption = node.getAttribute("caption");
        String label = node.getAttribute("label");
        int status = 0;
        if (node.getAttribute("status").length() > 0)
        {
            status = Integer.parseInt(node.getAttribute("status"));
        }
        int id;
        if (node.getAttribute("id").length() > 0)
        {
            id = Integer.parseInt(node.getAttribute("id"));
            m.addNode(id, caption, label, status);
        } else
        {
            id = m.addNode(caption, label, status);
        }
        if (parentID != -1)
        {
            m.addChild(parentID, id);
        }
        // go through children recursively
        NodeList childNodes = node.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++)
        {
            Node n = childNodes.item(i);
            if (n.getNodeName().equals("node"))
            {
                importXMLNode(m, (Element) n, id);
            }
        }
    }

    @Override
    public void finalize() throws Throwable
    {
        clear();
        super.finalize();
    }
}
