package org.kahina.io.database;

import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashSet;
import java.util.Set;

public class KahinaDatabaseHandler
{
    // database connection
    private Connection connection;
    private File db;
    
    public KahinaDatabaseHandler(File db)
    {
        this.db = db;
        try
        {
            startDatabase();
        }
        catch (ClassNotFoundException e)
        {
            System.err.println("Problem loading Apache Derby: " + e.getMessage());
        }
        catch (SQLException e)
        {
            System.err.println("A database error occured: " + e.getMessage());
        }
        catch (IOException e)
        {
            System.err.println("Database file problem: " + e.getMessage());
        }
    }
    
    public void execute(String sqlString)
    {
        try
        {
            Statement statement = connection.createStatement();
            statement.execute(sqlString);
        }
        catch (SQLException e)
        {
            System.err.println("SQL error: " + e.getMessage());
        }
    }
    
    public void update(String updateString)
    {
        try
        {
            Statement statement = connection.createStatement();
            statement.executeUpdate(updateString);
        }
        catch (SQLException e)
        {
            System.err.println("SQL error: " + e.getMessage());
        }
    }
    
    public int queryInteger(String queryString)
    {
        try
        {
            Statement statement = connection.createStatement();
            ResultSet res = statement.executeQuery(queryString);
            return res.getInt(0);
        }
        catch (SQLException e)
        {
            System.err.println("SQL error: " + e.getMessage());
        }
        return -1;
    }
    
    public Set<Integer> queryIntSet(String queryString)
    {
        HashSet<Integer> resultSet = new HashSet<Integer>();
        try
        {
            Statement statement = connection.createStatement();
            ResultSet res = statement.executeQuery(queryString);
            resultSet.add(res.getInt(0));
            while (res.next())
            {
                resultSet.add(res.getInt(0));
            }
        }
        catch (SQLException e)
        {
            System.err.println("SQL error: " + e.getMessage());
        }
        return resultSet;
    }
    
    private void startDatabase() throws ClassNotFoundException, SQLException, IOException
    {
        Class.forName("org.apache.derby.jdbc.EmbeddedDriver");
        db = File.createTempFile("kahinadb", null);
        deleteRecursively(db);
        connection = DriverManager.getConnection("jdbc:derby:" + db.getPath() + ";create=true");
        // db.deleteOnExit(); // should work but doesn't
        Statement statement = connection.createStatement();
        try
        {
            statement.executeUpdate("DROP TABLE data");
        }
        catch (SQLException e)
        {
            // ignore - gotta hate Derby for not supporting DROP TABLE IF EXISTS
        }
    
        statement.executeUpdate("CREATE TABLE data (id BIGINT NOT NULL , value VARCHAR(32) NOT NULL, PRIMARY KEY (id))");
        statement.close();
    }
    
    private static void deleteRecursively(File directory) 
    {
        if (directory.isDirectory()) 
        {
            for (File file : directory.listFiles()) 
            {
                deleteRecursively(file);
            }
        }     
        directory.delete();
    }
}
