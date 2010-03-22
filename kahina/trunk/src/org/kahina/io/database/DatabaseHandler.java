package org.kahina.io.database;

import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kahina.core.KahinaException;

public class DatabaseHandler
{

	// TODO should support persisting a state by closing the connection and then
	// copying the temporary directory to a user-specified location

	private static final String TABLE_NAME_PREFIX = DatabaseHandler.class
			.getSimpleName()
			+ "_";

	private static final String CLIENTS_TABLE_NAME = TABLE_NAME_PREFIX
			+ "clients";

	private PreparedStatement isRegisteredStatement;

	private PreparedStatement registerStatement;

	private File dbDirectory;

	private Connection connection;

	/**
	 * Creates a database handler with an empty database.
	 */
	public DatabaseHandler() throws KahinaException
	{
		createTemporaryDatabaseDirectory();
		startDatabase();
		createTables();
		prepareStatements();
	}

	/**
	 * Creates a database handler with an existing database (loaded from a
	 * file).
	 * 
	 * @param file
	 */
	public DatabaseHandler(File file)
	{
		dbDirectory = file;
		startDatabase();
		prepareStatements();
	}

	public void execute(String sqlString)
	{
		try
		{
			Statement statement = connection.createStatement();
			statement.execute(sqlString);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public Integer queryInteger(PreparedStatement statement)
	{
		return queryInteger(statement, null);
	}

	public Integer queryInteger(PreparedStatement statement,
			Integer defaultValue)
	{
		try
		{
			ResultSet resultSet = statement.executeQuery();
			if (!resultSet.next())
			{
				return defaultValue;
			}
			int result = resultSet.getInt(1);
			if (resultSet.wasNull())
			{
				return defaultValue;
			}
			return result;
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	/**
	 * Returns the result of a statement as a list of integers. <tt>NULL</tt>
	 * values are represented as {@code null} values.
	 * 
	 * @param statement
	 * @return
	 */
	public List<Integer> queryIntList(PreparedStatement statement)
	{
		List<Integer> result = new ArrayList<Integer>();
		try
		{
			ResultSet resultSet = statement.executeQuery();
			while (resultSet.next())
			{
				int element = resultSet.getInt(1);

				if (resultSet.wasNull())
				{
					result.add(null);
				} else
				{
					result.add(element);
				}
			}
			return result;
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	/**
	 * Returns the result of a statement as a set of integers. <tt>NULL</tt>
	 * values are omitted.
	 * 
	 * @param statement
	 * @return
	 */
	public Set<Integer> queryIntSet(PreparedStatement statement)
	{
		Set<Integer> result = new HashSet<Integer>();
		try
		{
			ResultSet resultSet = statement.executeQuery();
			while (resultSet.next())
			{
				int element = resultSet.getInt(1);

				if (!resultSet.wasNull())
				{
					result.add(element);
				}
			}
			return result;
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	public String queryString(PreparedStatement statement)
	{
		return queryString(statement, null);
	}

	public String queryString(PreparedStatement statement, String defaultValue)
	{
		try
		{
			ResultSet resultSet = statement.executeQuery();
			if (!resultSet.next())
			{
				return defaultValue;
			}
			String result = resultSet.getString(1);
			if (resultSet.wasNull())
			{
				return defaultValue;
			}
			return result;
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	private void createTemporaryDatabaseDirectory()
	{
		try
		{
			dbDirectory = File.createTempFile("kahinadb", null);
		} catch (IOException e)
		{
			throw new KahinaException("Could not create database directory.", e);
		}
		deleteRecursively(dbDirectory);
	}

	private void startDatabase()
	{
		try
		{
			Class.forName("org.apache.derby.jdbc.EmbeddedDriver");
		} catch (ClassNotFoundException e)
		{
			throw new KahinaException("Derby JDBC driver not available.", e);
		}

		try
		{
			connection = DriverManager.getConnection("jdbc:derby:"
					+ dbDirectory.getPath() + ";create=true");
		} catch (SQLException e)
		{
			throw new KahinaException(
					"Failed to establish a database connection.", e);
		}
	}

	private void createTables()
	{
		execute("CREATE TABLE " + CLIENTS_TABLE_NAME
				+ " (client_id VARCHAR(255) PRIMARY KEY)");
	}

	private void prepareStatements()
	{
		isRegisteredStatement = prepareStatement("SELECT COUNT(*) FROM "
				+ CLIENTS_TABLE_NAME + " WHERE client_id = ?");
		registerStatement = prepareStatement("INSERT INTO "
				+ CLIENTS_TABLE_NAME + " (client_id) VALUES (?)");
	}

	/**
	 * Closes the underlying database connection. This method should always be
	 * invoked as soon as this handler is no longer needed.
	 */
	public void close()
	{
		try
		{
			connection.close();
		} catch (SQLException e)
		{
			throw new KahinaException("Failed to close database handler.", e);
		}
		deleteRecursively(dbDirectory);
	}

	/**
	 * A client using this {@link DatabaseHandler} should register here with a
	 * unique ID (e.g. a fully qualified class name of a class all of whose
	 * instances will use the same set of database tables) after the required
	 * tables have been created.
	 * 
	 * @param clientID
	 */
	public void register(String clientID)
	{
		try
		{
			registerStatement.setString(1, clientID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		execute(registerStatement);
	}

	/**
	 * Clients using this {@link DatabaseHandler} can use this method to quickly
	 * determine if they already created the tables they need.
	 * 
	 * @param clientID
	 * @return
	 */
	public boolean isRegistered(String clientID)
	{
		try
		{
			isRegisteredStatement.setString(1, clientID);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return queryInteger(isRegisteredStatement) > 0;
	}

	public PreparedStatement prepareStatement(String sql)
	{
		try
		{
			return connection.prepareStatement(sql);
		} catch (SQLException e)
		{
			throw new KahinaException("Failed to prepare statement.", e);
		}
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

	public void execute(PreparedStatement statement)
	{
		try
		{
			statement.execute();
		} catch (SQLException e)
		{
			throw new KahinaException("Failed to execute statement.", e);
		}
	}
}
