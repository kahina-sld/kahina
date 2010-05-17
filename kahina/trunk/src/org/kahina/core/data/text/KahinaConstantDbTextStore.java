package org.kahina.core.data.text;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;

import org.kahina.core.KahinaException;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.data.DbDataStore;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.io.database.DatabaseHandler;

/**
 * A data store for {@link KahinaText}s which stores each object only once, i.e.
 * does not support changes to the objects it stores.
 * @author ke
 *
 */
public class KahinaConstantDbTextStore extends DbDataStore
{
	private static final String CLIENT_ID = KahinaConstantDbTextStore.class.getName();

	private static final String TABLE_NAME_PREFIX = KahinaConstantDbTextStore.class.getSimpleName() + "_";

	private static final String LINE_TABLE_NAME = TABLE_NAME_PREFIX + "lines";

	private PreparedStatement insertLineStatement;

	private PreparedStatement getLinesStatement;

	private PreparedStatement isStoredStatement;

	public KahinaConstantDbTextStore(DbDataManager manager, DatabaseHandler db)
	{
		super(manager, db);
		createTablesIfNecessary();
		prepareStatements();
	}

	private void createTablesIfNecessary()
	{
		if (!db.isRegistered(CLIENT_ID))
		{
			db.createTable(LINE_TABLE_NAME, "text_id INT", "line LONG VARCHAR");
			db.createIndex(LINE_TABLE_NAME, "_text_id", "text_id");
		}
	}

	private void prepareStatements()
	{
		insertLineStatement = db.prepareStatement("INSERT INTO " + LINE_TABLE_NAME + " (text_id, line) VALUES (?, ?)");
		getLinesStatement = db.prepareStatement("SELECT line FROM " + LINE_TABLE_NAME + " WHERE text_id = ?");
		isStoredStatement = db.prepareStatement("SELECT COUNT(*) FROM " + LINE_TABLE_NAME + " WHERE text_id= ?");
		// TODO more efficient way to determine if there is a row with text_id = ??
	}

	@Override
	public KahinaObject retrieve(int id)
	{
		KahinaText result = new KahinaText(getLines(id));
		result.setID(id);
		return result;
	}

	private List<String> getLines(int id)
	{
		try
		{
			getLinesStatement.setInt(1, id);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
		return db.queryStringList(getLinesStatement);
	}

	@Override
	public void store(KahinaObject object, int id)
	{
		if (!(object instanceof KahinaText))
		{
			throw new KahinaException(this + " cannot store " + object + ".");
		}
		if (!isStored(id))
		{
			try
			{
				insertLineStatement.setInt(1, id);
				for (String line : ((KahinaText) object).lines)
				{
					insertLineStatement.setString(2, line);
					insertLineStatement.execute();
				}
			} catch (SQLException e)
			{
				throw new KahinaException("SQL error.", e);
			}
		}
	}

	private boolean isStored(int id)
	{
		try
		{
			isStoredStatement.setInt(1, id);
			return db.queryInteger(isStoredStatement) > 0;
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}
}
