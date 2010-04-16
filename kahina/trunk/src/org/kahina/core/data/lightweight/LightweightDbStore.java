package org.kahina.core.data.lightweight;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kahina.core.KahinaException;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.data.DbDataStore;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.io.database.DatabaseHandler;

public class LightweightDbStore extends DbDataStore
{

	private static final String CLIENT_ID = LightweightDbStore.class.getName();

	private static final String TABLE_NAME_PREFIX = LightweightDbStore.class
			.getSimpleName();

	private static final String FIELD_VALUES_INT_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_field_values_int";

	private static final String FIELD_VALUES_LONG_VARCHAR_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_field_values_long_varchar";

	private static final String COLLECTION_ELEMENTS_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_collection_elements";

	private static final String MAP_ENTRIES_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_map_entries";

	private static final String REFERENCE_VALUES_LONG_VARCHAR_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_string_values";

	private Constructor<?> constructor;

	private Field[] fields;

	private LVT[] lvts;

	private PreparedStatement selectFieldValueIntStatement;

	private PreparedStatement updateFieldValueIntStatement;

	private PreparedStatement insertFieldValueIntStatement;

	private PreparedStatement selectFieldValueLongVarcharStatement;

	private PreparedStatement updateFieldValueLongVarcharStatement;

	private PreparedStatement insertFieldValueLongVarcharStatement;

	private PreparedStatement selectCollectionStatement;

	private PreparedStatement selectReferenceValueLongVarcharStatement;

	private PreparedStatement insertReferenceValueLongVarcharStatement;

	private PreparedStatement deleteCollectionStatement;

	private PreparedStatement deleteLongVarcharStatement;

	private PreparedStatement getNewCollectionReferenceStatement;

	private PreparedStatement insertCollectionElementStatement;

	private Set<Integer> currentlyBeingStored = new HashSet<Integer>();

	private Map<Integer, KahinaObject> currentlyBeingRetrieved = new HashMap<Integer, KahinaObject>();

	public LightweightDbStore(Class<? extends KahinaObject> datatype, DbDataManager manager, DatabaseHandler db)
	{
		super(manager, db);
		createTablesIfNecessary();
		prepareStatements();
		try
		{
			constructor = datatype.getConstructor();
		} catch (NoSuchMethodException e)
		{
			throw new KahinaException("Lightweight datatype " + datatype
					+ " fails to provide zero-arg constructor.", e);
		}
		examineType(datatype);
	}

	private void examineType(Class<? extends KahinaObject> datatype)
	{
		List<Field> fields = new ArrayList<Field>();
		List<LVT> lvts = new ArrayList<LVT>();
		Field[] allFields = datatype.getFields();
		Arrays.sort(allFields, new Comparator<Field>()
		{

			@Override
			public int compare(Field o1, Field o2)
			{
				return o1.getName().compareTo(o2.getName());
			}

		});
		for (Field field : datatype.getFields())
		{
			if (Modifier.isPublic(field.getModifiers()))
			{
				LVT lvt = LVT.createLVT((Class<?>) field.getGenericType(),
						this, manager);
				if (lvt != null)
				{
					fields.add(field);
					lvts.add(lvt);
				}
			}
		}
		this.fields = fields.toArray(new Field[fields.size()]);
		this.lvts = lvts.toArray(new LVT[lvts.size()]);
	}

	private void createTablesIfNecessary()
	{
		if (db.isRegistered(CLIENT_ID))
		{
			return;
		}
		db.createTable(FIELD_VALUES_INT_TABLE_NAME, "object_id INT",
				"field_id INT", "value INT",
				"PRIMARY KEY (object_id, field_id)");
		db.createTable(FIELD_VALUES_LONG_VARCHAR_TABLE_NAME, "object_id INT",
				"field_id INT", "value LONG VARCHAR",
				"PRIMARY KEY (object_id, field_id)");
		db.createTable(COLLECTION_ELEMENTS_TABLE_NAME, "collection_id INT",
				"element INT");
		db.createIndex(COLLECTION_ELEMENTS_TABLE_NAME, "_collection_id",
				"collection_id");
		db.createTable(MAP_ENTRIES_TABLE_NAME, "map_id INT", "\"key\" INT",
				"value INT", "PRIMARY KEY (map_id,  \"key\")");
		db
				.createTable(
						REFERENCE_VALUES_LONG_VARCHAR_TABLE_NAME,
						"value_id INT GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1)",
						"value LONG VARCHAR", "PRIMARY KEY (value_id)");
		db.register(CLIENT_ID);
	}

	private void prepareStatements()
	{
		selectFieldValueIntStatement = db.prepareStatement("SELECT value FROM "
				+ FIELD_VALUES_INT_TABLE_NAME
				+ " WHERE object_id = ? AND field_id = ?");
		updateFieldValueIntStatement = db.prepareStatement("UPDATE "
				+ FIELD_VALUES_INT_TABLE_NAME
				+ " SET value = ? WHERE object_id = ? AND field_id = ?");
		insertFieldValueIntStatement = db.prepareStatement("INSERT INTO "
				+ FIELD_VALUES_INT_TABLE_NAME
				+ " (object_id, field_id, value) VALUES (?, ?, ?)");
		selectFieldValueLongVarcharStatement = db
				.prepareStatement("SELECT value FROM "
						+ FIELD_VALUES_LONG_VARCHAR_TABLE_NAME
						+ " WHERE object_id = ? AND field_id = ?");
		updateFieldValueLongVarcharStatement = db.prepareStatement("UPDATE "
				+ FIELD_VALUES_LONG_VARCHAR_TABLE_NAME
				+ " SET value = ? WHERE object_id = ? AND field_id = ?");
		insertFieldValueLongVarcharStatement = db
				.prepareStatement("INSERT INTO "
						+ FIELD_VALUES_LONG_VARCHAR_TABLE_NAME
						+ " (object_id, field_id, value) VALUES (?, ?, ?)");
		selectCollectionStatement = db.prepareStatement("SELECT element FROM "
				+ COLLECTION_ELEMENTS_TABLE_NAME + " WHERE collection_id = ?");
		selectReferenceValueLongVarcharStatement = db
				.prepareStatement("SELECT value FROM "
						+ REFERENCE_VALUES_LONG_VARCHAR_TABLE_NAME
						+ " WHERE value_id = ?");
		insertReferenceValueLongVarcharStatement = db.prepareStatement(
				"INSERT INTO " + REFERENCE_VALUES_LONG_VARCHAR_TABLE_NAME
						+ " (value) VALUES (?)", new int[] { 1 });
		deleteCollectionStatement = db.prepareStatement("DELETE FROM "
				+ COLLECTION_ELEMENTS_TABLE_NAME + " WHERE collection_id = ?");
		deleteLongVarcharStatement = db.prepareStatement("DELETE FROM "
				+ REFERENCE_VALUES_LONG_VARCHAR_TABLE_NAME
				+ " WHERE value_id = ?");
		getNewCollectionReferenceStatement = db // TODO not fully satisfying
				.prepareStatement("SELECT MAX(collection_id) + 1 FROM "
						+ COLLECTION_ELEMENTS_TABLE_NAME);
		insertCollectionElementStatement = db.prepareStatement("INSERT INTO "
				+ COLLECTION_ELEMENTS_TABLE_NAME
				+ " (collection_id, element) VALUES (?, ?)");
	}

	@Override
	public KahinaObject retrieve(int id)
	{
		if (currentlyBeingRetrieved.containsKey(id))
		{
			return currentlyBeingRetrieved.get(id);
		}
		try
		{
			KahinaObject result = (KahinaObject) constructor.newInstance();
			currentlyBeingRetrieved.put(id, result);
			result.setID(id);
			for (int fieldID = 0; fieldID < fields.length; fieldID++)
			{
				lvts[fieldID].retrieveFieldValue(id, fieldID, fields[fieldID],
						result);
			}
			return result;
		} catch (IllegalArgumentException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (InstantiationException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (IllegalAccessException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (InvocationTargetException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} finally
		{
			currentlyBeingRetrieved.remove(id);
		}
	}

	@Override
	public void store(KahinaObject object, int id)
	{
		if (!currentlyBeingStored.add(id))
		{
			return;
		}
		try
		{
			for (int fieldID = 0; fieldID < fields.length; fieldID++)
			{
				lvts[fieldID].storeFieldValue(id, fieldID, fields[fieldID],
						object);
			}
		} catch (IllegalAccessException e)
		{
			throw new KahinaException("Could not store object.", e);
		} finally
		{
			currentlyBeingStored.remove(id);
		}
	}

	Integer retrieveInt(int id, int fieldID)
	{
		try
		{
			selectFieldValueIntStatement.setInt(1, id);
			selectFieldValueIntStatement.setInt(2, fieldID);
			return db.queryInteger(selectFieldValueIntStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	void storeInt(int objectID, int fieldID, Integer value)
	{
		try
		{
			if (value == null)
			{
				updateFieldValueIntStatement.setNull(1, Types.INTEGER);
			} else
			{
				updateFieldValueIntStatement.setInt(1, value);
			}

			updateFieldValueIntStatement.setInt(2, objectID);
			updateFieldValueIntStatement.setInt(3, fieldID);

			if (updateFieldValueIntStatement.executeUpdate() == 0)
			{
				insertFieldValueIntStatement.setInt(1, objectID);
				insertFieldValueIntStatement.setInt(2, fieldID);

				if (value == null)
				{
					insertFieldValueIntStatement.setNull(3, Types.INTEGER);
				} else
				{
					insertFieldValueIntStatement.setInt(3, value);
				}
				insertFieldValueIntStatement.execute();
			}
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	String retrieveLongVarchar(int id, int fieldID)
	{
		try
		{
			selectFieldValueLongVarcharStatement.setInt(1, id);
			selectFieldValueLongVarcharStatement.setInt(2, fieldID);
			return db.queryString(selectFieldValueLongVarcharStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	void storeLongVarchar(int objectID, int fieldID, String value)
	{
		try
		{
			updateFieldValueLongVarcharStatement.setString(1, value);
			updateFieldValueLongVarcharStatement.setInt(2, objectID);
			updateFieldValueLongVarcharStatement.setInt(3, fieldID);

			if (updateFieldValueLongVarcharStatement.executeUpdate() == 0)
			{
				insertFieldValueLongVarcharStatement.setInt(1, objectID);
				insertFieldValueLongVarcharStatement.setInt(2, fieldID);
				insertFieldValueLongVarcharStatement.setString(3, value);
				insertFieldValueLongVarcharStatement.execute();
			}
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	List<Integer> retrieveCollection(int collectionID)
	{
		try
		{
			selectCollectionStatement.setInt(1, collectionID);
			return db.queryIntList(selectCollectionStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	String retrieveReferenceValueLongVarchar(Integer reference)
	{
		try
		{
			selectReferenceValueLongVarcharStatement.setInt(1, reference);
			return db.queryString(selectReferenceValueLongVarcharStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	int storeAsReferenceValueLongVarchar(String element)
	{
		try
		{
			insertReferenceValueLongVarcharStatement.setString(1, element);
			insertReferenceValueLongVarcharStatement.execute();
			return db.getGeneratedKey(insertReferenceValueLongVarcharStatement,
					1);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	void deleteCollection(Integer reference)
	{
		try
		{
			deleteCollectionStatement.setInt(1, reference);
			deleteCollectionStatement.execute();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	void deleteLongVarchar(Integer reference)
	{
		try
		{
			deleteLongVarcharStatement.setInt(1, reference);
			deleteLongVarcharStatement.execute();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}

	}

	@Override
	public void persist()
	{
		// do nothing
		// TODO garbage-collect objects to which there are no more references
		// and which have not been stored from outside, as soon as we have a
		// way to know that
	}

	int getNewCollectionReference()
	{
		return db.queryInteger(getNewCollectionReferenceStatement, 1);
	}

	void storeCollectionElement(int collectionID, int elementReference)
	{
		try
		{
			insertCollectionElementStatement.setInt(1, collectionID);
			insertCollectionElementStatement.setInt(2, elementReference);
			insertCollectionElementStatement.execute();
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}

	}

}
