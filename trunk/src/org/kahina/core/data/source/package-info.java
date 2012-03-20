/**
* Provides data types for representing source code files and locations.
* <p>
* A {@link org.kahina.core.data.source.KahinaSourceCodeLocation} 
* is a wrapper for a line in a source code file, define by an absolute path
* to a source code file and a line number.
* <p>
* A source code file is modeled by a {@link org.kahina.core.data.source.KahinaSourceFileModel},
* a specialization of {@link org.kahina.core.data.text.KahinaTextFileModel}
* which serves as the ancestor class for application-specific source file models.
* For instance, a specialized model of a Prolog source file would include the locations of clauses.
* <p>
* A {@link org.kahina.core.data.source.KahinaSourceFileRegistry} is used to cache
* <code>KahinaSourceFileModels</code> under absolute path names,
* which can lead to a speedup especially if a lot of preprocessing is needed.
* <p>
* @since 1.0 
*/

package org.kahina.core.data.source;