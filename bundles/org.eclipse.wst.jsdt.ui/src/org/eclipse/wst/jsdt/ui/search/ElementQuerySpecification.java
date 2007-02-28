/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.wst.jsdt.ui.search;

import org.eclipse.wst.jsdt.core.IJavaElement;
import org.eclipse.wst.jsdt.core.search.IJavaSearchScope;

/**
 * <p>
 * Describes a search query by giving the {@link IJavaElement} to search
 * for.
 * </p>
 * <p>
 * This class is not intended to be instantiated or subclassed by clients.
 * </p>
 * 
 * @see org.eclipse.wst.jsdt.ui.search.QuerySpecification
 *
 * @since 3.0
 */
public class ElementQuerySpecification extends QuerySpecification {
	private IJavaElement fElement;

	/**
	 * A constructor.
	 * @param javaElement The java element the query should search for.
	 * @param limitTo		  The kind of occurrence the query should search for.
	 * @param scope		  The scope to search in.
	 * @param scopeDescription A human readable description of the search scope.
	 */
	public ElementQuerySpecification(IJavaElement javaElement, int limitTo, IJavaSearchScope scope, String scopeDescription) {
		super(limitTo, scope, scopeDescription);
		fElement= javaElement;
	}
	
	/**
	 * Returns the element to search for.
	 * @return The element to search for.
	 */
	public IJavaElement getElement() {
		return fElement;
	}
}
