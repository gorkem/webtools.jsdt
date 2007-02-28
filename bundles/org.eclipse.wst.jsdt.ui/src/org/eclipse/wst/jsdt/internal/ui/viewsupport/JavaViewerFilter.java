/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.eclipse.wst.jsdt.internal.ui.viewsupport;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

/**
 * 
 */
public abstract class JavaViewerFilter extends ViewerFilter {

	public void filteringStart() {
	}
	
	public void filteringEnd() {
	}
	
	/*
 	 * Overrides method from ViewerFilter
 	 */
	public Object[] filter(Viewer viewer, Object parent, Object[] elements) {
		try {
			filteringStart();
			return super.filter(viewer, parent, elements);
		} finally {
			filteringEnd();
		}
	}


}
