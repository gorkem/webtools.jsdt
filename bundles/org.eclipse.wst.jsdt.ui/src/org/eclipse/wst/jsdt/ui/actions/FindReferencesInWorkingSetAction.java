/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.wst.jsdt.ui.actions;

import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.PlatformUI;

import org.eclipse.wst.jsdt.core.IJavaElement;
import org.eclipse.wst.jsdt.core.JavaModelException;
import org.eclipse.wst.jsdt.core.search.IJavaSearchScope;

import org.eclipse.wst.jsdt.ui.search.ElementQuerySpecification;
import org.eclipse.wst.jsdt.ui.search.QuerySpecification;

import org.eclipse.wst.jsdt.internal.ui.IJavaHelpContextIds;
import org.eclipse.wst.jsdt.internal.ui.JavaPluginImages;
import org.eclipse.wst.jsdt.internal.ui.javaeditor.JavaEditor;
import org.eclipse.wst.jsdt.internal.ui.search.JavaSearchScopeFactory;
import org.eclipse.wst.jsdt.internal.ui.search.SearchMessages;
import org.eclipse.wst.jsdt.internal.ui.search.SearchUtil;


/**
 * Finds references of the selected element in working sets.
 * The action is applicable to selections representing a Java element.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * 
 * @since 2.0
 */
public class FindReferencesInWorkingSetAction extends FindReferencesAction {

	private IWorkingSet[] fWorkingSets;
	
	/**
	 * Creates a new <code>FindReferencesInWorkingSetAction</code>. The action 
	 * requires that the selection provided by the site's selection provider is of type 
	 * <code>org.eclipse.jface.viewers.IStructuredSelection</code>. The user will 
	 * be prompted to select the working sets.
	 * 
	 * @param site the site providing context information for this action
	 */
	public FindReferencesInWorkingSetAction(IWorkbenchSite site) {
		this(site, null);
	}

	/**
	 * Creates a new <code>FindReferencesInWorkingSetAction</code>. The action 
	 * requires that the selection provided by the site's selection provider is of type 
	 * <code>org.eclipse.jface.viewers.IStructuredSelection</code>.
	 * 
	 * @param site			the site providing context information for this action
	 * @param workingSets	the working sets to be used in the search
	 */
	public FindReferencesInWorkingSetAction(IWorkbenchSite site, IWorkingSet[] workingSets) {
		super(site);
		fWorkingSets= workingSets;
	}

	/**
	 * Note: This constructor is for internal use only. Clients should not call this constructor.
	 * @param editor the Java editor
	 */
	public FindReferencesInWorkingSetAction(JavaEditor editor) {
		this(editor, null);
	}

	/**
	 * Note: This constructor is for internal use only. Clients should not call this constructor.
	 * @param editor the Java editor
	 * @param workingSets the working sets to be used in the search
	 */
	public FindReferencesInWorkingSetAction(JavaEditor editor, IWorkingSet[] workingSets) {
		super(editor);
		fWorkingSets= workingSets;
	}
	
	void init() {
		setText(SearchMessages.Search_FindReferencesInWorkingSetAction_label); 
		setToolTipText(SearchMessages.Search_FindReferencesInWorkingSetAction_tooltip); 
		setImageDescriptor(JavaPluginImages.DESC_OBJS_SEARCH_REF);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this, IJavaHelpContextIds.FIND_REFERENCES_IN_WORKING_SET_ACTION);
	}

	QuerySpecification createQuery(IJavaElement element) throws JavaModelException {
		JavaSearchScopeFactory factory= JavaSearchScopeFactory.getInstance();
		
		IWorkingSet[] workingSets= fWorkingSets;
		if (fWorkingSets == null) {
			workingSets= factory.queryWorkingSets();
			if (workingSets == null)
				return null;
		}
		SearchUtil.updateLRUWorkingSets(workingSets);
		IJavaSearchScope scope= factory.createJavaSearchScope(workingSets, true);
		String description= factory.getWorkingSetScopeDescription(workingSets, true);
		return new ElementQuerySpecification(element, getLimitTo(), scope, description);
	}
}
