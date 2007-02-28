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
package org.eclipse.wst.jsdt.internal.corext.refactoring.reorg;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.text.edits.TextEditGroup;

import org.eclipse.wst.jsdt.core.IField;
import org.eclipse.wst.jsdt.core.IJavaElement;
import org.eclipse.wst.jsdt.core.ISourceRange;
import org.eclipse.wst.jsdt.core.IType;
import org.eclipse.wst.jsdt.core.JavaModelException;
import org.eclipse.wst.jsdt.core.dom.ASTNode;
import org.eclipse.wst.jsdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.wst.jsdt.core.dom.CompilationUnit;
import org.eclipse.wst.jsdt.core.dom.FieldDeclaration;
import org.eclipse.wst.jsdt.core.dom.VariableDeclarationFragment;

import org.eclipse.wst.jsdt.internal.corext.dom.GenericVisitor;
import org.eclipse.wst.jsdt.internal.corext.refactoring.structure.ASTNodeSearchUtil;
import org.eclipse.wst.jsdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.eclipse.wst.jsdt.internal.corext.util.JdtFlags;

public class ASTNodeDeleteUtil {

	private static ASTNode[] getNodesToDelete(IJavaElement element, CompilationUnit cuNode) throws JavaModelException {
		// fields are different because you don't delete the whole declaration but only a fragment of it
		if (element.getElementType() == IJavaElement.FIELD) {
			if (JdtFlags.isEnum((IField) element))
				return new ASTNode[] { ASTNodeSearchUtil.getEnumConstantDeclaration((IField) element, cuNode)};
			else
				return new ASTNode[] { ASTNodeSearchUtil.getFieldDeclarationFragmentNode((IField) element, cuNode)};
		}
		if (element.getElementType() == IJavaElement.TYPE && ((IType) element).isLocal()) {
			IType type= (IType) element;
			if (type.isAnonymous()) {
				if (type.getParent().getElementType() == IJavaElement.FIELD) {
					final ISourceRange range= type.getSourceRange();
					if (range != null) {
						final ASTNode node= ASTNodeSearchUtil.getAstNode(cuNode, range.getOffset(), range.getLength());
						if (node instanceof AnonymousClassDeclaration)
							return new ASTNode[] { node};
					}
				}
				return new ASTNode[] { ASTNodeSearchUtil.getClassInstanceCreationNode(type, cuNode)};
			} else {
				ASTNode[] nodes= ASTNodeSearchUtil.getDeclarationNodes(element, cuNode);
				// we have to delete the TypeDeclarationStatement
				nodes[0]= nodes[0].getParent();
				return nodes;
			}
		}
		return ASTNodeSearchUtil.getDeclarationNodes(element, cuNode);
	}

	private static Set getRemovedNodes(final List removed, final CompilationUnitRewrite rewrite) {
		final Set result= new HashSet();
		rewrite.getRoot().accept(new GenericVisitor(true) {

			protected boolean visitNode(ASTNode node) {
				if (removed.contains(node))
					result.add(node);
				return true;
			}
		});
		return result;
	}

	public static void markAsDeleted(IJavaElement[] javaElements, CompilationUnitRewrite rewrite, TextEditGroup group) throws JavaModelException {
		final List removed= new ArrayList();
		for (int i= 0; i < javaElements.length; i++) {
			markAsDeleted(removed, javaElements[i], rewrite, group);
		}
		propagateFieldDeclarationNodeDeletions(removed, rewrite, group);
	}

	private static void markAsDeleted(List list, IJavaElement element, CompilationUnitRewrite rewrite, TextEditGroup group) throws JavaModelException {
		ASTNode[] declarationNodes= getNodesToDelete(element, rewrite.getRoot());
		for (int i= 0; i < declarationNodes.length; i++) {
			ASTNode node= declarationNodes[i];
			if (node != null) {
				list.add(node);
				rewrite.getASTRewrite().remove(node, group);
				rewrite.getImportRemover().registerRemovedNode(node);
			}
		}
	}

	private static void propagateFieldDeclarationNodeDeletions(final List removed, final CompilationUnitRewrite rewrite, final TextEditGroup group) {
		Set removedNodes= getRemovedNodes(removed, rewrite);
		for (Iterator iter= removedNodes.iterator(); iter.hasNext();) {
			ASTNode node= (ASTNode) iter.next();
			if (node instanceof VariableDeclarationFragment) {
				if (node.getParent() instanceof FieldDeclaration) {
					FieldDeclaration fd= (FieldDeclaration) node.getParent();
					if (!removed.contains(fd) && removedNodes.containsAll(fd.fragments()))
						rewrite.getASTRewrite().remove(fd, group);
					rewrite.getImportRemover().registerRemovedNode(fd);
				}
			}
		}
	}

	private ASTNodeDeleteUtil() {
	}
}
