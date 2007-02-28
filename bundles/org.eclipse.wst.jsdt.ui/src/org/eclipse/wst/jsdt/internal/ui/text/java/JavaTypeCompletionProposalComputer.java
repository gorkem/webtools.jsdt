/*******************************************************************************
 * Copyright (c) 2005, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.wst.jsdt.internal.ui.text.java;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

import org.eclipse.wst.jsdt.core.CompletionProposal;
import org.eclipse.wst.jsdt.core.IJavaElement;
import org.eclipse.wst.jsdt.core.IJavaProject;
import org.eclipse.wst.jsdt.core.IType;
import org.eclipse.wst.jsdt.core.JavaCore;
import org.eclipse.wst.jsdt.core.JavaModelException;
import org.eclipse.wst.jsdt.core.Signature;

import org.eclipse.wst.jsdt.ui.text.java.CompletionProposalCollector;
import org.eclipse.wst.jsdt.ui.text.java.ContentAssistInvocationContext;
import org.eclipse.wst.jsdt.ui.text.java.IJavaCompletionProposal;
import org.eclipse.wst.jsdt.ui.text.java.JavaContentAssistInvocationContext;

import org.eclipse.wst.jsdt.internal.ui.JavaPlugin;
import org.eclipse.wst.jsdt.internal.ui.text.JavaHeuristicScanner;
import org.eclipse.wst.jsdt.internal.ui.text.Symbols;

/**
 * 
 * @since 3.2
 */
public class JavaTypeCompletionProposalComputer extends JavaCompletionProposalComputer {
	/*
	 * @see org.eclipse.wst.jsdt.internal.ui.text.java.JavaCompletionProposalComputer#createCollector(org.eclipse.wst.jsdt.ui.text.java.JavaContentAssistInvocationContext)
	 */
	protected CompletionProposalCollector createCollector(JavaContentAssistInvocationContext context) {
		CompletionProposalCollector collector= super.createCollector(context);
		collector.setIgnored(CompletionProposal.ANNOTATION_ATTRIBUTE_REF, true);
		collector.setIgnored(CompletionProposal.ANONYMOUS_CLASS_DECLARATION, true);
		collector.setIgnored(CompletionProposal.FIELD_REF, true);
		collector.setIgnored(CompletionProposal.KEYWORD, true);
		collector.setIgnored(CompletionProposal.LABEL_REF, true);
		collector.setIgnored(CompletionProposal.LOCAL_VARIABLE_REF, true);
		collector.setIgnored(CompletionProposal.METHOD_DECLARATION, true);
		collector.setIgnored(CompletionProposal.METHOD_NAME_REFERENCE, true);
		collector.setIgnored(CompletionProposal.METHOD_REF, true);
		collector.setIgnored(CompletionProposal.PACKAGE_REF, true);
		collector.setIgnored(CompletionProposal.POTENTIAL_METHOD_DECLARATION, true);
		collector.setIgnored(CompletionProposal.VARIABLE_DECLARATION, true);
		
		collector.setIgnored(CompletionProposal.JAVADOC_BLOCK_TAG, true);
		collector.setIgnored(CompletionProposal.JAVADOC_FIELD_REF, true);
		collector.setIgnored(CompletionProposal.JAVADOC_INLINE_TAG, true);
		collector.setIgnored(CompletionProposal.JAVADOC_METHOD_REF, true);
		collector.setIgnored(CompletionProposal.JAVADOC_PARAM_REF, true);
		collector.setIgnored(CompletionProposal.JAVADOC_TYPE_REF, true);
		collector.setIgnored(CompletionProposal.JAVADOC_VALUE_REF, true);
		
		collector.setIgnored(CompletionProposal.TYPE_REF, false);
		return collector;
	}
	
	/*
	 * @see org.eclipse.wst.jsdt.internal.ui.text.java.JavaCompletionProposalComputer#computeCompletionProposals(org.eclipse.jface.text.contentassist.TextContentAssistInvocationContext, org.eclipse.core.runtime.IProgressMonitor)
	 */
	public List computeCompletionProposals(ContentAssistInvocationContext context, IProgressMonitor monitor) {
		List types= super.computeCompletionProposals(context, monitor);
		if (context instanceof JavaContentAssistInvocationContext) {
			JavaContentAssistInvocationContext javaContext= (JavaContentAssistInvocationContext) context;
			try {
				if (types.size() > 0 && context.computeIdentifierPrefix().length() == 0) {
					IType expectedType= javaContext.getExpectedType();
					if (expectedType != null) {
						// empty prefix completion - insert LRU types if known, but prune if they already occur in the core list
						
						// compute minmimum relevance and already proposed list
						int relevance= Integer.MAX_VALUE;
						Set proposed= new HashSet();
						for (Iterator it= types.iterator(); it.hasNext();) {
							AbstractJavaCompletionProposal p= (AbstractJavaCompletionProposal) it.next();
							IJavaElement element= p.getJavaElement();
							if (element instanceof IType)
								proposed.add(((IType) element).getFullyQualifiedName());
							relevance= Math.min(relevance, p.getRelevance());
						}

						// insert history types
						List history= JavaPlugin.getDefault().getContentAssistHistory().getHistory(expectedType.getFullyQualifiedName()).getTypes();
						relevance-= history.size() + 1;
						for (Iterator it= history.iterator(); it.hasNext();) {
							String type= (String) it.next();
							if (proposed.contains(type))
								continue;
							
							IJavaCompletionProposal proposal= createTypeProposal(relevance, type, javaContext);
							
							if (proposal != null)
								types.add(proposal);
							relevance++;
						}
					}
				}
			} catch (BadLocationException x) {
				// log & ignore
				JavaPlugin.log(x);
			} catch (JavaModelException x) {
				// log & ignore
				JavaPlugin.log(x);
			}
		}
		return types;
	}

	private IJavaCompletionProposal createTypeProposal(int relevance, String fullyQualifiedType, JavaContentAssistInvocationContext context) throws JavaModelException {
		IType type= context.getCompilationUnit().getJavaProject().findType(fullyQualifiedType);
		if (type == null)
			return null;
		
		CompletionProposal proposal= CompletionProposal.create(CompletionProposal.TYPE_REF, context.getInvocationOffset());
		proposal.setCompletion(fullyQualifiedType.toCharArray());
		proposal.setDeclarationSignature(type.getPackageFragment().getElementName().toCharArray());
		proposal.setFlags(type.getFlags());
		proposal.setRelevance(relevance);
		proposal.setReplaceRange(context.getInvocationOffset(), context.getInvocationOffset());
		proposal.setSignature(Signature.createTypeSignature(fullyQualifiedType, true).toCharArray());

		if (shouldProposeGenerics(context.getProject()))
			return new LazzyGenericTypeProposal(proposal, context);
		else
			return new LazyJavaTypeCompletionProposal(proposal, context);
	}
	
	/**
	 * Returns <code>true</code> if generic proposals should be allowed,
	 * <code>false</code> if not. Note that even though code (in a library)
	 * may be referenced that uses generics, it is still possible that the
	 * current source does not allow generics.
	 *
	 * @return <code>true</code> if the generic proposals should be allowed,
	 *         <code>false</code> if not
	 */
	private final boolean shouldProposeGenerics(IJavaProject project) {
		String sourceVersion;
		if (project != null)
			sourceVersion= project.getOption(JavaCore.COMPILER_SOURCE, true);
		else
			sourceVersion= JavaCore.getOption(JavaCore.COMPILER_SOURCE);

		return sourceVersion != null && JavaCore.VERSION_1_5.compareTo(sourceVersion) <= 0;
	}
	
	protected int guessContextInformationPosition(ContentAssistInvocationContext context) {
		final int contextPosition= context.getInvocationOffset();
		
		IDocument document= context.getDocument();
		JavaHeuristicScanner scanner= new JavaHeuristicScanner(document);
		int bound= Math.max(-1, contextPosition - 200);
		
		// try the innermost scope of angle brackets that looks like a generic type argument list
		try {
			int pos= contextPosition - 1;
			do {
				int angle= scanner.findOpeningPeer(pos, bound, '<', '>');
				if (angle == JavaHeuristicScanner.NOT_FOUND)
					break;
				int token= scanner.previousToken(angle - 1, bound);
				// next token must be a method name that is a generic type
				if (token == Symbols.TokenIDENT) {
					int off= scanner.getPosition() + 1;
					int end= angle;
					String ident= document.get(off, end - off).trim();
					if (JavaHeuristicScanner.isGenericStarter(ident))
						return angle + 1;
				}
				pos= angle - 1;
			} while (true);
		} catch (BadLocationException x) {
		}
		
		return super.guessContextInformationPosition(context);
	}

}
