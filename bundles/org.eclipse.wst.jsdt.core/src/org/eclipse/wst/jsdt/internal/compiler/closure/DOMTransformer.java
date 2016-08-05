/*******************************************************************************
 * Copyright (c) 2016 Red Hat, Inc. 
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * 	Contributors:
 * 		 Red Hat Inc. - initial API and implementation and/or initial documentation
 *******************************************************************************/

package org.eclipse.wst.jsdt.internal.compiler.closure;

import java.util.Iterator;

import org.eclipse.core.runtime.Assert;
import org.eclipse.wst.jsdt.core.dom.AST;
import org.eclipse.wst.jsdt.core.dom.ASTNode;
import org.eclipse.wst.jsdt.core.dom.ArrayAccess;
import org.eclipse.wst.jsdt.core.dom.ArrayInitializer;
import org.eclipse.wst.jsdt.core.dom.ArrayName;
import org.eclipse.wst.jsdt.core.dom.ArrowFunctionExpression;
import org.eclipse.wst.jsdt.core.dom.Block;
import org.eclipse.wst.jsdt.core.dom.BooleanLiteral;
import org.eclipse.wst.jsdt.core.dom.BreakStatement;
import org.eclipse.wst.jsdt.core.dom.CatchClause;
import org.eclipse.wst.jsdt.core.dom.ClassInstanceCreation;
import org.eclipse.wst.jsdt.core.dom.ConditionalExpression;
import org.eclipse.wst.jsdt.core.dom.ContinueStatement;
import org.eclipse.wst.jsdt.core.dom.DoStatement;
import org.eclipse.wst.jsdt.core.dom.ExportDeclaration;
import org.eclipse.wst.jsdt.core.dom.Expression;
import org.eclipse.wst.jsdt.core.dom.ExpressionStatement;
import org.eclipse.wst.jsdt.core.dom.FieldAccess;
import org.eclipse.wst.jsdt.core.dom.FieldDeclaration;
import org.eclipse.wst.jsdt.core.dom.ForInStatement;
import org.eclipse.wst.jsdt.core.dom.ForOfStatement;
import org.eclipse.wst.jsdt.core.dom.ForStatement;
import org.eclipse.wst.jsdt.core.dom.FunctionDeclaration;
import org.eclipse.wst.jsdt.core.dom.FunctionExpression;
import org.eclipse.wst.jsdt.core.dom.FunctionInvocation;
import org.eclipse.wst.jsdt.core.dom.IfStatement;
import org.eclipse.wst.jsdt.core.dom.ImportDeclaration;
import org.eclipse.wst.jsdt.core.dom.InfixExpression;
import org.eclipse.wst.jsdt.core.dom.JavaScriptUnit;
import org.eclipse.wst.jsdt.core.dom.LabeledStatement;
import org.eclipse.wst.jsdt.core.dom.ListExpression;
import org.eclipse.wst.jsdt.core.dom.Modifier.ModifierKeyword;
import org.eclipse.wst.jsdt.core.dom.ModuleSpecifier;
import org.eclipse.wst.jsdt.core.dom.Name;
import org.eclipse.wst.jsdt.core.dom.NullLiteral;
import org.eclipse.wst.jsdt.core.dom.NumberLiteral;
import org.eclipse.wst.jsdt.core.dom.ObjectLiteral;
import org.eclipse.wst.jsdt.core.dom.ObjectLiteralField;
import org.eclipse.wst.jsdt.core.dom.ObjectLiteralField.FieldKind;
import org.eclipse.wst.jsdt.core.dom.ObjectName;
import org.eclipse.wst.jsdt.core.dom.ParenthesizedExpression;
import org.eclipse.wst.jsdt.core.dom.PostfixExpression;
import org.eclipse.wst.jsdt.core.dom.PrefixExpression;
import org.eclipse.wst.jsdt.core.dom.ProgramElement;
import org.eclipse.wst.jsdt.core.dom.RegularExpressionLiteral;
import org.eclipse.wst.jsdt.core.dom.RestElementName;
import org.eclipse.wst.jsdt.core.dom.ReturnStatement;
import org.eclipse.wst.jsdt.core.dom.SimpleName;
import org.eclipse.wst.jsdt.core.dom.SingleVariableDeclaration;
import org.eclipse.wst.jsdt.core.dom.SpreadElement;
import org.eclipse.wst.jsdt.core.dom.Statement;
import org.eclipse.wst.jsdt.core.dom.StringLiteral;
import org.eclipse.wst.jsdt.core.dom.SwitchCase;
import org.eclipse.wst.jsdt.core.dom.SwitchStatement;
import org.eclipse.wst.jsdt.core.dom.TemplateElement;
import org.eclipse.wst.jsdt.core.dom.TemplateLiteral;
import org.eclipse.wst.jsdt.core.dom.ThrowStatement;
import org.eclipse.wst.jsdt.core.dom.TryStatement;
import org.eclipse.wst.jsdt.core.dom.Type;
import org.eclipse.wst.jsdt.core.dom.TypeDeclaration;
import org.eclipse.wst.jsdt.core.dom.VariableDeclarationFragment;
import org.eclipse.wst.jsdt.core.dom.VariableDeclarationStatement;
import org.eclipse.wst.jsdt.core.dom.VariableKind;
import org.eclipse.wst.jsdt.core.dom.WhileStatement;
import org.eclipse.wst.jsdt.core.dom.WithStatement;
import org.eclipse.wst.jsdt.core.dom.YieldExpression;

import com.google.javascript.jscomp.parsing.parser.IdentifierToken;
import com.google.javascript.jscomp.parsing.parser.Token;
import com.google.javascript.jscomp.parsing.parser.TokenType;
import com.google.javascript.jscomp.parsing.parser.trees.ArrayLiteralExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.ArrayPatternTree;
import com.google.javascript.jscomp.parsing.parser.trees.AssignmentRestElementTree;
import com.google.javascript.jscomp.parsing.parser.trees.BinaryOperatorTree;
import com.google.javascript.jscomp.parsing.parser.trees.BlockTree;
import com.google.javascript.jscomp.parsing.parser.trees.BreakStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.CallExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.CaseClauseTree;
import com.google.javascript.jscomp.parsing.parser.trees.CatchTree;
import com.google.javascript.jscomp.parsing.parser.trees.ClassDeclarationTree;
import com.google.javascript.jscomp.parsing.parser.trees.CommaExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.ComputedPropertyDefinitionTree;
import com.google.javascript.jscomp.parsing.parser.trees.ComputedPropertyGetterTree;
import com.google.javascript.jscomp.parsing.parser.trees.ComputedPropertyMemberVariableTree;
import com.google.javascript.jscomp.parsing.parser.trees.ComputedPropertyMethodTree;
import com.google.javascript.jscomp.parsing.parser.trees.ComputedPropertySetterTree;
import com.google.javascript.jscomp.parsing.parser.trees.ConditionalExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.ContinueStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.DebuggerStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.DefaultClauseTree;
import com.google.javascript.jscomp.parsing.parser.trees.DefaultParameterTree;
import com.google.javascript.jscomp.parsing.parser.trees.DoWhileStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.EmptyStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.ExportDeclarationTree;
import com.google.javascript.jscomp.parsing.parser.trees.ExportSpecifierTree;
import com.google.javascript.jscomp.parsing.parser.trees.ExpressionStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.ForInStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.ForOfStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.ForStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.FunctionDeclarationTree;
import com.google.javascript.jscomp.parsing.parser.trees.FunctionDeclarationTree.Kind;
import com.google.javascript.jscomp.parsing.parser.trees.GetAccessorTree;
import com.google.javascript.jscomp.parsing.parser.trees.IdentifierExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.IfStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.ImportDeclarationTree;
import com.google.javascript.jscomp.parsing.parser.trees.ImportSpecifierTree;
import com.google.javascript.jscomp.parsing.parser.trees.LabelledStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.LiteralExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.MemberExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.MemberLookupExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.ModuleImportTree;
import com.google.javascript.jscomp.parsing.parser.trees.NewExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.NullTree;
import com.google.javascript.jscomp.parsing.parser.trees.ObjectLiteralExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.ObjectPatternTree;
import com.google.javascript.jscomp.parsing.parser.trees.ParenExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.ParseTree;
import com.google.javascript.jscomp.parsing.parser.trees.ParseTreeType;
import com.google.javascript.jscomp.parsing.parser.trees.PostfixExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.ProgramTree;
import com.google.javascript.jscomp.parsing.parser.trees.PropertyNameAssignmentTree;
import com.google.javascript.jscomp.parsing.parser.trees.RestParameterTree;
import com.google.javascript.jscomp.parsing.parser.trees.ReturnStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.SetAccessorTree;
import com.google.javascript.jscomp.parsing.parser.trees.SpreadExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.SuperExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.SwitchStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.TemplateLiteralExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.TemplateLiteralPortionTree;
import com.google.javascript.jscomp.parsing.parser.trees.TemplateSubstitutionTree;
import com.google.javascript.jscomp.parsing.parser.trees.ThisExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.ThrowStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.TryStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.TypeNameTree;
import com.google.javascript.jscomp.parsing.parser.trees.UnaryExpressionTree;
import com.google.javascript.jscomp.parsing.parser.trees.VariableDeclarationTree;
import com.google.javascript.jscomp.parsing.parser.trees.VariableStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.WhileStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.WithStatementTree;
import com.google.javascript.jscomp.parsing.parser.trees.YieldExpressionTree;

/**
 * @author Gorkem Ercan
 *
 */
@SuppressWarnings("unchecked")
public class DOMTransformer {

	private final AST ast;
	
	public DOMTransformer(AST t){
		this.ast = t;
	}

    private ASTNode process(ParseTree node) {
      switch (node.type) {
        case BINARY_OPERATOR:
          return processBinaryExpression(node.asBinaryOperator());
        case ARRAY_LITERAL_EXPRESSION:
          return processArrayLiteral(node.asArrayLiteralExpression());
        case TEMPLATE_LITERAL_EXPRESSION:
          return processTemplateLiteral(node.asTemplateLiteralExpression());
        case TEMPLATE_LITERAL_PORTION:
          return processTemplateLiteralPortion(node.asTemplateLiteralPortion());
        case TEMPLATE_SUBSTITUTION:
          return processTemplateSubstitution(node.asTemplateSubstitution());
        case UNARY_EXPRESSION:
          return processUnaryExpression(node.asUnaryExpression());
        case BLOCK:
          return processBlock(node.asBlock());
        case BREAK_STATEMENT:
          return processBreakStatement(node.asBreakStatement());
        case CALL_EXPRESSION:
          return processFunctionCall(node.asCallExpression());
        case SWITCH_STATEMENT:
        	return processSwitchStatement(node.asSwitchStatement());
        case CASE_CLAUSE:
          return processSwitchCase(node.asCaseClause());
        case DEFAULT_CLAUSE:
          return processSwitchDefault(node.asDefaultClause());
        case CATCH:
          return processCatchClause(node.asCatch());
        case CONTINUE_STATEMENT:
          return processContinueStatement(node.asContinueStatement());
        case DO_WHILE_STATEMENT:
          return processDoLoop(node.asDoWhileStatement());
        case EMPTY_STATEMENT:
          return processEmptyStatement(node.asEmptyStatement());
        case EXPRESSION_STATEMENT:
          return processExpressionStatement(node.asExpressionStatement());
        case DEBUGGER_STATEMENT:
          return processDebuggerStatement(node.asDebuggerStatement());
        case THIS_EXPRESSION:
          return processThisExpression(node.asThisExpression());
        case FOR_STATEMENT:
          return processForLoop(node.asForStatement());
        case FOR_IN_STATEMENT:
          return processForInLoop(node.asForInStatement());
        case FUNCTION_DECLARATION:
          return processFunction(node.asFunctionDeclaration());
        case MEMBER_LOOKUP_EXPRESSION:
          return processElementGet(node.asMemberLookupExpression());
        case MEMBER_EXPRESSION:
          return processPropertyGet(node.asMemberExpression());
        case CONDITIONAL_EXPRESSION:
          return processConditionalExpression(node.asConditionalExpression());
        case IF_STATEMENT:
          return processIfStatement(node.asIfStatement());
        case LABELLED_STATEMENT:
          return processLabeledStatement(node.asLabelledStatement());
        case PAREN_EXPRESSION:
          return processParenthesizedExpression(node.asParenExpression());
        case IDENTIFIER_EXPRESSION:
          return processName(node.asIdentifierExpression());
        case NEW_EXPRESSION:
          return processNewExpression(node.asNewExpression());
        case OBJECT_LITERAL_EXPRESSION:
          return processObjectLiteral(node.asObjectLiteralExpression());
        case COMPUTED_PROPERTY_DEFINITION:
        case COMPUTED_PROPERTY_SETTER:
        case COMPUTED_PROPERTY_GETTER:
        case COMPUTED_PROPERTY_METHOD:
        case COMPUTED_PROPERTY_MEMBER_VARIABLE:
        	// Handled on processObjectLiteral should never happen here
        	Assert.isTrue(false);
        case RETURN_STATEMENT:
          return processReturnStatement(node.asReturnStatement());
        case POSTFIX_EXPRESSION:
          return processPostfixExpression(node.asPostfixExpression());
        case PROGRAM:
          return processAstRoot(node.asProgram());
        case LITERAL_EXPRESSION: // STRING, NUMBER, TRUE, FALSE, NULL, REGEXP
          return processLiteralExpression(node.asLiteralExpression());
        case THROW_STATEMENT:
          return processThrowStatement(node.asThrowStatement());
        case TRY_STATEMENT:
          return processTryStatement(node.asTryStatement());
        case VARIABLE_STATEMENT: // var const let
          return processVariableStatement(node.asVariableStatement());
        case VARIABLE_DECLARATION_LIST:
        	//Handled on processVariableStatement
        	Assert.isTrue(false);
        case VARIABLE_DECLARATION:
          return processVariableDeclaration(node.asVariableDeclaration());
        case WHILE_STATEMENT:
          return processWhileLoop(node.asWhileStatement());
        case WITH_STATEMENT:
          return processWithStatement(node.asWithStatement());

        case COMMA_EXPRESSION:
          return processCommaExpression(node.asCommaExpression());
        case NULL: // this is not the null literal
          return processNull(node.asNull());
        case FINALLY:
          return transform(node.asFinally().block);

        case MISSING_PRIMARY_EXPRESSION:
        	// DOM AST provides a syntactically plausible initial value to required
        	// properties skip processing this node type.
        	return null;
        case PROPERTY_NAME_ASSIGNMENT:
          return processPropertyNameAssignment(node.asPropertyNameAssignment());
        case GET_ACCESSOR:
          return processGetAccessor(node.asGetAccessor());
        case SET_ACCESSOR:
          return processSetAccessor(node.asSetAccessor());
        case FORMAL_PARAMETER_LIST:
        	//Should be handled on processFunction
        	Assert.isTrue(false);

        case CLASS_DECLARATION:
          return processClassDeclaration(node.asClassDeclaration());
        case SUPER_EXPRESSION:
          return processSuper(node.asSuperExpression());
        case YIELD_EXPRESSION:
          return processYield(node.asYieldStatement());
        case FOR_OF_STATEMENT:
          return processForOf(node.asForOfStatement());

        case EXPORT_DECLARATION:
          return processExportDecl(node.asExportDeclaration());
        case EXPORT_SPECIFIER:
          return processExportSpec(node.asExportSpecifier());
        case IMPORT_DECLARATION:
          return processImportDecl(node.asImportDeclaration());
        case IMPORT_SPECIFIER:
          return processImportSpec(node.asImportSpecifier());
        case MODULE_IMPORT:
          return processModuleImport(node.asModuleImport());

        case ARRAY_PATTERN:
          return processArrayPattern(node.asArrayPattern());
        case OBJECT_PATTERN:
          return processObjectPattern(node.asObjectPattern());
        case ASSIGNMENT_REST_ELEMENT:
          return processAssignmentRestElement(node.asAssignmentRestElement());

//        case COMPREHENSION:
//          return processComprehension(node.asComprehension());
//        case COMPREHENSION_FOR:
//          return processComprehensionFor(node.asComprehensionFor());
//        case COMPREHENSION_IF:
//          return processComprehensionIf(node.asComprehensionIf());

        case DEFAULT_PARAMETER:
          return processDefaultParameter(node.asDefaultParameter());
        case REST_PARAMETER:
          return processRestParameter(node.asRestParameter());
        case SPREAD_EXPRESSION:
          return processSpreadExpression(node.asSpreadExpression());

        // ES6 Typed
        case TYPE_NAME:
          return processTypeName(node.asTypeName());
//        case TYPED_PARAMETER:
//          return processTypedParameter(node.asTypedParameter());
//        case OPTIONAL_PARAMETER:
//          return processOptionalParameter(node.asOptionalParameter());
//        case PARAMETERIZED_TYPE_TREE:
//          return processParameterizedType(node.asParameterizedType());
//        case ARRAY_TYPE:
//          return processArrayType(node.asArrayType());
//        case RECORD_TYPE:
//          return processRecordType(node.asRecordType());
//        case UNION_TYPE:
//          return processUnionType(node.asUnionType());
//        case FUNCTION_TYPE:
//          return processFunctionType(node.asFunctionType());
//        case TYPE_QUERY:
//          return processTypeQuery(node.asTypeQuery());
//        case GENERIC_TYPE_LIST:
//          return processGenericTypeList(node.asGenericTypeList());
//        case MEMBER_VARIABLE:
//          return processMemberVariable(node.asMemberVariable());
//
//        case INTERFACE_DECLARATION:
//          return processInterfaceDeclaration(node.asInterfaceDeclaration());
//        case ENUM_DECLARATION:
//          return processEnumDeclaration(node.asEnumDeclaration());
//
//        case TYPE_ALIAS:
//          return processTypeAlias(node.asTypeAlias());
//        case AMBIENT_DECLARATION:
//          return processAmbientDeclaration(node.asAmbientDeclaration());
//        case NAMESPACE_DECLARATION:
//          return processNamespaceDeclaration(node.asNamespaceDeclaration());
//
//        case INDEX_SIGNATURE:
//          return processIndexSignature(node.asIndexSignature());
//        case CALL_SIGNATURE:
//          return processCallSignature(node.asCallSignature());

        // TODO(johnlenz): handle these or remove parser support
        case ARGUMENT_LIST:
        default:
          break;
      }
      return processIllegalToken(node);
    }
   
    private ASTNode processIllegalToken(ParseTree node) {
    	System.out.println( "Unsupported syntax: " + node.type +" at "+ node.location.start.line +1);
        return ast.newEmptyStatement();
      }
    


	/**
	 * @param asTypeName
	 * @return
	 */
	private ASTNode processTypeName(TypeNameTree tree) {
		Name $ = null;
		Iterator<String> segmentsIt = tree.segments.iterator();
		while(segmentsIt.hasNext()){
			SimpleName n = ast.newSimpleName(segmentsIt.next());
			if($ == null ){
				$ = n;
			}else{
				$ = ast.newQualifiedName($,n);
			}
		}
		return $;
	}

	/**
	 * @param asRestParameter
	 * @return
	 */
	private ASTNode processRestParameter(RestParameterTree tree) {
		SingleVariableDeclaration $ = ast.newSingleVariableDeclaration();
		$.setName(transformLabelName(tree.identifier.asIdentifier()));
		$.setVarargs(true);
		return $;
	}

	/**
	 * @param asDefaultParameter
	 * @return
	 */
	private ASTNode processDefaultParameter(DefaultParameterTree tree) {
		// TODO: handle default values properly. DOM ast does not have support 
		// for it and needs to be enhanced.
		return transform(tree.lhs);
	}

	/**
	 * @param asAssignmentRestElement
	 * @return
	 */
	private ASTNode processAssignmentRestElement(AssignmentRestElementTree tree) {
		RestElementName $ = ast.newRestElementName();
		$.setArgument(transformLabelName(tree.identifier));
		return $;
	}

	/**
	 * @param asObjectPattern
	 * @return
	 */
	private ASTNode processObjectPattern(ObjectPatternTree tree) {
		ObjectName $ = ast.newObjectName();
		for(ParseTree child: tree.fields){
			$.objectProperties().add(transform(child));
		}
		return $;
	}

	/**
	 * @param asArrayPattern
	 * @return
	 */
	private ASTNode processArrayPattern(ArrayPatternTree tree) {
		ArrayName $ = ast.newArrayName();
		for(ParseTree child: tree.elements){
			$.elements().add(transform(child));
		}
		return $;
	}

	/**
	 * @param asModuleImport
	 * @return
	 */
	private ASTNode processModuleImport(ModuleImportTree tree) {
		ImportDeclaration $ = ast.newImportDeclaration();
		ModuleSpecifier m = ast.newModuleSpecifier();
		m.setLocal(transformLabelName(tree.name));
		m.setDiscoverableName(transformLabelName(tree.from.asIdentifier()));
		m.setNamespace(true);
		$.specifiers().add(m);
		return $;
	}

	/**
	 * @param asImportSpecifier
	 * @return
	 */
	private ASTNode processImportSpec(ImportSpecifierTree tree) {
		ModuleSpecifier $ = ast.newModuleSpecifier();
		$.setDiscoverableName(transformLabelName(tree.destinationName.asIdentifier()));
		$.setLocal(transformLabelName(tree.importedName.asIdentifier()));
		return $;
	}

	/**
	 * @param asImportDeclaration
	 * @return
	 */
	private ASTNode processImportDecl(ImportDeclarationTree tree) {
		ImportDeclaration $ = ast.newImportDeclaration();
		if(tree.defaultBindingIdentifier != null){
			$.setName(transformLabelName(tree.defaultBindingIdentifier));
		}
		if(tree.nameSpaceImportIdentifier != null ){
			ModuleSpecifier m = ast.newModuleSpecifier();
			m.setNamespace(true);
			m.setLocal(transformLabelName(tree.nameSpaceImportIdentifier.asIdentifier()));
			setSourceRange(m,tree);
		}else{
			for(ParseTree spec : tree.importSpecifierList){
				$.specifiers().add(transform(spec));
			}
		}
		return $;
	}

	/**
	 * @param asExportSpecifier
	 * @return
	 */
	private ASTNode processExportSpec(ExportSpecifierTree tree) {
		ModuleSpecifier $ = ast.newModuleSpecifier();
		$.setLocal(transformLabelName(tree.importedName));
		$.setDiscoverableName(transformLabelName(tree.destinationName));
		return $;
	}

	/**
	 * @param asExportDeclaration
	 * @return
	 */
	private ASTNode processExportDecl(ExportDeclarationTree tree) {
		ExportDeclaration $ = ast.newExportDeclaration();
		$.setAll(tree.isExportAll);
		$.setDefault(tree.isDefault);
		$.setDeclaration((ProgramElement) transform(tree.declaration));
		for(ParseTree spec : tree.exportSpecifierList){
			$.specifiers().add(transform(spec));
		}
		if(tree.from != null){
			$.setSource(transformStringLiteral(tree.from));
		}
		return $;
	}

	/**
	 * @param asSuperExpression
	 * @return
	 */
	private ASTNode processSuper(SuperExpressionTree tree) {
		//FIXME: we need a better way to handle super references. Simply
		// treating as a name is not enough.
		return ast.newSimpleName("super");
	}

	/**
	 * @param asClassDeclaration
	 * @return
	 */
	private ASTNode processClassDeclaration(ClassDeclarationTree tree) {
		TypeDeclaration $ = ast.newTypeDeclaration();
		$.setName(transformLabelName(tree.name));
		if(tree.superClass != null )
			$.setSuperclassExpression((Expression) transform(tree.superClass));
		for(ParseTree child : tree.elements){
			$.bodyDeclarations().add(transform(child));
		}
		return $;
	}

	/**
	 * @param asSetAccessor
	 * @return
	 */
	private ASTNode processSetAccessor(SetAccessorTree tree) {
		FunctionDeclaration $ = ast.newFunctionDeclaration();
		$.setMethodName(transformObjectLitKeyAsString(tree.propertyName));
		$.setBody((Block) transform(tree.body));
		final SingleVariableDeclaration p = ast.newSingleVariableDeclaration();
		p.setName(transformLabelName(tree.parameter));
		$.parameters().add(p);
		if(tree.isStatic){
			$.modifiers().add(ast.newModifier(ModifierKeyword.STATIC_KEYWORD));
		}
		return $;
	}

	/**
	 * @param asGetAccessor
	 * @return
	 */
	private ASTNode processGetAccessor(GetAccessorTree tree) {
		FunctionDeclaration $ = ast.newFunctionDeclaration();
		$.setMethodName(transformObjectLitKeyAsString(tree.propertyName));
		$.setBody((Block) transform(tree.body));
		$.modifiers().add(ast.newModifier(ModifierKeyword.GET_KEYWORD));
		if(tree.isStatic){
			$.modifiers().add(ast.newModifier(ModifierKeyword.STATIC_KEYWORD));
		}
		return $;
	}

	/**
	 * @param asPropertyNameAssignment
	 * @return
	 */
	private ASTNode processPropertyNameAssignment(PropertyNameAssignmentTree tree) {
		VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
		vdf.setName(transformLabelName((IdentifierToken) tree.name));
		vdf.setInitializer((Expression) transform(tree.value));
		FieldDeclaration $ = ast.newFieldDeclaration(vdf);
		return $;
	}

	/**
	 * @param asParenExpression
	 * @return
	 */
	private ASTNode processParenthesizedExpression(ParenExpressionTree tree) {
		ParenthesizedExpression $ = ast.newParenthesizedExpression();
		$.setExpression((Expression) transform(tree.expression));
		return $;
	}

	/**
	 * @param asNull
	 * @return
	 */
	private ASTNode processNull(NullTree tree) {
		return ast.newEmptyStatement();
	}

	/**
	 * @param asCommaExpression
	 * @return
	 */
	private ASTNode processCommaExpression(CommaExpressionTree tree) {
		ListExpression $ = ast.newListExpression();
		for(ParseTree expr : tree.expressions){
			$.expressions().add(transform(expr));
		}
		return $;
	}

	/**
	 * @param asVariableDeclaration
	 * @return
	 */
	private ASTNode processVariableDeclaration(VariableDeclarationTree tree) {
		VariableDeclarationFragment $ = ast.newVariableDeclarationFragment();
		//TODO: Handle destructuring assignment
		$.setName((SimpleName) transform(tree.lvalue));
		if(tree.initializer != null){
			$.setInitializer((Expression) transform(tree.initializer));
		}
		return $;
	}

	/**
	 * @param asVariableStatement
	 * @return
	 */
	private ASTNode processVariableStatement(VariableStatementTree tree) {
		VariableDeclarationStatement $ = ast.newVariableDeclarationStatement();
		switch (tree.declarations.declarationType) {
			case CONST :
				$.setKind(VariableKind.CONST);
				break;
			case LET: 
				$.setKind(VariableKind.LET);
				break;
			default :
				$.setKind(VariableKind.VAR);
				break;
		}
		for(ParseTree decl : tree.declarations.declarations){
			$.fragments().add(transform(decl));
		}
		return $;
	}

	/**
	 * @param asLiteralExpression
	 * @return
	 */
	private ASTNode processLiteralExpression(LiteralExpressionTree tree) {
		switch (tree.literalToken.type) {
			case NUMBER :
				return transformNumberLiteral(tree.literalToken);
			case STRING: 
				return transformStringLiteral(tree.literalToken);
			case FALSE:
			case TRUE:
				return transformBooleanLiteral(tree.literalToken);
			case NULL:
				return transformNullLiteral(tree.literalToken);
			case REGULAR_EXPRESSION:
				return transformRegExpLiteral(tree.literalToken);
			default :
		          throw new IllegalStateException("Unexpected literal type: "
		                      + tree.literalToken.getClass() + " type: "
		                      + tree.literalToken.type);
		}
	}
	
	private ASTNode transformRegExpLiteral(Token token) {
		RegularExpressionLiteral $ = ast.newRegularExpressionLiteral(token.asLiteral().value);
		setSourceRange($,token);
		return $;
	}	

	private ASTNode transformNullLiteral(Token token) {
		NullLiteral $ = ast.newNullLiteral();
		setSourceRange($,token);
		return $;
	}

	private ASTNode transformBooleanLiteral(Token token) {
		BooleanLiteral $ = ast.newBooleanLiteral(token.type == TokenType.TRUE);
		setSourceRange($,token);
		return $;
	}

	private StringLiteral transformStringLiteral(Token token) {
		StringLiteral $ = ast.newStringLiteral(token.asLiteral().value);
		setSourceRange($,token);
		return $;
	}

	private ASTNode transformNumberLiteral(Token token) {
        NumberLiteral $ = ast.newNumberLiteral(token.asLiteral().value);
        setSourceRange($, token);
        return $;
      }
	
	private FunctionExpression transformAsFunctionExpression(ParseTree tree){
		FunctionExpression $ = ast.newFunctionExpression();
		$.setMethod((FunctionDeclaration) transform(tree));
		setSourceRange($,tree);
		return $;
	}

	/**
	 * @param asObjectLiteralExpression
	 * @return
	 */
	private ASTNode processObjectLiteral(ObjectLiteralExpressionTree tree) {
		ObjectLiteral $ = ast.newObjectLiteral(); 
		for(ParseTree  elem : tree.propertyNameAndValues){
			ObjectLiteralField f = ast.newObjectLiteralField();
			switch (elem.type) {
				case GET_ACCESSOR:
					f.setKind(FieldKind.GET);
					final GetAccessorTree getAccessor = elem.asGetAccessor();
					f.setFieldName(transformObjectLitKeyAsString(getAccessor.propertyName));
					f.setInitializer(transformAsFunctionExpression(getAccessor));
					break;
				case COMPUTED_PROPERTY_GETTER:
					f.setKind(FieldKind.GET);
					final ComputedPropertyGetterTree compGetter= elem.asComputedPropertyGetter();
					f.setFieldName((Expression) transform(compGetter.property));
					f.setInitializer(transformAsFunctionExpression(compGetter));
					break;
				case SET_ACCESSOR:
					f.setKind(FieldKind.SET);
					final SetAccessorTree setAccessor= elem.asSetAccessor();
					f.setFieldName(transformObjectLitKeyAsString(setAccessor.propertyName));
					f.setInitializer(transformAsFunctionExpression(setAccessor));
					break;
				case COMPUTED_PROPERTY_SETTER:
					f.setKind(FieldKind.SET);
					final ComputedPropertySetterTree compSetter = elem.asComputedPropertySetter();
					f.setFieldName((Expression) transform(compSetter.property));
					f.setInitializer(transformAsFunctionExpression(compSetter));
					break;
				case COMPUTED_PROPERTY_DEFINITION:
					f.setKind(FieldKind.INIT);
					final ComputedPropertyDefinitionTree compDef = elem.asComputedPropertyDefinition();
					f.setFieldName((Expression) transform(compDef.property));
					f.setInitializer((Expression) transform(compDef.value));
					break;
				case COMPUTED_PROPERTY_MEMBER_VARIABLE:
					f.setKind(FieldKind.INIT);
					final ComputedPropertyMemberVariableTree compVariable = elem.asComputedPropertyMemberVariable();
					f.setFieldName((Expression) transform(compVariable.property));
					f.setInitializer((Expression) transform(compVariable.declaredType));
					break;
				case COMPUTED_PROPERTY_METHOD:
					f.setKind(FieldKind.INIT);
					final ComputedPropertyMethodTree compMethod = elem.asComputedPropertyMethod();
					f.setFieldName((Expression) transform(compMethod.property));
					f.setInitializer(transformAsFunctionExpression(compMethod));
					break;
				default :
					f.setKind(FieldKind.INIT);
					final PropertyNameAssignmentTree assignment = elem.asPropertyNameAssignment();
					f.setFieldName(transformObjectLitKeyAsString(assignment.name));
					f.setInitializer((Expression) transform(assignment.value));
					break;
			}
			$.fields().add(f);
		}
		return $;
	}
	
	private SimpleName transformObjectLitKeyAsString(com.google.javascript.jscomp.parsing.parser.Token token) {
		SimpleName $ = null;
		if (token == null) {
			$ = ast.newSimpleName("");
		}
		else if (token.type == TokenType.IDENTIFIER) {
			$ = transformLabelName(token.asIdentifier());
		}
		else {// literal or number
			$ = ast.newSimpleName(token.asLiteral().value);
		}
		return $;
	}


	/**
	 * @param asProgram
	 * @return
	 */
	private ASTNode processAstRoot(ProgramTree tree) {
		JavaScriptUnit $ = ast.newJavaScriptUnit();
	    for (ParseTree child : tree.sourceElements) {
	    	ASTNode node =  transform(child);
	    	switch (node.getNodeType()) {
				case ASTNode.EXPORT_DECLARATION :
					$.exports().add(node);
					break;
				case ASTNode.IMPORT_DECLARATION : 
					$.imports().add(node);
					break;
				default :
					$.statements().add(node);
					break;
			}
	    }
		return $;
	}

	/**
	 * @param asIdentifierExpression
	 * @return
	 */
	private ASTNode processName(IdentifierExpressionTree tree) {
		return transformLabelName(tree.identifierToken);
	}

	/**
	 * @param asMemberExpression
	 * @return
	 */
	private ASTNode processPropertyGet(MemberExpressionTree tree) {
		FieldAccess $ = ast.newFieldAccess();
		$.setExpression((Expression) transform(tree.operand));
		$.setName(transformLabelName(tree.memberName));
		return $;
	}

	/**
	 * @param asMemberLookupExpression
	 * @return
	 */
	private ASTNode processElementGet(MemberLookupExpressionTree tree) {
		ArrayAccess $ = ast.newArrayAccess();
		$.setArray((Expression) transform(tree.memberExpression));
		$.setIndex((Expression) transform(tree.operand));
		return $;
	}

	/**
	 * @param asFunctionDeclaration
	 * @return
	 */
	private ASTNode processFunction(FunctionDeclarationTree tree) {
		
		if(tree.kind == FunctionDeclarationTree.Kind.ARROW){
			ArrowFunctionExpression $ = ast.newArrowFunctionExpression();
			$.setBody((Block) transform(tree.functionBody));
			for(ParseTree param : tree.formalParameterList.parameters){
				$.parameters().add(transform(param));
			}
			return $;
		}
		//TODO: Kind.Member
		
		FunctionDeclaration $ = ast.newFunctionDeclaration();
		$.setGenerator(tree.isGenerator);
		$.setBody((Block) transform(tree.functionBody));
		$.setMethodName(transformLabelName(tree.name));
		if(tree.isStatic)
			$.modifiers().add(ast.newModifier(ModifierKeyword.STATIC_KEYWORD));
		
		for(ParseTree param : tree.formalParameterList.parameters){
			$.parameters().add(transform(param));
		}
		if(tree.returnType != null ){
			$.setReturnType2((Type) transform(tree.returnType));
		}
		// Kind.Expression
		if(tree.kind == Kind.EXPRESSION) {
			return $;
		}
		//Kind.Declaration
		return ast.newFunctionDeclarationStatement($);
			
	}

	/**
	 * @param asSpreadExpression
	 * @return
	 */
	private ASTNode processSpreadExpression(SpreadExpressionTree tree) {
		SpreadElement $ = ast.newSpreadElement();
		$.setArgument((Expression) transform(tree.expression));
		return $;
	}

	/**
	 * @param asForOfStatement
	 * @return
	 */
	private ASTNode processForOf(ForOfStatementTree tree) {
		ForOfStatement $ = ast.newForOfStatement();
		$.setBody((Statement) transform(tree.body));
		$.setCollection((Expression) transform(tree.collection));
		return $;
	}

	/**
	 * @param asYieldStatement
	 * @return
	 */
	private ASTNode processYield(YieldExpressionTree tree) {
		YieldExpression $ = ast.newYieldExpression();
		$.setDelegate(Boolean.valueOf(tree.isYieldFor));
		if(tree.expression != null ){
			$.setArgument((Expression) transform(tree.expression));
		}
		return $;
	}

	/**
	 * @param asWithStatement
	 * @return
	 */
	private ASTNode processWithStatement(WithStatementTree tree) {
		WithStatement $ = ast.newWithStatement();
		$.setBody((Statement) transform(tree.body));
		$.setExpression((Expression) transform(tree.expression));
		return $;
	}

	/**
	 * @param asWhileStatement
	 * @return
	 */
	private ASTNode processWhileLoop(WhileStatementTree tree) {
		WhileStatement $ = ast.newWhileStatement();
		$.setExpression((Expression) transform(tree.condition));
		$.setBody((Statement) transform(tree.body));
		return $;
	}

	/**
	 * @param asTryStatement
	 * @return
	 */
	private ASTNode processTryStatement(TryStatementTree tree) {
		TryStatement $ = ast.newTryStatement();
		$.setBody((Block) transform(tree.body));
		if(tree.catchBlock != null ){
			$.catchClauses().add(transform(tree.catchBlock));
		}
		if(tree.finallyBlock != null ){
			$.setFinally((Block) transform(tree.finallyBlock));
		}
		return $;
	}

	/**
	 * @param asThrowStatement
	 * @return
	 */
	private ASTNode processThrowStatement(ThrowStatementTree tree) {
		ThrowStatement $ = ast.newThrowStatement();
		$.setExpression((Expression) transform(tree.value));
		return $;	
	}

	/**
	 * @param asPostfixExpression
	 * @return
	 */
	private ASTNode processPostfixExpression(PostfixExpressionTree tree) {
		PostfixExpression $ = ast.newPostfixExpression();
		$.setOperand((Expression) transform(tree.operand));
		$.setOperator(org.eclipse.wst.jsdt.core.dom.PostfixExpression.Operator.toOperator(tree.operator.toString()));
		return $;
	}

	/**
	 * @param asReturnStatement
	 * @return
	 */
	private ASTNode processReturnStatement(ReturnStatementTree tree	) {
		ReturnStatement $ = ast.newReturnStatement();
		if(tree.expression != null)
			$.setExpression((Expression) transform(tree.expression));
		return $;
	}

	/**
	 * @param asNewExpression
	 * @return
	 */
	private ASTNode processNewExpression(NewExpressionTree tree) {
		ClassInstanceCreation $ = ast.newClassInstanceCreation();
		$.setExpression((Expression) transform(tree.operand));
		if(tree.arguments != null ){
			for(ParseTree arg : tree.arguments.arguments){
				$.arguments().add(transform(arg));
			}
		}
		return $;
	}

	/**
	 * @param asLabelledStatement
	 * @return
	 */
	private ASTNode processLabeledStatement(LabelledStatementTree tree) {
		LabeledStatement $ = ast.newLabeledStatement();
		$.setLabel(transformLabelName(tree.name));
		$.setBody((Statement) transform(tree.statement));
		return $;
	}

	/**
	 * @param asIfStatement
	 * @return
	 */
	private ASTNode processIfStatement(IfStatementTree tree) {
		IfStatement $ = ast.newIfStatement();
		$.setExpression((Expression) transform(tree.condition));
		$.setThenStatement((Statement) transform(tree.ifClause));
		if(tree.elseClause != null)
			$.setElseStatement((Statement) transform(tree.elseClause));
		return $;
	}

	/**
	 * @param asConditionalExpression
	 * @return
	 */
	private ASTNode processConditionalExpression(ConditionalExpressionTree tree) {
		ConditionalExpression $ = ast.newConditionalExpression();
		$.setExpression( (Expression) transform(tree.condition));
		$.setThenExpression((Expression) transform(tree.left));
		$.setElseExpression((Expression) transform(tree.right));
		return $;
	}

	/**
	 * @param asForInStatement
	 * @return
	 */
	private ASTNode processForInLoop(ForInStatementTree tree) {
		ForInStatement $ = ast.newForInStatement();
		$.setBody((Statement) transform(tree.body));
		$.setIterationVariable((Statement) transform(tree.initializer));
		$.setCollection((Expression) transform(tree.collection));
		return $;
	}

	/**
	 * @param asForStatement
	 * @return
	 */
	private ASTNode processForLoop(ForStatementTree tree) {
		ForStatement $ = ast.newForStatement();
		$.setBody((Statement) transform(tree.body));
		if(notNullStatement(tree.condition))
			$.setExpression((Expression) transform(tree.condition));
		if(notNullStatement(tree.initializer))
			$.initializers().add(transform(tree.initializer));
		if( notNullStatement(tree.increment))
			$.updaters().add(transform(tree.increment));
		return $;
	}

	/**
	 * @param asThisExpression
	 * @return
	 */
	private ASTNode processThisExpression(ThisExpressionTree tree) {
		return ast.newThisExpression(); 
	}

	/**
	 * @param asDebuggerStatement
	 * @return
	 */
	private ASTNode processDebuggerStatement(DebuggerStatementTree tree) {
		return ast.newDebuggerStatement();
	}

	/**
	 * @param asExpressionStatement
	 * @return
	 */
	private ASTNode processExpressionStatement(ExpressionStatementTree tree ) {
		ExpressionStatement $ = ast.newExpressionStatement();
		Expression e = (Expression) transform(tree.expression);
		if( e != null )
			$.setExpression(e);
		return $;
	}

	/**
	 * @param asEmptyStatement
	 * @return
	 */
	private ASTNode processEmptyStatement(EmptyStatementTree tree) {
		return ast.newEmptyStatement();
	}

	/**
	 * @param asDoWhileStatement
	 * @return
	 */
	private ASTNode processDoLoop(DoWhileStatementTree tree) {
		DoStatement $ = ast.newDoStatement();
		$.setExpression((Expression) transform(tree.condition));
		$.setBody((Statement) transform(tree.body));
		return $;
	}

	/**
	 * @param asContinueStatement
	 * @return
	 */
	private ASTNode processContinueStatement(ContinueStatementTree tree) {
		ContinueStatement $ = ast.newContinueStatement();
		$.setLabel(transformLabelName(tree.name));
		return $;
	}

	/**
	 * @param asCatch
	 * @return
	 */
	private ASTNode processCatchClause(CatchTree tree) {
		CatchClause $ = ast.newCatchClause();
		$.setException((SingleVariableDeclaration) transform(tree.exception));
		$.setBody((Block) transform(tree.catchBody));
		return $;
	}

	/**
	 * @param asSwitchStatement
	 * @return
	 */
	private ASTNode processSwitchStatement(SwitchStatementTree tree) {
		SwitchStatement $ = ast.newSwitchStatement();
		$.setExpression((Expression) transform(tree.expression));
		for (ParseTree pt : tree.caseClauses) {
			if(pt.type == ParseTreeType.DEFAULT_CLAUSE){
				DefaultClauseTree dct = pt.asDefaultClause();
				$.statements().add(transform(dct));
				for (ParseTree dcs : dct.statements) {
					$.statements().add(transform(dcs));
				}
			}else{
				CaseClauseTree cct = pt.asCaseClause();
				$.statements().add(transform(cct));
				for(ParseTree ccs : cct.statements){
					$.statements().add(transform(ccs));
				}
			}
		}
		return $;
	}

	/**
	 * @param asDefaultClause
	 * @return
	 */
	private ASTNode processSwitchDefault(DefaultClauseTree tree) {
		// statements of DefaultClauseTree are handled on processSwitchStatement()
		SwitchCase $ = ast.newSwitchCase();
		return $;
	}

	/**
	 * @param asCaseClause
	 * @return
	 */
	private ASTNode processSwitchCase(CaseClauseTree tree) {
		// statements of CaseClauseTree are handled on processSwitchStatement()
		SwitchCase $ = ast.newSwitchCase();
		$.setExpression((Expression) transform(tree.expression));
		return $;
	}

	/**
	 * @param asCallExpression
	 * @return
	 */
	private ASTNode processFunctionCall(CallExpressionTree tree) {
		FunctionInvocation $  = ast.newFunctionInvocation();
		ASTNode expr = transform(tree.operand);
		if(expr.getNodeType() == ASTNode.SIMPLE_NAME){
			$.setName((SimpleName) expr);
		}else{
			$.setExpression((Expression) expr);
		}
		
		for (ParseTree pt : tree.arguments.arguments) {
			$.arguments().add(transform(pt));
		}
		return $;
	}

	/**
	 * @param asBreakStatement
	 * @return
	 */
	private ASTNode processBreakStatement(BreakStatementTree tree) {
		BreakStatement $ = ast.newBreakStatement();
		$.setLabel((SimpleName) transformLabelName(tree.name));
		return $;
	}
	
   private SimpleName transformLabelName(IdentifierToken token) {
    	SimpleName $ = ast.newSimpleName(token.value);
        setSourceRange($, token);
        return $;
      }

	/**
	 * @param asBlock
	 * @return
	 */
	private ASTNode processBlock(BlockTree tree) {
		Block $ = ast.newBlock();
		for (ParseTree pt : tree.statements) {
			$.statements().add(transform(pt));
		}
		return $;
	}

	/**
	 * @param asUnaryExpression
	 * @return
	 */
	private ASTNode processUnaryExpression(UnaryExpressionTree tree) {
		PrefixExpression $ = ast.newPrefixExpression();
		$.setOperator(PrefixExpression.Operator.toOperator(tree.operator.toString()));
		$.setOperand((Expression) transform(tree.operand));
		return $;
	}

	/**
	 * @param asTemplateSubstitution
	 * @return
	 */
	private ASTNode processTemplateSubstitution(TemplateSubstitutionTree tree) {
		return transform(tree.expression);
	}

	/**
	 * @param asTemplateLiteralPortion
	 * @return
	 */
	private ASTNode processTemplateLiteralPortion(TemplateLiteralPortionTree tree) {
		TemplateElement $ = ast.newTemplateElement();
		$.setStructuralProperty(TemplateElement.RAW_VALUE_PROPERTY, tree.value.asLiteral().value);
		return $;
	}

	/**
	 * @param asTemplateLiteralExpression
	 * @return
	 */
	private ASTNode processTemplateLiteral(TemplateLiteralExpressionTree tree) {
		TemplateLiteral $ = ast.newTemplateLiteral();
		for ( ParseTree pt : tree.elements) {
			ASTNode n = transform(pt);
			if(n.getNodeType() == ASTNode.TEMPLATE_ELEMENT)
				$.elements().add(transform(pt));
			else
				$.expressions().add(n);
		}
		return $;
	}

	/**
	 * @param asArrayLiteralExpression
	 * @return
	 */
	private ASTNode processArrayLiteral(ArrayLiteralExpressionTree tree) {
		ArrayInitializer $ = ast.newArrayInitializer();
		for ( ParseTree pe : tree.elements) {
			if(notNullStatement(pe))
				$.expressions().add(transform(pe));
		}
		return $;
	}

	public ASTNode transform(ParseTree tree) {
    	//TODO: JSDoc
//        JSDocInfo info = handleJsDoc(tree);
        ASTNode node = process(tree);
        if (node == null ) return null;
//        if (info != null) {
//          node = maybeInjectCastNode(tree, info, node);
//          node.setJSDocInfo(info);
//        }

        setSourceRange(node, tree);
        return node;
      }

	/**
	 * @param node
	 * @param tree
	 */
	private void setSourceRange(ASTNode node, ParseTree tree) {
		node.setSourceRange(tree.location.start.offset, tree.location.end.offset - tree.location.start.offset);
	}
	
	/**
	 * @param node
	 * @param tree
	 */
	private void setSourceRange(ASTNode node, Token token) {
		node.setSourceRange(token.location.start.offset, token.location.end.offset - token.location.start.offset);
	}

	/**
	 * @param asBinaryOperator
	 * @return
	 */
	private ASTNode processBinaryExpression(BinaryOperatorTree tree) {
		InfixExpression $ = ast.newInfixExpression();
		$.setLeftOperand((Expression) transform(tree.left));
		$.setRightOperand((Expression) transform(tree.right));
		$.setOperator(convertBinaryOperator(tree.operator));
		return $;
	}
	
	private InfixExpression.Operator convertBinaryOperator(Token operator) {
		return InfixExpression.Operator.toOperator(operator.toString());
	}
	
	private boolean notNullStatement(ParseTree tree){
		return tree.type != ParseTreeType.NULL;
	}
	
}
