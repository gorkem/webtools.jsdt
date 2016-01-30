/*******************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. 
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * 	Contributors:
 * 		 Red Hat Inc. - initial API and implementation and/or initial documentation
 *******************************************************************************/
package org.eclipse.wst.jsdt.internal.esprima;

import static org.eclipse.wst.jsdt.core.dom.ASTNode.*;

import java.util.Stack;
import org.eclipse.core.runtime.Assert;
import org.eclipse.wst.jsdt.core.UnimplementedException;
import org.eclipse.wst.jsdt.core.dom.AST;
import org.eclipse.wst.jsdt.core.dom.ASTNode;
import org.eclipse.wst.jsdt.core.dom.ArrayAccess;
import org.eclipse.wst.jsdt.core.dom.ArrayInitializer;
import org.eclipse.wst.jsdt.core.dom.ArrowFunctionExpression;
import org.eclipse.wst.jsdt.core.dom.Assignment;
import org.eclipse.wst.jsdt.core.dom.Block;
import org.eclipse.wst.jsdt.core.dom.BreakStatement;
import org.eclipse.wst.jsdt.core.dom.CatchClause;
import org.eclipse.wst.jsdt.core.dom.ClassInstanceCreation;
import org.eclipse.wst.jsdt.core.dom.ConditionalExpression;
import org.eclipse.wst.jsdt.core.dom.ContinueStatement;
import org.eclipse.wst.jsdt.core.dom.DebuggerStatement;
import org.eclipse.wst.jsdt.core.dom.DoStatement;
import org.eclipse.wst.jsdt.core.dom.EmptyStatement;
import org.eclipse.wst.jsdt.core.dom.Expression;
import org.eclipse.wst.jsdt.core.dom.ExpressionStatement;
import org.eclipse.wst.jsdt.core.dom.FieldAccess;
import org.eclipse.wst.jsdt.core.dom.FunctionDeclaration;
import org.eclipse.wst.jsdt.core.dom.FunctionExpression;
import org.eclipse.wst.jsdt.core.dom.FunctionInvocation;
import org.eclipse.wst.jsdt.core.dom.IfStatement;
import org.eclipse.wst.jsdt.core.dom.InfixExpression;
import org.eclipse.wst.jsdt.core.dom.JavaScriptUnit;
import org.eclipse.wst.jsdt.core.dom.LabeledStatement;
import org.eclipse.wst.jsdt.core.dom.ListExpression;
import org.eclipse.wst.jsdt.core.dom.Name;
import org.eclipse.wst.jsdt.core.dom.ObjectLiteral;
import org.eclipse.wst.jsdt.core.dom.ObjectLiteralField;
import org.eclipse.wst.jsdt.core.dom.ObjectLiteralField.FieldKind;
import org.eclipse.wst.jsdt.core.dom.PostfixExpression;
import org.eclipse.wst.jsdt.core.dom.PrefixExpression;
import org.eclipse.wst.jsdt.core.dom.ReturnStatement;
import org.eclipse.wst.jsdt.core.dom.SimpleName;
import org.eclipse.wst.jsdt.core.dom.SingleVariableDeclaration;
import org.eclipse.wst.jsdt.core.dom.Statement;
import org.eclipse.wst.jsdt.core.dom.SwitchCase;
import org.eclipse.wst.jsdt.core.dom.SwitchStatement;
import org.eclipse.wst.jsdt.core.dom.ThisExpression;
import org.eclipse.wst.jsdt.core.dom.ThrowStatement;
import org.eclipse.wst.jsdt.core.dom.TryStatement;
import org.eclipse.wst.jsdt.core.dom.VariableDeclaration;
import org.eclipse.wst.jsdt.core.dom.VariableDeclarationFragment;
import org.eclipse.wst.jsdt.core.dom.VariableDeclarationStatement;
import org.eclipse.wst.jsdt.core.dom.VariableDeclarationStatement.Kind;
import org.eclipse.wst.jsdt.core.dom.WhileStatement;
import org.eclipse.wst.jsdt.core.dom.WithStatement;
import org.eclipse.wst.jsdt.core.dom.YieldExpression;

import jdk.nashorn.api.scripting.ScriptObjectMirror;

/**
 * Converts ESTree AST to DOM AST.
 * 
 * @author Gorkem Ercan
 *
 */
public class DOMASTConverter extends EStreeVisitor{

	private final AST ast;
	private Stack<ASTNode> nodes = new Stack<ASTNode>();
	private JavaScriptUnit root;
	// Because switch also hosts all the statements in the case
	// statement we need to pointer to hold the currently processed switch
	private SwitchStatement processingSwitchStatement;
 	
	/**
	 * 
	 */
	public DOMASTConverter() {
		this.ast = AST.newAST(AST.JLS3);
	}
	
	public JavaScriptUnit convert(ScriptObjectMirror jsobject){
		this.traverse(jsobject);
		Assert.isTrue(nodes.empty(),"Some nodes are not processed"); //$NON-NLS-1$
		return root;
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.wst.jsdt.internal.esprima.EStreeVisitor#visit(jdk.nashorn.api.scripting.ScriptObjectMirror, org.eclipse.wst.jsdt.internal.esprima.ESTreeNodeTypes)
	 */
	public VisitOptions visit(final ScriptObjectMirror object, final ESTreeNodeTypes nodeType, final String key) {
		//Generate the dom AST objects and push it to stack.
		switch(nodeType){
			case Program:
				return convertProgram(object);
			case Identifier:
				return convertIdentifier(object);
			case Literal:
				return convertLiteral(object);
			// Declarations	
			case VariableDeclaration: 
				return convertVariableDeclaration(object);
			case VariableDeclarator:
				return convertVariableDeclarator(object);
		    //Statements
			case ExpressionStatement:
				return convertExpressionStatement(object);
			case BlockStatement:
				return convertBlockStatement(object);
			case EmptyStatement:
				return convertEmptyStatement(object);
			case DebuggerStatement:
				return convertDebuggerStatememt(object);
			case WithStatement:
				return convertWithStatement(object);
			case ReturnStatement:
				return convertReturnStatement(object);
			case LabeledStatement:
				return converLabeledStatement(object);
			case BreakStatement:
				return convertBreakStatement(object);
			case ContinueStatement:
				return convertContinueStatement(object);
			case IfStatement:
				return convertIfStatement(object);
			case SwitchStatement:
				return convertSwitchStatement(object);
			case SwitchCase:
				return convertSwitchCaseStatement(object);
			case ThrowStatement:
				return convertThrowStatement(object);
			case TryStatement:
				return convertTryStatement(object);
			case CatchClause:
				return convertCatchClause(object);
			case WhileStatement:
				return convertWhileStatement(object);
			case DoWhileStatement:
				return convertDoWhileStatement(object);
			// Expressions	
			case ThisExpression:
				return convertThisExpression(object);
			case ArrayExpression:
				return convertArrayExpression(object);
			case ObjectExpression:
				return convertObjectExpression(object);
			case Property:// related to ObjectExpression
				return convertPropertyExpression(object);
			case FunctionExpression:
				return convertFunctionExpression(object);
			case UnaryExpression://intentional
			case UpdateExpression:
				return convertUnaryOperation(object);
			case BinaryExpression: //intentional
			case LogicalExpression:
				return convertBinaryExpression(object);
			case AssignmentExpression:
				return convertAssignmentExpression(object);
			case MemberExpression:
				return convertMemberExpression(object);
			case ConditionalExpression:
				return convertConditionalExpression(object);
			case CallExpression:
				return convertCallExpression(object);
			case SequenceExpression:
				return convertSequenceExpression(object);
			case YieldExpression:
				return convertYieldExpression(object);
			case NewExpression:
				return convertNewExpression(object);
			case ArrowFunctionExpression:
				return convertArrowFunctionExpression(object);
			default: 
				throw new UnimplementedException(nodeType.getTypeString() + " conversion is not implemented");
				
		}
	}


	/* (non-Javadoc)
	 * @see org.eclipse.wst.jsdt.internal.esprima.EStreeVisitor#endVisit(jdk.nashorn.api.scripting.ScriptObjectMirror, org.eclipse.wst.jsdt.internal.esprima.ESTreeNodeTypes)
	 */
	public VisitOptions endVisit(final ScriptObjectMirror object, final ESTreeNodeTypes nodeType, final String key) {
		//Read the dom ast model objects from stack and connect them to parent.
		ASTNode current = nodes.pop();
		//Set source range for all the nodes.
		//we can safely move this to visit
		setRange(object, current);
		ASTNode parent = null;
		if( nodeType != ESTreeNodeTypes.Program){ //Nodes can be empty only for Program
			Assert.isTrue(!nodes.empty());
			parent = nodes.peek();
		}
		
		if(current instanceof Expression ){
			assignExpressionToParent((Expression)current, parent, key);
		}else
		if(current instanceof Statement){
			assignStatementToParent((Statement)current,parent, key);
		}else
		if(current instanceof VariableDeclaration){
			assignVariableDeclarationToParent((VariableDeclaration)current, parent);
		}else
		if(current.getNodeType() == CATCH_CLAUSE){
			assignCatchToTry(current, parent);
		}
		//clean-up the switch statement
		if(current == processingSwitchStatement){
			processingSwitchStatement = null;
		}
		return VisitOptions.CONTINUE;
	}

	
	private void setRange(final ScriptObjectMirror object, final ASTNode node){
		Object o = object.getMember("range");
		if( !ScriptObjectMirror.isUndefined(o) ){
			ScriptObjectMirror range = (ScriptObjectMirror)o;
			Number x = (Number) range.getSlot(0);
			Number y = (Number) range.getSlot(1);
			node.setSourceRange(x.intValue(), y.intValue()-x.intValue());
		}
	}
	
	private void assignStatementToParent(final Statement statement, final ASTNode parent, final String key){
		switch(parent.getNodeType()){
			case JAVASCRIPT_UNIT:
				JavaScriptUnit unit = (JavaScriptUnit)parent;
				unit.statements().add(statement);
				break;
			case FUNCTION_EXPRESSION:
				FunctionExpression fe = (FunctionExpression)parent;
				//assign to contained FunctionDeclaration
				fe.getMethod().setBody((Block) statement);
				break;
			case ARROW_FUNCTION_EXPRESSION:
				ArrowFunctionExpression af = (ArrowFunctionExpression) parent;
				af.setBody((Block)statement);
				break;
			case BLOCK:
				Block b = (Block) parent;
				b.statements().add(statement);
				break;
			case WITH_STATEMENT:
				WithStatement ws = (WithStatement)parent;
				ws.setBody(statement);
				break;
			case LABELED_STATEMENT:
				LabeledStatement ls = (LabeledStatement)parent;
				ls.setBody(statement);
				break;
			case IF_STATEMENT:
				IfStatement is = (IfStatement)parent;
				if(key!= null && key.equals("consequent")){
					is.setElseStatement(statement);
				}else{
					is.setThenStatement(statement);
				}
				break;
			case SWITCH_STATEMENT:
				SwitchStatement ss  = (SwitchStatement) parent;
				if(statement.getNodeType() != SWITCH_CASE){
					// case is added during #convertSwitchCase to keep order.
					ss.statements().add(statement);
				}
				break;
			case SWITCH_CASE:
				// all statements on the switchcase goes into switch.
				processingSwitchStatement.statements().add(statement);
				break;
			case CATCH_CLAUSE:
				CatchClause cc = (CatchClause)parent;
				cc.setBody((Block)statement);
				break;
			case TRY_STATEMENT:
				TryStatement ts = (TryStatement)parent;
				if("block".equals(key)){
					ts.setBody((Block)statement);
				}else{
					ts.setFinally((Block)statement);
				}
				break;
			case WHILE_STATEMENT:
				WhileStatement whileS = (WhileStatement)parent;
				whileS.setBody(statement);
				break;
			case DO_STATEMENT:
				DoStatement ds = (DoStatement)parent;
				ds.setBody(statement);
				break;
			default:
				throw new UnimplementedException("Assigning "+statement + " to "+parent+ " is not handled");	
				}
	}
	
	
	private void assignVariableDeclarationToParent(VariableDeclaration declaration, ASTNode parent) {
		switch(parent.getNodeType()){
			
			case VARIABLE_DECLARATION_STATEMENT:
				VariableDeclarationStatement vd = (VariableDeclarationStatement)parent;
				vd.fragments().add(declaration);
				break;
			case FUNCTION_EXPRESSION:
				FunctionExpression fe = (FunctionExpression)parent;
				fe.getMethod().parameters().add((SingleVariableDeclaration)declaration);
				break;
			default:
				throw new UnimplementedException("Assigning "+ declaration + " to "+parent+ " is not handled");	
		}
		
	}
	
	private void assignExpressionToParent(final Expression expression, final ASTNode parent, final String key ){
		
		switch (parent.getNodeType()) {
			case EXPRESSION_STATEMENT :
				((ExpressionStatement)parent).setExpression(expression);
				break;
			case ASSIGNMENT:
				if(key != null && key.equals("left")){
					((Assignment)parent).setLeftHandSide(expression);
				}else{
					((Assignment)parent).setRightHandSide(expression);
				}
				break;
			case VARIABLE_DECLARATION_FRAGMENT:
				if(expression.getNodeType() == SIMPLE_NAME){
					((VariableDeclarationFragment)parent).setName((SimpleName)expression);	
				}else{
					((VariableDeclarationFragment)parent).setInitializer(expression);
				}
				break;
			case ARRAY_INITIALIZER:
				((ArrayInitializer)parent).expressions().add(expression);
				break;
			case OBJECT_LITERAL:
				((ObjectLiteral)parent).fields().add(expression);
				break;
			case OBJECT_LITERAL_FIELD:
				if(key!= null && key.equals("key")){
					((ObjectLiteralField)parent).setFieldName(expression);
				}else{
					((ObjectLiteralField)parent).setInitializer(expression);					
				}
				break;
			case FUNCTION_EXPRESSION:
				FunctionExpression fe = (FunctionExpression)parent;
				//assign to contained FunctionDeclaration
				fe.getMethod().setName((SimpleName) expression);
				break;
			case POSTFIX_EXPRESSION:
				((PostfixExpression)parent).setOperand(expression);
				break;
			case PREFIX_EXPRESSION:
				((PrefixExpression)parent).setOperand(expression);
				break;
			case INFIX_EXPRESSION:
				if(key != null && key.equals("left")){
					((InfixExpression)parent).setLeftOperand(expression);
				}else{
					((InfixExpression)parent).setRightOperand(expression);
				}
				break;
			case FIELD_ACCESS:
				if(key != null && key.equals("object")){
					((FieldAccess)parent).setExpression(expression);
				}else{
					((FieldAccess)parent).setName((SimpleName)expression);
				}
				break;
			case ARRAY_ACCESS:
				if(key != null && key.equals("object")){
					((ArrayAccess)parent).setArray(expression);
				}else{
					((ArrayAccess)parent).setIndex(expression);
				}
				break;
			case CONDITIONAL_EXPRESSION:
				ConditionalExpression conditional = (ConditionalExpression)parent;
				if(key!= null && key.equals("test") ){
					conditional.setExpression(expression);
				}else if("consequent".equals(key)){
					conditional.setThenExpression(expression);
				}else{
					conditional.setElseExpression(expression);
				}
				break;
			case FUNCTION_INVOCATION:
				FunctionInvocation fi = (FunctionInvocation)parent;
				if(key != null && key.equals("callee")){
					fi.setExpression(expression);
				}else{
					fi.arguments().add(expression);
				}
				break;
			case LIST_EXPRESSION:
				((ListExpression)parent).expressions().add(expression);
				break;
			case YIELD_EXPRESSION:
				((YieldExpression)parent).setArgument(expression);
				break;
			case CLASS_INSTANCE_CREATION:
				ClassInstanceCreation ci = (ClassInstanceCreation)parent;
				if(key != null && key.equals("callee")){
					ci.setName((Name) expression);
				}else
				{
					ci.arguments().add(expression);
				}
				break;
			case ARROW_FUNCTION_EXPRESSION:
				ArrowFunctionExpression af = (ArrowFunctionExpression)parent;
				if(key!= null && key.equals("params")){
					af.parameters().add(expression);
				}else{
					af.setExpression(expression);
				}
				break;
			case WITH_STATEMENT:
				WithStatement ws = (WithStatement)parent;
				ws.setExpression(expression);
				break;
			case RETURN_STATEMENT:
				ReturnStatement rs = (ReturnStatement)parent;
				rs.setExpression(expression);
				break;
			case LABELED_STATEMENT:
				LabeledStatement ls = (LabeledStatement)parent;
				ls.setLabel((SimpleName)expression);
				break;
			case BREAK_STATEMENT:
				BreakStatement bs = (BreakStatement)parent;
				bs.setLabel((SimpleName)expression);
				break;
			case CONTINUE_STATEMENT:
				ContinueStatement cs = (ContinueStatement)parent;
				cs.setLabel((SimpleName)expression);
				break;
			case IF_STATEMENT:
				IfStatement is = (IfStatement)parent;
				is.setExpression(expression);
				break;
			case SWITCH_STATEMENT:
				SwitchStatement ss = (SwitchStatement)parent;
				ss.setExpression(expression);
				break;
			case SWITCH_CASE:
				SwitchCase sc = (SwitchCase) parent;
				if(key != null && key.equals("test")){
					sc.setExpression(expression);
				}
				break;
			case THROW_STATEMENT:
				ThrowStatement ts = (ThrowStatement)parent;
				ts.setExpression(expression);
				break;
			case CATCH_CLAUSE:
				CatchClause c =(CatchClause) parent;
				SingleVariableDeclaration d = ast.newSingleVariableDeclaration();
				d.setName((SimpleName) expression);
				c.setException(d);
				break;
			case WHILE_STATEMENT:
				WhileStatement whileS = (WhileStatement)parent;
				whileS.setExpression(expression);
				break;
			case DO_STATEMENT:
				DoStatement ds = (DoStatement)parent;
				ds.setExpression(expression);
				break;
			default :
				throw new UnimplementedException("Assigning "+expression + " to "+parent+ " is not handled");	
		}
	}

	private void assignCatchToTry(ASTNode current, ASTNode parent) {
		TryStatement ts = (TryStatement) parent;
		ts.catchClauses().add(current);
	}
	
	


	private VisitOptions convertLiteral(final ScriptObjectMirror object) {
		Object value = object.getMember("value");
		String raw = (String)object.getMember("raw");
		Expression literal = null;
		if(value instanceof Number ){
			literal = ast.newNumberLiteral(raw);
		}
		if(value instanceof Boolean){
			literal = ast.newBooleanLiteral(raw);
		}
		if(value instanceof String){
			literal = ast.newStringLiteral(raw);
		}
		if(value == null ){
			literal = ast.newNullLiteral();
		}
		
		if(literal == null ){
			throw new UnimplementedException("Failed to translate Literal " + value);
		}
		else{
			nodes.push(literal);
		}
		return VisitOptions.CONTINUE;
	}
	 
	private VisitOptions convertProgram(final ScriptObjectMirror object){
		root = ast.newJavaScriptUnit();
		nodes.push(root);	
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertVariableDeclaration(final ScriptObjectMirror object) {
		String kind = (String) object.getMember("kind");
		final VariableDeclarationStatement e = ast.newVariableDeclarationStatement();
		if(kind.equals("let")){
			e.setKind(Kind.LET);
		}else if(kind.equals("const")){
			e.setKind(Kind.CONST);
		}else{
			e.setKind(Kind.VAR);
		}
		nodes.push(e);
		return VisitOptions.CONTINUE;	
	}
	
	private VisitOptions convertVariableDeclarator(final ScriptObjectMirror object) {
		final VariableDeclarationFragment f  = ast.newVariableDeclarationFragment();
		nodes.push(f);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertIdentifier(final ScriptObjectMirror object) {
		final String s = (String)object.getMember("name");
		final SimpleName name = ast.newSimpleName(s);
		nodes.push(name);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertExpressionStatement(final ScriptObjectMirror object) {
		final ExpressionStatement es = ast.newExpressionStatement();
		nodes.push(es);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertAssignmentExpression(final ScriptObjectMirror object) {
		final Assignment a = ast.newAssignment();
		final String op = (String) object.getMember("operator");
		a.setOperator(Assignment.Operator.toOperator(op));
		nodes.push(a);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertThisExpression(final ScriptObjectMirror object) {
		final ThisExpression t = ast.newThisExpression();
		nodes.push(t);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertArrayExpression(final ScriptObjectMirror object) {
		ArrayInitializer ai = ast.newArrayInitializer();
		nodes.push(ai);
		return VisitOptions.CONTINUE;
	}

	private VisitOptions convertObjectExpression(final ScriptObjectMirror object) {
		ObjectLiteral o = ast.newObjectLiteral();
		nodes.push(o);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertPropertyExpression(final ScriptObjectMirror object) {
		ObjectLiteralField of = ast.newObjectLiteralField();
		String kind = (String) object.getMember("kind");
		FieldKind k = null;
		if("init".equals(kind)){
			k = FieldKind.INIT;
		}else if("get".equals(kind)){
			k = FieldKind.GET;
		}else if("set".equals(kind)){
			k = FieldKind.SET;
		}
		of.setKind(k);
		nodes.push(of);
		return VisitOptions.CONTINUE; 
	}
	
	private VisitOptions convertFunctionExpression(final ScriptObjectMirror object) {
		// We break from the usual pattern and create a functionDeclaration. 
		// has a single object type for function expression while DOM AST 
		// needs two objects for the same result. All further assignments needs 
		// to handle this properly
		FunctionExpression fe = ast.newFunctionExpression();
		FunctionDeclaration d = ast.newFunctionDeclaration();
		fe.setMethod(d);
		nodes.add(fe);
		return VisitOptions.CONTINUE;
	}
	

	private VisitOptions convertUnaryOperation(final ScriptObjectMirror object) {
		final Boolean isPrefix = (Boolean) object.getMember("prefix");
		final String operator = (String) object.getMember("operator");
		if(isPrefix){
			PrefixExpression pe= ast.newPrefixExpression();
			pe.setOperator( PrefixExpression.Operator.toOperator(operator));
			nodes.push(pe);
		}else{
			PostfixExpression po = ast.newPostfixExpression();
			po.setOperator( PostfixExpression.Operator.toOperator(operator));
			nodes.push(po);
		}
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertBinaryExpression(final ScriptObjectMirror object) {
		final String operator = (String) object.getMember("operator");
		InfixExpression ie = ast.newInfixExpression();
		ie.setOperator(InfixExpression.Operator.toOperator(operator));
		nodes.push(ie);
		return VisitOptions.CONTINUE;
	}

	private VisitOptions convertMemberExpression(final ScriptObjectMirror object) {
		Boolean computed = (Boolean)object.getMember("computed");
		if(computed){
			ArrayAccess aa = ast.newArrayAccess();
			nodes.push(aa);
		}else{
			FieldAccess fa = ast.newFieldAccess();
			nodes.push(fa);
		}
		return VisitOptions.CONTINUE; 
	}
	
	private VisitOptions convertConditionalExpression(final ScriptObjectMirror object) {
		ConditionalExpression ce = ast.newConditionalExpression();
		nodes.push(ce);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertCallExpression(final ScriptObjectMirror object) {
		FunctionInvocation fi = ast.newFunctionInvocation();
		nodes.push(fi);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertSequenceExpression(final ScriptObjectMirror object) {
		ListExpression le = ast.newListExpression();
		nodes.push(le);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertYieldExpression(final ScriptObjectMirror object) {
		Boolean isDelegate = (Boolean)object.getMember("delegate");
		YieldExpression ye = ast.newYieldExpression();
		ye.setDelegate(isDelegate);
		nodes.push(ye);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertNewExpression(final ScriptObjectMirror object) {
		ClassInstanceCreation ci = ast.newClassInstanceCreation();
		nodes.push(ci);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertArrowFunctionExpression(final ScriptObjectMirror object) {
		ArrowFunctionExpression af = ast.newArrowFunctionExpression();
		nodes.push(af);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertBlockStatement(final ScriptObjectMirror object) {
		Block b = ast.newBlock();
		nodes.push(b);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertEmptyStatement(final ScriptObjectMirror object) {
		EmptyStatement es = ast.newEmptyStatement();
		nodes.push(es);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertDebuggerStatememt(final ScriptObjectMirror object) {
		DebuggerStatement ds = ast.newDebuggerStatement();
		nodes.push(ds);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertWithStatement(final ScriptObjectMirror object) {
		WithStatement ws = ast.newWithStatement();
		nodes.push(ws);
		return VisitOptions.CONTINUE;
	}

	private VisitOptions convertReturnStatement(final ScriptObjectMirror object) {
		ReturnStatement rs = ast.newReturnStatement();
		nodes.push(rs);
		return VisitOptions.CONTINUE;
	}

	private VisitOptions converLabeledStatement(final ScriptObjectMirror object) {
		LabeledStatement ls = ast.newLabeledStatement();
		nodes.push(ls);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertBreakStatement(final ScriptObjectMirror object) {
		BreakStatement bs = ast.newBreakStatement();
		nodes.push(bs);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertContinueStatement(final ScriptObjectMirror object) {
		ContinueStatement cs = ast.newContinueStatement();
		nodes.push(cs);
		return VisitOptions.CONTINUE;
	}

	private VisitOptions convertIfStatement(final ScriptObjectMirror object) {
		IfStatement is = ast.newIfStatement();
		nodes.push(is);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertSwitchStatement(final ScriptObjectMirror object) {
		SwitchStatement ss = ast.newSwitchStatement();
		this.processingSwitchStatement = ss;
		nodes.push(ss);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertSwitchCaseStatement(final ScriptObjectMirror object) {
		SwitchCase sc = ast.newSwitchCase();
		nodes.push(sc);
		if(processingSwitchStatement == null ){
			throw new IllegalStateException("Case statement without a switch");
		}
		//add the case to switch statement here so that the order is correct
		processingSwitchStatement.statements().add(sc);
		return VisitOptions.CONTINUE;
	}

	private VisitOptions convertThrowStatement(final ScriptObjectMirror object) {
		ThrowStatement ts = ast.newThrowStatement();
		nodes.push(ts);
		return VisitOptions.CONTINUE;
	}

	private VisitOptions convertCatchClause(final ScriptObjectMirror object) {
		CatchClause cc = ast.newCatchClause();
		nodes.push(cc);
		return VisitOptions.CONTINUE;
	}

	private VisitOptions convertTryStatement(final ScriptObjectMirror object) {
		TryStatement ts = ast.newTryStatement();
		nodes.push(ts);
		return VisitOptions.CONTINUE;
	}
	
	private VisitOptions convertWhileStatement(final ScriptObjectMirror object) {
		WhileStatement ws = ast.newWhileStatement();
		nodes.push(ws);
		return VisitOptions.CONTINUE;
	}

	private VisitOptions convertDoWhileStatement(final ScriptObjectMirror object) {
		DoStatement ds = ast.newDoStatement();
		nodes.push(ds);
		return VisitOptions.CONTINUE;
	}
}
