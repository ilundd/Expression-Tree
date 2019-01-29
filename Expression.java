import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.*;
import java.util.function.*;

class Expression {
	private String _type;
	private String _value;
	private Expression _left, _right;

	private Expression(String type, String value) {
		this(type, value, null, null);
	}

	private Expression(String type, String value, Expression left, Expression right) {
		_type = type;
		_value = value;
		_left = left;
		_right = right;
	}

	/**
	 * Creates an operator expression.
	 */
	public static Expression Operator(Expression left, String operator, Expression right) {
		return new Expression("Operator", operator, left, right);
	}

	/**
	 * Creates a number expression.
	 */
	public static Expression Number(double value) {
		return new Expression("Number", Double.toString(value));
	}

	/**
	 * Creates a variable expression.
	 */
	public static Expression Variable(String name) {
		return new Expression("Variable", name);
	}

	/**
	 * Very quick-and-dirty expression parser; doesn't really do any error checking.
	 * But it's enough to build an Expression from a (known-to-be-correct) String.
	 */
	public static Expression quickParse(String input) {
		StreamTokenizer tokenizer = new StreamTokenizer(new StringReader(input));
		tokenizer.ordinaryChar('-');
		tokenizer.ordinaryChar('/');
		Stack<Character> operators = new Stack<>();
		Stack<Expression> operands = new Stack<>();
		try {
			tokenizer.nextToken();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		while (tokenizer.ttype != StreamTokenizer.TT_EOF) {
			int prec = 2;
			switch (tokenizer.ttype) {
			case StreamTokenizer.TT_NUMBER:
				operands.push(Number(tokenizer.nval));
				break;
			case StreamTokenizer.TT_WORD:
				operands.push(Variable(tokenizer.sval));
				break;
			case '^':
			case '(':
				operators.push((char) tokenizer.ttype);
				break;
			case ')':
				while (operators.peek() != '(')
					poperator(operators, operands);
				operators.pop();
				break;
			case '+':
			case '-':
				prec = 1; // fall thru
			case '*':
			case '/':
				while (!operators.empty()) {
					char top = operators.peek();
					int topPrec = (top == '^') ? 3 : (top == '*' || top == '/') ? 2 : 1;
					if (top == '(' || topPrec < prec)
						break;
					poperator(operators, operands);
				}
				operators.push((char) tokenizer.ttype);
				break;
			default:
				throw new RuntimeException("wat");
			}
			try {
				tokenizer.nextToken();
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		while (!operators.empty()) {
			poperator(operators, operands);
		}
		return operands.pop();
	}

	private static void poperator(Stack<Character> operators, Stack<Expression> operands) {
		Expression r = operands.pop();
		Expression l = operands.pop();
		operands.push(Operator(l, operators.pop() + "", r));
	}

	// These can be used to quickly check if an Expression is an Operator, Number,
	// or Variable.
	public boolean isOperator() {
		return _type.equals("Operator");
	}

	public boolean isNumber() {
		return _type.equals("Number");
	}

	public boolean isVariable() {
		return _type.equals("Variable");
	}

	/**
	 * For Numbers, converts the _value to a double and returns it. Will crash for
	 * non-Numbers.
	 */
	private double getNumberValue() {
		return Double.parseDouble(_value);
	}

	/**
	 * Recursively clones an entire Expression Infix. Note how this method works:
	 * operators are the recursive case, and numbers and variables are base cases.
	 */
	public Expression clone() {
		if (this.isOperator()) {
			return Expression.Operator(_left.clone(), _value, _right.clone());
		} else if (this.isVariable()) {
			return Expression.Variable(_value);
		} else {
			return Expression.Number(getNumberValue());
		}
	}

	///////////////////////////////////////////////////////////////////////////////////////////
	

	/**
	 * Converts this expression to an infix expression representation.
	 */
	public String toString() {
		
		if (this.isNumber()){
			return _value;
		} else if (this.isVariable()) {
			return _value;
		} else {
			return "(" + _left.toString() + " " + _value + " " + _right.toString() + ")";
		}
		
		
	}

	/**
	 * Converts this expression to a postfix expression representation.
	 */
	public String toPostfix() {

		if (this.isNumber()){
			return _value;
		} else if (this.isVariable()) {
			return _value;
		} else {
			return _left.toPostfix() + " " + _right.toPostfix() + " " + _value;
		}
		
	}

	/**
	 * Given the variables map (which tells what values each variable has),
	 * evaluates the expression and returns the computed value.
	 */
	public double evaluate(Map<String, Double> variables) {

		if(this.isNumber()){
			return getNumberValue();
		} else if(this.isVariable()){
			if (variables.containsKey(_value)){
				return variables.get(_value);
			} else 
				throw new ExpressionError("Variable: " + _value + " is not assigned a value!");
		} else {
			switch(_value){

				case "+":
					return _left.evaluate(variables) + _right.evaluate(variables);
				case "-":
					return _left.evaluate(variables) - _right.evaluate(variables);
				case "/":
					return _left.evaluate(variables) / _right.evaluate(variables);
				case "*":
					return _left.evaluate(variables) * _right.evaluate(variables);
				case "^":
					return Math.pow(_left.evaluate(variables), _right.evaluate(variables));
				default:
					throw new ExpressionError("Operator: " + _value + " is not a valid operator!");
			}
		}
	}

	/**
	 * Creates a new Expression that is the reciprocal of this one.
	 */
	public Expression reciprocal() {

		if (this.isNumber()){
			return Number(1.0/getNumberValue());
		} else if (this.isOperator() && _value.equals("/")) {
				return Operator(_right.clone(), _value, _left.clone());
		} else {
			return Operator(Number(1.0), "/", this.clone());
		}
	}

	/**
	 * Gets a set of all variables which appear in this expression.
	 */
	public Set<String> getVariables() {
		Set<String> variables = new HashSet<>();
		variableSearch(variables, this);
		return variables;
	}

	private void variableSearch(Set<String> set, Expression exp) {

		if (exp == null)
			return;

		variableSearch(set, exp._left);

		if (exp.isVariable())
			set.add(exp._value);

		variableSearch(set, exp._right);
	}

	/**
	 * Constructs a new Expression of the form: (numbers[0] * numbers[1] * ...
	 * numbers[n-1]) ^ (1 / n) and returns it.
	 */
	public static Expression geometricMean(double[] numbers) {
		Stack<Expression> stack = new Stack<Expression>();
		Expression temp;

		if (numbers.length == 1){
			return Operator(Number(numbers[0]), "^", Number(numbers.length).reciprocal());
		} else if (numbers.length == 0) {
			return Number(0);
		}

		for(int i = 0; i < numbers.length; i++){
			if (i == numbers.length - 1){
				temp = Operator(stack.pop(), "^", Number(numbers.length).reciprocal());
				stack.push(temp);
			} else {
				if (i < numbers.length - 1){
					temp = Operator(Number(numbers[i]), "*", Number(numbers[i+1]));
					if (!stack.isEmpty())
						temp._left = stack.pop();
					if (!stack.isEmpty())
						temp._right = stack.pop();
					stack.push(temp);
				}
			}


		}
		temp = stack.pop();
		return temp;
	}

	/**
	 * EXTRA CREDIT: converts this expression to an infix expression representation,
	 * but only places parentheses where needed to override the order of operations.
	 */
	public String toNiceString() {
		// TODO
		return "<not implemented>";
	}
}