/**
 * AST Simplifier
 *
 * Converts the original AST to a simplified logical AST by:
 * - Removing all token offsets and position information
 * - Removing parentheses (tree structure preserves precedence)
 * - Flattening compound expressions where appropriate
 * - Simplifying redundant wrapper nodes
 */

import * as AST from '../parser/ast';
import * as LAST from './types';

/**
 * Convert an original AST node to a logical AST node
 */
export function simplify(node: AST.ASTNode | null | undefined): LAST.Node | null {
  if (!node) return null;

  const simplifier = new ASTSimplifier();
  return simplifier.simplifyNode(node);
}

/**
 * Convert a program AST to a logical program
 */
export function simplifyProgram(program: any): LAST.Program {
  const simplifier = new ASTSimplifier();
  return simplifier.simplifyProgram(program);
}

class ASTSimplifier {
  simplifyNode(node: AST.ASTNode): LAST.Node | null {
    if (!node) return null;
    if (!node.type) {
      console.warn('Node without type:', node);
      return null;
    }
    switch (node.type) {
      // Expressions
      case 'Literal':
        return this.simplifyLiteral(node as AST.LiteralExpression);
      case 'Identifier':
        return this.simplifyIdentifier(node as AST.IdentifierExpression);
      case 'BinaryExpression':
        return this.simplifyBinary(node as AST.BinaryExpression);
      case 'UnaryExpression':
        return this.simplifyUnary(node as AST.UnaryExpression);
      case 'AssignmentExpression':
        return this.simplifyAssignment(node as AST.AssignmentExpression);
      case 'MemberExpression':
        return this.simplifyMember(node as AST.MemberExpression);
      case 'CallExpression':
        return this.simplifyCall(node as AST.CallExpression);
      case 'ArrayExpression':
        return this.simplifyArray(node as AST.ArrayExpression);
      case 'ObjectConstructorExpression':
        return this.simplifyObjectConstructor(node as AST.ObjectConstructorExpression);
      case 'RangeExpression':
        return this.simplifyRange(node as AST.RangeExpression);
      case 'LambdaExpression':
        return this.simplifyLambda(node as AST.LambdaExpression);
      case 'SetExpression':
        return this.simplifySet(node as AST.SetExpression);

      // Control flow
      case 'IfExpression':
        return this.simplifyIf(node as AST.IfExpression);
      case 'ForExpression':
        return this.simplifyFor(node as AST.ForExpression);
      case 'LoopExpression':
        return this.simplifyLoop(node as AST.LoopExpression);
      case 'CaseExpression':
        return this.simplifyCase(node as AST.CaseExpression);
      case 'BreakExpression':
        return { type: 'Break' } as LAST.Break;
      case 'ContinueExpression':
        return { type: 'Continue' } as LAST.Continue;
      case 'ReturnExpression':
        return this.simplifyReturn(node as AST.ReturnExpression);

      // Concurrent constructs
      case 'SpawnExpression':
        return this.simplifySpawn(node as AST.SpawnExpression);
      case 'RaceExpression':
        return this.simplifyRace(node as AST.RaceExpression);
      case 'SyncExpression':
        return this.simplifySync(node as AST.SyncExpression);
      case 'BranchExpression':
        return this.simplifyBranch(node as AST.BranchExpression);

      // Compounds and blocks
      case 'CompoundExpression':
        return this.simplifyCompound(node as AST.CompoundExpression);
      case 'IdentedCompoundExpression':
        return this.simplifyIdentedCompound(node as AST.IdentedCompoundExpression);
      case 'BlockExpression':
        return this.simplifyBlock(node as AST.BlockExpression);

      // Parenthesized expressions are unwrapped
      case 'ParenthesizedExpression':
        return this.simplifyNode((node as AST.ParenthesizedExpression).expression);

      // Declarations
      case 'ConstantDeclaration':
        return this.simplifyConstant(node as AST.ConstantDeclaration);
      case 'VariableDeclaration':
        return this.simplifyVariable(node as AST.VariableDeclaration);
      case 'FunctionDeclaration':
        return this.simplifyFunction(node as AST.FunctionDeclaration);
      case 'DataStructureDeclaration':
        return this.simplifyDataStructure(node as AST.DataStructureDeclaration);

      default:
        console.warn(`Unknown node type: ${node.type}`);
        return null;
    }
  }

  private simplifyExpression(node: AST.Expression): LAST.Expression {
    if (!node) {
      // Return a placeholder for null/undefined expressions
      return { type: 'Identifier', name: '_null_' } as LAST.Identifier;
    }
    const result = this.simplifyNode(node);
    if (!result) {
      // Return a placeholder for unknown expressions
      return { type: 'Identifier', name: '_unknown_' } as LAST.Identifier;
    }
    return result as LAST.Expression;
  }

  private simplifyLiteral(node: AST.LiteralExpression): LAST.Literal {
    return {
      type: 'Literal',
      value: node.value,
      literalType: node.literalType
    };
  }

  private simplifyIdentifier(node: AST.IdentifierExpression): LAST.Identifier {
    return {
      type: 'Identifier',
      name: node.name
    };
  }

  private simplifyBinary(node: AST.BinaryExpression): LAST.BinaryOp {
    return {
      type: 'BinaryOp',
      operator: node.operator,
      left: this.simplifyExpression(node.left),
      right: this.simplifyExpression(node.right)
    };
  }

  private simplifyUnary(node: AST.UnaryExpression): LAST.UnaryOp {
    return {
      type: 'UnaryOp',
      operator: node.operator,
      operand: this.simplifyExpression(node.operand)
    };
  }

  private simplifyAssignment(node: AST.AssignmentExpression): LAST.Assignment {
    return {
      type: 'Assignment',
      operator: node.operator,
      left: this.simplifyExpression(node.left),
      right: this.simplifyExpression(node.right)
    };
  }

  private simplifyMember(node: AST.MemberExpression): LAST.MemberAccess {
    return {
      type: 'MemberAccess',
      object: this.simplifyExpression(node.object),
      property: this.simplifyExpression(node.property),
      computed: node.computed
    };
  }

  private simplifyCall(node: AST.CallExpression): LAST.Call {
    return {
      type: 'Call',
      callee: this.simplifyExpression(node.callee),
      arguments: node.arguments.map(arg => this.simplifyExpression(arg))
    };
  }

  private simplifyArray(node: AST.ArrayExpression): LAST.Array {
    return {
      type: 'Array',
      elements: node.elements.map(el => this.simplifyExpression(el))
    };
  }

  private simplifyObjectConstructor(node: AST.ObjectConstructorExpression): LAST.ObjectConstruction {
    return {
      type: 'ObjectConstruction',
      typeName: node.typeName,
      fields: node.fields.map(field => ({
        name: field.name,
        value: this.simplifyExpression(field.value)
      }))
    };
  }

  private simplifyRange(node: AST.RangeExpression): LAST.Range {
    return {
      type: 'Range',
      start: this.simplifyExpression(node.start),
      end: this.simplifyExpression(node.end)
    };
  }

  private simplifyLambda(node: AST.LambdaExpression): LAST.Lambda {
    return {
      type: 'Lambda',
      parameters: node.parameters.map(p => {
        // Lambda parameters might be identifiers or actual Parameter nodes
        if ((p as any).type === 'Parameter') {
          // It's a Parameter node
          return this.simplifyParameter(p as any);
        } else {
          // It's an identifier expression
          const id = p as AST.IdentifierExpression;
          return { name: id.name };
        }
      }),
      body: this.simplifyExpression(node.body)
    };
  }

  private simplifySet(node: AST.SetExpression): LAST.Set {
    return {
      type: 'Set',
      target: this.simplifyExpression(node.target),
      value: this.simplifyExpression(node.value)
    };
  }

  private simplifyIf(node: AST.IfExpression): LAST.If {
    return {
      type: 'If',
      condition: this.simplifyExpression(node.condition),
      thenBranch: node.thenBranch ? this.simplifyExpression(node.thenBranch) : undefined,
      elseBranch: node.elseBranch ? this.simplifyExpression(node.elseBranch) : undefined
    };
  }

  private simplifyFor(node: AST.ForExpression): LAST.For {
    return {
      type: 'For',
      variable: node.variable,
      indexVariable: node.indexVariable,
      iterable: this.simplifyExpression(node.iterable),
      body: this.simplifyExpression(node.body)
    };
  }

  private simplifyLoop(node: AST.LoopExpression): LAST.Loop {
    return {
      type: 'Loop',
      body: this.simplifyExpression(node.body)
    };
  }

  private simplifyCase(node: AST.CaseExpression): LAST.Case {
    return {
      type: 'Case',
      scrutinee: this.simplifyExpression(node.scrutinee),
      branches: node.branches.map(branch => ({
        pattern: branch.pattern === '_' ? '_' : this.simplifyExpression(branch.pattern),
        body: this.simplifyExpression(branch.body)
      }))
    };
  }

  private simplifyReturn(node: AST.ReturnExpression): LAST.Return {
    return {
      type: 'Return',
      value: node.value ? this.simplifyExpression(node.value) : undefined
    };
  }

  private simplifySpawn(node: AST.SpawnExpression): LAST.Spawn {
    return {
      type: 'Spawn',
      body: this.simplifyExpression(node.body)
    };
  }

  private simplifyRace(node: AST.RaceExpression): LAST.Race {
    return {
      type: 'Race',
      branches: node.branches.map(branch => this.simplifyExpression(branch))
    };
  }

  private simplifySync(node: AST.SyncExpression): LAST.Sync {
    return {
      type: 'Sync',
      operations: node.operations.map(op => this.simplifyExpression(op))
    };
  }

  private simplifyBranch(node: AST.BranchExpression): LAST.Branch {
    return {
      type: 'Branch',
      branches: node.branches.map(branch => this.simplifyExpression(branch))
    };
  }

  private simplifyCompound(node: AST.CompoundExpression): LAST.Block {
    return {
      type: 'Block',
      expressions: node.expressions.map(expr => this.simplifyExpression(expr))
    };
  }

  private simplifyIdentedCompound(node: AST.IdentedCompoundExpression): LAST.Block {
    return {
      type: 'Block',
      expressions: node.expressions.map(expr => this.simplifyExpression(expr))
    };
  }

  private simplifyBlock(node: AST.BlockExpression): LAST.Block {
    // BlockExpression wraps a compound, so unwrap it
    const body = this.simplifyExpression(node.body);
    if (body.type === 'Block') {
      return body;
    }
    // If body is not a block, wrap it
    return {
      type: 'Block',
      expressions: [body]
    };
  }

  // Declarations

  private simplifyConstant(node: AST.ConstantDeclaration): LAST.ConstDecl {
    return {
      type: 'ConstDecl',
      name: node.name,
      declaredType: node.declaredType ? this.simplifyType(node.declaredType) : undefined,
      initializer: node.initializer ? this.simplifyExpression(node.initializer) : undefined,
      specifiers: node.specifiers ? this.extractSpecifiers(node.specifiers) : undefined
    };
  }

  private simplifyVariable(node: AST.VariableDeclaration): LAST.VarDecl {
    return {
      type: 'VarDecl',
      name: node.name,
      declaredType: this.simplifyType(node.declaredType),
      initializer: node.initializer ? this.simplifyExpression(node.initializer) : undefined,
      specifiers: node.specifiers ? this.extractSpecifiers(node.specifiers) : undefined
    };
  }

  private simplifyFunction(node: AST.FunctionDeclaration): LAST.FunctionDecl {
    return {
      type: 'FunctionDecl',
      name: node.name,
      parameters: node.parameters.map(p => this.simplifyParameter(p)),
      returnType: node.returnType ? this.simplifyType(node.returnType) : undefined,
      body: node.body ? this.simplifyExpression(node.body) : { type: 'Identifier', name: '_empty_' } as LAST.Identifier,
      specifiers: node.postSpecifiers ? this.extractSpecifiers(node.postSpecifiers) : undefined
    };
  }

  private simplifyDataStructure(node: AST.DataStructureDeclaration): LAST.Declaration {
    const specifiers = node.nameSpecifiers ? this.extractSpecifiers(node.nameSpecifiers) :
                       node.kindSpecifiers ? this.extractSpecifiers(node.kindSpecifiers) :
                       node.postSpecifiers ? this.extractSpecifiers(node.postSpecifiers) : undefined;

    switch (node.kind) {
      case 'class':
        return {
          type: 'ClassDecl',
          name: node.name,
          members: this.simplifyMembers(node.body),
          specifiers,
          parents: node.argument ? this.extractParents(node.argument) : undefined
        } as LAST.ClassDecl;

      case 'struct':
        return {
          type: 'StructDecl',
          name: node.name,
          members: this.simplifyMembers(node.body),
          specifiers
        } as LAST.StructDecl;

      case 'interface':
        return {
          type: 'InterfaceDecl',
          name: node.name,
          members: this.simplifyMembers(node.body),
          specifiers
        } as LAST.InterfaceDecl;

      case 'enum':
        return {
          type: 'EnumDecl',
          name: node.name,
          members: this.simplifyEnumMembers(node.body),
          specifiers
        } as LAST.EnumDecl;

      default:
        // Fallback to class for unknown types
        return {
          type: 'ClassDecl',
          name: node.name,
          members: this.simplifyMembers(node.body),
          specifiers
        } as LAST.ClassDecl;
    }
  }

  private simplifyMembers(body: AST.Declaration[]): LAST.Declaration[] {
    return body.map(member => {
      const simplified = this.simplifyNode(member);
      return simplified as LAST.Declaration;
    }).filter(m => m !== null);
  }

  private simplifyEnumMembers(body: AST.Declaration[]): LAST.EnumMember[] {
    return body.map(member => {
      // Enum members are typically EnumMember nodes
      if ((member as any).type === 'EnumMember') {
        const enumMember = member as AST.EnumMember;
        return {
          name: enumMember.name,
          value: enumMember.value ? this.simplifyExpression(enumMember.value) : undefined
        };
      }
      // Fallback for other member types
      return { name: 'unknown' };
    });
  }

  // Helper functions

  private simplifyType(type: AST.TypeExpression): LAST.Type {
    return {
      type: 'Type',
      name: type.typeName,
      isOptional: type.isOptional,
      isArray: (type.arrayOffsets && type.arrayOffsets.length > 0) || undefined,
      arrayDimensions: type.arrayOffsets ? type.arrayOffsets.length : undefined,
      whereConstraint: type.whereConstraint ? this.simplifyWhereConstraint(type.whereConstraint) : undefined
    };
  }

  private simplifyWhereConstraint(constraint: AST.WhereConstraint): LAST.WhereConstraint {
    return {
      type: 'WhereConstraint',
      parameter: constraint.parameter,
      constraint: constraint.constraint
    };
  }

  private simplifyParameter(param: AST.Parameter): LAST.Parameter {
    return {
      name: param.name,
      paramType: param.paramType ? this.simplifyType(param.paramType) : undefined
    };
  }

  private extractSpecifiers(specList: AST.SpecifierList): string[] {
    return specList.specifiers;
  }

  private extractParents(arg: AST.Expression): LAST.Expression[] {
    // The argument could be a single parent or multiple (if Verse supports multiple inheritance)
    // For now, treat it as a single parent expression
    // If it's a compound with commas, we'd need to split it
    return [this.simplifyExpression(arg)];
  }

  simplifyProgram(program: any): LAST.Program {
    const result: LAST.Program = {
      type: 'Program',
      declarations: []
    };

    // Extract using paths
    if (program.usingStatements && program.usingStatements.length > 0) {
      result.usingPaths = program.usingStatements.map((stmt: any) => stmt.path);
    }

    // Simplify declarations
    if (program.declarations) {
      result.declarations = program.declarations
        .map((decl: any) => this.simplifyNode(decl))
        .filter((d: any) => d !== null) as LAST.Declaration[];
    }

    return result;
  }
}