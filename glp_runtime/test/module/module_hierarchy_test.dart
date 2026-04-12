import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart';

void main() {
  // Set prelude sources from programs/self.glp (same as GlpEngine constructor)
  final rootSelfGlp = File('../programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    setPreludeEnvironmentSource(rootSelfGlp.readAsStringSync());
  }
  // Helper: parse source into Module AST
  Module parseModule(String source) {
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    return parser.parseModule();
  }

  // Helper: create a temp directory structure for hierarchy tests
  // Returns the root directory path. Caller must clean up.
  Future<Directory> createTempHierarchy(Map<String, String> files) async {
    final tempDir = await Directory.systemTemp.createTemp('glp_hierarchy_test_');
    for (final entry in files.entries) {
      final file = File('${tempDir.path}/${entry.key}');
      await file.parent.create(recursive: true);
      await file.writeAsString(entry.value);
    }
    return tempDir;
  }

  group('Phase 2 - 2a: self.glp chain discovery', () {
    test('discovers self.glp chain from root to target directory', () async {
      final tempDir = await createTempHierarchy({
        'self.glp': 'TypeA ::= a ; b.',
        'sub/self.glp': 'TypeB ::= x ; y.',
        'sub/module.glp': 'procedure foo(TypeA?, TypeB?).\nfoo(A, B) :- true | true.',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/sub/module.glp',
          rootDir: tempDir.path,
        );

        expect(chain.length, 2);
        expect(chain[0], '${tempDir.path}/self.glp');
        expect(chain[1], '${tempDir.path}/sub/self.glp');
      } finally {
        await tempDir.delete(recursive: true);
      }
    });

    test('returns empty chain when no self.glp exists', () async {
      final tempDir = await createTempHierarchy({
        'module.glp': 'procedure foo(Integer?, Integer).\nfoo(N, R) :- true | R := N?.',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/module.glp',
          rootDir: tempDir.path,
        );

        expect(chain, isEmpty);
      } finally {
        await tempDir.delete(recursive: true);
      }
    });

    test('skips missing intermediate self.glp', () async {
      // root/self.glp exists, root/sub/ has no self.glp, root/sub/deep/self.glp exists
      final tempDir = await createTempHierarchy({
        'self.glp': 'TypeA ::= a.',
        'sub/deep/self.glp': 'TypeC ::= c.',
        'sub/deep/module.glp': 'procedure foo(TypeA?, TypeC?).\nfoo(A, C) :- true | true.',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/sub/deep/module.glp',
          rootDir: tempDir.path,
        );

        expect(chain.length, 2);
        expect(chain[0], '${tempDir.path}/self.glp');
        expect(chain[1], '${tempDir.path}/sub/deep/self.glp');
      } finally {
        await tempDir.delete(recursive: true);
      }
    });

    test('does not include self.glp from target file directory if target IS self.glp', () async {
      // When compiling self.glp itself, it should not see its own self.glp in the chain
      // (the chain is for ancestors only; the module's own definitions come from parsing it)
      final tempDir = await createTempHierarchy({
        'self.glp': 'TypeA ::= a.',
        'sub/self.glp': 'TypeB ::= b.',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/sub/self.glp',
          rootDir: tempDir.path,
        );

        // When target IS self.glp, only ancestors above it
        expect(chain.length, 1);
        expect(chain[0], '${tempDir.path}/self.glp');
      } finally {
        await tempDir.delete(recursive: true);
      }
    });
  });

  group('Phase 2 - 2b: type scope assembly from ancestor chain', () {
    test('types from ancestor self.glp are visible in descendant module', () async {
      final tempDir = await createTempHierarchy({
        'self.glp': 'Response ::= accept(Channel) ; no.',
        'sub/module.glp': 'procedure foo(Response?, Constant).\nfoo(R, C) :- true | true.',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/sub/module.glp',
          rootDir: tempDir.path,
        );
        final moduleSource = await File('${tempDir.path}/sub/module.glp').readAsString();
        final module = parseModule(moduleSource);
        final env = assembleTypeScope(chain: chain, module: module);

        // Response from root/self.glp should be visible
        expect(env.hasType('Response'), isTrue);
        final responseDef = env.getType('Response');
        expect(responseDef, isNotNull);
        expect(responseDef!.alternatives.length, 2);
      } finally {
        await tempDir.delete(recursive: true);
      }
    });

    test('types from multiple ancestor levels are all visible', () async {
      final tempDir = await createTempHierarchy({
        'self.glp': 'Response ::= accept(Channel) ; no.',
        'sub/self.glp': 'AgentContent ::= befriend(Constant, Response?).',
        'sub/module.glp': 'procedure foo(AgentContent?, Response?).\nfoo(A, R) :- true | true.',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/sub/module.glp',
          rootDir: tempDir.path,
        );
        final moduleSource = await File('${tempDir.path}/sub/module.glp').readAsString();
        final module = parseModule(moduleSource);
        final env = assembleTypeScope(chain: chain, module: module);

        // Both Response (from root) and AgentContent (from sub) should be visible
        expect(env.hasType('Response'), isTrue);
        expect(env.hasType('AgentContent'), isTrue);
      } finally {
        await tempDir.delete(recursive: true);
      }
    });
  });

  group('Phase 2 - 2c: shadowing', () {
    test('child self.glp shadows parent type definition', () async {
      final tempDir = await createTempHierarchy({
        'self.glp': 'Response ::= accept(Channel) ; no.',
        'sub/self.glp': 'Response ::= accept(Channel) ; no ; maybe.',
        'sub/module.glp': 'procedure foo(Response?).\nfoo(R) :- true | true.',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/sub/module.glp',
          rootDir: tempDir.path,
        );
        final moduleSource = await File('${tempDir.path}/sub/module.glp').readAsString();
        final module = parseModule(moduleSource);
        final env = assembleTypeScope(chain: chain, module: module);

        // Should see child's 3-alternative Response, not parent's 2-alternative
        final responseDef = env.getType('Response');
        expect(responseDef, isNotNull);
        expect(responseDef!.alternatives.length, 3);
      } finally {
        await tempDir.delete(recursive: true);
      }
    });

    test('module own type shadows ancestor type', () async {
      final tempDir = await createTempHierarchy({
        'self.glp': 'Foo ::= a ; b.',
        'module.glp': 'Foo ::= x ; y ; z.\nprocedure bar(Foo?).\nbar(F) :- true | true.',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/module.glp',
          rootDir: tempDir.path,
        );
        final moduleSource = await File('${tempDir.path}/module.glp').readAsString();
        final module = parseModule(moduleSource);
        final env = assembleTypeScope(chain: chain, module: module);

        // Module's own Foo (3 alternatives) should shadow ancestor's (2 alternatives)
        final fooDef = env.getType('Foo');
        expect(fooDef, isNotNull);
        expect(fooDef!.alternatives.length, 3);
      } finally {
        await tempDir.delete(recursive: true);
      }
    });
  });

  group('Phase 2 - 2d: sibling isolation', () {
    test('sibling files do NOT see each other types', () async {
      final tempDir = await createTempHierarchy({
        'self.glp': 'SharedType ::= a ; b.',
        'agent.glp': 'AgentType ::= x ; y.\nprocedure agent(SharedType?, AgentType?).\nagent(S, A) :- true | true.',
        'mediator.glp': 'procedure mediator(SharedType?).\nmediator(S) :- true | true.',
      });

      try {
        // Build scope for mediator.glp
        final mediatorChain = discoverSelfChain(
          targetFile: '${tempDir.path}/mediator.glp',
          rootDir: tempDir.path,
        );
        final mediatorSource = await File('${tempDir.path}/mediator.glp').readAsString();
        final mediatorModule = parseModule(mediatorSource);
        final mediatorEnv = assembleTypeScope(chain: mediatorChain, module: mediatorModule);

        // mediator.glp should see SharedType from self.glp
        expect(mediatorEnv.hasType('SharedType'), isTrue);
        // mediator.glp should NOT see AgentType from agent.glp (sibling)
        expect(mediatorEnv.hasType('AgentType'), isFalse);
      } finally {
        await tempDir.delete(recursive: true);
      }
    });
  });

  group('Phase 2 - 2e: type-only self.glp', () {
    test('self.glp with only type definitions (no procedures) provides types', () async {
      final tempDir = await createTempHierarchy({
        'self.glp': 'Response ::= accept(Channel) ; no.\nAgentContent ::= befriend(Constant, Response?).',
        'module.glp': 'procedure foo(Response?, AgentContent?).\nfoo(R, A) :- true | true.',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/module.glp',
          rootDir: tempDir.path,
        );
        final moduleSource = await File('${tempDir.path}/module.glp').readAsString();
        final module = parseModule(moduleSource);
        final env = assembleTypeScope(chain: chain, module: module);

        // Types from type-only self.glp should be visible
        expect(env.hasType('Response'), isTrue);
        expect(env.hasType('AgentContent'), isTrue);
      } finally {
        await tempDir.delete(recursive: true);
      }
    });
  });

  group('Phase 2 - 2f: prelude as root ancestor', () {
    test('prelude types are always visible even without any self.glp', () async {
      final tempDir = await createTempHierarchy({
        'module.glp': 'procedure foo(Integer?, Constant).\nfoo(N, C) :- true | true.',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/module.glp',
          rootDir: tempDir.path,
        );
        final moduleSource = await File('${tempDir.path}/module.glp').readAsString();
        final module = parseModule(moduleSource);
        final env = assembleTypeScope(chain: chain, module: module);

        // Prelude types should always be available
        // Stream and Channel are now parametric: Stream(X), Channel(In, Out)
        expect(env.hasType('Integer'), isTrue);
        expect(env.hasType('Constant'), isTrue);
        // Prelude procedures should be available
        expect(env.hasProcedure('constant', 1), isTrue);
      } finally {
        await tempDir.delete(recursive: true);
      }
    });
  });

  group('Phase 2 - 2g: procedure declarations from ancestor self.glp', () {
    test('exported procedure declarations in self.glp are visible to descendants', () async {
      final tempDir = await createTempHierarchy({
        'self.glp': 'exported procedure shared_proc(Integer?, Integer).\nshared_proc(N, R) :- true | R := N?.',
        'sub/module.glp': 'procedure compute(Integer?, Integer).\ncompute(N, R) :- true | shared_proc(N?, R).',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/sub/module.glp',
          rootDir: tempDir.path,
        );
        final moduleSource = await File('${tempDir.path}/sub/module.glp').readAsString();
        final module = parseModule(moduleSource);
        final env = assembleTypeScope(chain: chain, module: module);

        // shared_proc from self.glp should be visible in descendant's scope
        expect(env.hasProcedure('shared_proc', 2), isTrue);
        final proc = env.getProcedure('shared_proc', 2);
        expect(proc, isNotNull);
        expect(proc!.exported, isTrue);
      } finally {
        await tempDir.delete(recursive: true);
      }
    });

    test('plain procedure declarations in self.glp are visible to descendants', () async {
      final tempDir = await createTempHierarchy({
        'self.glp': 'procedure helper(Integer?, Integer).\nhelper(N, R) :- true | R := N?.',
        'module.glp': 'procedure compute(Integer?, Integer).\ncompute(N, R) :- true | helper(N?, R).',
      });

      try {
        final chain = discoverSelfChain(
          targetFile: '${tempDir.path}/module.glp',
          rootDir: tempDir.path,
        );
        final moduleSource = await File('${tempDir.path}/module.glp').readAsString();
        final module = parseModule(moduleSource);
        final env = assembleTypeScope(chain: chain, module: module);

        // plain procedures from self.glp are also visible (ancestor scoping)
        expect(env.hasProcedure('helper', 2), isTrue);
      } finally {
        await tempDir.delete(recursive: true);
      }
    });
  });
}
