import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';

void main() {
  group('BootLoader', () {
    late BootLoader loader;

    setUp(() {
      loader = BootLoader();
    });

    group('valid boot files', () {
      test('parses three-agent boot clause', () {
        final source = '''
procedure boot.
boot :-
    agent_init(alice, _)@alice,
    agent_init(bob, _)@bob,
    agent_init(charlie, _)@charlie.

procedure agent_init(_?, Channel?).
agent_init(Id, Ch) :- true.
''';

        final config = loader.load(source);

        expect(config.directives.length, equals(3));
        expect(config.directives[0].agentId, equals('alice'));
        expect(config.directives[0].goalFunctor, equals('agent_init'));
        expect(config.directives[1].agentId, equals('bob'));
        expect(config.directives[1].goalFunctor, equals('agent_init'));
        expect(config.directives[2].agentId, equals('charlie'));
        expect(config.directives[2].goalFunctor, equals('agent_init'));
      });

      test('parses two-agent boot with different functors', () {
        final source = '''
procedure boot.
boot :-
    ping_agent(alice, _)@alice,
    pong_agent(bob, _)@bob.
''';

        final config = loader.load(source);

        expect(config.directives.length, equals(2));
        expect(config.directives[0].agentId, equals('alice'));
        expect(config.directives[0].goalFunctor, equals('ping_agent'));
        expect(config.directives[1].agentId, equals('bob'));
        expect(config.directives[1].goalFunctor, equals('pong_agent'));
      });

      test('parses single-agent boot', () {
        final source = '''
procedure boot.
boot :- agent(solo, _)@solo.
''';

        final config = loader.load(source);

        expect(config.directives.length, equals(1));
        expect(config.directives[0].agentId, equals('solo'));
        expect(config.directives[0].goalFunctor, equals('agent'));
      });

      test('handles comments in source', () {
        final source = '''
%% This is a comment
procedure boot.
%% Another comment
boot :-
    %% Comment in middle
    agent_init(alice, _)@alice,
    agent_init(bob, _)@bob.

%% More comments
procedure agent_init(_?, Channel?).
''';

        final config = loader.load(source);

        expect(config.directives.length, equals(2));
        expect(config.directives[0].agentId, equals('alice'));
        expect(config.directives[1].agentId, equals('bob'));
      });

      test('handles flexible whitespace', () {
        final source = '''
procedure  boot .
boot:-agent_init( alice , _ ) @ alice.
''';

        final config = loader.load(source);

        expect(config.directives.length, equals(1));
        expect(config.directives[0].agentId, equals('alice'));
      });

      test('preserves full source and strips boot clause', () {
        final source = '''
procedure boot.
boot :- agent(a, _)@a.

procedure agent(_?, Channel?).
agent(Id, Ch) :- true.
''';

        final config = loader.load(source);

        // fullSource preserves original
        expect(config.fullSource, equals(source));

        // source is stripped (no boot clause, no @ character)
        expect(config.source, isNot(contains('@')));
        expect(config.source, isNot(contains('procedure boot')));
        expect(config.source, contains('procedure agent'));
      });
    });

    group('error cases', () {
      test('throws if no procedure boot declaration', () {
        final source = '''
boot :- agent(a, _)@a.
''';

        expect(
          () => loader.load(source),
          throwsA(isA<BootLoaderException>().having(
            (e) => e.message,
            'message',
            contains('First procedure must be boot/0'),
          )),
        );
      });

      test('throws if no boot clause', () {
        final source = '''
procedure boot.

procedure agent(_?, Channel?).
agent(Id, Ch) :- true.
''';

        expect(
          () => loader.load(source),
          throwsA(isA<BootLoaderException>().having(
            (e) => e.message,
            'message',
            contains('Could not find boot clause'),
          )),
        );
      });

      test('throws if agent ID mismatch', () {
        final source = '''
procedure boot.
boot :- agent(alice, _)@bob.
''';

        expect(
          () => loader.load(source),
          throwsA(isA<BootLoaderException>().having(
            (e) => e.message,
            'message',
            contains('Agent ID mismatch'),
          )),
        );
      });

      test('throws if duplicate agent IDs', () {
        final source = '''
procedure boot.
boot :-
    agent(alice, _)@alice,
    other_agent(alice, _)@alice.
''';

        expect(
          () => loader.load(source),
          throwsA(isA<BootLoaderException>().having(
            (e) => e.message,
            'message',
            contains('Duplicate agent ID: alice'),
          )),
        );
      });

      test('throws if no spawn directives in boot', () {
        final source = '''
procedure boot.
boot :- true.
''';

        expect(
          () => loader.load(source),
          throwsA(isA<BootLoaderException>().having(
            (e) => e.message,
            'message',
            contains('no spawn directives'),
          )),
        );
      });
    });

    group('real file content', () {
      test('parses play_alice_bob_charlie_actor_boot.glp content', () {
        // Simulating the actual file content
        final source = '''
%% play_alice_bob_charlie_actor_boot.glp - Actor-driven test with ui_agent layer

procedure boot.
boot :-
    agent_init(alice, _)@alice,
    agent_init(bob, _)@bob,
    agent_init(charlie, _)@charlie.

%% TYPE DEFINITIONS
Response ::= accept(Channel) ; no.

procedure agent_init(Constant?, Channel).
agent_init(Id, ch(NetOut?, NetIn)) :-
    ground(Id?), new_channel(ch(UserIn, UserOut?), ActorCh) |
    ui_agent_actor(Id?, ActorCh?),
    merge(UserIn?, NetIn?, In),
    agent(Id?, In?, [output('_user', UserOut), output('_net', NetOut)]).
''';

        final config = loader.load(source);

        expect(config.directives.length, equals(3));
        expect(config.directives.map((d) => d.agentId).toList(),
            equals(['alice', 'bob', 'charlie']));
        expect(config.directives.every((d) => d.goalFunctor == 'agent_init'),
            isTrue);
      });
    });
  });
}
