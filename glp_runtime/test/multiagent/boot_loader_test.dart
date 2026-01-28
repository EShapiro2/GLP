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
    agent_init(alice, ch(_?,_), ch(_?,_))@alice,
    agent_init(bob, ch(_?,_), ch(_?,_))@bob,
    agent_init(charlie, ch(_?,_), ch(_?,_))@charlie.

procedure agent_init(_?, Channel?, Channel?).
agent_init(Id, UserCh, NetCh) :- true.
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
    ping_agent(alice, ch(_?,_), ch(_?,_))@alice,
    pong_agent(bob, ch(_?,_), ch(_?,_))@bob.
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
boot :- agent(solo, ch(_?,_), ch(_?,_))@solo.
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
    agent_init(alice, ch(_?,_), ch(_?,_))@alice,
    agent_init(bob, ch(_?,_), ch(_?,_))@bob.

%% More comments
procedure agent_init(_?, Channel?, Channel?).
''';

        final config = loader.load(source);

        expect(config.directives.length, equals(2));
        expect(config.directives[0].agentId, equals('alice'));
        expect(config.directives[1].agentId, equals('bob'));
      });

      test('handles flexible whitespace', () {
        final source = '''
procedure  boot .
boot:-agent_init( alice , ch( _? , _ ) , ch( _? , _ ) ) @ alice.
''';

        final config = loader.load(source);

        expect(config.directives.length, equals(1));
        expect(config.directives[0].agentId, equals('alice'));
      });

      test('preserves full source in config', () {
        final source = '''
procedure boot.
boot :- agent(a, ch(_?,_), ch(_?,_))@a.

procedure agent(_?, Channel?, Channel?).
agent(Id, U, N) :- true.
''';

        final config = loader.load(source);

        expect(config.source, equals(source));
      });
    });

    group('error cases', () {
      test('throws if no procedure boot declaration', () {
        final source = '''
boot :- agent(a, ch(_?,_), ch(_?,_))@a.
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

procedure agent(_?, Channel?, Channel?).
agent(Id, U, N) :- true.
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

      test('throws if agent ID mismatch', () {
        final source = '''
procedure boot.
boot :- agent(alice, ch(_?,_), ch(_?,_))@bob.
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
    agent(alice, ch(_?,_), ch(_?,_))@alice,
    other_agent(alice, ch(_?,_), ch(_?,_))@alice.
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
      test('parses play_alice_bob_charlie_boot.glp content', () {
        // Simulating the actual file content
        final source = '''
%% play_alice_bob_charlie.glp - Cold call + messaging + Friend-mediated introduction

procedure boot.
boot :-
    agent_init(alice, ch(_?,_), ch(_?,_))@alice,
    agent_init(bob, ch(_?,_), ch(_?,_))@bob,
    agent_init(charlie, ch(_?,_), ch(_?,_))@charlie. 

%% TYPE DEFINITIONS
Response ::= accept(Channel) ; no.

procedure agent_init(_?, Channel?, Channel?).
agent_init(Id, ch(UserIn, UserOut?), ch(NetIn, NetOut?)) :-
    merge(UserIn?, NetIn?, In),
    agent(Id?, In?, [friend(user, UserOut), friend(net, NetOut)]).
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
