/// The boot clause (IGLP, Implementation Notes, "Boot"): each body goal
/// `agent(ch(_?,_),ch(_?,_))@p` declares the agent p, every agent's initial
/// goal being the same (GLP-Spec maglp.tex, Definition "Multiagent GLP"), and
/// a second goal `P@p` names the person scripted in GLP at p.  Until
/// 2026-11-02 a boot clause's goals were `G(p, ..., _)@p`, each agent's own
/// entry, and these tests held that form.
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';

void main() {
  group('BootLoader', () {
    late BootLoader loader;

    setUp(() {
      loader = BootLoader();
    });

    group('valid boot files', () {
      test('parses three agents, each with a scripted person', () {
        final source = '''
procedure boot.
boot :-
    agent(ch(_?, _), ch(_?, _))@alice,
    say(alice, ch(_?, _))@alice,
    agent(ch(_?, _), ch(_?, _))@bob,
    say(bob, ch(_?, _))@bob,
    agent(ch(_?, _), ch(_?, _))@charlie,
    say(charlie, ch(_?, _))@charlie.
''';

        final config = loader.load(source);

        expect(config.directives.map((d) => d.agentId).toList(),
            equals(['alice', 'bob', 'charlie']));
        for (final d in config.directives) {
          expect(d.person!.functor, equals('say'));
          expect(d.person!.text('O', 'I'), equals('say(${d.agentId}, ch(O?, I))'));
        }
      });

      test('an agent with no scripted person', () {
        final source = '''
procedure boot.
boot :- agent(ch(_?, _), ch(_?, _))@solo.
''';

        final config = loader.load(source);

        expect(config.directives.length, equals(1));
        expect(config.directives[0].agentId, equals('solo'));
        expect(config.directives[0].person, isNull);
      });

      test('the scripted person takes ground arguments around its channel', () {
        final source = '''
procedure boot.
boot :-
    agent(ch(_?, _), ch(_?, _))@alice,
    person(caller(bob), ch(_?, _), [1, two])@alice.
''';

        final person = loader.load(source).directives.single.person!;
        expect(person.channelIndex, equals(1));
        expect(person.text('O', 'I'),
            equals('person(caller(bob), ch(O?, I), [1|[two|[]]])'));
      });

      test('handles comments in source', () {
        final source = '''
%% This is a comment
procedure boot.
%% Another comment
boot :-
    %% Comment in middle
    agent(ch(_?, _), ch(_?, _))@alice,
    agent(ch(_?, _), ch(_?, _))@bob.

%% More comments
procedure agent_init(_?, Channel?).
''';

        final config = loader.load(source);

        expect(config.directives.map((d) => d.agentId).toList(),
            equals(['alice', 'bob']));
      });

      test('handles flexible whitespace', () {
        final source = '''
procedure  boot .
boot:-agent( ch( _? , _ ) , ch(_?,_) ) @ alice.
''';

        final config = loader.load(source);

        expect(config.directives.single.agentId, equals('alice'));
      });

      test('preserves full source and strips boot clause', () {
        final source = '''
procedure boot.
boot :- agent(ch(_?, _), ch(_?, _))@a.

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
      Matcher refused(String text) => throwsA(isA<BootLoaderException>()
          .having((e) => e.message, 'message', contains(text)));

      test('throws if no procedure boot declaration', () {
        expect(() => loader.load('boot :- agent(ch(_?, _), ch(_?, _))@a.\n'),
            refused('First procedure must be boot/0'));
      });

      test('throws if no boot clause', () {
        final source = '''
procedure boot.

procedure agent(_?, Channel?).
agent(Id, Ch) :- true.
''';
        expect(() => loader.load(source), refused('Could not find boot clause'));
      });

      test('an agent goal other than agent(ch(_?,_),ch(_?,_)) is refused', () {
        final source = '''
procedure boot.
boot :- agent(ch(_, _), ch(_?, _))@alice.
''';
        expect(() => loader.load(source),
            refused("every agent's initial goal is agent(ch(_?,_),ch(_?,_))"));
      });

      test('throws if duplicate agent IDs', () {
        final source = '''
procedure boot.
boot :-
    agent(ch(_?, _), ch(_?, _))@alice,
    agent(ch(_?, _), ch(_?, _))@alice.
''';
        expect(() => loader.load(source), refused('Duplicate agent ID: alice'));
      });

      test('two scripted persons at one agent are refused', () {
        final source = '''
procedure boot.
boot :-
    agent(ch(_?, _), ch(_?, _))@alice,
    say(a, ch(_?, _))@alice,
    say(b, ch(_?, _))@alice.
''';
        expect(() => loader.load(source), refused('has a scripted person already'));
      });

      test('a scripted person at an agent the clause does not declare is refused',
          () {
        final source = '''
procedure boot.
boot :-
    agent(ch(_?, _), ch(_?, _))@alice,
    say(b, ch(_?, _))@bob.
''';
        expect(() => loader.load(source), refused('declares no agent bob'));
      });

      test('a scripted person needs one channel argument, the rest ground', () {
        expect(
            () => loader.load('procedure boot.\nboot :- agent(ch(_?, _), '
                'ch(_?, _))@a, say(a, _)@a.\n'),
            refused('this goal has 0'));
        expect(
            () => loader.load('procedure boot.\nboot :- agent(ch(_?, _), '
                'ch(_?, _))@a, say(X, ch(_?, _))@a.\n'),
            refused('is not ground'));
        // Two channel arguments: the person has one channel (IGLP 8bec699,
        // Implementation Notes, "Boot": "whose one channel argument, written
        // ch(_?,_), is the person's end of p's person channel, its other
        // arguments ground").
        expect(
            () => loader.load('procedure boot.\nboot :- agent(ch(_?, _), '
                'ch(_?, _))@a, say(ch(_?, _), ch(_?, _))@a.\n'),
            refused('this goal has 2'));
        // A channel of the wrong polarity is no ch(_?,_), and not ground.
        expect(
            () => loader.load('procedure boot.\nboot :- agent(ch(_?, _), '
                'ch(_?, _))@a, say(a, ch(_, _?))@a.\n'),
            refused('this goal has 0'));
      });

      test('a goal that is no G@p is refused', () {
        final source = '''
procedure boot.
boot :- agent(ch(_?, _), ch(_?, _))@a, true.
''';
        expect(() => loader.load(source), refused('is no goal G@p'));
      });

      test('throws if no spawn directives in boot', () {
        expect(() => loader.load('procedure boot.\nboot :- true.\n'),
            refused('is no goal G@p'));
      });
    });
  });
}
