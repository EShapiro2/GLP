/// Multi-isolate test using project-compiled modules.
///
/// Tests that cssg_modules/ can be loaded via loadProject() in each
/// agent isolate, with madGLP boot procedures layered on top.
/// Runs CSSG play 4 (all accept child intro) with 4 agents.

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// madGLP boot source for CSSG play 4 using project-compiled modules.
///
/// The linked project provides: agent/4, ui_mediator/5, merge/3, tee/3,
/// send_to_user_tagged/3, and all actor procedures (alice4, bob4, carol4, dave4).
///
/// This boot source provides: the @-directive boot clause,
/// parent_init/4, child_init/3, extract_child_stream/2, and ui_actor/3 dispatch.
const cssgPlay4BootSource = r'''
-mode(system).

%% =============================================================================
%% BOOT - Spawns agents on separate isolates (CSSG play 4)
%% =============================================================================

procedure boot.
boot :-
    parent_init(alice, carol, 4, _)@alice,
    parent_init(bob, dave, 4, _)@bob,
    child_init(carol, 4, _)@carol,
    child_init(dave, 4, _)@dave.

%% =============================================================================
%% EXTRACT_CHILD_STREAM — extract stream from accept(Stream) response
%% =============================================================================

extract_child_stream(accept(Stream), Stream?).

%% =============================================================================
%% PARENT INIT (CSSG)
%% =============================================================================

parent_init(Id, Child, PlayNum, NetIn) :-
    ground(Id?), ground(Child?), ground(PlayNum?) |
    send_to_net([msg(Child?, parent_connect(Id?, ParentToChild?, Response))
                 | AgentNetOut?]),
    extract_child_stream(Response?, ChildToParent),
    merge(ChildToParent?, NetIn?, AgentNetIn),
    agent(Id?, AgentIn?, AgentNetIn?,
          [output('_user', AgentToUser),
           output('_net', AgentNetOut),
           output(child(Child?), ParentToChild)]),
    ui_mediator(Id?, ch(AgentToUser?, AgentIn),
                ch(MedIn?, MedOut), [], 1),
    ui_actor(Id?, PlayNum?, ch(ActorIn?, ActorOut)),
    tee(ActorOut?, MedIn, DispCmd),
    tee(MedOut?, ActorIn, DispNotify),
    send_to_user_tagged(Id?, DispCmd?, DispNotify?).

%% =============================================================================
%% CHILD INIT (CSSG)
%% =============================================================================

child_init(Id, PlayNum,
           [parent_connect(Parent, ParentToChild, accept(ChildToParent?)) | NetInRest]) :-
    ground(Id?), ground(PlayNum?), ground(Parent?) |
    merge(ParentToChild?, NetInRest?, AgentNetIn),
    agent(Id?, AgentIn?, AgentNetIn?,
          [output('_user', AgentToUser),
           output(child(Parent?), ChildToParent)]),
    ui_mediator(Id?, ch(AgentToUser?, AgentIn),
                ch(MedIn?, MedOut), [], 1),
    ui_actor(Id?, PlayNum?, ch(ActorIn?, ActorOut)),
    tee(ActorOut?, MedIn, DispCmd),
    tee(MedOut?, ActorIn, DispNotify),
    send_to_user_tagged(Id?, DispCmd?, DispNotify?).

%% =============================================================================
%% UI_ACTOR DISPATCH — selects actor for (agent, play) pair
%% =============================================================================

ui_actor(alice, 4, Ch) :- alice4(Ch?).
ui_actor(bob, 4, Ch) :- bob4(Ch?).
ui_actor(carol, 4, Ch) :- carol4(Ch?).
ui_actor(dave, 4, Ch) :- dave4(Ch?).
''';

void main() {
  final projectDir = '../programs/cssg_modules';

  if (!Directory(projectDir).existsSync()) {
    print('cssg_modules not found at $projectDir, skipping tests');
    return;
  }

  group('Multi-isolate with project-compiled modules', () {
    late IsolateManager manager;

    setUp(() {
      manager = IsolateManager();
    });

    tearDown(() async {
      await manager.shutdown();
    });

    test('boots CSSG play 4 with project-linked modules', () async {
      final loader = BootLoader();
      final config = loader.load(cssgPlay4BootSource);
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
      config.projectDir = projectDir;

      // Verify directives parsed correctly
      expect(config.directives.length, equals(4));
      expect(
          config.directives.map((d) => d.agentId).toSet(),
          equals({'alice', 'bob', 'carol', 'dave'}));

      // Boot all 4 agents
      await manager.boot(config,
          traceConfig: TraceConfig(glp: false, mad: true));

      // Start and let the protocol run
      manager.start();
      await Future.delayed(Duration(seconds: 5));

      // If we reach here without crash, boot + project loading + execution works
    }, timeout: Timeout(Duration(seconds: 30)));
  });
}
